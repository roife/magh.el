;;; gh-client.el --- Asynchronous GitHub CLI transport -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; Author: gh.el contributors
;; Keywords: tools, vc, github
;; Package-Requires: ((emacs "29.1"))

;;; Commentary:

;; This is the only gh.el module which starts the `gh' executable.  It keeps
;; stdout and stderr separate, preserves argv boundaries, implements typed
;; errors, successful-read caching, in-flight de-duplication, cancellation,
;; streaming, and interactive PTY processes.

;;; Code:

(require 'cl-lib)
(require 'comint)
(require 'json)
(require 'subr-x)
(require 'gh-core)

(defcustom gh-client-cache-ttl 30
  "Seconds a successful read remains in the gh.el query cache."
  :type 'number
  :group 'gh)

(defcustom gh-client-cache-max-entries 256
  "Soft maximum number of completed entries in the gh.el query cache."
  :type 'natnum
  :group 'gh)

(cl-defstruct (gh-client--subscriber
               (:constructor gh-client--subscriber-create))
  success error buffer)

(cl-defstruct (gh-client--cache-entry
               (:constructor gh-client--cache-entry-create))
  value created domain)

(cl-defstruct (gh-client--request
               (:constructor gh-client--request-create))
  process stdout-buffer stderr-buffer key cache-key domain subscribers
  json json-false-object stream cancelled)

(defvar gh-client--cache (make-hash-table :test #'equal)
  "Completed successful read cache.")

(defvar gh-client--inflight (make-hash-table :test #'equal)
  "Map normalized request keys to active `gh-client--request' objects.")

(defvar gh-client--account-generation 0
  "Generation included in cache keys and incremented after account switches.")

(defun gh-client--executable ()
  "Return the absolute GitHub CLI executable or signal a typed error."
  (or (executable-find gh-executable)
      (and (file-executable-p gh-executable) gh-executable)
      (signal 'gh-missing-executable
              (list (format "Cannot find GitHub CLI executable `%s'"
                            gh-executable)))))

(defun gh-client--environment (context)
  "Return a private process environment for CONTEXT."
  (let ((process-environment (copy-sequence process-environment))
        (host (and context (gh-context-host context))))
    (dolist (entry '("NO_COLOR=1" "CLICOLOR=0" "GH_PAGER=cat" "PAGER=cat"))
      (setenv (car (split-string entry "="))
              (string-join (cdr (split-string entry "=")) "=")))
    (when host
      (setenv "GH_HOST" host))
    process-environment))

(defun gh-client--cache-key (argv context json false-object)
  "Build a cache key for ARGV, CONTEXT, and JSON parsing options."
  (list :account-generation gh-client--account-generation
        :host (and context (gh-context-host context))
        :repository (and context (gh-context-repository context))
        :ref (and context (gh-context-ref context))
        :path (and context (gh-context-path context))
        :output (if json (list 'json false-object) 'text)
        :argv (copy-sequence argv)))

(defun gh-client--entry-fresh-p (entry)
  "Return non-nil when cache ENTRY is still fresh."
  (< (- (float-time) (gh-client--cache-entry-created entry))
     gh-client-cache-ttl))

(defun gh-client--subscriber-live-p (subscriber)
  "Return non-nil if SUBSCRIBER can still receive a callback."
  (let ((buffer (gh-client--subscriber-buffer subscriber)))
    (or (null buffer) (buffer-live-p buffer))))

(defun gh-client--deliver-success (subscriber value)
  "Deliver VALUE to SUBSCRIBER if its source buffer is live."
  (when (gh-client--subscriber-live-p subscriber)
    (let ((buffer (gh-client--subscriber-buffer subscriber))
          (function (gh-client--subscriber-success subscriber)))
      (when function
        (if buffer
            (with-current-buffer buffer (funcall function value))
          (funcall function value))))))

(defun gh-client--deliver-error (subscriber error)
  "Deliver typed ERROR to SUBSCRIBER if its source buffer is live."
  (when (gh-client--subscriber-live-p subscriber)
    (let ((buffer (gh-client--subscriber-buffer subscriber))
          (function (gh-client--subscriber-error subscriber)))
      (if function
          (if buffer
              (with-current-buffer buffer (funcall function error))
            (funcall function error))
        (message "gh: %s" (gh-error-message error))))))

(defun gh-client--deliver-cached (subscriber value)
  "Asynchronously deliver cached VALUE to SUBSCRIBER."
  (gh-core--call-later #'gh-client--deliver-success subscriber value))

(defun gh-client--redact (text)
  "Remove token-like values from diagnostic TEXT."
  (when text
    (let ((case-fold-search nil))
      (replace-regexp-in-string
       "\\b\\(?:gh[pousr]_[A-Za-z0-9_]+\\|github_pat_[A-Za-z0-9_]+\\)\\b"
       "<redacted>" text t t))))

(defun gh-client--read-buffer (buffer)
  "Return BUFFER contents without text properties."
  (if (buffer-live-p buffer)
      (with-current-buffer buffer
        (buffer-substring-no-properties (point-min) (point-max)))
    ""))

(defun gh-client--parse-json (text &optional false-object)
  "Parse JSON TEXT, representing false with FALSE-OBJECT."
  (condition-case error
      (if (string-empty-p (string-trim text))
          nil
        (json-parse-string text
                           :object-type 'alist
                           :array-type 'list
                           :null-object nil
                           ;; Resource pages use nil; generic API pages opt in
                           ;; to `:json-false' so false and null stay distinct.
                           :false-object false-object))
    (json-parse-error
     (signal 'gh-json-error
             (list (format "Invalid JSON returned by GitHub CLI: %s"
                           (error-message-string error)))))))

(defun gh-client--cache-trim ()
  "Trim oldest completed cache entries to the configured soft maximum."
  (when (> (hash-table-count gh-client--cache)
           gh-client-cache-max-entries)
    (let (entries)
      (maphash (lambda (key value)
                 (push (cons key (gh-client--cache-entry-created value)) entries))
               gh-client--cache)
      (setq entries (sort entries (lambda (a b) (< (cdr a) (cdr b)))))
      (dotimes (index (- (length entries) gh-client-cache-max-entries))
        (remhash (car (nth index entries)) gh-client--cache)))))

(defun gh-client--finish (request process)
  "Finish REQUEST after PROCESS exits."
  (let* ((status (process-exit-status process))
         (stdout (gh-client--read-buffer
                  (gh-client--request-stdout-buffer request)))
         (stderr (string-trim
                  (gh-client--redact
                   (gh-client--read-buffer
                    (gh-client--request-stderr-buffer request)))))
         (subscribers (reverse (gh-client--request-subscribers request)))
         result error)
    (when (equal (gethash (gh-client--request-key request)
                          gh-client--inflight)
                 request)
      (remhash (gh-client--request-key request) gh-client--inflight))
    (cond
     ((gh-client--request-cancelled request)
      (setq error (gh-core--error 'gh-cancelled "GitHub request was cancelled")))
     ((not (zerop status))
      (setq error
            (gh-core--error
             (if (= status 4) 'gh-auth-error 'gh-command-error)
             (if (string-empty-p stderr)
                 (format "GitHub CLI exited with status %d" status)
               stderr)
             (list :exit-code status))))
     (t
      (condition-case parse-error
          (setq result
                (if (gh-client--request-json request)
                    (gh-client--parse-json
                     stdout (gh-client--request-json-false-object request))
                  stdout))
        (gh-error (setq error parse-error))
        (error
         (setq error
               (gh-core--error 'gh-json-error
                               (error-message-string parse-error)))))))
    (unwind-protect
        (if error
            (dolist (subscriber subscribers)
              (gh-client--deliver-error subscriber error))
          (when-let* ((key (gh-client--request-cache-key request)))
            (puthash key
                     (gh-client--cache-entry-create
                      :value result :created (float-time)
                      :domain (gh-client--request-domain request))
                     gh-client--cache)
            (gh-client--cache-trim))
          (dolist (subscriber subscribers)
            (gh-client--deliver-success subscriber result)))
      (dolist (buffer (list (gh-client--request-stdout-buffer request)
                            (gh-client--request-stderr-buffer request)))
        (when (buffer-live-p buffer)
          (kill-buffer buffer))))))

(defun gh-client--sentinel (process _event)
  "Process sentinel used for all non-PTY GitHub CLI requests."
  (when (memq (process-status process) '(exit signal))
    (when-let* ((request (process-get process 'gh-client-request)))
      ;; Both the real sentinel and the fast-exit fallback may run.  Claim the
      ;; request exactly once before invoking user callbacks.
      (process-put process 'gh-client-request nil)
      (gh-client--finish request process))))

(defun gh-client--filter (process chunk)
  "Insert PROCESS output CHUNK and forward it to a stream callback."
  (when-let* ((request (process-get process 'gh-client-request)))
    (when-let* ((buffer (gh-client--request-stdout-buffer request)))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (goto-char (point-max))
          (insert chunk))))
    (when-let* ((stream (gh-client--request-stream request)))
      (condition-case error
          (funcall stream chunk request)
        (error (message "gh stream callback: %s"
                        (error-message-string error)))))))

(cl-defun gh-client--request-async
    (argv success error
          &key context (json nil) (cache t) (force nil) domain
          source-buffer (dedupe t) stream stdin json-false-object)
  "Run GitHub CLI ARGV asynchronously.
SUCCESS receives parsed JSON or text.  ERROR receives a typed condition list.
CONTEXT controls host and cache scope.  Successful reads are cached when
CACHE is non-nil.  FORCE bypasses completed cache while retaining force-request
de-duplication.  DOMAIN is metadata used for selective invalidation.  Callbacks
are skipped after SOURCE-BUFFER dies.  STREAM receives each stdout chunk and
the request object.  STDIN, when non-nil, is sent verbatim then closed.
JSON-FALSE-OBJECT controls the parsed representation of JSON false."
  (unless (and (listp argv) (cl-every #'stringp argv))
    (signal 'gh-invalid-input (list "GitHub CLI argv must be a list of strings")))
  (let* ((context (or context (gh-context-create :host gh-host)))
         (subscriber
          (gh-client--subscriber-create
           :success success :error error
           :buffer (or source-buffer (current-buffer))))
         (cache-key (gh-client--cache-key argv context json json-false-object))
         (inflight-key
          (if dedupe
              (list (if force 'force 'normal) cache-key)
            (list 'unique (gensym "gh-request"))))
         (entry (and cache (not force) (gethash cache-key gh-client--cache)))
         (existing (and dedupe (gethash inflight-key gh-client--inflight))))
    (cond
     ((and entry (gh-client--entry-fresh-p entry))
      (gh-client--deliver-cached subscriber
                                 (gh-client--cache-entry-value entry))
      nil)
     (existing
      (push subscriber (gh-client--request-subscribers existing))
      existing)
     (t
      (when entry (remhash cache-key gh-client--cache))
      (condition-case start-error
          (let* ((stdout (generate-new-buffer " *gh stdout*"))
                 (stderr (generate-new-buffer " *gh stderr*"))
                 (request
                  (gh-client--request-create
                   :key inflight-key :cache-key (and cache cache-key)
                   :domain domain :subscribers (list subscriber)
                   :stdout-buffer stdout :stderr-buffer stderr
                   :json json :json-false-object json-false-object
                   :stream stream))
                 (process-environment (gh-client--environment context))
                 (default-directory
                  (or (and (gh-context-root context)
                           (file-directory-p (gh-context-root context))
                           (gh-context-root context))
                      default-directory))
                 (process
                  (make-process
                   :name (format "gh:%s" (or (car argv) "request"))
                   ;; Let Emacs insert any output produced before our filter
                   ;; is installed.  This closes the fast-process race where
                   ;; `gh' can print and exit before `process-put' below.
                   :buffer stdout :stderr stderr
                   :command (cons (gh-client--executable) argv)
                   :connection-type 'pipe :coding 'utf-8-unix
                   :noquery t)))
            (setf (gh-client--request-process request) process)
            (process-put process 'gh-client-request request)
            (puthash inflight-key request gh-client--inflight)
            (set-process-filter process #'gh-client--filter)
            (set-process-sentinel process #'gh-client--sentinel)
            (when stdin
              (process-send-string process stdin)
              (process-send-eof process))
            ;; `set-process-sentinel' does not promise to replay an exit that
            ;; happened before installation.  Schedule the same guarded
            ;; sentinel path when the process was exceptionally fast.
            (when (memq (process-status process) '(exit signal))
              (gh-core--call-later #'gh-client--sentinel process "finished"))
            request)
        (gh-error
         (gh-core--call-later #'gh-client--deliver-error subscriber start-error)
         nil)
        (file-missing
         (gh-core--call-later
          #'gh-client--deliver-error subscriber
          (gh-core--error 'gh-missing-executable
                          (error-message-string start-error)))
         nil)
        (error
         (gh-core--call-later
          #'gh-client--deliver-error subscriber
          (gh-core--error 'gh-command-error
                          (error-message-string start-error)))
         nil))))))

(cl-defun gh-client--json-async
    (argv success error &rest keys &key &allow-other-keys)
  "Run ARGV asynchronously and parse JSON; KEYS tune the request."
  (apply #'gh-client--request-async argv success error :json t keys))

(cl-defun gh-client--text-async
    (argv success error &rest keys &key &allow-other-keys)
  "Run ARGV asynchronously as text; KEYS tune the request."
  (apply #'gh-client--request-async argv success error :json nil keys))

(cl-defun gh-client--mutate-json
    (argv success error &rest keys &key &allow-other-keys)
  "Run a non-cacheable JSON mutation with ARGV."
  (apply #'gh-client--request-async argv success error
         :json t :cache nil :dedupe nil keys))

(cl-defun gh-client--mutate-text
    (argv success error &rest keys &key &allow-other-keys)
  "Run a non-cacheable text mutation with ARGV."
  (apply #'gh-client--request-async argv success error
         :json nil :cache nil :dedupe nil keys))

(cl-defun gh-client--stream
    (argv chunk success error &rest keys &key &allow-other-keys)
  "Run ARGV as a cancellable stream.
CHUNK receives each text chunk and request; SUCCESS receives complete text."
  (apply #'gh-client--request-async argv success error
         :json nil :cache nil :dedupe nil :stream chunk keys))

(defun gh-client-cancel (request)
  "Cancel active gh.el REQUEST.
The error callback receives a `gh-cancelled' condition."
  (when (gh-client--request-p request)
    (setf (gh-client--request-cancelled request) t)
    (when-let* ((process (gh-client--request-process request)))
      (when (process-live-p process)
        (delete-process process)))
    t))

(defun gh-client--domain-matches-p (entry-domain target)
  "Return non-nil when ENTRY-DOMAIN is covered by TARGET plist."
  (and entry-domain
       (cl-loop for (key value) on target by #'cddr
                always (equal (plist-get entry-domain key) value))))

(defun gh-client-invalidate (&optional domain)
  "Invalidate completed request cache entries matching DOMAIN.
DOMAIN is a plist such as (:host HOST :repository REPO :resource issue
:id 42).  With nil, clear all completed entries."
  (if (null domain)
      (clrhash gh-client--cache)
    (let (remove)
      (maphash
       (lambda (key entry)
         (when (gh-client--domain-matches-p
                (gh-client--cache-entry-domain entry) domain)
           (push key remove)))
       gh-client--cache)
      (dolist (key remove) (remhash key gh-client--cache))))
  nil)

(defun gh-client-invalidate-account ()
  "Invalidate all cache data after a GitHub CLI account switch."
  (cl-incf gh-client--account-generation)
  (gh-client-invalidate))

(defun gh-client-clear-cache ()
  "Clear all completed gh.el query cache entries interactively."
  (interactive)
  (gh-client-invalidate)
  (message "gh.el query cache cleared"))

(defun gh-client-cache-size ()
  "Return the number of completed cached gh.el requests."
  (hash-table-count gh-client--cache))

(defun gh-client-inflight-size ()
  "Return the number of active de-duplicated gh.el requests."
  (hash-table-count gh-client--inflight))

(cl-defun gh-client--request-sync (argv &key context json stdin)
  "Synchronously run ARGV and return text or parsed JSON.
This low-level helper exists for tests and genuinely immediate local
completion operations.  No resource fetch or page renderer calls it."
  (let ((stdout (generate-new-buffer " *gh sync stdout*"))
        (stderr-file (make-temp-file "gh-sync-stderr-")))
    (unwind-protect
        (let ((process-environment (gh-client--environment context))
              (default-directory
               (or (and context (gh-context-root context)) default-directory))
              status)
          (with-temp-buffer
            (when stdin (insert stdin))
            (setq status
                  (apply #'call-process-region
                         (point-min) (point-max)
                         (gh-client--executable)
                         nil (list stdout stderr-file) nil argv)))
          (unless (zerop status)
            (signal (if (= status 4) 'gh-auth-error 'gh-command-error)
                    (list
                     (with-temp-buffer
                       (insert-file-contents stderr-file)
                       (string-trim (buffer-string))))))
          (let ((text (gh-client--read-buffer stdout)))
            (if json (gh-client--parse-json text) text)))
      (when (buffer-live-p stdout) (kill-buffer stdout))
      (ignore-errors (delete-file stderr-file)))))

(defun gh-client--start-pty (argv buffer &optional context)
  "Start interactive GitHub CLI ARGV in PTY BUFFER using CONTEXT."
  (unless (and (listp argv) (cl-every #'stringp argv))
    (signal 'gh-invalid-input (list "GitHub CLI argv must be a list of strings")))
  (let* ((buffer (get-buffer-create buffer))
         (process-environment (gh-client--environment context))
         (process-connection-type t)
         (default-directory
          (or (and context (gh-context-root context)) default-directory)))
    (when-let* ((old (get-buffer-process buffer)))
      (when (process-live-p old) (delete-process old)))
    (with-current-buffer buffer
      (let ((inhibit-read-only t)) (erase-buffer))
      (apply #'make-comint-in-buffer
             "gh" buffer (gh-client--executable) nil argv)
      (comint-mode))
    buffer))

(provide 'gh-client)
;;; gh-client.el ends here
