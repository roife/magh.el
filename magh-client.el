;;; magh-client.el --- Asynchronous GitHub CLI transport -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; Author: magh.el contributors
;; Keywords: tools, vc, github

;;; Commentary:

;; This is the only magh.el module which starts the `gh' executable.  It keeps
;; stdout and stderr separate, preserves argv boundaries, implements typed
;; errors, successful-read caching, in-flight de-duplication, cancellation,
;; streaming, and interactive PTY processes.

;;; Code:

(require 'cl-lib)
(require 'comint)
(require 'json)
(require 'subr-x)
(require 'magh-core)

(defcustom magh-client-cache-ttl 30
  "Seconds a successful read remains in the magh.el query cache."
  :type 'number
  :group 'magh)

(defcustom magh-client-cache-max-entries 256
  "Soft maximum number of completed entries in the magh.el query cache."
  :type 'natnum
  :group 'magh)

(cl-defstruct (magh-client--subscriber
               (:constructor magh-client--subscriber-create))
  success error buffer)

(cl-defstruct (magh-client--cache-entry
               (:constructor magh-client--cache-entry-create))
  value created domain)

(cl-defstruct (magh-client--request
               (:constructor magh-client--request-create))
  process stdout-buffer stderr-buffer key cache-key domain subscribers
  json json-false-object stream cancelled)

(defvar magh-client--cache (make-hash-table :test #'equal)
  "Completed successful read cache.")

(defvar magh-client--inflight (make-hash-table :test #'equal)
  "Map normalized request keys to active `magh-client--request' objects.")

(defvar magh-client--account-generation 0
  "Generation included in cache keys and incremented after account switches.")

(defun magh-client--executable ()
  "Return the absolute GitHub CLI executable or signal a typed error."
  (or (executable-find magh-executable)
      (signal 'magh-missing-executable
              (list (format "Cannot find GitHub CLI executable `%s'"
                            magh-executable)))))

(defun magh-client--environment (context)
  "Return a private process environment for CONTEXT."
  (let ((process-environment (copy-sequence process-environment))
        (host (magh-context-host context)))
    (setenv "NO_COLOR" "1")
    (setenv "CLICOLOR" "0")
    (setenv "GH_PAGER" "cat")
    (setenv "PAGER" "cat")
    (when host
      (setenv "GH_HOST" host))
    process-environment))

(defun magh-client--cache-key (argv context json false-object)
  "Build a cache key for ARGV, CONTEXT, and JSON parsing options."
  (list :account-generation magh-client--account-generation
        :host (magh-context-host context)
        :repository (magh-context-repository context)
        :ref (magh-context-ref context)
        :path (magh-context-path context)
        :output (if json (list 'json false-object) 'text)
        :argv argv))

(defun magh-client--subscriber-live-p (subscriber)
  "Return non-nil if SUBSCRIBER can still receive a callback."
  (buffer-live-p (magh-client--subscriber-buffer subscriber)))

(defun magh-client--deliver-success (subscriber value)
  "Deliver VALUE to SUBSCRIBER if its source buffer is live."
  (when (magh-client--subscriber-live-p subscriber)
    (with-current-buffer (magh-client--subscriber-buffer subscriber)
      (funcall (magh-client--subscriber-success subscriber) value))))

(defun magh-client--deliver-error (subscriber error)
  "Deliver typed ERROR to SUBSCRIBER if its source buffer is live."
  (when (magh-client--subscriber-live-p subscriber)
    (with-current-buffer (magh-client--subscriber-buffer subscriber)
      (funcall (magh-client--subscriber-error subscriber) error))))

(defun magh-client--redact (text)
  "Remove token-like values from diagnostic TEXT."
  (let ((case-fold-search nil))
    (replace-regexp-in-string
     "\\b\\(?:gh[pousr]_[A-Za-z0-9_]+\\|github_pat_[A-Za-z0-9_]+\\)\\b"
     "<redacted>" text t t)))

(defun magh-client--read-buffer (buffer)
  "Return BUFFER contents without text properties."
  (if (buffer-live-p buffer)
      (with-current-buffer buffer
        (buffer-substring-no-properties (point-min) (point-max)))
    ""))

(defun magh-client--parse-json (text &optional false-object)
  "Parse JSON TEXT, representing false with FALSE-OBJECT."
  (condition-case error
      (if (string-blank-p text)
          nil
        (json-parse-string text
                           :object-type 'alist
                           :array-type 'list
                           :null-object nil
                           ;; Resource pages use nil; generic API pages opt in
                           ;; to `:json-false' so false and null stay distinct.
                           :false-object false-object))
    (json-parse-error
     (signal 'magh-json-error
             (list (format "Invalid JSON returned by GitHub CLI: %s"
                           (error-message-string error)))))))

(defun magh-client--cache-trim ()
  "Trim oldest completed cache entries to the configured soft maximum."
  (when (> (hash-table-count magh-client--cache)
           magh-client-cache-max-entries)
    (let (entries)
      (maphash (lambda (key value)
                 (push (cons key (magh-client--cache-entry-created value)) entries))
               magh-client--cache)
      (setq entries (sort entries (lambda (a b) (< (cdr a) (cdr b)))))
      (dolist (entry (take (- (length entries) magh-client-cache-max-entries)
                           entries))
        (remhash (car entry) magh-client--cache)))))

(defun magh-client--finish (request process)
  "Finish REQUEST after PROCESS exits."
  (let* ((status (process-exit-status process))
         (stdout (magh-client--read-buffer
                  (magh-client--request-stdout-buffer request)))
         (stderr (string-trim
                  (magh-client--redact
                   (magh-client--read-buffer
                    (magh-client--request-stderr-buffer request)))))
         (subscribers (reverse (magh-client--request-subscribers request)))
         result error)
    (remhash (magh-client--request-key request) magh-client--inflight)
    (cond
     ((magh-client--request-cancelled request)
      (setq error (magh-core--error 'magh-cancelled "GitHub request was cancelled")))
     ((not (zerop status))
      (setq error
            (magh-core--error
             (if (= status 4) 'magh-auth-error 'magh-command-error)
             (if (string-empty-p stderr)
                 (format "GitHub CLI exited with status %d" status)
               stderr)
             (list :exit-code status))))
     (t
      (condition-case parse-error
          (setq result
                (if (magh-client--request-json request)
                    (magh-client--parse-json
                     stdout (magh-client--request-json-false-object request))
                  stdout))
        (magh-error (setq error parse-error)))))
    (unwind-protect
        (if error
            (dolist (subscriber subscribers)
              (magh-client--deliver-error subscriber error))
          (when-let* ((key (magh-client--request-cache-key request)))
            (puthash key
                     (magh-client--cache-entry-create
                      :value result :created (float-time)
                      :domain (magh-client--request-domain request))
                     magh-client--cache)
            (magh-client--cache-trim))
          (dolist (subscriber subscribers)
            (magh-client--deliver-success subscriber result)))
      (dolist (buffer (list (magh-client--request-stdout-buffer request)
                            (magh-client--request-stderr-buffer request)))
        ;; Stream callbacks receive REQUEST and may have killed its buffers.
        (when (buffer-live-p buffer)
          (kill-buffer buffer))))))

(defun magh-client--sentinel (process _event)
  "Process sentinel used for all non-PTY GitHub CLI requests."
  (when (memq (process-status process) '(exit signal))
    (when-let* ((request (process-get process 'magh-client-request)))
      ;; Both the real sentinel and the fast-exit fallback may run.  Claim the
      ;; request exactly once before invoking user callbacks.
      (process-put process 'magh-client-request nil)
      (magh-client--finish request process))))

(defun magh-client--filter (process chunk)
  "Insert PROCESS output CHUNK and forward it to a stream callback."
  (when-let* ((request (process-get process 'magh-client-request)))
    (let ((buffer (magh-client--request-stdout-buffer request)))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (goto-char (point-max))
          (insert chunk))))
    (when-let* ((stream (magh-client--request-stream request)))
      (funcall stream chunk request))))

(cl-defun magh-client--request-async
    (argv success error
          &key context json (cache t) force domain
          source-buffer (dedupe t) stream stdin json-false-object)
  "Run GitHub CLI ARGV asynchronously.
SUCCESS receives parsed JSON or text.  ERROR receives a typed condition list.
CONTEXT controls host and cache scope.  Successful reads are cached when
CACHE is non-nil.  FORCE bypasses completed cache while retaining force-request
de-duplication.  DOMAIN is metadata used for selective invalidation.  Callbacks
are skipped after SOURCE-BUFFER dies.  STREAM receives each stdout chunk and
the request object.  STDIN, when non-nil, is sent verbatim then closed.
JSON-FALSE-OBJECT controls the parsed representation of JSON false."
  (let* ((context (or context (magh-context-create :host magh-host)))
         (subscriber
          (magh-client--subscriber-create
           :success success :error error
           :buffer (or source-buffer (current-buffer))))
         (cache-key (and (or cache dedupe)
                         (magh-client--cache-key
                          argv context json json-false-object)))
         (inflight-key
          (if dedupe
              (list (if force 'force 'normal) cache-key)
            (list 'unique (gensym "magh-request"))))
         (entry (and cache (not force) (gethash cache-key magh-client--cache)))
         (existing (and dedupe (gethash inflight-key magh-client--inflight))))
    (cond
     ((and entry
           (< (- (float-time) (magh-client--cache-entry-created entry))
              magh-client-cache-ttl))
      (magh-core--call-later #'magh-client--deliver-success subscriber
                           (magh-client--cache-entry-value entry))
      nil)
     (existing
      (push subscriber (magh-client--request-subscribers existing))
      existing)
     (t
      (when entry (remhash cache-key magh-client--cache))
      (let ((stdout (generate-new-buffer " *gh stdout*"))
            (stderr (generate-new-buffer " *gh stderr*")))
        (condition-case start-error
            (let* ((request
                    (magh-client--request-create
                     :key inflight-key :cache-key (and cache cache-key)
                     :domain domain :subscribers (list subscriber)
                     :stdout-buffer stdout :stderr-buffer stderr
                     :json json :json-false-object json-false-object
                     :stream stream))
                   (process-environment (magh-client--environment context))
                   (default-directory
                    (or (magh-context-root context) default-directory))
                   (process
                    (make-process
                     :name (format "gh:%s" (car argv))
                     ;; Let Emacs insert any output produced before our filter
                     ;; is installed.  This closes the fast-process race where
                     ;; `gh' can print and exit before `process-put' below.
                     :buffer stdout :stderr stderr
                     :command (cons (magh-client--executable) argv)
                     :connection-type 'pipe :coding 'utf-8-unix
                     :noquery t)))
              (setf (magh-client--request-process request) process)
              (process-put process 'magh-client-request request)
              (puthash inflight-key request magh-client--inflight)
              (set-process-filter process #'magh-client--filter)
              (set-process-sentinel process #'magh-client--sentinel)
              (when stdin
                (process-send-string process stdin)
                (process-send-eof process))
              ;; `set-process-sentinel' does not promise to replay an exit that
              ;; happened before installation.  Schedule the same guarded
              ;; sentinel path when the process was exceptionally fast.
              (when (memq (process-status process) '(exit signal))
                (magh-core--call-later #'magh-client--sentinel process "finished"))
              request)
          (error
           (mapc #'kill-buffer (list stdout stderr))
           (magh-core--call-later
            #'magh-client--deliver-error subscriber
            (if (eq (car start-error) 'magh-missing-executable)
                start-error
              (magh-core--error 'magh-command-error
                              (error-message-string start-error))))
           nil)))))))

(cl-defun magh-client--json-async
    (argv success error &rest keys &key &allow-other-keys)
  "Run ARGV asynchronously and parse JSON; KEYS tune the request."
  (apply #'magh-client--request-async argv success error :json t keys))

(cl-defun magh-client--text-async
    (argv success error &rest keys &key &allow-other-keys)
  "Run ARGV asynchronously as text; KEYS tune the request."
  (apply #'magh-client--request-async argv success error keys))

(cl-defun magh-client--mutate-json
    (argv success error &rest keys &key &allow-other-keys)
  "Run a non-cacheable JSON mutation with ARGV."
  (apply #'magh-client--request-async argv success error
         :json t :cache nil :dedupe nil keys))

(cl-defun magh-client--mutate-text
    (argv success error &rest keys &key &allow-other-keys)
  "Run a non-cacheable text mutation with ARGV."
  (apply #'magh-client--request-async argv success error
         :cache nil :dedupe nil keys))

(cl-defun magh-client--stream
    (argv chunk success error &rest keys &key &allow-other-keys)
  "Run ARGV as a cancellable stream.
CHUNK receives each text chunk and request; SUCCESS receives complete text."
  (apply #'magh-client--request-async argv success error
         :cache nil :dedupe nil :stream chunk keys))

(defun magh-client-cancel (request)
  "Cancel active magh.el REQUEST.
The error callback receives a `magh-cancelled' condition."
  (setf (magh-client--request-cancelled request) t)
  (delete-process (magh-client--request-process request))
  t)

(defun magh-client-invalidate (&optional domain)
  "Invalidate completed request cache entries matching DOMAIN.
DOMAIN is a plist such as (:host HOST :repository REPO :resource issue
:id 42).  With nil, clear all completed entries."
  (if (null domain)
      (clrhash magh-client--cache)
    (maphash
     (lambda (key entry)
       (let ((entry-domain (magh-client--cache-entry-domain entry)))
         (when (and entry-domain
                    (cl-loop for (field value) on domain by #'cddr
                             always (equal (plist-get entry-domain field)
                                           value)))
           (remhash key magh-client--cache))))
     magh-client--cache))
  nil)

(defun magh-client-invalidate-account ()
  "Invalidate all cache data after a GitHub CLI account switch."
  (cl-incf magh-client--account-generation)
  (magh-client-invalidate))

(defun magh-client-clear-cache ()
  "Clear all completed magh.el query cache entries interactively."
  (interactive)
  (magh-client-invalidate)
  (message "magh.el query cache cleared"))

(defun magh-client-cache-size ()
  "Return the number of completed cached magh.el requests."
  (hash-table-count magh-client--cache))

(defun magh-client-inflight-size ()
  "Return the number of active magh.el requests."
  (hash-table-count magh-client--inflight))

(defun magh-client--start-pty (argv buffer context)
  "Start interactive GitHub CLI ARGV in PTY BUFFER using CONTEXT."
  (let* ((buffer (get-buffer-create buffer))
         (process-environment (magh-client--environment context))
         (process-connection-type t)
         (default-directory
          (or (magh-context-root context) default-directory)))
    (when-let* ((old (get-buffer-process buffer)))
      (delete-process old))
    (with-current-buffer buffer
      (let ((inhibit-read-only t)) (erase-buffer))
      (apply #'make-comint-in-buffer
             "gh" buffer (magh-client--executable) nil argv)
      (comint-mode))
    buffer))

(provide 'magh-client)
;;; magh-client.el ends here
