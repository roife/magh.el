;;; magh-core.el --- Core types and configuration for magh.el -*- lexical-binding: t; -*-

;; Copyright (C) 2026
;; SPDX-License-Identifier: GPL-2.0-only

;; Author: magh.el contributors
;; Keywords: tools, vc, github

;;; Commentary:

;; This file contains configuration, typed errors, repository contexts, and
;; utilities which do not talk to GitHub.  In particular, resolving a local
;; context only invokes local Git commands; all GitHub I/O lives in
;; `magh-client.el'.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'url-util)

(defgroup magh nil
  "A Magit-like GitHub frontend powered by the GitHub CLI."
  :group 'tools
  :group 'vc
  :prefix "magh-")

(defcustom magh-executable "gh"
  "Name or absolute path of the GitHub CLI executable."
  :type 'file)

(defcustom magh-host nil
  "GitHub host used for requests.
When nil, let the GitHub CLI select its authenticated default host.  A host
parsed from a local Git remote takes precedence for that repository."
  :type '(choice (const :tag "GitHub CLI default" nil) string))

(defcustom magh-list-limit 50
  "Number of items requested per resource-list page.
GitHub APIs with a fixed maximum page size clamp this value to 100."
  :type 'natnum)

(defcustom magh-default-issue-state "open"
  "Default state used by `magh-issue-list'."
  :type '(choice (const "open") (const "closed") (const "all")))

(defcustom magh-default-pr-state "open"
  "Default state used by `magh-pr-list'."
  :type '(choice (const "open") (const "closed")
                 (const "merged") (const "all")))

(defcustom magh-confirm-destructive-actions t
  "Whether destructive GitHub and filesystem actions require confirmation."
  :type 'boolean)

(defcustom magh-favorite-organizations nil
  "Organizations whose repositories are included in favorite repositories."
  :type '(repeat string))

(defcustom magh-known-repositories nil
  "Repository names retained for user-defined shortcuts and Embark actions."
  :type '(repeat string))

(defcustom magh-workflow-template-repositories nil
  "Repositories used as sources for workflow templates."
  :type '(repeat string))

(defcustom magh-view-inline-images nil
  "Whether remote images in rendered GitHub Markdown are fetched asynchronously.
Remote Markdown can reference arbitrary HTTP(S) hosts, so images are disabled
by default to avoid unsolicited network requests."
  :type 'boolean)

(defcustom magh-view-inline-image-max-width 640
  "Maximum display width in pixels for an inline Markdown image."
  :type 'natnum)

(defcustom magh-view-inline-image-max-bytes (* 5 1024 1024)
  "Maximum response size accepted for an inline Markdown image."
  :type 'natnum)

(defcustom magh-view-truncate-lines t
  "Whether native magh.el section pages truncate long lines."
  :type 'boolean)

(defcustom magh-section-initial-visibility-alist
  '((description . show)
    (conversation . show)
    (t . hide))
  "Initial visibility rules for magh.el sections.
This has the same shape as `magit-section-initial-visibility-alist'."
  :type '(alist :key-type (choice symbol string (const t))
                :value-type (choice (const show) (const hide))))

(defcustom magh-section-cache-visibility t
  "Whether section visibility is preserved between magh.el page refreshes.
When a list, preserve only pages whose resource kind is in that list."
  :type '(choice boolean (repeat symbol)))

(defcustom magh-refresh-point-strategy 'section
  "How point is restored after an asynchronous page refresh."
  :type '(choice (const section) (const line) (const start)))

(defcustom magh-date-format-function #'magh-date-relative
  "Function called with an ISO-8601 timestamp to produce display text."
  :type 'function)

(defcustom magh-display-buffer-function #'pop-to-buffer
  "Function used to display permanent magh.el buffers."
  :type 'function)

(defcustom magh-bury-buffer-function #'quit-window
  "Function used to leave a magh.el buffer."
  :type 'function)

(defcustom magh-resource-actions nil
  "Overrides for default actions by resource kind.
Each entry is (KIND . FUNCTION).  FUNCTION receives a resource plist."
  :type '(alist :key-type symbol :value-type function))

(defcustom magh-temporary-clone-directory
  (expand-file-name "magh.el-clones" temporary-file-directory)
  "Directory containing temporary repository clones created by magh.el."
  :type 'directory)

(defcustom magh-download-directory nil
  "Initial directory for release asset downloads.
When nil, use `default-directory'."
  :type '(choice (const nil) directory))

(defcustom magh-search-minimum-input 2
  "Minimum query length before a remote dynamic search starts."
  :type 'natnum)

(defcustom magh-search-debounce 0.25
  "Seconds to wait after search input before starting a request."
  :type 'number)

(defcustom magh-notifications-unread-only t
  "Whether notification selection initially shows only unread threads."
  :type 'boolean)

(defcustom magh-notifications-group-by 'repository
  "Initial grouping used by `magh-notifications'."
  :type '(choice (const repository) (const reason) (const type)
                 (const state) (const date) (const nil)))

(defvar magh-pre-display-buffer-hook nil
  "Hook run before displaying a permanent magh.el buffer.")
(defvar magh-post-display-buffer-hook nil
  "Hook run after displaying a permanent magh.el buffer.")
(defvar magh-pre-refresh-hook nil
  "Hook run before an asynchronous magh.el page refresh.")
(defvar magh-post-refresh-hook nil
  "Hook run after an asynchronous magh.el page refresh completes.")
(defvar magh-repository-post-clone-hook nil
  "Hook run with the clone directory after a repository clone succeeds.")
(defvar magh-repository-post-fork-hook nil
  "Hook run with the repository context after a fork succeeds.")
(defvar magh-auth-post-switch-hook nil
  "Hook run after an interactive GitHub CLI account switch succeeds.")

;;; Errors

(define-error 'magh-error "magh.el error")
(define-error 'magh-missing-executable "GitHub CLI executable is missing" 'magh-error)
(define-error 'magh-auth-error "GitHub CLI authentication or host error" 'magh-error)
(define-error 'magh-command-error "GitHub CLI command failed" 'magh-error)
(define-error 'magh-json-error "GitHub CLI returned invalid JSON" 'magh-error)
(define-error 'magh-api-error "GitHub API request failed" 'magh-error)
(define-error 'magh-cancelled "GitHub request was cancelled" 'magh-error)
(define-error 'magh-invalid-input "Invalid magh.el input" 'magh-error)

(defun magh-core--error (type message &optional data)
  "Return a typed condition list of TYPE with MESSAGE and DATA."
  (list type message data))

(defun magh-error-message (error)
  "Return a user-facing message for typed ERROR."
  (if (and (consp error)
           (memq 'magh-error (get (car error) 'error-conditions))
           (stringp (cadr error)))
      (cadr error)
    (error-message-string error)))

(defun magh-core--user-error (error)
  "Report typed ERROR from an asynchronous command callback."
  (message "magh: %s" (magh-error-message error)))

;;; Aggregate and pagination values

(cl-defstruct (magh-batch-result
               (:constructor magh-batch-result-create (&key values errors)))
  "Values and errors collected from independent asynchronous requests."
  values errors)

(defun magh-batch-value (result key)
  "Return successful KEY value from aggregate RESULT.
Plain alists are accepted for compatibility with renderer callers."
  (alist-get key (if (magh-batch-result-p result)
                     (magh-batch-result-values result)
                   result)))

(defun magh-batch-error (result key)
  "Return KEY error from aggregate RESULT, or nil."
  (and (magh-batch-result-p result)
       (alist-get key (magh-batch-result-errors result))))

(cl-defstruct (magh-page
               (:constructor magh-page-create (&key items next)))
  "One page of ITEMS and an opaque NEXT continuation token."
  items next)

(defun magh-page-append (page continuation)
  "Return PAGE with CONTINUATION items appended and its next token adopted."
  (unless (and (magh-page-p page) (magh-page-p continuation))
    (signal 'wrong-type-argument (list 'magh-page-p
                                      (if (magh-page-p page)
                                          continuation page))))
  (magh-page-create :items (append (magh-page-items page)
                                  (magh-page-items continuation))
                    :next (magh-page-next continuation)))

;;; Context

(cl-defstruct
    (magh-context
     (:constructor magh-context-create
                   (&key host owner name repository root remote branch
                         default-branch ref path)))
  "Structured GitHub repository and navigation context."
  host owner name repository root remote branch default-branch ref path)

(defun magh-context-copy (context &rest overrides)
  "Copy CONTEXT and apply plist OVERRIDES to its slots."
  (let ((copy (copy-magh-context context)))
    (cl-loop for (key value) on overrides by #'cddr
             do (cl-ecase key
                  (:host (setf (magh-context-host copy) value))
                  (:owner (setf (magh-context-owner copy) value))
                  (:name (setf (magh-context-name copy) value))
                  (:repository (setf (magh-context-repository copy) value))
                  (:root (setf (magh-context-root copy) value))
                  (:remote (setf (magh-context-remote copy) value))
                  (:branch (setf (magh-context-branch copy) value))
                  (:default-branch
                   (setf (magh-context-default-branch copy) value))
                  (:ref (setf (magh-context-ref copy) value))
                  (:path (setf (magh-context-path copy) value))))
    (magh-context-normalize copy)))

(defun magh-context-normalize (context)
  "Normalize and return CONTEXT."
  (when-let* ((repo (magh-context-repository context)))
    (setq repo (string-remove-suffix ".git" repo))
    (when (string-match "\\`\\([^/]+\\)/\\([^/]+\\)\\'" repo)
      (setf (magh-context-owner context) (match-string 1 repo)
            (magh-context-name context) (match-string 2 repo))))
  (when (and (magh-context-owner context) (magh-context-name context))
    (setf (magh-context-repository context)
          (format "%s/%s" (magh-context-owner context)
                  (magh-context-name context))))
  (when-let* ((path (magh-context-path context)))
    (setf (magh-context-path context)
          (string-remove-prefix "./" (string-remove-prefix "/" path))))
  context)

(defun magh-core--remote-components (remote)
  "Parse REMOTE into (HOST OWNER NAME), or return nil."
  (when (and remote (not (string-empty-p remote)))
    (let* ((remote (car (split-string remote "[?#]")))
           (remote (string-remove-suffix "/" remote))
           host path)
      (cond
       ((string-match
         "\\`\\(?:https?\\|ssh\\)://\\(?:[^/@]+@\\)?\\([^/]+\\)/\\(.+\\)\\'"
         remote)
        (setq host (match-string 1 remote)
              path (match-string 2 remote)))
       ((string-match "\\`\\(?:[^@]+@\\)?\\([^:]+\\):\\(.+\\)\\'" remote)
        (setq host (match-string 1 remote)
              path (match-string 2 remote))))
      (when (and path
                 (string-match "\\`\\([^/]+\\)/\\([^/]+?\\)\\(?:\\.git\\)?\\'" path))
        (list host (match-string 1 path) (match-string 2 path))))))

(defun magh-core--git-root (&optional directory)
  "Return local Git root for DIRECTORY, without contacting a remote."
  (let ((directory (file-name-as-directory
                    (expand-file-name (or directory default-directory)))))
    (and (not (file-remote-p directory))
         (locate-dominating-file directory ".git"))))

(defun magh-core--git-output (root &rest args)
  "Run a local Git command in ROOT with ARGS and return trimmed output."
  (when-let* ((git (executable-find "git")))
    (with-temp-buffer
      (let ((default-directory root))
        (when (zerop (apply #'process-file git nil t nil args))
          (let ((output (string-trim (buffer-string))))
            (unless (string-empty-p output) output)))))))

(defun magh-core--git-remotes (root)
  "Return local Git remote names configured in ROOT."
  (split-string (or (magh-core--git-output root "remote") "") "\n" t))

(defun magh-core--git-remote-url (root remote)
  "Return fetch URL for REMOTE in ROOT."
  (magh-core--git-output root "remote" "get-url" remote))

(defun magh-core--preferred-remote (root branch &optional requested)
  "Return the best GitHub REMOTE in ROOT for BRANCH.
REQUESTED, when non-nil, is considered before Git's push and upstream
configuration.  Remotes whose URLs cannot identify a GitHub repository are
skipped."
  (let* ((remotes (magh-core--git-remotes root))
         (ordered
          (delq nil
                (append
                 (list requested
                       (and branch
                            (magh-core--git-output
                             root "config" "--get"
                             (format "branch.%s.pushRemote" branch)))
                       (magh-core--git-output
                        root "config" "--get" "remote.pushDefault")
                       (and branch
                            (magh-core--git-output
                             root "config" "--get"
                             (format "branch.%s.remote" branch)))
                       "origin")
                 remotes)))
         candidates)
    ;; `delete-dups' retains later duplicates, which would invert the explicit
    ;; priority above when a requested remote also occurs in REMOTES.
    (dolist (candidate ordered)
      (unless (member candidate candidates)
        (setq candidates (append candidates (list candidate)))))
    (cl-find-if
     (lambda (remote)
       (and (member remote remotes)
            (magh-core--remote-components
             (magh-core--git-remote-url root remote))))
     candidates)))

(defun magh-core--local-context (&optional directory remote)
  "Build a context from local Git metadata under DIRECTORY.
When REMOTE is non-nil, prefer that named Git remote."
  (when-let* ((root (magh-core--git-root directory)))
    (let* ((branch (magh-core--git-output root "branch" "--show-current"))
           (remote-name (magh-core--preferred-remote root branch remote))
           (remote-url (and remote-name
                            (magh-core--git-remote-url root remote-name)))
           (parts (magh-core--remote-components remote-url))
           (head (and remote-name
                      (magh-core--git-output
                       root "symbolic-ref" "--short"
                       (format "refs/remotes/%s/HEAD" remote-name))))
           (default-branch
            (and head
                 (string-remove-prefix (concat remote-name "/") head))))
      (magh-context-normalize
       (magh-context-create :host (or (car parts) magh-host)
                          :owner (nth 1 parts)
                          :name (nth 2 parts)
                          :root root
                          :remote remote-name
                          :branch branch
                          :default-branch default-branch
                          :ref (or branch default-branch))))))

(defun magh-context-from-local-remote (directory remote)
  "Create a local repository context for named REMOTE under DIRECTORY."
  (let* ((root (magh-core--git-root directory))
         (url (and root (member remote (magh-core--git-remotes root))
                   (magh-core--git-remote-url root remote))))
    (unless (and url (magh-core--remote-components url))
      (signal 'magh-invalid-input
              (list (format "Remote `%s' does not identify a GitHub repository"
                            remote))))
    (magh-core--local-context root remote)))

(defun magh-context-local-remotes (&optional context)
  "Return supported local Git remotes for CONTEXT as (NAME . CONTEXT) pairs."
  (setq context (magh-context-resolve context))
  (when-let* ((root (magh-context-root context)))
    (delq nil
          (mapcar
           (lambda (remote)
             (condition-case nil
                 (cons remote (magh-context-from-local-remote root remote))
               (magh-invalid-input nil)))
           (magh-core--git-remotes root)))))

(defun magh-context-from-repository (repository &optional host)
  "Create a context from REPOSITORY and optional HOST.
REPOSITORY can be OWNER/NAME or a supported Git remote URL."
  (unless (and (stringp repository) (not (string-empty-p repository)))
    (signal 'magh-invalid-input (list "Repository must not be empty")))
  (let ((parts (magh-core--remote-components repository)))
    (if parts
        (magh-context-normalize
         (magh-context-create :host (or host (car parts) magh-host)
                            :owner (nth 1 parts) :name (nth 2 parts)))
      (unless (string-match "\\`\\([^/[:space:]]+\\)/\\([^/[:space:]]+\\)\\'"
                            repository)
        (signal 'magh-invalid-input
                (list (format "Expected OWNER/NAME, got: %s" repository))))
      (magh-context-normalize
       (magh-context-create :host (or host magh-host)
                          :owner (match-string 1 repository)
                          :name (string-remove-suffix
                                 ".git" (match-string 2 repository)))))))

(defun magh-context-resolve (&optional value require-repository)
  "Resolve VALUE to a `magh-context'.
VALUE may be a context, OWNER/NAME, Git remote URL, directory, or nil.  With
REQUIRE-REPOSITORY non-nil, signal `magh-invalid-input' when no repository can
be inferred.  This operation performs local Git inspection only."
  (let ((context
         (cond
          ((magh-context-p value) (magh-context-normalize value))
          ((and (stringp value) (file-directory-p value))
           (or (magh-core--local-context value)
               (magh-context-create :host magh-host)))
          ((stringp value) (magh-context-from-repository value))
          (t (or (magh-core--local-context)
                 (magh-context-create :host magh-host))))))
    (when (and require-repository
               (not (magh-context-repository context)))
      (signal 'magh-invalid-input
              (list "No GitHub repository could be inferred; specify OWNER/NAME")))
    context))

(defun magh-context-read-repository (&optional prompt)
  "Return the current repository context, prompting with PROMPT when needed.
Repository names from `magh-known-repositories' are offered as completions,
but any OWNER/NAME value is accepted."
  (let ((context (magh-context-resolve)))
    (if (magh-context-repository context)
        context
      (magh-context-from-repository
       (completing-read (or prompt "Repository (OWNER/NAME): ")
                        magh-known-repositories nil nil)
       (magh-context-host context)))))

(defun magh-context-web-url (context &optional suffix)
  "Return the web URL for CONTEXT, optionally followed by SUFFIX."
  (let ((repo (magh-context-repository context)))
    (unless repo
      (signal 'magh-invalid-input (list "This action requires a repository")))
    (format "https://%s/%s%s"
            (or (magh-context-host context) magh-host "github.com")
            repo
            (if suffix
                (concat "/" (string-remove-prefix "/" suffix))
              ""))))

(defun magh-core--repo-endpoint (context &optional suffix)
  "Return a REST endpoint for CONTEXT and optional SUFFIX."
  (concat "repos/" (magh-context-repository context)
          (and suffix (concat "/" (string-remove-prefix "/" suffix)))))

(defun magh-core--url-path (path)
  "URL-encode every component in PATH while retaining slashes."
  (mapconcat #'url-hexify-string (split-string path "/" t) "/"))

(defun magh-core--parse-key-value (text)
  "Parse TEXT as KEY=VALUE and return a cons cell."
  (unless (string-match "\\`\\([^=]+\\)=\\(.*\\)\\'" text)
    (user-error "Expected key=value: %s" text))
  (cons (match-string 1 text) (match-string 2 text)))

;;; Display helpers

(defun magh-date-relative (timestamp)
  "Format ISO-8601 TIMESTAMP as a compact relative age."
  (if (null timestamp)
      ""
    (let* ((seconds (max 0 (float-time
                            (time-subtract nil (date-to-time timestamp)))))
           (minute 60.0)
           (hour (* 60 minute))
           (day (* 24 hour))
           (week (* 7 day))
           (month (* 30 day))
           (year (* 365 day)))
      (cond
       ((< seconds minute) "now")
       ((< seconds hour) (format "%dm" (floor (/ seconds minute))))
       ((< seconds day) (format "%dh" (floor (/ seconds hour))))
       ((< seconds week) (format "%dd" (floor (/ seconds day))))
       ((< seconds month) (format "%dw" (floor (/ seconds week))))
       ((< seconds year) (format "%dmo" (floor (/ seconds month))))
       (t (format "%dy" (floor (/ seconds year))))))))

(defun magh-core--date (timestamp)
  "Format TIMESTAMP using `magh-date-format-function'."
  (funcall magh-date-format-function timestamp))

(defun magh-core--name (value)
  "Extract a useful name/login/string from JSON VALUE."
  (cond
   ((stringp value) value)
   ((consp value)
   (or (alist-get 'login value)
        (alist-get 'name value)
        (alist-get 'full_name value)
        (alist-get 'title value)
        (alist-get 'message value)))
   (t nil)))

(defun magh-core--names (values)
  "Return a comma-separated list of names from JSON VALUES."
  (mapconcat #'magh-core--name values ", "))

(defun magh-core--comments-count (data)
  "Return the normalized comment count from DATA."
  (let ((comments (alist-get 'comments data)))
    (or (alist-get 'commentsCount data)
        (alist-get 'totalCount comments)
        (length comments))))

(defun magh-core--state-face (state)
  "Return the semantic magh.el face symbol for STATE."
  (pcase (upcase (or state ""))
    ((or "OPEN" "SUCCESS" "ACTIVE" "PUBLISHED" "APPROVED" "COMPLETED"
         "MERGED" "ENABLED")
     'magh-open-state)
    ((or "PENDING" "QUEUED" "IN_PROGRESS" "WAITING" "REQUESTED"
         "REVIEW_REQUIRED" "EXPECTED" "ACTION_REQUIRED")
     'magh-pending-state)
    ((or "DRAFT" "PRERELEASE" "NEUTRAL" "SKIPPED" "UNKNOWN")
     'magh-draft-state)
    (_ 'magh-closed-state)))

(defun magh-core--confirm (prompt)
  "Return non-nil if action described by PROMPT may proceed."
  (or (not magh-confirm-destructive-actions) (yes-or-no-p prompt)))

(defun magh-core--call-later (function &rest args)
  "Call FUNCTION with ARGS on a zero-delay timer.
This is used to preserve asynchronous callback semantics for cache hits and
locally detected errors."
  (apply #'run-at-time 0 nil function args))

(defun magh-core--collect-async (requests callback errback)
  "Run asynchronous REQUESTS in parallel and collect their results.
REQUESTS is a non-empty alist of (KEY . START).  START receives success and
error callbacks.  CALLBACK receives a key-preserving alist after every request
has succeeded.  ERRBACK is called at most once."
  (let ((remaining (length requests))
        (failed nil)
        results)
    (dolist (request requests)
      (let ((key (car request))
            (start (cdr request)))
        (funcall
         start
         (lambda (value)
           (unless failed
             (push (cons key value) results)
             (cl-decf remaining)
             (when (zerop remaining)
               (funcall callback (nreverse results)))))
         (lambda (error)
           (unless failed
             (setq failed t)
             (funcall errback error))))))))

(defun magh-core--collect-async-settled (requests callback)
  "Run independent asynchronous REQUESTS and call CALLBACK after all settle.
REQUESTS has the same (KEY . START) shape as `magh-core--collect-async'.
CALLBACK receives a `magh-batch-result'; one failed request does not discard
successful sibling values."
  (if (null requests)
      (funcall callback (magh-batch-result-create))
    (let ((remaining (length requests))
          (outcomes (make-hash-table :test #'equal)))
      (cl-labels
          ((finish (key tag value)
             (puthash key (cons tag value) outcomes)
             (cl-decf remaining)
             (when (zerop remaining)
               (let (values errors)
                 (dolist (request requests)
                   (let* ((request-key (car request))
                          (outcome (gethash request-key outcomes)))
                     (if (eq (car outcome) :value)
                         (push (cons request-key (cdr outcome)) values)
                       (push (cons request-key (cdr outcome)) errors))))
                 (funcall callback
                          (magh-batch-result-create
                           :values (nreverse values)
                           :errors (nreverse errors)))))))
        (dolist (request requests)
          (let ((key (car request)))
            (funcall (cdr request)
                     (lambda (value) (finish key :value value))
                     (lambda (error) (finish key :error error)))))))))

(provide 'magh-core)
;;; magh-core.el ends here
