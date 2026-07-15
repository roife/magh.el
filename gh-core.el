;;; gh-core.el --- Core types and configuration for gh.el -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; Author: gh.el contributors
;; Keywords: tools, vc, github
;; Package-Requires: ((emacs "29.1"))

;;; Commentary:

;; This file contains configuration, typed errors, repository contexts, and
;; utilities which do not talk to GitHub.  In particular, resolving a local
;; context only invokes local Git commands; all GitHub I/O lives in
;; `gh-client.el'.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'url-util)

(defgroup gh nil
  "A Magit-like GitHub frontend powered by the GitHub CLI."
  :group 'tools
  :group 'vc
  :prefix "gh-")

(defcustom gh-executable "gh"
  "Name or absolute path of the GitHub CLI executable."
  :type 'file)

(defcustom gh-host nil
  "GitHub host used for requests.
When nil, let the GitHub CLI select its authenticated default host.  A host
parsed from a local Git remote takes precedence for that repository."
  :type '(choice (const :tag "GitHub CLI default" nil) string))

(defcustom gh-list-limit 50
  "Maximum number of items requested by ordinary resource lists."
  :type 'natnum)

(defcustom gh-default-issue-state "open"
  "Default state used by `gh-issue-list'."
  :type '(choice (const "open") (const "closed") (const "all")))

(defcustom gh-default-pr-state "open"
  "Default state used by `gh-pr-list'."
  :type '(choice (const "open") (const "closed")
                 (const "merged") (const "all")))

(defcustom gh-confirm-destructive-actions t
  "Whether destructive GitHub and filesystem actions require confirmation."
  :type 'boolean)

(defcustom gh-favorite-organizations nil
  "Organizations whose repositories are included in favorite repositories."
  :type '(repeat string))

(defcustom gh-known-repositories nil
  "Repository names retained for user-defined shortcuts and Embark actions."
  :type '(repeat string))

(defcustom gh-workflow-template-repositories nil
  "Repositories used as sources for workflow templates."
  :type '(repeat string))

(defcustom gh-view-inline-images t
  "Whether remote images in rendered GitHub Markdown are fetched asynchronously."
  :type 'boolean)

(defcustom gh-view-inline-image-max-width 640
  "Maximum display width in pixels for an inline Markdown image."
  :type 'natnum)

(defcustom gh-view-inline-image-max-bytes (* 5 1024 1024)
  "Maximum response size accepted for an inline Markdown image."
  :type 'natnum)

(defcustom gh-view-truncate-lines t
  "Whether native gh.el section pages truncate long lines."
  :type 'boolean)

(defcustom gh-section-initial-visibility-alist
  '((description . show)
    (conversation . show)
    (t . hide))
  "Initial visibility rules for gh.el sections.
This has the same shape as `magit-section-initial-visibility-alist'."
  :type '(alist :key-type (choice symbol string (const t))
                :value-type (choice (const show) (const hide))))

(defcustom gh-section-cache-visibility t
  "Whether section visibility is preserved between gh.el page refreshes.
When a list, preserve only pages whose resource kind is in that list."
  :type '(choice boolean (repeat symbol)))

(defcustom gh-refresh-point-strategy 'section
  "How point is restored after an asynchronous page refresh."
  :type '(choice (const section) (const line) (const start)))

(defcustom gh-date-format-function #'gh-date-relative
  "Function called with an ISO-8601 timestamp to produce display text."
  :type 'function)

(defcustom gh-display-buffer-function #'pop-to-buffer
  "Function used to display permanent gh.el buffers."
  :type 'function)

(defcustom gh-bury-buffer-function #'quit-window
  "Function used to leave a gh.el buffer."
  :type 'function)

(defcustom gh-resource-actions nil
  "Overrides for default actions by resource kind.
Each entry is (KIND . FUNCTION).  FUNCTION receives a resource plist."
  :type '(alist :key-type symbol :value-type function))

(defcustom gh-temporary-clone-directory
  (expand-file-name "gh.el-clones" temporary-file-directory)
  "Directory containing temporary repository clones created by gh.el."
  :type 'directory)

(defcustom gh-download-directory nil
  "Initial directory for release asset downloads.
When nil, use `default-directory'."
  :type '(choice (const nil) directory))

(defcustom gh-search-minimum-input 2
  "Minimum query length before a remote dynamic search starts."
  :type 'natnum)

(defcustom gh-search-debounce 0.25
  "Seconds to wait after search input before starting a request."
  :type 'number)

(defcustom gh-notifications-unread-only t
  "Whether notification selection initially shows only unread threads."
  :type 'boolean)

(defcustom gh-notifications-group-by 'repository
  "Initial grouping used by `gh-notifications'."
  :type '(choice (const repository) (const reason) (const type)
                 (const state) (const date) (const nil)))

(defvar gh-pre-display-buffer-hook nil
  "Hook run before displaying a permanent gh.el buffer.")
(defvar gh-post-display-buffer-hook nil
  "Hook run after displaying a permanent gh.el buffer.")
(defvar gh-pre-refresh-hook nil
  "Hook run before an asynchronous gh.el page refresh.")
(defvar gh-post-refresh-hook nil
  "Hook run after an asynchronous gh.el page refresh completes.")
(defvar gh-repository-post-clone-hook nil
  "Hook run with the clone directory after a repository clone succeeds.")
(defvar gh-repository-post-fork-hook nil
  "Hook run with the repository context after a fork succeeds.")
(defvar gh-auth-post-switch-hook nil
  "Hook run after an interactive GitHub CLI account switch succeeds.")

;;; Errors

(define-error 'gh-error "gh.el error")
(define-error 'gh-missing-executable "GitHub CLI executable is missing" 'gh-error)
(define-error 'gh-auth-error "GitHub CLI authentication or host error" 'gh-error)
(define-error 'gh-command-error "GitHub CLI command failed" 'gh-error)
(define-error 'gh-json-error "GitHub CLI returned invalid JSON" 'gh-error)
(define-error 'gh-api-error "GitHub API request failed" 'gh-error)
(define-error 'gh-cancelled "GitHub request was cancelled" 'gh-error)
(define-error 'gh-invalid-input "Invalid gh.el input" 'gh-error)

(defun gh-core--error (type message &optional data)
  "Return a typed condition list of TYPE with MESSAGE and DATA."
  (list type message data))

(defun gh-error-message (error)
  "Return a user-facing message for typed ERROR."
  (condition-case nil
      (error-message-string error)
    (error (format "%s" error))))

(defun gh-core--user-error (error)
  "Raise typed ERROR as a `user-error'."
  (user-error "%s" (gh-error-message error)))

;;; Context

(cl-defstruct
    (gh-context
     (:constructor gh-context-create
                   (&key host owner name repository root branch
                         default-branch ref path)))
  "Structured GitHub repository and navigation context."
  host owner name repository root branch default-branch ref path)

(defun gh-context-copy (context &rest overrides)
  "Copy CONTEXT and apply plist OVERRIDES to its slots."
  (let ((copy (copy-gh-context context)))
    (while overrides
      (let ((key (pop overrides))
            (value (pop overrides)))
        (pcase key
          (:host (setf (gh-context-host copy) value))
          (:owner (setf (gh-context-owner copy) value))
          (:name (setf (gh-context-name copy) value))
          (:repository (setf (gh-context-repository copy) value))
          (:root (setf (gh-context-root copy) value))
          (:branch (setf (gh-context-branch copy) value))
          (:default-branch (setf (gh-context-default-branch copy) value))
          (:ref (setf (gh-context-ref copy) value))
          (:path (setf (gh-context-path copy) value))
          (_ (signal 'gh-invalid-input
                     (list (format "Unknown context field: %S" key)))))))
    (gh-context-normalize copy)))

(defun gh-context-normalize (context)
  "Normalize and return CONTEXT."
  (let ((repo (gh-context-repository context)))
    (when (and repo (string-match
                     "\\`\\([^/]+\\)/\\([^/]+\\)\\'"
                     (string-remove-suffix ".git" repo)))
      (setf (gh-context-owner context) (match-string 1 repo)
            (gh-context-name context)
            (string-remove-suffix ".git" (match-string 2 repo))))
    (when (and (gh-context-owner context) (gh-context-name context))
      (setf (gh-context-repository context)
            (format "%s/%s" (gh-context-owner context)
                    (gh-context-name context))))
    (when-let* ((path (gh-context-path context)))
      (setf (gh-context-path context)
            (string-remove-prefix "./" (string-remove-prefix "/" path))))
    context))

(defun gh-core--remote-components (remote)
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

(defun gh-core--git-root (&optional directory)
  "Return local Git root for DIRECTORY, without contacting a remote."
  (let ((directory (file-name-as-directory
                    (expand-file-name (or directory default-directory)))))
    (and (not (file-remote-p directory))
         (locate-dominating-file directory ".git"))))

(defun gh-core--git-output (root &rest args)
  "Run a local Git command in ROOT with ARGS and return trimmed output."
  (when (and root (executable-find "git"))
    (with-temp-buffer
      (let ((default-directory root))
        (when (zerop (apply #'process-file "git" nil t nil args))
          (string-trim (buffer-string)))))))

(defun gh-core--local-context (&optional directory)
  "Build a context from local Git metadata under DIRECTORY."
  (when-let* ((root (gh-core--git-root directory)))
    (let* ((remote-name
            (or (gh-core--git-output root "config" "--get" "remote.pushDefault")
                "origin"))
           (remote
            (or (gh-core--git-output root "config" "--get"
                                     (format "remote.%s.url" remote-name))
                (gh-core--git-output root "config" "--get" "remote.origin.url")))
           (parts (gh-core--remote-components remote))
           (branch (gh-core--git-output root "branch" "--show-current"))
           (head (gh-core--git-output root "symbolic-ref" "--short"
                                     "refs/remotes/origin/HEAD"))
           (default-branch (and head (string-remove-prefix "origin/" head))))
      (gh-context-normalize
       (gh-context-create :host (or (car parts) gh-host)
                          :owner (nth 1 parts)
                          :name (nth 2 parts)
                          :root root
                          :branch (unless (string-empty-p (or branch "")) branch)
                          :default-branch default-branch
                          :ref (or branch default-branch))))))

(defun gh-context-from-repository (repository &optional host)
  "Create a context from REPOSITORY and optional HOST.
REPOSITORY can be OWNER/NAME or a supported Git remote URL."
  (unless (and (stringp repository) (not (string-empty-p repository)))
    (signal 'gh-invalid-input (list "Repository must not be empty")))
  (let ((parts (gh-core--remote-components repository)))
    (if parts
        (gh-context-normalize
         (gh-context-create :host (or host (car parts) gh-host)
                            :owner (nth 1 parts) :name (nth 2 parts)))
      (unless (string-match "\\`\\([^/[:space:]]+\\)/\\([^/[:space:]]+\\)\\'"
                            repository)
        (signal 'gh-invalid-input
                (list (format "Expected OWNER/NAME, got: %s" repository))))
      (gh-context-normalize
       (gh-context-create :host (or host gh-host)
                          :owner (match-string 1 repository)
                          :name (string-remove-suffix
                                 ".git" (match-string 2 repository)))))))

(defun gh-context-resolve (&optional value require-repository)
  "Resolve VALUE to a `gh-context'.
VALUE may be a context, OWNER/NAME, Git remote URL, directory, or nil.  With
REQUIRE-REPOSITORY non-nil, signal `gh-invalid-input' when no repository can
be inferred.  This operation performs local Git inspection only."
  (let ((context
         (cond
          ((gh-context-p value) (gh-context-normalize value))
          ((and (stringp value) (file-directory-p value))
           (gh-core--local-context value))
          ((stringp value) (gh-context-from-repository value))
          (t (or (gh-core--local-context)
                 (gh-context-create :host gh-host))))))
    (when (and require-repository
               (not (gh-context-repository context)))
      (signal 'gh-invalid-input
              (list "No GitHub repository could be inferred; specify OWNER/NAME")))
    context))

(defun gh-context-web-url (context &optional suffix)
  "Return the web URL for CONTEXT, optionally followed by SUFFIX."
  (let ((repo (gh-context-repository context)))
    (unless repo
      (signal 'gh-invalid-input (list "This action requires a repository")))
    (format "https://%s/%s%s"
            (or (gh-context-host context) gh-host "github.com")
            repo
            (if suffix
                (concat "/" (string-remove-prefix "/" suffix))
              ""))))

(defun gh-core--repo-endpoint (context &optional suffix)
  "Return a REST endpoint for CONTEXT and optional SUFFIX."
  (let ((repo (gh-context-repository context)))
    (unless repo
      (signal 'gh-invalid-input (list "This request requires a repository")))
    (concat "repos/" repo
            (and suffix (concat "/" (string-remove-prefix "/" suffix))))))

(defun gh-core--url-path (path)
  "URL-encode every component in PATH while retaining slashes."
  (mapconcat #'url-hexify-string (split-string (or path "") "/" t) "/"))

;;; Display helpers

(defun gh-date-relative (timestamp)
  "Format ISO-8601 TIMESTAMP as a compact relative age."
  (if (not (and timestamp (stringp timestamp)
                (not (string-empty-p timestamp))))
      ""
    (condition-case nil
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
           (t (format "%dy" (floor (/ seconds year))))))
      (error timestamp))))

(defun gh-core--date (timestamp)
  "Format TIMESTAMP using `gh-date-format-function'."
  (funcall gh-date-format-function timestamp))

(defun gh-core--alist-get (key alist &optional default)
  "Get KEY from ALIST, tolerating symbol and string JSON keys."
  (or (alist-get key alist)
      (and (symbolp key) (alist-get (symbol-name key) alist nil nil #'equal))
      (and (stringp key) (alist-get (intern key) alist))
      default))

(defun gh-core--name (value)
  "Extract a useful name/login/string from JSON VALUE."
  (cond
   ((stringp value) value)
   ((consp value)
    (or (gh-core--alist-get 'login value)
        (gh-core--alist-get 'name value)
        (gh-core--alist-get 'full_name value)
        (gh-core--alist-get 'title value)
        (gh-core--alist-get 'message value)
        ""))
   (t "")))

(defun gh-core--names (values)
  "Return a comma-separated list of names from JSON VALUES."
  (mapconcat #'gh-core--name (or values nil) ", "))

(defun gh-core--state-face (state)
  "Return the semantic gh.el face symbol for STATE."
  (pcase (upcase (format "%s" state))
    ((or "OPEN" "SUCCESS" "ACTIVE" "PUBLISHED" "APPROVED" "COMPLETED"
         "MERGED" "ENABLED")
     'gh-open-state)
    ((or "PENDING" "QUEUED" "IN_PROGRESS" "WAITING" "REQUESTED"
         "REVIEW_REQUIRED" "EXPECTED" "ACTION_REQUIRED")
     'gh-pending-state)
    ((or "DRAFT" "PRERELEASE" "NEUTRAL" "SKIPPED" "UNKNOWN")
     'gh-draft-state)
    (_ 'gh-closed-state)))

(defun gh-core--confirm (prompt)
  "Return non-nil if action described by PROMPT may proceed."
  (or (not gh-confirm-destructive-actions) (yes-or-no-p prompt)))

(defun gh-core--call-later (function &rest args)
  "Call FUNCTION with ARGS on a zero-delay timer.
This is used to preserve asynchronous callback semantics for cache hits and
locally detected errors."
  (apply #'run-at-time 0 nil function args))

(defun gh-core--collect-async (requests callback errback)
  "Run asynchronous REQUESTS in parallel and collect their results.
REQUESTS is an alist of (KEY . START), where START receives success and error
callbacks.  CALLBACK receives a key-preserving alist after every request has
succeeded.  ERRBACK is called at most once."
  (if (null requests)
      (gh-core--call-later callback nil)
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
               (funcall errback error)))))))))

(provide 'gh-core)
;;; gh-core.el ends here
