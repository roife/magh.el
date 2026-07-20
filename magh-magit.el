;;; magh-magit.el --- Asynchronous GitHub sections in Magit status -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; Author: magh.el contributors
;; Keywords: tools, vc, github

;;; Commentary:

;; This optional integration appends GitHub summaries to existing Magit status
;; buffers.  The status hook never waits for GitHub: it renders cached state or
;; a loading row, starts requests through `magh-api.el', and refreshes matching
;; live Magit buffers from a zero-delay timer after the requests finish.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'transient)
(require 'magit)
(require 'magh-api)
(require 'magh-actions)
(require 'magh-candidate)
(require 'magh-issue)
(require 'magh-pr)
(require 'magh-repo)
(require 'magh-dispatch)
(require 'magh-ui)

(defgroup magh-magit nil
  "GitHub summaries embedded in Magit status."
  :group 'magh
  :group 'magit)

(defcustom magh-magit-list-limit 10
  "Maximum number of rows requested for each Magit GitHub summary."
  :type 'natnum)

(defcustom magh-magit-status-sections '(pr issue run)
  "GitHub resource summaries inserted into Magit status.
Supported members are `pr', `issue', and `run'."
  :type '(set (const :tag "Pull requests" pr)
              (const :tag "Issues" issue)
              (const :tag "Actions runs" run)))

(defcustom magh-magit-summary-scope 'repository
  "Scope used by GitHub summaries in Magit status."
  :type '(choice (const :tag "Current repository" repository)
                 (const :tag "Current user" user)))

(defcustom magh-magit-cache-ttl 30
  "Seconds a completed Magit GitHub summary remains fresh."
  :type 'number)

(defcustom magh-magit-dispatch-key "@"
  "Key used for the GitHub suffix in `magit-dispatch'."
  :type 'string)

(defcustom magh-hide-forge-duplicates t
  "Hide Issue and Pull Request summaries when Forge is loaded."
  :type 'boolean)

(defvar magh-magit--cache (make-hash-table :test #'equal)
  "Summary cache keyed separately from the common magh query cache.")

(defvar magh-magit--generation 0
  "Monotonic generation used to reject late summary callbacks.")

(defvar-local magh-magit--context-key nil
  "Cache key represented by the current Magit buffer.")

(defvar-keymap magh-magit-resource-section-map
  :doc "Keymap shared by GitHub resource sections in Magit status."
  :parent magit-section-mode-map
  "RET" #'magh-ui-visit
  "o" #'magh-ui-browse
  "w" #'magh-ui-copy-url
  "." #'magh-ui-dispatch)

;; `magit-section' discovers maps through these names.
(defvar magit-magh-pr-section-map magh-magit-resource-section-map)
(defvar magit-magh-issue-section-map magh-magit-resource-section-map)
(defvar magit-magh-run-section-map magh-magit-resource-section-map)
(defvar magit-magh-topic-section-map magh-magit-resource-section-map)

(defun magh-magit--effective-sections ()
  "Return enabled sections after optional Forge duplicate suppression."
  (if (and magh-hide-forge-duplicates (featurep 'forge))
      (seq-remove (lambda (kind) (memq kind '(pr issue)))
                  magh-magit-status-sections)
    magh-magit-status-sections))

(defun magh-magit--context ()
  "Infer a repository context from the current Magit status buffer."
  (when-let* ((root (magit-toplevel)))
    (condition-case nil
        (magh-context-resolve root t)
      (magh-error nil))))

(defun magh-magit--key (context sections)
  "Return summary cache key for CONTEXT and SECTIONS."
  (list (magh-context-host context)
        (magh-context-repository context)
        magh-magit-summary-scope
        (magh-context-branch context)
        sections))

(defun magh-magit--fresh-p (entry)
  "Return non-nil when completed cache ENTRY is fresh."
  (and entry
       (not (plist-get entry :loading))
       (< (- (float-time) (plist-get entry :created))
          magh-magit-cache-ttl)))

(defun magh-magit--task (key context success error force)
  "Start summary task KEY in CONTEXT, calling SUCCESS or ERROR.
FORCE bypasses the shared query cache."
  (pcase-exhaustive key
    ('pr
     (magh-api--pr-list context `(:state "open" :limit ,magh-magit-list-limit)
                      success error force))
    ('issue
     (magh-api--issue-list context `(:state "open" :limit ,magh-magit-list-limit)
                         success error force))
    ('run
     (magh-api--run-list context `(:limit ,magh-magit-list-limit)
                       success error force))
    ('branch-prs
     (if (magh-context-branch context)
         (magh-api--pr-list
          context `(:state "open" :head ,(magh-context-branch context)
                    :limit ,magh-magit-list-limit)
          success error force)
       (funcall success nil)))
    ('review-prs (magh-api--review-requests context success error force))
    ('assigned-prs (magh-api--assigned-prs context success error force))
    ('assigned-issues (magh-api--assigned-issues context success error force))
    ('mentioned-prs
     (magh-api--search context 'prs "mentions:@me state:open"
                     success error force))
    ('mentioned-issues
     (magh-api--search context 'issues "mentions:@me state:open"
                     success error force))
    ('created-prs
     (magh-api--search context 'prs "" success error force
                     '(:author "@me" :state "open")))
    ('created-issues
     (magh-api--search context 'issues "" success error force
                     '(:author "@me" :state "open")))))

(defun magh-magit--task-keys (sections)
  "Return request keys needed for SECTIONS in the current scope."
  (if (eq magh-magit-summary-scope 'repository)
      sections
    (append
     (when (memq 'pr sections)
       '(branch-prs review-prs assigned-prs mentioned-prs created-prs))
     (when (memq 'issue sections)
       '(assigned-issues mentioned-issues created-issues))
     (when (memq 'run sections) '(run)))))

(defun magh-magit--refresh-buffers (key)
  "Refresh live Magit status buffers displaying summary KEY."
  (dolist (buffer (buffer-list))
    (when (with-current-buffer buffer
            (and (derived-mode-p 'magit-status-mode)
                 (equal magh-magit--context-key key)))
      (with-current-buffer buffer
        (condition-case error
            (magit-refresh-buffer)
          (error
           (message "magh.el could not refresh %s: %s"
                    (buffer-name) (error-message-string error))))))))

(defun magh-magit--finish-request (key generation data errors)
  "Complete KEY at GENERATION with DATA and ERRORS, then refresh buffers."
  (let ((entry (gethash key magh-magit--cache)))
    (when (and entry (= generation (plist-get entry :generation)))
      (puthash key
               (list :loading nil :generation generation :data data
                     :errors errors :created (float-time))
               magh-magit--cache)
      (run-at-time 0 nil #'magh-magit--refresh-buffers key))))

(defun magh-magit--start-request (key context sections &optional force)
  "Start all summary requests for KEY, CONTEXT, and SECTIONS.
When FORCE is non-nil, bypass the shared query cache."
  (let* ((old (gethash key magh-magit--cache))
         (generation (cl-incf magh-magit--generation))
         (tasks (magh-magit--task-keys sections))
         (remaining (length tasks))
         (data (copy-alist (plist-get old :data)))
         errors)
    (puthash key
             (list :loading t :generation generation :data data
                   :errors nil :created (plist-get old :created))
             magh-magit--cache)
    (dolist (task tasks)
      (magh-magit--task
       task context
       (lambda (value)
         (setf (alist-get task data) value)
         (when (zerop (cl-decf remaining))
           (magh-magit--finish-request key generation data errors)))
       (lambda (request-error)
         (push (cons task request-error) errors)
         (when (zerop (cl-decf remaining))
           (magh-magit--finish-request key generation data errors)))
       force))))

(defun magh-magit--resource (kind context data)
  "Create a KIND resource from summary DATA and CONTEXT."
  (when-let* ((name (alist-get 'nameWithOwner (alist-get 'repository data))))
    (setq context (magh-context-from-repository name (magh-context-host context))))
  (pcase kind
    ('pr (magh-pr--resource context data))
    ('issue (magh-issue--resource context data))
    ('run (magh-actions--run-resource context data))))

(defun magh-magit--insert-topic (kind context data)
  "Insert one Issue or Pull Request DATA row of KIND in CONTEXT."
  (let* ((resource (magh-magit--resource kind context data))
         (number (alist-get 'number data))
         (state (if (alist-get 'isDraft data)
                    "DRAFT" (alist-get 'state data)))
         (title (alist-get 'title data))
         (repo (plist-get resource :repository))
         (author (magh-core--name (alist-get 'author data)))
         (review (and (eq kind 'pr)
                      (alist-get 'reviewDecision data))))
    (magh-ui--section (magh-topic (cons repo number) resource t)
      (magh-ui--row
       (magh-ui--styled (upcase state) (magh-core--state-face state))
       (and review (magh-ui--styled review (magh-core--state-face review)))
       (magh-ui--styled (format "#%s" number) 'magh-resource-number)
       (magh-ui--styled title 'magh-resource-title)
       (and (eq magh-magit-summary-scope 'user)
            (magh-ui--styled (format "[%s]" repo) 'magh-repository)))
      (magh-ui--insert-header "Author" author 'magh-author)
      (when (eq kind 'pr)
        (when-let* ((head (alist-get 'headRefName data))
                    (base (alist-get 'baseRefName data)))
          (magh-ui--insert-header "Branches" (format "%s → %s" head base)
                                'magh-branch))
        (when review
          (magh-ui--insert-header "Review" review
                                (magh-core--state-face review))))
      (magh-ui--insert-header "Labels"
                            (magh-core--names
                             (alist-get 'labels data))
                            'magh-label)
      (when (eq kind 'issue)
        (magh-ui--insert-header
         "Assigned"
         (magh-core--names (alist-get 'assignees data)) 'magh-author))
      (magh-ui--insert-header "Comments" (magh-core--comments-count data))
      (magh-ui--insert-header "Updated"
                            (magh-core--date (alist-get 'updatedAt data))
                            'magh-date))))

(defun magh-magit--insert-run (context data)
  "Insert one Actions run DATA row in CONTEXT."
  (let* ((resource (magh-magit--resource 'run context data))
         (id (plist-get resource :id))
         (state (or (alist-get 'conclusion data)
                    (alist-get 'status data))))
    (magh-ui--section (magh-run id resource t)
      (magh-ui--row
       (magh-ui--styled (upcase state) (magh-core--state-face state))
       (magh-ui--styled (alist-get 'displayTitle data)
                      'magh-resource-title)
       (magh-ui--styled (or (alist-get 'workflowName data)
                          (alist-get 'name data))
                      'magh-workflow))
      (magh-ui--insert-header "Branch"
                            (alist-get 'headBranch data) 'magh-branch)
      (magh-ui--insert-header "Event" (alist-get 'event data))
      (magh-ui--insert-header "Created"
                            (magh-core--date (alist-get 'createdAt data))
                            'magh-date))))

(defun magh-magit--insert-group (heading kind context items)
  "Insert HEADING containing KIND rows from ITEMS in CONTEXT."
  (magh-ui--ensure-section-gap)
  (magit-insert-section (magh-group heading)
    (magit-insert-heading heading)
    (magit-insert-section-body
      (if items
          (dolist (item (seq-take items magh-magit-list-limit))
            (if (eq kind 'run)
                (magh-magit--insert-run context item)
              (magh-magit--insert-topic kind context item)))
        (insert (propertize "  No matching items\n"
                            'font-lock-face 'shadow))))))

(defun magh-magit--insert-items (kind context items)
  "Insert KIND rows from ITEMS directly into the current section."
  (if items
      (dolist (item (seq-take items magh-magit-list-limit))
        (if (eq kind 'run)
            (magh-magit--insert-run context item)
          (magh-magit--insert-topic kind context item)))
    (insert (propertize "  No matching items\n"
                        'font-lock-face 'shadow))))

(defun magh-magit--insert-repository-data (context sections data)
  "Insert repository-scoped SECTIONS from DATA in CONTEXT."
  (when (memq 'pr sections)
    (let* ((items (alist-get 'pr data))
           (branch (magh-context-branch context))
           (current (and branch
                         (seq-filter
                          (lambda (item)
                            (equal branch
                                   (alist-get 'headRefName item)))
                          items)))
           (other (seq-difference items current #'equal)))
      (magh-ui--ensure-section-gap)
      (magit-insert-section (magh-pull-requests)
        (magit-insert-heading "Open Pull Requests")
        (magit-insert-section-body
          (if current
              (progn
                (magh-magit--insert-group "Current branch" 'pr context current)
                (magh-magit--insert-group "Other" 'pr context other))
            (magh-magit--insert-items 'pr context other))))))
  (when (memq 'issue sections)
    (magh-ui--ensure-section-gap)
    (magit-insert-section (magh-issues)
      (magit-insert-heading "Open Issues")
      (magit-insert-section-body
        (magh-magit--insert-items 'issue context (alist-get 'issue data)))))
  (when (memq 'run sections)
    (magh-ui--ensure-section-gap)
    (magit-insert-section (magh-actions)
      (magit-insert-heading "Actions")
      (magit-insert-section-body
        (magh-magit--insert-group "Recent" 'run context
                                (alist-get 'run data))))))

(defun magh-magit--insert-user-data (context sections data)
  "Insert user-scoped SECTIONS from DATA in CONTEXT."
  (when (memq 'pr sections)
    (magh-ui--ensure-section-gap)
    (magit-insert-section (magh-pull-requests)
      (magit-insert-heading "Pull requests")
      (magit-insert-section-body
        (dolist (group '(("Current branch" . branch-prs)
                         ("Needs review" . review-prs)
                         ("Assigned" . assigned-prs)
                         ("Mentioned" . mentioned-prs)
                         ("Created by you" . created-prs)))
          (magh-magit--insert-group (car group) 'pr context
                                  (alist-get (cdr group) data))))))
  (when (memq 'issue sections)
    (magh-ui--ensure-section-gap)
    (magit-insert-section (magh-issues)
      (magit-insert-heading "Issues")
      (magit-insert-section-body
        (dolist (group '(("Assigned" . assigned-issues)
                         ("Mentioned" . mentioned-issues)
                         ("Created by you" . created-issues)))
          (magh-magit--insert-group (car group) 'issue context
                                  (alist-get (cdr group) data))))))
  (when (memq 'run sections)
    (magh-ui--ensure-section-gap)
    (magit-insert-section (magh-actions)
      (magit-insert-heading "Actions")
      (magit-insert-section-body
        (magh-magit--insert-group "Recent in this repository" 'run context
                                (alist-get 'run data))))))

(defun magh-magit--insert-errors (errors)
  "Insert compact inline ERRORS for failed summary requests."
  (dolist (entry errors)
    (insert (propertize
             (format "  %s: %s\n" (car entry)
                     (magh-error-message (cdr entry)))
             'font-lock-face 'magh-error))))

;;;###autoload
(defun magh-magit-insert-github ()
  "Insert asynchronous GitHub summaries into the current Magit status."
  (when-let* ((context (magh-magit--context))
              (sections (magh-magit--effective-sections)))
    (let* ((key (magh-magit--key context sections))
           (entry (gethash key magh-magit--cache)))
      (setq magh-magit--context-key key)
      (unless (or (plist-get entry :loading) (magh-magit--fresh-p entry))
        (magh-magit--start-request key context sections))
      (setq entry (gethash key magh-magit--cache))
      (magh-ui--ensure-section-gap)
      (magit-insert-section (github)
        (magit-insert-heading "GitHub")
        (magit-insert-section-body
          (let ((data (plist-get entry :data)))
            (if (and (plist-get entry :loading) (null data))
                (insert (propertize "  loading…\n"
                                    'font-lock-face 'magh-loading))
              (when (plist-get entry :loading)
                (insert (propertize "  refreshing…\n"
                                    'font-lock-face 'magh-loading)))
              (if (eq magh-magit-summary-scope 'user)
                  (magh-magit--insert-user-data context sections data)
                (magh-magit--insert-repository-data context sections data))
              (magh-magit--insert-errors (plist-get entry :errors)))))))))

;;;###autoload
(defun magh-magit-refresh (&optional both-layers)
  "Refresh GitHub summaries in Magit status.
With prefix argument BOTH-LAYERS, also invalidate matching common API cache."
  (interactive "P")
  (let ((context (magh-magit--context)))
    (if magh-magit--context-key
        (remhash magh-magit--context-key magh-magit--cache)
      (clrhash magh-magit--cache))
    (when (and both-layers context)
      (magh-api--invalidate
       (list :host (magh-context-host context)
             :repository (magh-context-repository context))))
    (if (derived-mode-p 'magit-status-mode)
        (magit-refresh-buffer)
      (message "GitHub Magit summary cache cleared"))))

(defun magh-magit-toggle-scope ()
  "Toggle between repository and current-user Magit summaries."
  (interactive)
  (setq magh-magit-summary-scope
        (if (eq magh-magit-summary-scope 'repository) 'user 'repository))
  (message "GitHub Magit scope: %s" magh-magit-summary-scope)
  (when (derived-mode-p 'magit-status-mode)
    (magit-refresh-buffer)))

(transient-define-prefix magh-magit-dispatch ()
  "GitHub actions for the current Magit repository."
  [["Open"
    ("s" "Repository status" magh-repo-status)
    ("p" "Pull requests" magh-pr-list)
    ("i" "Issues" magh-issue-list)
    ("a" "Actions" magh-run-list)]
   ["Summary"
    ("g" "Refresh" magh-magit-refresh)
    ("m" "Toggle Magit scope" magh-magit-toggle-scope)]
   ["More"
    ("@" "Main GitHub dispatch" magh-dispatch)]])

(defun magh-magit--install-dispatch ()
  "Install the GitHub suffix after Magit's Run suffix."
  (unless (ignore-errors
            (transient-get-suffix 'magit-dispatch magh-magit-dispatch-key))
    (transient-append-suffix
      'magit-dispatch "!"
      `(,magh-magit-dispatch-key "GitHub" magh-magit-dispatch))))

(defun magh-magit--remove-dispatch ()
  "Remove the GitHub suffix from Magit's dispatcher."
  (when (ignore-errors
          (transient-get-suffix 'magit-dispatch magh-magit-dispatch-key))
    (transient-remove-suffix 'magit-dispatch magh-magit-dispatch-key)))

;;;###autoload
(define-minor-mode magh-magit-mode
  "Globally add asynchronous GitHub summaries to Magit status."
  :global t
  :group 'magh-magit
  (if magh-magit-mode
      (progn
        (add-hook 'magit-status-sections-hook #'magh-magit-insert-github t)
        (magh-magit--install-dispatch))
    (remove-hook 'magit-status-sections-hook #'magh-magit-insert-github)
    (magh-magit--remove-dispatch)))

(provide 'magh-magit)
;;; magh-magit.el ends here
