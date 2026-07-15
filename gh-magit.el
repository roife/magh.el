;;; gh-magit.el --- Asynchronous GitHub sections in Magit status -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; Author: gh.el contributors
;; Keywords: tools, vc, github
;; Package-Requires: ((emacs "29.1") (magit "4.0.0") (transient "0.7.0"))

;;; Commentary:

;; This optional integration appends GitHub summaries to existing Magit status
;; buffers.  The status hook never waits for GitHub: it renders cached state or
;; a loading row, starts requests through `gh-api.el', and refreshes matching
;; live Magit buffers from a zero-delay timer after the requests finish.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'transient)
(require 'magit)
(require 'gh-api)
(require 'gh-actions)
(require 'gh-candidate)
(require 'gh-issue)
(require 'gh-pr)
(require 'gh-repo)
(require 'gh-dispatch)
(require 'gh-ui)

(defgroup gh-magit nil
  "GitHub summaries embedded in Magit status."
  :group 'gh
  :group 'magit)

(defcustom gh-magit-list-limit 10
  "Maximum number of rows requested for each Magit GitHub summary."
  :type 'natnum)

(defcustom gh-magit-status-sections '(pr issue run)
  "GitHub resource summaries inserted into Magit status.
Supported members are `pr', `issue', and `run'."
  :type '(set (const :tag "Pull requests" pr)
              (const :tag "Issues" issue)
              (const :tag "Actions runs" run)))

(defcustom gh-magit-summary-scope 'repository
  "Scope used by GitHub summaries in Magit status."
  :type '(choice (const :tag "Current repository" repository)
                 (const :tag "Current user" user)))

(defcustom gh-magit-cache-ttl 30
  "Seconds a completed Magit GitHub summary remains fresh."
  :type 'number)

(defcustom gh-magit-dispatch-key "@"
  "Key used for the GitHub suffix in `magit-dispatch'."
  :type 'string)

(defcustom gh-hide-forge-duplicates t
  "Hide Issue and Pull Request summaries when Forge is loaded."
  :type 'boolean)

(defvar gh-magit--cache (make-hash-table :test #'equal)
  "Summary cache keyed separately from the common gh query cache.")

(defvar gh-magit--generation 0
  "Monotonic generation used to reject late summary callbacks.")

(defvar-local gh-magit--context-key nil
  "Cache key represented by the current Magit buffer.")

(defvar gh-magit-resource-section-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map magit-section-mode-map)
    (define-key map (kbd "RET") #'gh-ui-visit)
    (define-key map (kbd "o") #'gh-ui-browse)
    (define-key map (kbd "w") #'gh-ui-copy-url)
    (define-key map (kbd ".") #'gh-ui-dispatch)
    map)
  "Keymap shared by GitHub resource sections in Magit status.")

;; `magit-section' discovers maps through these names.
(defvar magit-gh-pr-section-map gh-magit-resource-section-map)
(defvar magit-gh-issue-section-map gh-magit-resource-section-map)
(defvar magit-gh-run-section-map gh-magit-resource-section-map)
(defvar magit-gh-topic-section-map gh-magit-resource-section-map)

(defun gh-magit--effective-sections ()
  "Return enabled sections after optional Forge duplicate suppression."
  (if (and gh-hide-forge-duplicates (featurep 'forge))
      (seq-remove (lambda (kind) (memq kind '(pr issue)))
                  gh-magit-status-sections)
    gh-magit-status-sections))

(defun gh-magit--context ()
  "Infer a repository context from the current Magit status buffer."
  (let ((root (and (fboundp 'magit-toplevel) (magit-toplevel))))
    (and root (condition-case nil
                  (gh-context-resolve root t)
                (gh-error nil)
                (error nil)))))

(defun gh-magit--key (context sections)
  "Return summary cache key for CONTEXT and SECTIONS."
  (list (gh-context-host context)
        (gh-context-repository context)
        gh-magit-summary-scope
        (gh-context-branch context)
        sections))

(defun gh-magit--fresh-p (entry)
  "Return non-nil when completed cache ENTRY is fresh."
  (and entry
       (not (plist-get entry :loading))
       (< (- (float-time) (or (plist-get entry :created) 0))
          gh-magit-cache-ttl)))

(defun gh-magit--task (key context success error force)
  "Start summary task KEY in CONTEXT, calling SUCCESS or ERROR.
FORCE bypasses the shared query cache."
  (pcase key
    ('pr
     (gh-api--pr-list context `(:state "open" :limit ,gh-magit-list-limit)
                      success error force))
    ('issue
     (gh-api--issue-list context `(:state "open" :limit ,gh-magit-list-limit)
                         success error force))
    ('run
     (gh-api--run-list context `(:limit ,gh-magit-list-limit)
                       success error force))
    ('branch-prs
     (if (gh-context-branch context)
         (gh-api--pr-list
          context `(:state "open" :head ,(gh-context-branch context)
                    :limit ,gh-magit-list-limit)
          success error force)
       (funcall success nil)))
    ('review-prs (gh-api--review-requests context success error force))
    ('assigned-prs (gh-api--assigned-prs context success error force))
    ('assigned-issues (gh-api--assigned-issues context success error force))
    ('mentioned-prs
     (gh-api--search context 'prs "mentions:@me state:open"
                     success error force))
    ('mentioned-issues
     (gh-api--search context 'issues "mentions:@me state:open"
                     success error force))
    ('created-prs
     (gh-api--search context 'prs "" success error force
                     '(:author "@me" :state "open")))
    ('created-issues
     (gh-api--search context 'issues "" success error force
                     '(:author "@me" :state "open")))
    (_ (funcall success nil))))

(defun gh-magit--task-keys (sections)
  "Return request keys needed for SECTIONS in the current scope."
  (if (eq gh-magit-summary-scope 'repository)
      sections
    (append
     (when (memq 'pr sections)
       '(branch-prs review-prs assigned-prs mentioned-prs created-prs))
     (when (memq 'issue sections)
       '(assigned-issues mentioned-issues created-issues))
     (when (memq 'run sections) '(run)))))

(defun gh-magit--refresh-buffers (key)
  "Refresh live Magit status buffers displaying summary KEY."
  (dolist (buffer (buffer-list))
    (when (and (buffer-live-p buffer)
               (with-current-buffer buffer
                 (and (derived-mode-p 'magit-status-mode)
                      (equal gh-magit--context-key key))))
      (with-current-buffer buffer
        (condition-case error
            (magit-refresh-buffer)
          (error
           (message "gh.el could not refresh %s: %s"
                    (buffer-name) (error-message-string error))))))))

(defun gh-magit--finish-request (key generation data errors)
  "Complete KEY at GENERATION with DATA and ERRORS, then refresh buffers."
  (let ((entry (gethash key gh-magit--cache)))
    (when (and entry (= generation (plist-get entry :generation)))
      (puthash key
               (list :loading nil :generation generation :data data
                     :errors errors :created (float-time))
               gh-magit--cache)
      (run-at-time 0 nil #'gh-magit--refresh-buffers key))))

(defun gh-magit--start-request (key context sections &optional force)
  "Start all summary requests for KEY, CONTEXT, and SECTIONS.
When FORCE is non-nil, bypass the shared query cache."
  (let* ((old (gethash key gh-magit--cache))
         (generation (cl-incf gh-magit--generation))
         (tasks (gh-magit--task-keys sections))
         (remaining (length tasks))
         (data (copy-tree (plist-get old :data)))
         errors)
    (puthash key
             (list :loading t :generation generation :data data
                   :errors nil :created (plist-get old :created))
             gh-magit--cache)
    (if (zerop remaining)
        (gh-magit--finish-request key generation data nil)
      (dolist (task tasks)
        (gh-magit--task
         task context
         (lambda (value)
           (setf (alist-get task data) value)
           (when (zerop (cl-decf remaining))
             (gh-magit--finish-request key generation data errors)))
         (lambda (request-error)
           (push (cons task request-error) errors)
           (when (zerop (cl-decf remaining))
             (gh-magit--finish-request key generation data errors)))
         force)))))

(defun gh-magit--item-context (fallback data)
  "Return context for DATA, using FALLBACK for repository-scoped rows."
  (let* ((repository (gh-core--alist-get 'repository data))
         (name (or (gh-core--alist-get 'nameWithOwner repository)
                   (gh-core--alist-get 'fullName repository)
                   (gh-core--alist-get 'full_name repository))))
    (if name
        (gh-context-from-repository name (gh-context-host fallback))
      fallback)))

(defun gh-magit--resource (kind context data)
  "Create a KIND resource from summary DATA and CONTEXT."
  (setq context (gh-magit--item-context context data))
  (pcase kind
    ('pr (gh-pr--resource context data))
    ('issue (gh-issue--resource context data))
    ('run (gh-actions--run-resource context data))))

(defun gh-magit--comments-count (data)
  "Return normalized comment count from summary DATA."
  (let ((comments (gh-core--alist-get 'comments data)))
    (or (gh-core--alist-get 'commentsCount data)
        (and (listp comments) (length comments)) 0)))

(defun gh-magit--insert-topic (kind context data)
  "Insert one Issue or Pull Request DATA row of KIND in CONTEXT."
  (let* ((resource (gh-magit--resource kind context data))
         (number (gh-core--alist-get 'number data))
         (state (if (gh-core--alist-get 'isDraft data)
                    "DRAFT" (or (gh-core--alist-get 'state data) "")))
         (title (or (gh-core--alist-get 'title data) ""))
         (repo (plist-get resource :repository))
         (author (gh-core--name (gh-core--alist-get 'author data)))
         (review (and (eq kind 'pr)
                      (gh-core--alist-get 'reviewDecision data))))
    (gh-ui--section (gh-topic (cons repo number) resource t)
      (gh-ui--row
       (gh-ui--styled (upcase state) (gh-core--state-face state))
       (gh-ui--styled (format "#%s" number) 'gh-resource-number)
       (gh-ui--styled title 'gh-resource-title)
       (and (eq gh-magit-summary-scope 'user)
            (gh-ui--styled (format "[%s]" repo) 'gh-repository))
       (gh-ui--styled author 'gh-author)
       (and review (gh-ui--styled review (gh-core--state-face review)))
       (gh-ui--styled
        (gh-core--date (gh-core--alist-get 'updatedAt data)) 'gh-date))
      (when (eq kind 'pr)
        (gh-ui--insert-header
         "Branches"
         (format "%s → %s"
                 (or (gh-core--alist-get 'headRefName data) "?")
                 (or (gh-core--alist-get 'baseRefName data) "?"))
         'gh-branch)
        (gh-ui--insert-header "Review"
                              review (and review (gh-core--state-face review))))
      (gh-ui--insert-header "Labels"
                            (gh-core--names
                             (gh-core--alist-get 'labels data))
                            'gh-label)
      (when (eq kind 'issue)
        (gh-ui--insert-header
         "Assigned"
         (gh-core--names (gh-core--alist-get 'assignees data)) 'gh-author))
      (gh-ui--insert-header "Comments" (gh-magit--comments-count data)))))

(defun gh-magit--insert-run (context data)
  "Insert one Actions run DATA row in CONTEXT."
  (let* ((resource (gh-magit--resource 'run context data))
         (id (plist-get resource :id))
         (state (or (gh-core--alist-get 'conclusion data)
                    (gh-core--alist-get 'status data) "")))
    (gh-ui--section (gh-run id resource t)
      (gh-ui--row
       (gh-ui--styled (upcase state) (gh-core--state-face state))
       (gh-ui--styled (gh-core--alist-get 'displayTitle data)
                      'gh-resource-title)
       (gh-ui--styled (or (gh-core--alist-get 'workflowName data)
                          (gh-core--alist-get 'name data))
                      'gh-workflow)
       (gh-ui--styled
        (gh-core--date (gh-core--alist-get 'createdAt data)) 'gh-date))
      (gh-ui--insert-header "Branch"
                            (gh-core--alist-get 'headBranch data) 'gh-branch)
      (gh-ui--insert-header "Event" (gh-core--alist-get 'event data)))))

(defun gh-magit--insert-group (heading kind context items)
  "Insert HEADING containing KIND rows from ITEMS in CONTEXT."
  (magit-insert-section (gh-group heading)
    (magit-insert-heading heading)
    (magit-insert-section-body
      (if items
          (dolist (item (seq-take items gh-magit-list-limit))
            (if (eq kind 'run)
                (gh-magit--insert-run context item)
              (gh-magit--insert-topic kind context item)))
        (insert (propertize "  No matching items\n"
                            'font-lock-face 'shadow))))))

(defun gh-magit--insert-repository-data (context sections data)
  "Insert repository-scoped SECTIONS from DATA in CONTEXT."
  (when (memq 'pr sections)
    (let* ((items (alist-get 'pr data))
           (branch (gh-context-branch context))
           (current (and branch
                         (seq-filter
                          (lambda (item)
                            (equal branch
                                   (gh-core--alist-get 'headRefName item)))
                          items)))
           (other (if current (seq-difference items current #'equal) items)))
      (magit-insert-section (gh-pull-requests)
        (magit-insert-heading "Pull requests")
        (magit-insert-section-body
          (when current
            (gh-magit--insert-group "Current branch" 'pr context current))
          (gh-magit--insert-group "Open" 'pr context other)))))
  (when (memq 'issue sections)
    (magit-insert-section (gh-issues)
      (magit-insert-heading "Issues")
      (magit-insert-section-body
        (gh-magit--insert-group "Open" 'issue context
                                (alist-get 'issue data)))))
  (when (memq 'run sections)
    (magit-insert-section (gh-actions)
      (magit-insert-heading "Actions")
      (magit-insert-section-body
        (gh-magit--insert-group "Recent" 'run context
                                (alist-get 'run data))))))

(defun gh-magit--insert-user-data (context sections data)
  "Insert user-scoped SECTIONS from DATA in CONTEXT."
  (when (memq 'pr sections)
    (magit-insert-section (gh-pull-requests)
      (magit-insert-heading "Pull requests")
      (magit-insert-section-body
        (dolist (group '(("Current branch" . branch-prs)
                         ("Needs review" . review-prs)
                         ("Assigned" . assigned-prs)
                         ("Mentioned" . mentioned-prs)
                         ("Created by you" . created-prs)))
          (gh-magit--insert-group (car group) 'pr context
                                  (alist-get (cdr group) data))))))
  (when (memq 'issue sections)
    (magit-insert-section (gh-issues)
      (magit-insert-heading "Issues")
      (magit-insert-section-body
        (dolist (group '(("Assigned" . assigned-issues)
                         ("Mentioned" . mentioned-issues)
                         ("Created by you" . created-issues)))
          (gh-magit--insert-group (car group) 'issue context
                                  (alist-get (cdr group) data))))))
  (when (memq 'run sections)
    (magit-insert-section (gh-actions)
      (magit-insert-heading "Actions")
      (magit-insert-section-body
        (gh-magit--insert-group "Recent in this repository" 'run context
                                (alist-get 'run data))))))

(defun gh-magit--insert-errors (errors)
  "Insert compact inline ERRORS for failed summary requests."
  (dolist (entry errors)
    (insert (propertize
             (format "  %s: %s\n" (car entry)
                     (gh-error-message (cdr entry)))
             'font-lock-face 'gh-error))))

;;;###autoload
(defun gh-magit-insert-github ()
  "Insert asynchronous GitHub summaries into the current Magit status."
  (when-let* ((context (gh-magit--context))
              (sections (gh-magit--effective-sections)))
    (let* ((key (gh-magit--key context sections))
           (entry (gethash key gh-magit--cache)))
      (setq gh-magit--context-key key)
      (unless (or (plist-get entry :loading) (gh-magit--fresh-p entry))
        (gh-magit--start-request key context sections))
      (setq entry (gethash key gh-magit--cache))
      (magit-insert-section (github)
        (magit-insert-heading "GitHub")
        (magit-insert-section-body
          (let ((data (plist-get entry :data)))
            (cond
             ((and (plist-get entry :loading) (null data))
              (insert (propertize "  loading…\n"
                                  'font-lock-face 'gh-loading)))
             (t
              (when (plist-get entry :loading)
                (insert (propertize "  refreshing…\n"
                                    'font-lock-face 'gh-loading)))
              (if (eq gh-magit-summary-scope 'user)
                  (gh-magit--insert-user-data context sections data)
                (gh-magit--insert-repository-data context sections data))
              (gh-magit--insert-errors (plist-get entry :errors))))))))))

;;;###autoload
(defun gh-magit-refresh (&optional both-layers)
  "Refresh GitHub summaries in Magit status.
With prefix argument BOTH-LAYERS, also invalidate matching common API cache."
  (interactive "P")
  (let ((context (gh-magit--context)))
    (if gh-magit--context-key
        (remhash gh-magit--context-key gh-magit--cache)
      (clrhash gh-magit--cache))
    (when (and both-layers context)
      (gh-api--invalidate
       (list :host (gh-context-host context)
             :repository (gh-context-repository context))))
    (if (derived-mode-p 'magit-status-mode)
        (magit-refresh-buffer)
      (message "GitHub Magit summary cache cleared"))))

(defun gh-magit-toggle-scope ()
  "Toggle between repository and current-user Magit summaries."
  (interactive)
  (setq gh-magit-summary-scope
        (if (eq gh-magit-summary-scope 'repository) 'user 'repository))
  (message "GitHub Magit scope: %s" gh-magit-summary-scope)
  (when (derived-mode-p 'magit-status-mode)
    (magit-refresh-buffer)))

(transient-define-prefix gh-magit-dispatch ()
  "GitHub actions for the current Magit repository."
  [["Open"
    ("s" "Repository status" gh-repo-status)
    ("p" "Pull requests" gh-pr-list)
    ("i" "Issues" gh-issue-list)
    ("a" "Actions" gh-run-list)]
   ["Summary"
    ("g" "Refresh" gh-magit-refresh)
    ("m" "Toggle Magit scope" gh-magit-toggle-scope)]
   ["More"
    ("@" "Main GitHub dispatch" gh-dispatch)]])

(defun gh-magit--install-dispatch ()
  "Install the GitHub suffix after Magit's Run suffix."
  (when (and (fboundp 'transient-append-suffix)
             (not (ignore-errors
                    (transient-get-suffix 'magit-dispatch gh-magit-dispatch-key))))
    (transient-append-suffix
      'magit-dispatch "!"
      `(,gh-magit-dispatch-key "GitHub" gh-magit-dispatch))))

(defun gh-magit--remove-dispatch ()
  "Remove the GitHub suffix from Magit's dispatcher."
  (when (and (fboundp 'transient-remove-suffix)
             (ignore-errors
               (transient-get-suffix 'magit-dispatch gh-magit-dispatch-key)))
    (transient-remove-suffix 'magit-dispatch gh-magit-dispatch-key)))

;;;###autoload
(define-minor-mode gh-magit-mode
  "Globally add asynchronous GitHub summaries to Magit status."
  :global t
  :group 'gh-magit
  (if gh-magit-mode
      (progn
        (add-hook 'magit-status-sections-hook #'gh-magit-insert-github t)
        (gh-magit--install-dispatch))
    (remove-hook 'magit-status-sections-hook #'gh-magit-insert-github)
    (gh-magit--remove-dispatch)))

(provide 'gh-magit)
;;; gh-magit.el ends here
