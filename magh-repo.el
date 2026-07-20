;;; magh-repo.el --- Repository pages and management -*- lexical-binding: t; -*-

;; Copyright (C) 2026
;; SPDX-License-Identifier: GPL-2.0-only

;; Author: magh.el contributors
;; Keywords: tools, vc, github

;;; Commentary:

;; Repository status, selection, statistics, settings, branches, and lifecycle
;; actions.  All remote reads and writes go through `magh-api.el'.

;;; Code:

(require 'seq)
(require 'subr-x)
(require 'transient)
(require 'magh-actions)
(require 'magh-api)
(require 'magh-candidate)
(require 'magh-commit)
(require 'magh-edit)
(require 'magh-release)
(require 'magh-topic)
(require 'magh-ui)

(declare-function magh-project-list "magh-project")
(declare-function magh-discussion-list "magh-discussion")

(defun magh-repo--projects ()
  "Open Projects for the repository context of the current page."
  (interactive)
  (magh-project-list magh-buffer-context))

(defun magh-repo--discussions ()
  "Open Discussions for the repository context of the current page."
  (interactive)
  (magh-discussion-list magh-buffer-context))

(defun magh-repo--buffer-name (context &optional suffix)
  "Return native repository buffer name for CONTEXT and SUFFIX."
  (format "*magh: %s%s*" (magh-context-repository context)
          (if suffix (concat " · " suffix) "")))

(defun magh-repo--resource (data context)
  "Convert repository DATA to a native resource."
  (let* ((data (or (alist-get 'repo data) data))
         (name (or (alist-get 'nameWithOwner data)
                   (alist-get 'full_name data)))
         (resource-context
          (magh-context-from-repository name (magh-context-host context))))
    (magh-resource-create
     'repository resource-context :title name
     :url (or (alist-get 'html_url data) (alist-get 'url data))
     :data data)))

(defun magh-repo--format-candidate (resource)
  "Format repository RESOURCE for completion."
  (let ((repo (plist-get resource :data)))
    (magh-ui--row
     (magh-ui--styled (downcase (alist-get 'visibility repo)) 'magh-permission)
     (magh-ui--styled (magh-resource-title resource) 'magh-repository)
     (alist-get 'description repo))))

(defun magh-repo--select (title data)
  "Select and open a repository from DATA using TITLE."
  (let ((context (magh-context-resolve)))
    (magh-candidate-select-and-open
     title (mapcar (lambda (item) (magh-repo--resource item context)) data)
     #'magh-repo--format-candidate t)))

;;; Status

(defvar-keymap magit-repo-branch-section-map
  :doc "Keymap for branch rows in Repo Status."
  "RET" #'magh-ui-visit
  "<mouse-1>" #'magh-repo-branch-click)

(defun magh-repo-branch-click (event)
  "Switch to the Repo Status branch clicked by mouse EVENT."
  (interactive "e")
  (mouse-set-point event)
  (magh-ui-visit))

(defun magh-repo--fetch-status (context success _error force)
  "Fetch repository status aggregates in CONTEXT."
  (magh-core--collect-async-settled
   (list
    (cons 'repository
          (lambda (ok fail) (magh-api--repo-get context ok fail force)))
    (cons 'viewer-forked
          (lambda (ok fail)
            (magh-api--repo-viewer-forked-p context ok fail force)))
    (cons 'languages
          (lambda (ok fail) (magh-api--repo-languages context ok fail force)))
    (cons 'branches
          (lambda (ok fail) (magh-api--repo-branches context ok fail force)))
    (cons 'issues
          (lambda (ok fail)
            (magh-api--issue-list context (list :state "open" :limit 10)
                                ok fail force)))
    (cons 'prs
          (lambda (ok fail)
            (magh-api--pr-list context (list :state "open" :limit 10)
                             ok fail force)))
    (cons 'runs
          (lambda (ok fail)
            (magh-api--run-list context (list :limit 10) ok fail force)))
    (cons 'commits
          (lambda (ok fail)
            (magh-api--commit-list
             context (list :limit 5 :ref (magh-context-ref context))
             ok fail force)))
    (cons 'releases
          (lambda (ok fail) (magh-api--release-list context ok fail force))))
   success))

(defun magh-repo--insert-topic (kind context data)
  "Insert compact KIND summary from DATA in CONTEXT."
  (pcase kind
    ((or 'issue 'pr)
     (let* ((resource (magh-topic--resource kind context data))
            (number (plist-get resource :number))
            (values (magh-topic--row-values kind data)))
       (magh-ui--section (topic (list kind number) resource t)
         (magh-ui--format-row
          values (if (eq kind 'pr)
                     '(:state :review :identifier :title)
                   '(:state :identifier :title)))
         (when (eq kind 'pr)
           (magh-topic--insert-pr-branches data))
         (magh-topic--insert-metadata kind data))))
    ('run
     (let* ((resource (magh-actions--run-resource context data))
            (id (plist-get resource :id)))
       (magh-ui--section (topic (list kind id) resource t)
         (magh-actions--run-row data)
         (magh-actions--insert-run-metadata
          data :date-label "Updated"
          :date (or (alist-get 'updatedAt data)
                    (alist-get 'createdAt data))))))
    ('release
     (let* ((resource (magh-release--resource context data))
            (tag (plist-get resource :tag))
            (state (magh-release--state data)))
       (magh-ui--section (topic (list kind tag) resource t)
         (magh-ui--row
          (magh-ui--styled tag 'magh-tag)
          (magh-ui--styled (magh-resource-title resource)
                           'magh-resource-title))
         (magh-ui--insert-header "State" state
                                 (magh-core--state-face state))
         (magh-ui--insert-header
          "Author" (magh-core--name (alist-get 'author data)) 'magh-author))))
    (_ (error "Unsupported repository summary kind: %s" kind))))

(defun magh-repo--insert-languages (languages)
  "Insert language percentages from LANGUAGES without byte counts."
  (if-let* ((pairs
             (mapcar (lambda (entry)
                       (cons (symbol-name (car entry)) (cdr entry)))
                     languages))
            (total (apply #'+ (mapcar #'cdr pairs)))
            ((> total 0)))
      (dolist (entry (seq-sort-by #'cdr #'> pairs))
        (insert
         (magh-ui--row
          (magh-ui--styled (concat (car entry) ":") 'magh-metadata-key)
          (format "%.1f%%" (* 100.0 (/ (cdr entry) (float total)))))
         "\n"))
    (insert (propertize "No language data.\n" 'font-lock-face 'shadow))))

(defun magh-repo--stats (repo forked)
  "Format repository statistics and positive viewer states.
FORKED is non-nil when the current viewer owns a fork of REPO."
  (format "%s stars%s, %s forks%s, %s watchers%s"
          (alist-get 'stargazerCount repo)
          (if (magh-api--true-p (alist-get 'viewerHasStarred repo))
              " (starred)"
            "")
          (alist-get 'forkCount repo)
          (if forked " (forked)" "")
          (alist-get 'totalCount (alist-get 'watchers repo))
          (if (equal (alist-get 'viewerSubscription repo) "SUBSCRIBED")
              " (watching)"
            "")))

(defun magh-repo--branch-resource (context data)
  "Create a branch resource for DATA in CONTEXT."
  (let ((name (alist-get 'name data)))
    (magh-resource-create
     'branch (magh-context-copy context :ref name)
     :name name :title name :data data)))

(defun magh-repo--insert-branch (context data current-ref)
  "Insert branch DATA in CONTEXT, marking CURRENT-REF."
  (let* ((name (alist-get 'name data))
         (current (equal name current-ref))
         (resource (magh-repo--branch-resource context data))
         (sha (alist-get 'sha (alist-get 'commit data))))
    (magh-ui--section (repo-branch name resource t)
      (magh-ui--row
       (and current (magh-ui--styled "*" 'magh-open-state))
       (propertize (magh-ui--styled name 'magh-branch)
                   'mouse-face 'highlight
                   'help-echo "Switch Repo Status to this branch"))
      (magh-ui--insert-header "Current" (if current "yes" "no"))
      (magh-ui--insert-header "Protected"
                            (if (magh-api--true-p (alist-get 'protected data))
                                "yes"
                              "no"))
      (magh-ui--insert-header "SHA" sha 'magh-hash))))

(defun magh-repo--render-status (context result)
  "Render repository status RESULT in CONTEXT."
  (let* ((repo (magh-batch-value result 'repository))
         (languages (magh-batch-value result 'languages))
         (branches (magh-batch-value result 'branches))
         (issues (magh-batch-value result 'issues))
         (prs (magh-batch-value result 'prs))
         (runs (magh-batch-value result 'runs))
         (commits (magh-batch-value result 'commits))
         (releases (seq-take (magh-batch-value result 'releases) 5))
         (name (or (alist-get 'nameWithOwner repo)
                   (magh-context-repository context)))
         (repo-resource
          (magh-resource-create
           'repository context :title name
           :url (or (alist-get 'html_url repo) (alist-get 'url repo))
           :data repo))
         (visibility (alist-get 'visibility repo))
         (permission (alist-get 'viewerPermission repo))
         (viewer-forked (magh-batch-value result 'viewer-forked))
         (default-branch (or (magh-core--name (alist-get 'defaultBranchRef repo))
                             (magh-context-default-branch context)))
         (current-ref (or (magh-context-ref context)
                          (magh-context-branch context)
                          default-branch)))
    (magh-ui--insert-header "Repository" name 'magh-repository repo-resource)
    (magh-ui--insert-header "Remote" (magh-context-remote context)
                          'magh-permission)
    (magh-ui--insert-header "Visibility" (and visibility (downcase visibility))
                          'magh-permission)
    (magh-ui--insert-header "User" (and permission (format "(%s)" permission))
                          'magh-permission)
    (magh-ui--insert-header "Default branch" default-branch 'magh-branch)
    (magh-ui--insert-header "Branch" current-ref 'magh-branch)
    (when repo
      (magh-ui--insert-header "Stats" (magh-repo--stats repo viewer-forked)))
    (insert "\n")
    (magh-ui--section (description 'description repo-resource nil)
      "Description"
      (if-let* ((error (magh-batch-error result 'repository)))
          (magh-ui--insert-request-error error)
        (let ((description (alist-get 'description repo)))
          (magh-ui--insert-markdown
           (if (string-empty-p (string-trim (or description "")))
               "No description."
             description)
           context))))
    (when-let* ((error (magh-batch-error result 'viewer-forked)))
      (magh-ui--section (viewer-relationship 'viewer-relationship nil t)
        "Viewer relationship"
        (magh-ui--insert-request-error error)))
    (magh-ui--section (statistics 'statistics
                                (magh-resource-create 'statistics context) t)
      "Statistics"
      (if-let* ((error (magh-batch-error result 'languages)))
          (magh-ui--insert-request-error error)
        (magh-repo--insert-languages languages)))
    (magh-ui--section (branches 'branches nil nil)
      "Branches"
      (if-let* ((error (magh-batch-error result 'branches)))
          (magh-ui--insert-request-error error)
        (dolist (item branches)
          (magh-repo--insert-branch context item current-ref))))
    (magh-ui--section (recent-commits 'recent-commits
                                    (magh-resource-create 'commit-list context) nil)
      "Recent commits"
      (if-let* ((error (magh-batch-error result 'commits)))
          (magh-ui--insert-request-error error)
        (dolist (item commits) (magh-commit--insert-row context item))))
    (magh-ui--section (pull-requests 'pull-requests
                                   (magh-resource-create 'pr-list context) nil)
      "Pull requests"
      (if-let* ((error (magh-batch-error result 'prs)))
          (magh-ui--insert-request-error error)
        (dolist (item prs) (magh-repo--insert-topic 'pr context item))))
    (magh-ui--section (issues 'issues (magh-resource-create 'issue-list context) nil)
      "Issues"
      (if-let* ((error (magh-batch-error result 'issues)))
          (magh-ui--insert-request-error error)
        (dolist (item issues) (magh-repo--insert-topic 'issue context item))))
    (magh-ui--section (actions 'actions (magh-resource-create 'run-list context) nil)
      "Actions"
      (if-let* ((error (magh-batch-error result 'runs)))
          (magh-ui--insert-request-error error)
        (dolist (item runs) (magh-repo--insert-topic 'run context item))))
    (magh-ui--section (releases 'releases
                              (magh-resource-create 'release-list context) t)
      "Releases"
      (if-let* ((error (magh-batch-error result 'releases)))
          (magh-ui--insert-request-error error)
        (dolist (item releases) (magh-repo--insert-topic 'release context item))))))

(defun magh-repo--setup-status-keys (context)
  "Install repository status navigation keys for CONTEXT."
  (local-set-key (kbd "O") #'magh-repo-switch-remote)
  (local-set-key (kbd "i")
                 (lambda () (interactive)
                   (magh-resource-open (magh-resource-create 'issue-list context))))
  (local-set-key (kbd "P")
                 (lambda () (interactive)
                   (magh-resource-open (magh-resource-create 'pr-list context))))
  (local-set-key (kbd "a")
                 (lambda () (interactive)
                   (magh-resource-open (magh-resource-create 'run-list context))))
  (local-set-key (kbd "j") #'magh-repo--projects)
  (local-set-key (kbd "d") #'magh-repo--discussions)
  (local-set-key (kbd "r")
                 (lambda () (interactive)
                   (magh-resource-open (magh-resource-create 'release-list context))))
  (local-set-key (kbd "H")
                 (lambda () (interactive)
                   (magh-resource-open (magh-resource-create 'commit-list context))))
  (local-set-key (kbd "t")
                 (lambda () (interactive)
                   (magh-resource-open
                    (magh-resource-create 'tree context
                                        :ref (magh-context-ref context) :path ""))))
  (local-set-key (kbd "s") #'magh-statistics)
  (local-set-key (kbd "/")
                 (lambda () (interactive)
                   (magh-resource-open
                    (magh-resource-create 'repository-search context))))
  (setq magh-buffer-dispatch-function #'magh-repository-dispatch))

;;;###autoload
(defun magh-repo-status (&optional context)
  "Open a Magit-like status page for repository CONTEXT."
  (interactive)
  (setq context (magh-ui--repository-context context))
  (magh-ui--open-page
   (magh-repo--buffer-name context) context 'repository
   (magh-context-repository context)
   (lambda (success error force)
     (magh-repo--fetch-status context success error force))
   (lambda (data) (magh-repo--render-status context data))
   :setup (lambda () (magh-repo--setup-status-keys context))))

;;;###autoload
(defun magh-repo-status-other (repository)
  "Prompt for REPOSITORY and open its status page."
  (interactive (list (read-string "Repository (OWNER/NAME): ")))
  (magh-repo-status (magh-context-from-repository repository)))

;;;###autoload
(defun magh-repo-switch-remote (&optional remote)
  "Open Repo Status using another local Git REMOTE.
Interactively, offer Git remotes whose URLs identify GitHub repositories."
  (interactive)
  (let* ((context (magh-ui--repository-context))
         (root (magh-context-root context)))
    (unless root
      (user-error "This repository page is not backed by a local Git worktree"))
    (let ((remotes (magh-context-local-remotes context)))
      (unless remotes
        (user-error "No local Git remote identifies a GitHub repository"))
      (let* ((names (mapcar #'car remotes))
             (selected
              (or remote
                  (completing-read
                   "Git remote: " names nil t nil nil
                   (and (member (magh-context-remote context) names)
                        (magh-context-remote context)))))
             (selected-context (cdr (assoc-string selected remotes))))
        (unless selected-context
          (user-error "Unknown GitHub remote: %s" selected))
        (magh-repo-status selected-context)))))

;;; Lists

;;;###autoload
(defun magh-repository-list (&optional owner)
  "Asynchronously list repositories for OWNER or the current account."
  (interactive (list (let ((text (read-string "Owner (empty for current): ")))
                       (unless (string-empty-p text) text))))
  (let ((context (magh-context-resolve)))
    (message "Fetching GitHub repositories…")
    (magh-api--user-repositories
     context owner
     (lambda (data) (magh-repo--select "Repository: " data))
     #'magh-core--user-error)))

;;;###autoload
(defun magh-starred-repositories ()
  "Asynchronously select from repositories starred by the current account."
  (interactive)
  (let ((context (magh-context-resolve)))
    (message "Fetching starred repositories…")
    (magh-api--starred-repositories
     context (lambda (data) (magh-repo--select "Starred repository: " data))
     #'magh-core--user-error)))

;;;###autoload
(defun magh-favorite-repositories ()
  "Select repositories aggregated from `magh-favorite-organizations'."
  (interactive)
  (unless magh-favorite-organizations
    (user-error "`magh-favorite-organizations' is empty"))
  (let ((context (magh-context-resolve)))
    (message "Fetching favorite organization repositories…")
    (magh-core--collect-async
     (mapcar
      (lambda (owner)
        (cons owner
              (lambda (success error)
                (magh-api--user-repositories context owner success error nil))))
      magh-favorite-organizations)
     (lambda (results)
       (magh-repo--select "Favorite repository: "
                          (seq-mapcat #'cdr results)))
     #'magh-core--user-error)))

;;; Statistics

;;;###autoload
(defun magh-statistics (&optional context)
  "Open repository statistics for CONTEXT."
  (interactive)
  (setq context (magh-ui--repository-context context))
  (magh-ui--open-page
   (magh-repo--buffer-name context "Statistics") context 'statistics
   (magh-context-repository context)
   (lambda (success error force)
     (magh-api--repo-languages context success error force))
   (lambda (languages)
     (magh-ui--insert-header "Repository" (magh-context-repository context)
                           'magh-repository)
     (insert "\n")
     (magh-ui--section (languages 'languages nil nil)
       "Languages"
       (magh-repo--insert-languages languages)))))

;;; Structured creation and settings

(defun magh-repository-create ()
  "Create a GitHub repository using a structured editor."
  (interactive)
  (let ((context (magh-context-resolve)))
    (magh-edit-open
     "*magh: Create Repository*"
     '((:name name :required t)
       (:name owner)
       (:name description)
       (:name homepage)
       (:name visibility :choices ("public" "private" "internal") :required t)
       (:name template)
       (:name source)
       (:name push :type boolean)
       (:name clone :type boolean))
     '(:visibility "public" :push :json-false :clone :json-false) ""
     (lambda (values _body success error)
       (let ((visibility (plist-get values :visibility)))
         (magh-api--repo-create
          context
          (append values
                  (pcase visibility
                    ("private" '(:private t)) ("internal" '(:internal t))
                    (_ '(:public t))))
          success error)))
     :after-success
     (lambda (result)
       (when-let* ((resource (magh-resource-from-url (string-trim result) context)))
         (magh-resource-open resource))))))

(defun magh-repo--settings-values (repo)
  "Convert repository REPO data to structured settings values."
  (list
   :repository (alist-get 'nameWithOwner repo)
   :default-branch (magh-core--name (alist-get 'defaultBranchRef repo))
   :description (or (alist-get 'description repo) "")
   :visibility (downcase (alist-get 'visibility repo))
   :homepage (or (alist-get 'homepageUrl repo) "")
   :topics (mapcar #'magh-core--name (alist-get 'repositoryTopics repo))
   :template (or (alist-get 'isTemplate repo) :json-false)
   :issues (or (alist-get 'hasIssuesEnabled repo) :json-false)
   :projects (or (alist-get 'hasProjectsEnabled repo) :json-false)
   :discussions (or (alist-get 'hasDiscussionsEnabled repo) :json-false)
   :wiki (or (alist-get 'hasWikiEnabled repo) :json-false)
   :merge-commit (or (alist-get 'mergeCommitAllowed repo) :json-false)
   :squash-merge (or (alist-get 'squashMergeAllowed repo) :json-false)
   :rebase-merge (or (alist-get 'rebaseMergeAllowed repo) :json-false)
   :delete-branch-on-merge
   (or (alist-get 'deleteBranchOnMerge repo) :json-false)))

;;;###autoload
(defun magh-repository-settings-edit (&optional context)
  "Asynchronously fetch and edit repository settings for CONTEXT."
  (interactive)
  (setq context (magh-ui--repository-context context))
  (message "Fetching repository settings…")
  (magh-api--repo-get
   context
   (lambda (repo)
     (let* ((original (magh-repo--settings-values repo))
            (boolean-fields
             '(template issues projects discussions wiki merge-commit
               squash-merge rebase-merge delete-branch-on-merge))
            (branch-fetch (magh-edit--completion-fetcher
                           #'magh-api--repo-branches context 'name)))
       (magh-edit-open
        (magh-repo--buffer-name context "Settings")
        (append
         `((:name repository :required t)
           (:name default-branch :required t :completion-fetch ,branch-fetch)
           (:name description :allow-empty t)
           (:name visibility :choices ("public" "private" "internal"))
           (:name homepage :allow-empty t)
           (:name topics :multiple t))
         (mapcar (lambda (field) (list :name field :type 'boolean))
                 boolean-fields))
        original "Edit fields above, then press C-c C-c to submit."
        (lambda (values _body success error)
          (let* ((old-topics (plist-get original :topics))
                 (new-topics (plist-get values :topics))
                 (settings
                  (list :default-branch (plist-get values :default-branch)
                        :description (plist-get values :description)
                        :visibility (plist-get values :visibility)
                        :homepage (plist-get values :homepage)
                        :add-topics (seq-difference new-topics old-topics #'string=)
                        :remove-topics (seq-difference old-topics new-topics #'string=))))
            (dolist (field boolean-fields)
              (let ((key (intern (format ":%s" field))))
                (setf (plist-get settings key) (plist-get values key))))
            (magh-api--repo-edit context settings success error))))))
   #'magh-core--user-error))

;;; Lifecycle actions

;;;###autoload
(defun magh-repository-clone (repository directory)
  "Asynchronously clone REPOSITORY into DIRECTORY."
  (interactive
   (let* ((repo (read-string "Repository (OWNER/NAME): "
                             (and magh-buffer-context
                                  (magh-context-repository magh-buffer-context))))
          (directory (read-directory-name "Clone into: " nil nil nil
                                           (file-name-nondirectory repo))))
     (list repo directory)))
  (let ((context (magh-ui--repository-context repository)))
    (message "Cloning %s…" (magh-context-repository context))
    (magh-api--repo-clone
     context directory
     (lambda (_)
       (run-hook-with-args 'magh-repository-post-clone-hook directory)
       (dired directory))
     #'magh-core--user-error)))

;;;###autoload
(defun magh-repository-fork (&optional context)
  "Asynchronously fork repository CONTEXT."
  (interactive)
  (setq context (magh-ui--repository-context context))
  (when (magh-core--confirm (format "Fork %s? " (magh-context-repository context)))
    (magh-api--repo-fork
     context nil
     (lambda (_)
       (run-hook-with-args 'magh-repository-post-fork-hook context)
       (message "Forked %s" (magh-context-repository context)))
     #'magh-core--user-error)))

;;;###autoload
(defun magh-repository-rename (name &optional context)
  "Rename repository CONTEXT to NAME."
  (interactive (list (read-string "New repository name: ")))
  (setq context (magh-ui--repository-context context))
  (when (magh-core--confirm
         (format "Rename %s to %s? " (magh-context-repository context) name))
    (magh-api--repo-rename
     context name
     (lambda (_)
       (magh-repo-status
        (magh-context-copy context :name name
                         :repository (format "%s/%s"
                                             (magh-context-owner context) name))))
     #'magh-core--user-error)))

;;;###autoload
(defun magh-repository-delete (&optional context)
  "Permanently delete repository CONTEXT."
  (interactive)
  (setq context (magh-ui--repository-context context))
  (let ((repo (magh-context-repository context)))
    (when (and (magh-core--confirm (format "Permanently delete %s? " repo))
               (or (not magh-confirm-destructive-actions)
                   (string= (read-string (format "Type %s to confirm: " repo)) repo)))
      (magh-api--repo-delete
       context (lambda (_) (message "Deleted %s" repo))
       #'magh-core--user-error))))

;;;###autoload
(defun magh-branch-create (branch ref &optional context)
  "Create remote BRANCH from REF in repository CONTEXT."
  (interactive (list (read-string "New branch: ")
                     (read-string "Start ref: "
                                  (or (and magh-buffer-context
                                           (magh-context-ref magh-buffer-context))
                                      "HEAD"))))
  (setq context (magh-ui--repository-context context))
  (magh-api--commit-get
   context ref
   (lambda (commit)
     (magh-api--branch-create
      context branch (alist-get 'sha commit)
      (lambda (_) (message "Created branch %s" branch))
      #'magh-core--user-error))
   #'magh-core--user-error))

;;;###autoload
(defun magh-branch-delete (branch &optional context)
  "Delete remote BRANCH in repository CONTEXT."
  (interactive (list (read-string "Delete remote branch: ")))
  (setq context (magh-ui--repository-context context))
  (when (magh-core--confirm (format "Delete remote branch %s? " branch))
    (magh-api--branch-delete
     context branch (lambda (_) (message "Deleted branch %s" branch))
     #'magh-core--user-error)))

(transient-define-prefix magh-repository-dispatch ()
  "Repository actions."
  [["View"
    ("g" "Refresh" magh-ui-refresh)
   ("s" "Statistics" magh-statistics)
    ("j" "Projects" magh-repo--projects)
    ("d" "Discussions" magh-repo--discussions)
    ("b" "Browse" magh-ui-browse)
    ("O" "Switch remote" magh-repo-switch-remote)]
   ["Manage"
    ("E" "Settings" magh-repository-settings-edit)
    ("c" "Clone" magh-repository-clone)
    ("f" "Fork" magh-repository-fork)]
   ["Danger"
    ("R" "Rename" magh-repository-rename)
    ("D" "Delete" magh-repository-delete)]])

;;; Candidate registration

(magh-candidate-register
 'repository
 :open (lambda (resource) (magh-repo-status (plist-get resource :context)))
 :preview (lambda (resource)
            (let ((context (plist-get resource :context)))
              (magh-ui--open-page
               (format "*magh preview: %s*" (magh-context-repository context))
               context 'repository (magh-context-repository context)
               (lambda (ok fail force)
                 (magh-repo--fetch-status context ok fail force))
               (lambda (data) (magh-repo--render-status context data))
               :preview t))))

(magh-candidate-register
 'branch
 :open (lambda (resource)
         (magh-repo-status (plist-get resource :context))))

(magh-candidate-register
 'statistics :open (lambda (resource) (magh-statistics (plist-get resource :context))))

(provide 'magh-repo)
;;; magh-repo.el ends here
