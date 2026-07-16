;;; magh-repo.el --- Repository pages and management -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; Author: magh.el contributors
;; Keywords: tools, vc, github

;;; Commentary:

;; Repository status, selection, statistics, settings, branches, and lifecycle
;; actions.  All remote reads and writes go through `magh-api.el'.

;;; Code:

(require 'seq)
(require 'subr-x)
(require 'transient)
(require 'magh-api)
(require 'magh-candidate)
(require 'magh-edit)
(require 'magh-ui)

(defun magh-repo--context (&optional value)
  "Resolve VALUE as a required repository context."
  (magh-context-resolve (or value magh-buffer-context) t))

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

(defun magh-repo--fetch-status (context success error force)
  "Fetch repository status aggregates in CONTEXT."
  (magh-core--collect-async
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
   success error))

(defun magh-repo--topic-resource (kind context data)
  "Create a resource of KIND in CONTEXT from DATA."
  (pcase kind
    ((or 'issue 'pr)
     (magh-resource-create
      kind context :number (alist-get 'number data)
      :title (alist-get 'title data)
      :url (alist-get 'url data)))
    ('run
     (magh-resource-create
      'run context :id (alist-get 'databaseId data)
      :title (alist-get 'displayTitle data)
      :url (alist-get 'url data)))
    ('release
     (magh-resource-create
      'release context :tag (alist-get 'tagName data)
      :title (or (alist-get 'name data)
                 (alist-get 'tagName data))
      :url (alist-get 'url data)))))

(defun magh-repo--insert-topic (kind context data)
  "Insert compact KIND topic from DATA in CONTEXT."
  (let* ((resource (magh-repo--topic-resource kind context data))
         (number (alist-get 'number data))
         (state (or (and (alist-get 'isDraft data) "DRAFT")
                    (and (alist-get 'isPrerelease data) "PRERELEASE")
                    (alist-get 'conclusion data)
                    (alist-get 'status data)
                    (alist-get 'state data)
                    (and (eq kind 'release) "PUBLISHED")))
         (title (or (alist-get 'title data)
                    (alist-get 'displayTitle data)
                    (alist-get 'name data)
                    (alist-get 'tagName data)))
         (author (magh-core--name (alist-get 'author data)))
         (review (alist-get 'reviewDecision data))
         (workflow (or (alist-get 'workflowName data)
                       (and (eq kind 'run) (alist-get 'name data))))
         (comment-count (magh-core--comments-count data))
         (updated (or (alist-get 'updatedAt data)
                      (alist-get 'publishedAt data)
                      (alist-get 'createdAt data)))
         (identifier
          (pcase kind
            ((or 'issue 'pr)
             (magh-ui--styled (format "#%s" number) 'magh-resource-number))
            ('release
             (magh-ui--styled (alist-get 'tagName data) 'magh-tag))))
         (key (or number (plist-get resource :id) (plist-get resource :tag))))
    (magh-ui--section (topic (list kind key) resource t)
      (magh-ui--format-row
       (list :state (and (not (eq kind 'release))
                         (magh-ui--styled (upcase state)
                                        (magh-core--state-face state)))
             :identifier identifier
             :title (magh-ui--styled title 'magh-resource-title)
             :author (magh-ui--styled author 'magh-author)
             :review (magh-ui--styled review (magh-core--state-face review))
             :workflow (magh-ui--styled workflow 'magh-workflow)
             :updated (magh-ui--styled (magh-core--date updated) 'magh-date))
       (pcase kind
         ('pr '(:state :identifier :title :review))
         ('issue '(:state :identifier :title))
         ('run '(:state :title :workflow))
         ('release '(:identifier :title))))
      (when (eq kind 'pr)
        (magh-ui--insert-header
         "Branches" (format "%s → %s"
                            (alist-get 'headRefName data)
                            (alist-get 'baseRefName data))
         'magh-branch))
      (when (eq kind 'run)
        (magh-ui--insert-header "Branch" (alist-get 'headBranch data)
                              'magh-branch)
        (magh-ui--insert-header "Event" (alist-get 'event data))
        (magh-ui--insert-header "Updated" (magh-core--date updated) 'magh-date))
      (when (memq kind '(issue pr))
        (magh-ui--insert-header "Author" author 'magh-author)
        (magh-ui--insert-header
         "Labels" (magh-core--names (alist-get 'labels data)) 'magh-label)
        (when (eq kind 'issue)
          (magh-ui--insert-header
           "Assigned" (magh-core--names (alist-get 'assignees data))
           'magh-author))
        (magh-ui--insert-header "Comments" comment-count)
        (magh-ui--insert-header "Updated" (magh-core--date updated) 'magh-date))
      (when (eq kind 'release)
        (magh-ui--insert-header "State" (downcase state)
                              (magh-core--state-face state))
        (magh-ui--insert-header "Author" author 'magh-author)))))

(defun magh-repo--insert-commit (context data)
  "Insert recent Commit DATA in CONTEXT."
  (let* ((sha (alist-get 'sha data))
         (commit (alist-get 'commit data))
         (message (alist-get 'message commit))
         (author (or (magh-core--name (alist-get 'author data))
                     (magh-core--name (alist-get 'author commit))))
         (resource (magh-resource-create
                    'commit context :sha sha
                    :title (car (split-string message "\n"))
                    :url (alist-get 'html_url data))))
    (magh-ui--section (commit sha resource t)
      (magh-ui--row
       (magh-ui--styled (substring sha 0 (min 10 (length sha))) 'magh-hash)
       (magh-ui--styled (magh-resource-title resource) 'magh-resource-title)
       (magh-ui--styled author 'magh-author)
       (magh-ui--styled
        (magh-core--date (alist-get 'date (alist-get 'author commit))) 'magh-date))
      (magh-ui--insert-header "SHA" sha 'magh-hash)
      (magh-ui--insert-header "Author" author 'magh-author))))

(defun magh-repo--insert-languages (languages)
  "Insert language percentages from LANGUAGES without byte counts."
  (if-let* ((pairs
             (mapcar (lambda (entry)
                       (cons (symbol-name (car entry)) (cdr entry)))
                     languages))
            (total (apply #'+ (mapcar #'cdr pairs)))
            ((> total 0)))
      (dolist (entry (sort pairs (lambda (a b) (> (cdr a) (cdr b)))))
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
  (let* ((repo (alist-get 'repository result))
         (languages (alist-get 'languages result))
         (branches (alist-get 'branches result))
         (issues (alist-get 'issues result))
         (prs (alist-get 'prs result))
         (runs (alist-get 'runs result))
         (commits (alist-get 'commits result))
         (releases (seq-take (alist-get 'releases result) 5))
         (repo-resource (magh-repo--resource repo context))
         (name (alist-get 'nameWithOwner repo))
         (visibility (alist-get 'visibility repo))
         (permission (alist-get 'viewerPermission repo))
         (viewer-forked (alist-get 'viewer-forked result))
         (default-branch (magh-core--name (alist-get 'defaultBranchRef repo)))
         (current-ref (or (magh-context-ref context)
                          (magh-context-branch context)
                          default-branch)))
    (magh-ui--insert-header "Repository" name 'magh-repository repo-resource)
    (magh-ui--insert-header "Visibility" (downcase visibility)
                          'magh-permission)
    (magh-ui--insert-header "User" (and permission (format "(%s)" permission))
                          'magh-permission)
    (magh-ui--insert-header "Default branch" default-branch 'magh-branch)
    (magh-ui--insert-header "Branch" current-ref 'magh-branch)
    (magh-ui--insert-header "Stats" (magh-repo--stats repo viewer-forked))
    (insert "\n")
    (magh-ui--section (description 'description repo-resource nil)
      "Description"
      (let ((description (alist-get 'description repo)))
        (magh-ui--insert-markdown
         (if (string-empty-p (string-trim (or description "")))
             "No description."
           description)
         context)))
    (magh-ui--section (statistics 'statistics
                                (magh-resource-create 'statistics context) t)
      "Statistics"
      (magh-repo--insert-languages languages))
    (magh-ui--section (branches 'branches nil nil)
      "Branches"
      (dolist (item branches)
        (magh-repo--insert-branch context item current-ref)))
    (magh-ui--section (recent-commits 'recent-commits
                                    (magh-resource-create 'commit-list context) nil)
      "Recent commits"
      (dolist (item commits) (magh-repo--insert-commit context item)))
    (magh-ui--section (pull-requests 'pull-requests
                                   (magh-resource-create 'pr-list context) nil)
      "Pull requests"
      (dolist (item prs) (magh-repo--insert-topic 'pr context item)))
    (magh-ui--section (issues 'issues (magh-resource-create 'issue-list context) nil)
      "Issues"
      (dolist (item issues) (magh-repo--insert-topic 'issue context item)))
    (magh-ui--section (actions 'actions (magh-resource-create 'run-list context) nil)
      "Actions"
      (dolist (item runs) (magh-repo--insert-topic 'run context item)))
    (magh-ui--section (releases 'releases
                              (magh-resource-create 'release-list context) t)
      "Releases"
      (dolist (item releases) (magh-repo--insert-topic 'release context item)))))

(defun magh-repo--setup-status-keys (context)
  "Install repository status navigation keys for CONTEXT."
  (local-set-key (kbd "i")
                 (lambda () (interactive)
                   (magh-resource-open (magh-resource-create 'issue-list context))))
  (local-set-key (kbd "P")
                 (lambda () (interactive)
                   (magh-resource-open (magh-resource-create 'pr-list context))))
  (local-set-key (kbd "a")
                 (lambda () (interactive)
                   (magh-resource-open (magh-resource-create 'run-list context))))
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
  (setq context (magh-repo--context context))
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
                        (apply #'append (mapcar #'cdr results))))
     #'magh-core--user-error)))

;;; Statistics

;;;###autoload
(defun magh-statistics (&optional context)
  "Open repository statistics for CONTEXT."
  (interactive)
  (setq context (magh-repo--context context))
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
  (setq context (magh-repo--context context))
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
  (let ((context (magh-repo--context repository)))
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
  (setq context (magh-repo--context context))
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
  (setq context (magh-repo--context context))
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
  (setq context (magh-repo--context context))
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
  (setq context (magh-repo--context context))
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
  (setq context (magh-repo--context context))
  (when (magh-core--confirm (format "Delete remote branch %s? " branch))
    (magh-api--branch-delete
     context branch (lambda (_) (message "Deleted branch %s" branch))
     #'magh-core--user-error)))

(transient-define-prefix magh-repository-dispatch ()
  "Repository actions."
  [["View"
    ("g" "Refresh" magh-ui-refresh)
    ("s" "Statistics" magh-statistics)
    ("b" "Browse" magh-ui-browse)]
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
