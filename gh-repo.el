;;; gh-repo.el --- Repository pages and management -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; Author: gh.el contributors
;; Keywords: tools, vc, github
;; Package-Requires: ((emacs "31.1") (transient "0.7.0"))

;;; Commentary:

;; Repository status, selection, statistics, settings, branches, and lifecycle
;; actions.  All remote reads and writes go through `gh-api.el'.

;;; Code:

(require 'seq)
(require 'subr-x)
(require 'transient)
(require 'gh-api)
(require 'gh-candidate)
(require 'gh-edit)
(require 'gh-ui)

(defun gh-repo--context (&optional value)
  "Resolve VALUE as a required repository context."
  (gh-context-resolve (or value gh-buffer-context) t))

(defun gh-repo--buffer-name (context &optional suffix)
  "Return native repository buffer name for CONTEXT and SUFFIX."
  (format "*gh: %s%s*" (gh-context-repository context)
          (if suffix (concat " · " suffix) "")))

(defun gh-repo--resource (data context)
  "Convert repository DATA to a native resource."
  (let* ((data (or (alist-get 'repo data) data))
         (name (or (alist-get 'nameWithOwner data)
                   (alist-get 'full_name data)))
         (resource-context
          (gh-context-from-repository name (gh-context-host context))))
    (gh-resource-create
     'repository resource-context :title name
     :url (or (alist-get 'html_url data) (alist-get 'url data))
     :data data)))

(defun gh-repo--format-candidate (resource)
  "Format repository RESOURCE for completion."
  (let ((repo (plist-get resource :data)))
    (gh-ui--row
     (gh-ui--styled (downcase (alist-get 'visibility repo)) 'gh-permission)
     (gh-ui--styled (gh-resource-title resource) 'gh-repository)
     (alist-get 'description repo))))

(defun gh-repo--select (title data)
  "Select and open a repository from DATA using TITLE."
  (let ((context (gh-context-resolve)))
    (gh-candidate-select-and-open
     title (mapcar (lambda (item) (gh-repo--resource item context)) data)
     #'gh-repo--format-candidate t)))

;;; Status

(defun gh-repo--fetch-status (context success error force)
  "Fetch repository status aggregates in CONTEXT."
  (gh-core--collect-async
   (list
    (cons 'repository
          (lambda (ok fail) (gh-api--repo-get context ok fail force)))
    (cons 'languages
          (lambda (ok fail) (gh-api--repo-languages context ok fail force)))
    (cons 'issues
          (lambda (ok fail)
            (gh-api--issue-list context (list :state "open" :limit 10)
                                ok fail force)))
    (cons 'prs
          (lambda (ok fail)
            (gh-api--pr-list context (list :state "open" :limit 10)
                             ok fail force)))
    (cons 'runs
          (lambda (ok fail)
            (gh-api--run-list context (list :limit 10) ok fail force)))
    (cons 'releases
          (lambda (ok fail) (gh-api--release-list context ok fail force))))
   success error))

(defun gh-repo--topic-resource (kind context data)
  "Create a resource of KIND in CONTEXT from DATA."
  (pcase kind
    ('issue
     (gh-resource-create
      'issue context :number (alist-get 'number data)
      :title (alist-get 'title data)
      :url (alist-get 'url data)))
    ('pr
     (gh-resource-create
      'pr context :number (alist-get 'number data)
      :title (alist-get 'title data)
      :url (alist-get 'url data)))
    ('run
     (gh-resource-create
      'run context :id (alist-get 'databaseId data)
      :title (alist-get 'displayTitle data)
      :url (alist-get 'url data)))
    ('release
     (gh-resource-create
      'release context :tag (alist-get 'tagName data)
      :title (or (alist-get 'name data)
                 (alist-get 'tagName data))
      :url (alist-get 'url data)))))

(defun gh-repo--insert-topic (kind context data)
  "Insert compact KIND topic from DATA in CONTEXT."
  (let* ((resource (gh-repo--topic-resource kind context data))
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
         (author (gh-core--name (alist-get 'author data)))
         (review (alist-get 'reviewDecision data))
         (workflow (or (alist-get 'workflowName data)
                       (and (eq kind 'run) (alist-get 'name data))))
         (comment-count (gh-core--comments-count data))
         (updated (or (alist-get 'updatedAt data)
                      (alist-get 'publishedAt data)
                      (alist-get 'createdAt data)))
         (identifier
          (pcase kind
            ((or 'issue 'pr)
             (gh-ui--styled (format "#%s" number) 'gh-resource-number))
            ('release
             (gh-ui--styled (alist-get 'tagName data) 'gh-tag))))
         (key (or number (plist-get resource :id) (plist-get resource :tag))))
    (gh-ui--section (topic (list kind key) resource t)
      (gh-ui--format-row
       (list :state (and (not (eq kind 'release))
                         (gh-ui--styled (upcase state)
                                        (gh-core--state-face state)))
             :identifier identifier
             :title (gh-ui--styled title 'gh-resource-title)
             :author (gh-ui--styled author 'gh-author)
             :review (gh-ui--styled review (gh-core--state-face review))
             :workflow (gh-ui--styled workflow 'gh-workflow)
             :updated (gh-ui--styled (gh-core--date updated) 'gh-date))
       (pcase kind
         ('pr '(:state :identifier :title :author :review :updated))
         ('issue '(:state :identifier :title :author :updated))
         ('run '(:state :title :workflow :updated))
         ('release '(:identifier :title))))
      (when (eq kind 'pr)
        (gh-ui--insert-header
         "Branches" (format "%s → %s"
                            (alist-get 'headRefName data)
                            (alist-get 'baseRefName data))
         'gh-branch))
      (when (eq kind 'run)
        (gh-ui--insert-header "Branch" (alist-get 'headBranch data)
                              'gh-branch)
        (gh-ui--insert-header "Event" (alist-get 'event data)))
      (when (memq kind '(issue pr))
        (gh-ui--insert-header
         "Labels" (gh-core--names (alist-get 'labels data)) 'gh-label)
        (when (eq kind 'issue)
          (gh-ui--insert-header
           "Assigned" (gh-core--names (alist-get 'assignees data))
           'gh-author))
        (gh-ui--insert-header "Comments" comment-count))
      (when (eq kind 'release)
        (gh-ui--insert-header "State" (downcase state)
                              (gh-core--state-face state))
        (gh-ui--insert-header "Author" author 'gh-author)))))

(defun gh-repo--render-status (context result)
  "Render repository status RESULT in CONTEXT."
  (let* ((repo (alist-get 'repository result))
         (languages (alist-get 'languages result))
         (issues (alist-get 'issues result))
         (prs (alist-get 'prs result))
         (runs (alist-get 'runs result))
         (releases (alist-get 'releases result))
         (repo-resource (gh-repo--resource repo context))
         (name (alist-get 'nameWithOwner repo))
         (visibility (alist-get 'visibility repo))
         (permission (alist-get 'viewerPermission repo))
         (stars (alist-get 'stargazerCount repo))
         (forks (alist-get 'forkCount repo)))
    (gh-ui--insert-header "Repository" name 'gh-repository repo-resource)
    (gh-ui--insert-header "Visibility" (downcase visibility)
                          'gh-permission)
    (gh-ui--insert-header "User" (and permission (format "(%s)" permission))
                          'gh-permission)
    (gh-ui--insert-header "Default branch"
                          (gh-core--name (alist-get 'defaultBranchRef repo))
                          'gh-branch)
    (gh-ui--insert-header "Stats" (format "%s stars, %s forks" stars forks))
    (insert "\n")
    (gh-ui--section (description 'description repo-resource nil)
      "Description"
      (gh-ui--insert-markdown (or (alist-get 'description repo)
                                  "No description.") context))
    (gh-ui--section (statistics 'statistics
                                (gh-resource-create 'statistics context) t)
      "Statistics"
      (gh-ui--insert-header "Stars" stars)
      (gh-ui--insert-header "Watchers"
                            (alist-get 'totalCount (alist-get 'watchers repo)))
      (gh-ui--insert-header "Forks" forks)
      (gh-ui--insert-header "Open issues"
                            (alist-get 'totalCount (alist-get 'issues repo)))
      (gh-ui--insert-header "Size" (and (alist-get 'diskUsage repo)
                                        (format "%s KiB"
                                                (alist-get 'diskUsage repo))))
      (when languages
        (insert "\n")
        (let* ((pairs (mapcar (lambda (entry) (cons (symbol-name (car entry))
                                                     (cdr entry))) languages))
               (total (apply #'+ (mapcar #'cdr pairs))))
          (dolist (entry (sort pairs (lambda (a b) (> (cdr a) (cdr b)))))
            (insert
             (gh-ui--row
              (gh-ui--styled (concat (car entry) ":") 'gh-metadata-key)
              (format "%.1f%%" (* 100.0 (/ (cdr entry) (float total))))
              (format "(%s bytes)" (cdr entry)))
             "\n")))))
    (gh-ui--section (pull-requests 'pull-requests
                                   (gh-resource-create 'pr-list context) nil)
      (format "Pull requests (%d)" (length prs))
      (dolist (item prs) (gh-repo--insert-topic 'pr context item)))
    (gh-ui--section (issues 'issues (gh-resource-create 'issue-list context) nil)
      (format "Issues (%d)" (length issues))
      (dolist (item issues) (gh-repo--insert-topic 'issue context item)))
    (gh-ui--section (actions 'actions (gh-resource-create 'run-list context) nil)
      (format "Actions (%d recent)" (length runs))
      (dolist (item runs) (gh-repo--insert-topic 'run context item)))
    (gh-ui--section (releases 'releases
                              (gh-resource-create 'release-list context) t)
      (format "Releases (%d recent)" (length releases))
      (dolist (item releases) (gh-repo--insert-topic 'release context item)))))

(defun gh-repo--setup-status-keys (context)
  "Install repository status navigation keys for CONTEXT."
  (local-set-key (kbd "i")
                 (lambda () (interactive)
                   (gh-resource-open (gh-resource-create 'issue-list context))))
  (local-set-key (kbd "P")
                 (lambda () (interactive)
                   (gh-resource-open (gh-resource-create 'pr-list context))))
  (local-set-key (kbd "a")
                 (lambda () (interactive)
                   (gh-resource-open (gh-resource-create 'run-list context))))
  (local-set-key (kbd "r")
                 (lambda () (interactive)
                   (gh-resource-open (gh-resource-create 'release-list context))))
  (local-set-key (kbd "H")
                 (lambda () (interactive)
                   (gh-resource-open (gh-resource-create 'commit-list context))))
  (local-set-key (kbd "t")
                 (lambda () (interactive)
                   (gh-resource-open
                    (gh-resource-create 'tree context
                                        :ref (gh-context-ref context) :path ""))))
  (local-set-key (kbd "s") #'gh-statistics)
  (local-set-key (kbd "/")
                 (lambda () (interactive)
                   (gh-resource-open
                    (gh-resource-create 'repository-search context))))
  (setq gh-buffer-dispatch-function #'gh-repository-dispatch))

;;;###autoload
(defun gh-repo-status (&optional context)
  "Open a Magit-like status page for repository CONTEXT."
  (interactive)
  (setq context (gh-repo--context context))
  (gh-ui--open-page
   (gh-repo--buffer-name context) context 'repository
   (gh-context-repository context)
   (lambda (success error force)
     (gh-repo--fetch-status context success error force))
   (lambda (data) (gh-repo--render-status context data))
   :setup (lambda () (gh-repo--setup-status-keys context))))

;;;###autoload
(defun gh-repo-status-other (repository)
  "Prompt for REPOSITORY and open its status page."
  (interactive (list (read-string "Repository (OWNER/NAME): ")))
  (gh-repo-status (gh-context-from-repository repository)))

;;; Lists

;;;###autoload
(defun gh-repository-list (&optional owner)
  "Asynchronously list repositories for OWNER or the current account."
  (interactive (list (let ((text (read-string "Owner (empty for current): ")))
                       (unless (string-empty-p text) text))))
  (let ((context (gh-context-resolve)))
    (message "Fetching GitHub repositories…")
    (gh-api--user-repositories
     context owner
     (lambda (data) (gh-repo--select "Repository: " data))
     #'gh-core--user-error)))

;;;###autoload
(defun gh-starred-repositories ()
  "Asynchronously select from repositories starred by the current account."
  (interactive)
  (let ((context (gh-context-resolve)))
    (message "Fetching starred repositories…")
    (gh-api--starred-repositories
     context (lambda (data) (gh-repo--select "Starred repository: " data))
     #'gh-core--user-error)))

;;;###autoload
(defun gh-favorite-repositories ()
  "Select repositories aggregated from `gh-favorite-organizations'."
  (interactive)
  (unless gh-favorite-organizations
    (user-error "`gh-favorite-organizations' is empty"))
  (let ((context (gh-context-resolve)))
    (message "Fetching favorite organization repositories…")
    (gh-core--collect-async
     (mapcar
      (lambda (owner)
        (cons owner
              (lambda (success error)
                (gh-api--user-repositories context owner success error nil))))
      gh-favorite-organizations)
     (lambda (results)
       (gh-repo--select "Favorite repository: "
                        (apply #'append (mapcar #'cdr results))))
     #'gh-core--user-error)))

;;; Statistics

;;;###autoload
(defun gh-statistics (&optional context)
  "Open repository statistics for CONTEXT."
  (interactive)
  (setq context (gh-repo--context context))
  (gh-ui--open-page
   (gh-repo--buffer-name context "Statistics") context 'statistics
   (gh-context-repository context)
   (lambda (success error force)
     (gh-core--collect-async
      (list
       (cons 'repository
             (lambda (ok fail) (gh-api--repo-get context ok fail force)))
       (cons 'languages
             (lambda (ok fail) (gh-api--repo-languages context ok fail force))))
      success error))
   (lambda (data)
     (let ((repo (alist-get 'repository data))
           (languages (alist-get 'languages data)))
       (gh-ui--insert-header "Repository" (gh-context-repository context)
                             'gh-repository)
       (insert "\n")
       (gh-ui--section (statistics 'repository-statistics nil nil)
         "Repository"
         (dolist (entry `(("Stars" . ,(alist-get 'stargazerCount repo))
                          ("Forks" . ,(alist-get 'forkCount repo))
                          ("Size" . ,(and (alist-get 'diskUsage repo)
                                          (format "%s KiB"
                                                  (alist-get 'diskUsage repo))))))
           (gh-ui--insert-header (car entry) (cdr entry))))
       (gh-ui--section (languages 'languages nil nil)
         "Languages"
         (let ((total (apply #'+ (mapcar #'cdr languages))))
           (dolist (entry (sort (copy-sequence languages)
                                (lambda (a b) (> (cdr a) (cdr b)))))
             (insert
              (gh-ui--row
               (gh-ui--styled
                (concat (symbol-name (car entry)) ":") 'gh-metadata-key)
               (format "%.2f%%" (* 100.0 (/ (cdr entry) (float total))))
               (format "%s bytes" (cdr entry)))
              "\n"))))))))

;;; Structured creation and settings

(defun gh-repository-create ()
  "Create a GitHub repository using a structured editor."
  (interactive)
  (let ((context (gh-context-resolve)))
    (gh-edit-open
     "*gh: Create Repository*"
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
         (gh-api--repo-create
          context
          (append values
                  (pcase visibility
                    ("private" '(:private t)) ("internal" '(:internal t))
                    (_ '(:public t))))
          success error)))
     :after-success
     (lambda (result)
       (when-let* ((resource (gh-resource-from-url (string-trim result) context)))
         (gh-resource-open resource))))))

(defun gh-repo--settings-values (repo)
  "Convert repository REPO data to structured settings values."
  (list
   :repository (alist-get 'nameWithOwner repo)
   :default-branch (gh-core--name (alist-get 'defaultBranchRef repo))
   :description (or (alist-get 'description repo) "")
   :visibility (downcase (alist-get 'visibility repo))
   :homepage (or (alist-get 'homepageUrl repo) "")
   :topics (mapcar #'gh-core--name (alist-get 'repositoryTopics repo))
   :template (if (alist-get 'isTemplate repo) t :json-false)
   :issues (if (alist-get 'hasIssuesEnabled repo) t :json-false)
   :projects (if (alist-get 'hasProjectsEnabled repo) t :json-false)
   :discussions (if (alist-get 'hasDiscussionsEnabled repo) t :json-false)
   :wiki (if (alist-get 'hasWikiEnabled repo) t :json-false)
   :merge-commit (if (alist-get 'mergeCommitAllowed repo) t :json-false)
   :squash-merge (if (alist-get 'squashMergeAllowed repo) t :json-false)
   :rebase-merge (if (alist-get 'rebaseMergeAllowed repo) t :json-false)
   :delete-branch-on-merge
   (if (alist-get 'deleteBranchOnMerge repo) t :json-false)))

;;;###autoload
(defun gh-repository-settings-edit (&optional context)
  "Asynchronously fetch and edit repository settings for CONTEXT."
  (interactive)
  (setq context (gh-repo--context context))
  (message "Fetching repository settings…")
  (gh-api--repo-get
   context
   (lambda (repo)
     (let* ((original (gh-repo--settings-values repo))
            (boolean-fields
             '(template issues projects discussions wiki merge-commit
               squash-merge rebase-merge delete-branch-on-merge))
            (branch-fetch
             (lambda (ok fail)
               (gh-api--repo-branches
                context
                (lambda (items)
                  (funcall ok (mapcar (lambda (item)
                                       (alist-get 'name item)) items)))
                fail))))
       (gh-edit-open
        (gh-repo--buffer-name context "Settings")
        (append
         `((:name repository :required t)
           (:name default-branch :required t :completion-fetch ,branch-fetch)
           (:name description)
           (:name visibility :choices ("public" "private" "internal"))
           (:name homepage)
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
                (setf (plist-get settings key) (eq (plist-get values key) t))))
            (gh-api--repo-edit context settings success error))))))
   #'gh-core--user-error))

;;; Lifecycle actions

;;;###autoload
(defun gh-repository-clone (repository directory)
  "Asynchronously clone REPOSITORY into DIRECTORY."
  (interactive
   (let* ((repo (read-string "Repository (OWNER/NAME): "
                             (and gh-buffer-context
                                  (gh-context-repository gh-buffer-context))))
          (directory (read-directory-name "Clone into: " nil nil nil
                                           (file-name-nondirectory repo))))
     (list repo directory)))
  (let ((context (gh-repo--context repository)))
    (when (file-exists-p directory)
      (user-error "Destination already exists: %s" directory))
    (message "Cloning %s…" (gh-context-repository context))
    (gh-api--repo-clone
     context directory
     (lambda (_)
       (run-hook-with-args 'gh-repository-post-clone-hook directory)
       (dired directory))
     #'gh-core--user-error)))

;;;###autoload
(defun gh-repository-fork (&optional context)
  "Asynchronously fork repository CONTEXT."
  (interactive)
  (setq context (gh-repo--context context))
  (when (gh-core--confirm (format "Fork %s? " (gh-context-repository context)))
    (gh-api--repo-fork
     context nil
     (lambda (_)
       (run-hook-with-args 'gh-repository-post-fork-hook context)
       (message "Forked %s" (gh-context-repository context)))
     #'gh-core--user-error)))

;;;###autoload
(defun gh-repository-rename (name &optional context)
  "Rename repository CONTEXT to NAME."
  (interactive (list (read-string "New repository name: ")))
  (setq context (gh-repo--context context))
  (when (gh-core--confirm
         (format "Rename %s to %s? " (gh-context-repository context) name))
    (gh-api--repo-rename
     context name
     (lambda (_)
       (gh-repo-status
        (gh-context-copy context :name name
                         :repository (format "%s/%s"
                                             (gh-context-owner context) name))))
     #'gh-core--user-error)))

;;;###autoload
(defun gh-repository-delete (&optional context)
  "Permanently delete repository CONTEXT."
  (interactive)
  (setq context (gh-repo--context context))
  (let ((repo (gh-context-repository context)))
    (when (and (gh-core--confirm (format "Permanently delete %s? " repo))
               (or (not gh-confirm-destructive-actions)
                   (string= (read-string (format "Type %s to confirm: " repo)) repo)))
      (gh-api--repo-delete
       context (lambda (_) (message "Deleted %s" repo))
       #'gh-core--user-error))))

;;;###autoload
(defun gh-branch-create (branch ref &optional context)
  "Create remote BRANCH from REF in repository CONTEXT."
  (interactive (list (read-string "New branch: ")
                     (read-string "Start ref: "
                                  (or (and gh-buffer-context
                                           (gh-context-ref gh-buffer-context))
                                      "HEAD"))))
  (setq context (gh-repo--context context))
  (gh-api--commit-get
   context ref
   (lambda (commit)
     (gh-api--branch-create
      context branch (alist-get 'sha commit)
      (lambda (_) (message "Created branch %s" branch))
      #'gh-core--user-error))
   #'gh-core--user-error))

;;;###autoload
(defun gh-branch-delete (branch &optional context)
  "Delete remote BRANCH in repository CONTEXT."
  (interactive (list (read-string "Delete remote branch: ")))
  (setq context (gh-repo--context context))
  (when (gh-core--confirm (format "Delete remote branch %s? " branch))
    (gh-api--branch-delete
     context branch (lambda (_) (message "Deleted branch %s" branch))
     #'gh-core--user-error)))

(transient-define-prefix gh-repository-dispatch ()
  "Repository actions."
  [["View"
    ("g" "Refresh" gh-ui-refresh)
    ("s" "Statistics" gh-statistics)
    ("b" "Browse" gh-ui-browse)]
   ["Manage"
    ("E" "Settings" gh-repository-settings-edit)
    ("c" "Clone" gh-repository-clone)
    ("f" "Fork" gh-repository-fork)]
   ["Danger"
    ("R" "Rename" gh-repository-rename)
    ("D" "Delete" gh-repository-delete)]])

;;; Candidate registration

(gh-candidate-register
 'repository
 :open (lambda (resource) (gh-repo-status (plist-get resource :context)))
 :preview (lambda (resource)
            (let ((context (plist-get resource :context)))
              (gh-ui--open-page
               (format "*gh preview: %s*" (gh-context-repository context))
               context 'repository (gh-context-repository context)
               (lambda (ok fail force)
                 (gh-repo--fetch-status context ok fail force))
               (lambda (data) (gh-repo--render-status context data))
               :preview t))))

(gh-candidate-register
 'statistics :open (lambda (resource) (gh-statistics (plist-get resource :context))))

(provide 'gh-repo)
;;; gh-repo.el ends here
