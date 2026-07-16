;;; magh-search.el --- Cancellable dynamic GitHub search -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; Author: magh.el contributors
;; Keywords: tools, vc, github

;;; Commentary:

;; Consult-powered global and repository-scoped searches.  Each input change
;; cancels the previous gh process; generation checks discard late results.

;;; Code:

(require 'cl-lib)
(require 'consult)
(require 'marginalia)
(require 'subr-x)
(require 'transient)
(require 'magh-api)
(require 'magh-candidate)
(require 'magh-ui)

(defun magh-search--resource (base-context kind data)
  "Convert KIND search DATA to a native resource."
  (let* ((kind (pcase-exhaustive kind
                 ('repos 'repository)
                 ('prs 'pr)
                 ('actions 'run)
                 ('releases 'release)
                 ('branches 'branch)
                 ((or 'issues 'code 'commits) kind)))
         (repo (if (eq kind 'repository)
                   (alist-get 'fullName data)
                 (or (alist-get 'nameWithOwner
                                (alist-get 'repository data))
                     (magh-context-repository base-context))))
         (context (if repo
                      (magh-context-from-repository
                       repo (magh-context-host base-context))
                    base-context)))
    (pcase-exhaustive kind
      ('repository
       (magh-resource-create 'repository context :name repo :title repo
                           :url (alist-get 'url data) :data data))
      ('issues
       (magh-resource-create
        (if (alist-get 'isPullRequest data) 'pr 'issue) context
        :number (alist-get 'number data)
        :title (alist-get 'title data)
        :url (alist-get 'url data) :data data))
      ('pr
       (magh-resource-create 'pr context :number (alist-get 'number data)
                           :title (alist-get 'title data)
                           :url (alist-get 'url data) :data data))
      ('code
       (let* ((path (alist-get 'path data))
              (url (alist-get 'url data))
              (url-resource (magh-resource-from-url url context))
              ;; Search returns a blob SHA, but Contents API needs the commit
              ;; ref embedded in the result URL.
              (ref (or (and (eq (plist-get url-resource :kind) 'file)
                            (plist-get url-resource :ref))
                       (alist-get 'sha data))))
         (magh-resource-create
          'file (magh-context-copy context :ref ref :path path)
          :path path :ref ref :fragment (alist-get 'textMatches data)
          :url url :data data)))
      ('commits
       (magh-resource-create 'commit context :sha (alist-get 'sha data)
                           :title (alist-get
                                   'message (alist-get 'commit data))
                           :url (alist-get 'url data) :data data))
      ('run
       (magh-resource-create 'run context :id (alist-get 'databaseId data)
                           :title (alist-get 'displayTitle data)
                           :url (alist-get 'url data) :data data))
      ('release
       (let ((tag (alist-get 'tagName data)))
         (magh-resource-create
          'release context :tag tag
          :title (or (alist-get 'name data) tag)
          :url (or (alist-get 'url data)
                   (magh-context-web-url context (format "releases/tag/%s" tag)))
          :data data)))
      ('branch
       (let ((name (alist-get 'name data)))
         (magh-resource-create 'branch (magh-context-copy context :ref name)
                             :name name :title name :data data))))))

(defun magh-search--styled (value face)
  "Return completion VALUE carrying FACE directly."
  (when (and value (not (equal value "")))
    (propertize (format "%s" value) 'face face)))

(defun magh-search--format (resource)
  "Format search RESOURCE as a completion candidate."
  (pcase-exhaustive (plist-get resource :kind)
    ('repository
     (magh-search--styled (plist-get resource :repository) 'magh-repository))
    ((or 'issue 'pr)
     (magh-ui--row
      (magh-search--styled (format "#%s" (plist-get resource :number))
                         'magh-resource-number)
      (magh-search--styled (plist-get resource :title) 'magh-resource-title)))
    ('file
     (magh-search--styled (plist-get resource :path) 'magh-file))
    ('commit
     (let ((sha (plist-get resource :sha)))
       (magh-ui--row
        (magh-search--styled (substring sha 0 (min 10 (length sha))) 'magh-hash)
        (magh-search--styled
         (car (split-string (plist-get resource :title) "\n"))
         'magh-resource-title))))
    ('run
     (let* ((data (plist-get resource :data))
            (state (or (alist-get 'conclusion data)
                       (alist-get 'status data))))
       (magh-ui--row
        (magh-search--styled (upcase (or state ""))
                           (magh-core--state-face state))
        (magh-search--styled (magh-resource-title resource) 'magh-resource-title)
        (magh-search--styled (or (alist-get 'workflowName data)
                               (alist-get 'name data))
                           'magh-workflow)
        (magh-search--styled (alist-get 'headBranch data) 'magh-branch))))
    ('release
     (let* ((data (plist-get resource :data))
            (state (cond ((magh-api--true-p (alist-get 'isDraft data)) "draft")
                         ((magh-api--true-p (alist-get 'isPrerelease data))
                          "prerelease")
                         (t "published"))))
       (magh-ui--row
        (magh-search--styled (upcase state) (magh-core--state-face state))
        (magh-search--styled (plist-get resource :tag) 'magh-tag)
        (magh-search--styled (magh-resource-title resource) 'magh-resource-title))))
    ('branch
     (magh-search--styled (plist-get resource :name) 'magh-branch))))

(defun magh-search--marginalia-annotate (candidate)
  "Annotate gh search CANDIDATE using its structured resource data."
  (when-let* ((resource (get-text-property 0 'magh-resource candidate)))
    (let* ((data (plist-get resource :data))
           (repository (plist-get resource :repository))
           (state (alist-get 'state data))
           (author (or (magh-core--name (alist-get 'author data))
                       (magh-core--name
                        (alist-get 'author (alist-get 'commit data))))))
      (pcase-exhaustive (plist-get resource :kind)
        ('repository
         (marginalia--fields
          ((and-let* ((stars (alist-get 'stargazersCount data)))
             (format "★%s" stars))
           :face 'marginalia-number)
          ((alist-get 'description data)
           :truncate 1.0 :face 'marginalia-documentation)))
        ((or 'issue 'pr)
         (marginalia--fields
          (repository :truncate 24 :face 'magh-repository)
          ((upcase (or state "")) :face (magh-core--state-face state))
          (author :truncate 20 :face 'magh-author)))
        ('file
         (marginalia--fields
          (repository :truncate 24 :face 'magh-repository)
          ((and-let* ((matches (alist-get 'textMatches data))
                      (fragment (alist-get 'fragment (car matches))))
             (string-trim fragment))
           :truncate 1.0 :face 'font-lock-string-face)))
        ('commit
         (marginalia--fields
          (repository :truncate 24 :face 'magh-repository)
          (author :truncate 20 :face 'magh-author)))
        ('run
         (let ((run-state (or (alist-get 'conclusion data)
                              (alist-get 'status data))))
           (marginalia--fields
            ((upcase (or run-state ""))
             :face (magh-core--state-face run-state))
            ((alist-get 'event data) :face 'marginalia-type)
            ((alist-get 'headBranch data) :face 'magh-branch))))
        ('release
         (marginalia--fields
          ((magh-core--date (or (alist-get 'publishedAt data)
                              (alist-get 'createdAt data)))
           :face 'magh-date)))
        ('branch
         (marginalia--fields
          ((and (magh-api--true-p (alist-get 'protected data)) "protected")
           :face 'magh-permission)
          ((alist-get 'sha (alist-get 'commit data))
           :truncate 10 :face 'magh-hash)))))))

(add-to-list 'marginalia-annotators
             '(magh-search magh-search--marginalia-annotate))

(defun magh-search--candidates (context kind items)
  "Convert search ITEMS into propertized candidates."
  (cl-loop for item in items
           for index from 0
           for resource = (magh-search--resource context kind item)
           collect (magh-candidate-string (magh-search--format resource)
                                        resource 'magh-search index)))

(defun magh-search--async-backend (context kind options)
  "Return Consult async backend for CONTEXT, KIND, and OPTIONS."
  (lambda (sink)
    (let (request last-input (generation 0))
      (lambda (action)
        (pcase action
          ((pred stringp)
           (funcall sink action)
           (unless (equal action last-input)
             (setq last-input action)
             (when request (magh-api--cancel request))
             (cl-incf generation)
             (let ((current generation))
               (funcall sink '[indicator running])
               (setq request
                     (magh-api--search-stream
                      context kind action
                      (lambda (items)
                        (when (= current generation)
                          (funcall sink 'flush)
                          (funcall sink (magh-search--candidates context kind items))
                          (funcall sink '[indicator finished])
                          (funcall sink 'refresh)))
                      (lambda (error)
                        (when (= current generation)
                          (funcall sink '[indicator failed])
                          (funcall sink 'refresh)
                          (message "magh search: %s" (magh-error-message error))))
                      options))))
           nil)
          ((or 'cancel 'destroy)
           (cl-incf generation)
           (when request (magh-api--cancel request) (setq request nil))
           (setq last-input nil)
           (funcall sink action))
          (_ (funcall sink action)))))))

(defun magh-search--consult (context kind options initial)
  "Run dynamic Consult search for KIND."
  (let* ((collection
          (consult--async-pipeline
           (consult--async-min-input magh-search-minimum-input)
           (consult--async-throttle 0 magh-search-debounce)
           (magh-search--async-backend context kind options)))
         (selected
          (consult--read
           collection :prompt (format "GitHub %s search: " kind)
           :initial initial :require-match t :sort nil :category 'magh-search
           ;; Recover the original propertized candidate on RET.  Dynamic
           ;; searches deliberately have no preview: moving through results
           ;; must not fetch or open the selected resource.
           :lookup #'consult--lookup-member))
         (resource (get-text-property 0 'magh-resource selected)))
    (magh-resource-open resource)))

(defun magh-search--repository-list-fetch (context kind success error)
  "Fetch repository-scoped list KIND in CONTEXT."
  (pcase-exhaustive kind
    ('actions (magh-api--run-list context (list :limit magh-list-limit)
                                success error))
    ('releases (magh-api--release-list context success error))
    ('branches (magh-api--repo-branches context success error))))

(defun magh-search--repository-list-backend (context kind)
  "Return a one-shot async Consult backend for repository KIND."
  (lambda (sink)
    (let (request state)
      (lambda (action)
        (pcase action
          ((pred stringp)
           (funcall sink action)
           (unless state
             (setq state 'running)
             (funcall sink '[indicator running])
             (setq request
                   (magh-search--repository-list-fetch
                    context kind
                    (lambda (items)
                      (when (eq state 'running)
                        (setq state 'finished request nil)
                        (funcall sink 'flush)
                        (funcall sink
                                 (magh-search--candidates context kind items))
                        (funcall sink '[indicator finished])
                        (funcall sink 'refresh)))
                    (lambda (error)
                      (when (eq state 'running)
                        (setq state 'failed request nil)
                        (funcall sink '[indicator failed])
                        (funcall sink 'refresh)
                        (message "magh repository search: %s"
                                 (magh-error-message error)))))))
           nil)
          ((or 'cancel 'destroy)
           (setq state 'cancelled)
           (when request
             (magh-api--cancel request)
             (setq request nil))
           (funcall sink action))
          (_ (funcall sink action)))))))

(defun magh-search--consult-repository-list (context kind &optional initial)
  "Asynchronously search repository KIND with Consult.
INITIAL seeds the local Consult filter."
  (let* ((selected
          (consult--read
           (magh-search--repository-list-backend context kind)
           :prompt (format "Repository %s: " kind)
           :initial initial :require-match t :sort t :category 'magh-search
           :lookup #'consult--lookup-member))
         (resource (get-text-property 0 'magh-resource selected)))
    (magh-resource-open resource)))

;;;###autoload
(defun magh-consult-search (kind &optional context initial options)
  "Dynamically search GitHub resource KIND.
CONTEXT supplies host and optional repository scope; INITIAL seeds input and
OPTIONS contains API filters."
  (interactive
   (list (intern (completing-read "Search type: "
                                  '("repos" "issues" "prs" "code" "commits")
                                  nil t))))
  (setq context (magh-context-resolve context))
  (magh-search--consult context kind options initial))

(defun magh-repository-consult-search (kind &optional context initial)
  "Search repository resource KIND with Consult.
CONTEXT defaults to the current repository and INITIAL seeds the search.
Issue, Pull Request, code, and commit searches use GitHub search.  Action,
Release, and branch searches fetch repository data and narrow it locally."
  (setq context (magh-context-resolve (or context magh-buffer-context) t))
  (pcase-exhaustive kind
    ((or 'issues 'prs 'code 'commits)
     (magh-consult-search kind context initial
                        (list :repo (magh-context-repository context))))
    ((or 'actions 'releases 'branches)
     (magh-search--consult-repository-list context kind initial))))

(defun magh-search-repositories ()
  "Search GitHub repositories with Consult."
  (interactive)
  (magh-consult-search 'repos))

(defun magh-search-issues ()
  "Search GitHub Issues with Consult."
  (interactive)
  (magh-consult-search 'issues))

(defun magh-search-prs ()
  "Search GitHub Pull Requests with Consult."
  (interactive)
  (magh-consult-search 'prs))

(defun magh-search-code ()
  "Search GitHub code with Consult."
  (interactive)
  (magh-consult-search 'code))

(defun magh-search-commits ()
  "Search GitHub commits with Consult."
  (interactive)
  (magh-consult-search 'commits))

;;;###autoload
(transient-define-prefix magh-search-dispatch ()
  "Global GitHub search."
  [["Search"
    ("r" "Repositories" magh-search-repositories)
    ("i" "Issues" magh-search-issues)
    ("p" "Pull requests" magh-search-prs)
    ("c" "Code" magh-search-code)
    ("m" "Commits" magh-search-commits)]])

;;;###autoload
(defun magh-repository-search-dispatch (&optional context)
  "Search within repository CONTEXT."
  (interactive)
  (setq context (magh-context-resolve (or context magh-buffer-context) t))
  (let* ((kind (intern
                (completing-read
                 "Repository search type: "
                 '("issues" "prs" "actions" "releases" "branches"
                   "code" "commits")
                 nil t))))
    (magh-repository-consult-search kind context)))

(magh-candidate-register
 'repository-search
 :open (lambda (resource)
         (magh-repository-search-dispatch (plist-get resource :context))))

(provide 'magh-search)
;;; magh-search.el ends here
