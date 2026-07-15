;;; gh-search.el --- Cancellable dynamic GitHub search -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; Author: gh.el contributors
;; Keywords: tools, vc, github
;; Package-Requires: ((emacs "31.1") (consult "2.0") (marginalia "1.0")
;;                    (transient "0.7.0"))

;;; Commentary:

;; Consult-powered global and repository-scoped searches.  Each input change
;; cancels the previous gh process; generation checks discard late results.

;;; Code:

(require 'cl-lib)
(require 'consult)
(require 'marginalia)
(require 'subr-x)
(require 'transient)
(require 'gh-api)
(require 'gh-candidate)
(require 'gh-ui)

(defun gh-search--resource (base-context kind data)
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
                     (gh-context-repository base-context))))
         (context (if repo
                      (gh-context-from-repository
                       repo (gh-context-host base-context))
                    base-context)))
    (pcase-exhaustive kind
      ('repository
       (gh-resource-create 'repository context :name repo :title repo
                           :url (alist-get 'url data) :data data))
      ('issues
       (gh-resource-create
        (if (alist-get 'isPullRequest data) 'pr 'issue) context
        :number (alist-get 'number data)
        :title (alist-get 'title data)
        :url (alist-get 'url data) :data data))
      ('pr
       (gh-resource-create 'pr context :number (alist-get 'number data)
                           :title (alist-get 'title data)
                           :url (alist-get 'url data) :data data))
      ('code
       (let* ((path (alist-get 'path data))
              (url (alist-get 'url data))
              (url-resource (gh-resource-from-url url context))
              ;; Search returns a blob SHA, but Contents API needs the commit
              ;; ref embedded in the result URL.
              (ref (or (and (eq (plist-get url-resource :kind) 'file)
                            (plist-get url-resource :ref))
                       (alist-get 'sha data))))
         (gh-resource-create
          'file (gh-context-copy context :ref ref :path path)
          :path path :ref ref :fragment (alist-get 'textMatches data)
          :url url :data data)))
      ('commits
       (gh-resource-create 'commit context :sha (alist-get 'sha data)
                           :title (alist-get
                                   'message (alist-get 'commit data))
                           :url (alist-get 'url data) :data data))
      ('run
       (gh-resource-create 'run context :id (alist-get 'databaseId data)
                           :title (alist-get 'displayTitle data)
                           :url (alist-get 'url data) :data data))
      ('release
       (let ((tag (alist-get 'tagName data)))
         (gh-resource-create
          'release context :tag tag
          :title (or (alist-get 'name data) tag)
          :url (or (alist-get 'url data)
                   (gh-context-web-url context (format "releases/tag/%s" tag)))
          :data data)))
      ('branch
       (let ((name (alist-get 'name data)))
         (gh-resource-create 'branch (gh-context-copy context :ref name)
                             :name name :title name :data data))))))

(defun gh-search--styled (value face)
  "Return completion VALUE carrying FACE directly."
  (when (and value (not (equal value "")))
    (propertize (format "%s" value) 'face face)))

(defun gh-search--format (resource)
  "Format search RESOURCE as a completion candidate."
  (pcase-exhaustive (plist-get resource :kind)
    ('repository
     (gh-search--styled (plist-get resource :repository) 'gh-repository))
    ((or 'issue 'pr)
     (gh-ui--row
      (gh-search--styled (format "#%s" (plist-get resource :number))
                         'gh-resource-number)
      (gh-search--styled (plist-get resource :title) 'gh-resource-title)))
    ('file
     (gh-search--styled (plist-get resource :path) 'gh-file))
    ('commit
     (let ((sha (plist-get resource :sha)))
       (gh-ui--row
        (gh-search--styled (substring sha 0 (min 10 (length sha))) 'gh-hash)
        (gh-search--styled
         (car (split-string (plist-get resource :title) "\n"))
         'gh-resource-title))))
    ('run
     (let* ((data (plist-get resource :data))
            (state (or (alist-get 'conclusion data)
                       (alist-get 'status data))))
       (gh-ui--row
        (gh-search--styled (upcase (or state ""))
                           (gh-core--state-face state))
        (gh-search--styled (gh-resource-title resource) 'gh-resource-title)
        (gh-search--styled (or (alist-get 'workflowName data)
                               (alist-get 'name data))
                           'gh-workflow)
        (gh-search--styled (alist-get 'headBranch data) 'gh-branch))))
    ('release
     (let* ((data (plist-get resource :data))
            (state (cond ((gh-api--true-p (alist-get 'isDraft data)) "draft")
                         ((gh-api--true-p (alist-get 'isPrerelease data))
                          "prerelease")
                         (t "published"))))
       (gh-ui--row
        (gh-search--styled (upcase state) (gh-core--state-face state))
        (gh-search--styled (plist-get resource :tag) 'gh-tag)
        (gh-search--styled (gh-resource-title resource) 'gh-resource-title))))
    ('branch
     (gh-search--styled (plist-get resource :name) 'gh-branch))))

(defun gh-search--marginalia-annotate (candidate)
  "Annotate gh search CANDIDATE using its structured resource data."
  (when-let* ((resource (get-text-property 0 'gh-resource candidate)))
    (let* ((data (plist-get resource :data))
           (repository (plist-get resource :repository))
           (state (alist-get 'state data))
           (author (or (gh-core--name (alist-get 'author data))
                       (gh-core--name
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
          (repository :truncate 24 :face 'gh-repository)
          ((upcase (or state "")) :face (gh-core--state-face state))
          (author :truncate 20 :face 'gh-author)))
        ('file
         (marginalia--fields
          (repository :truncate 24 :face 'gh-repository)
          ((and-let* ((matches (alist-get 'textMatches data))
                      (fragment (alist-get 'fragment (car matches))))
             (string-trim fragment))
           :truncate 1.0 :face 'font-lock-string-face)))
        ('commit
         (marginalia--fields
          (repository :truncate 24 :face 'gh-repository)
          (author :truncate 20 :face 'gh-author)))
        ('run
         (let ((run-state (or (alist-get 'conclusion data)
                              (alist-get 'status data))))
           (marginalia--fields
            ((upcase (or run-state ""))
             :face (gh-core--state-face run-state))
            ((alist-get 'event data) :face 'marginalia-type)
            ((alist-get 'headBranch data) :face 'gh-branch))))
        ('release
         (marginalia--fields
          ((gh-core--date (or (alist-get 'publishedAt data)
                              (alist-get 'createdAt data)))
           :face 'gh-date)))
        ('branch
         (marginalia--fields
          ((and (gh-api--true-p (alist-get 'protected data)) "protected")
           :face 'gh-permission)
          ((alist-get 'sha (alist-get 'commit data))
           :truncate 10 :face 'gh-hash)))))))

(add-to-list 'marginalia-annotators
             '(gh-search gh-search--marginalia-annotate))

(defun gh-search--candidates (context kind items)
  "Convert search ITEMS into propertized candidates."
  (cl-loop for item in items
           for index from 0
           for resource = (gh-search--resource context kind item)
           collect (gh-candidate-string (gh-search--format resource)
                                        resource 'gh-search index)))

(defun gh-search--async-backend (context kind options)
  "Return Consult async backend for CONTEXT, KIND, and OPTIONS."
  (lambda (sink)
    (let (request last-input (generation 0))
      (lambda (action)
        (pcase action
          ((pred stringp)
           (funcall sink action)
           (unless (equal action last-input)
             (setq last-input action)
             (when request (gh-api--cancel request))
             (cl-incf generation)
             (let ((current generation))
               (funcall sink '[indicator running])
               (setq request
                     (gh-api--search-stream
                      context kind action
                      (lambda (items)
                        (when (= current generation)
                          (funcall sink 'flush)
                          (funcall sink (gh-search--candidates context kind items))
                          (funcall sink '[indicator finished])
                          (funcall sink 'refresh)))
                      (lambda (error)
                        (when (= current generation)
                          (funcall sink '[indicator failed])
                          (funcall sink 'refresh)
                          (message "gh search: %s" (gh-error-message error))))
                      options))))
           nil)
          ((or 'cancel 'destroy)
           (cl-incf generation)
           (when request (gh-api--cancel request) (setq request nil))
           (setq last-input nil)
           (funcall sink action))
          (_ (funcall sink action)))))))

(defun gh-search--consult (context kind options initial)
  "Run dynamic Consult search for KIND."
  (let* ((collection
          (consult--async-pipeline
           (consult--async-min-input gh-search-minimum-input)
           (consult--async-throttle 0 gh-search-debounce)
           (gh-search--async-backend context kind options)))
         (selected
          (consult--read
           collection :prompt (format "GitHub %s search: " kind)
           :initial initial :require-match t :sort nil :category 'gh-search
           ;; Recover the original propertized candidate on RET.  Dynamic
           ;; searches deliberately have no preview: moving through results
           ;; must not fetch or open the selected resource.
           :lookup #'consult--lookup-member))
         (resource (get-text-property 0 'gh-resource selected)))
    (gh-resource-open resource)))

(defun gh-search--repository-list-fetch (context kind success error)
  "Fetch repository-scoped list KIND in CONTEXT."
  (pcase-exhaustive kind
    ('actions (gh-api--run-list context (list :limit gh-list-limit)
                                success error))
    ('releases (gh-api--release-list context success error))
    ('branches (gh-api--repo-branches context success error))))

(defun gh-search--repository-list-backend (context kind)
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
             (let ((new-request
                    (gh-search--repository-list-fetch
                     context kind
                     (lambda (items)
                       (when (eq state 'running)
                         (setq state 'finished request nil)
                         (funcall sink 'flush)
                         (funcall sink
                                  (gh-search--candidates context kind items))
                         (funcall sink '[indicator finished])
                         (funcall sink 'refresh)))
                     (lambda (error)
                       (when (eq state 'running)
                         (setq state 'failed request nil)
                         (funcall sink '[indicator failed])
                         (funcall sink 'refresh)
                         (message "gh repository search: %s"
                                  (gh-error-message error)))))))
               ;; A test double or cache may invoke its callback immediately.
               (when (eq state 'running)
                 (setq request new-request))))
           nil)
          ((or 'cancel 'destroy)
           (setq state 'cancelled)
           (when request
             (gh-api--cancel request)
             (setq request nil))
           (funcall sink action))
          (_ (funcall sink action)))))))

(defun gh-search--consult-repository-list (context kind &optional initial)
  "Asynchronously search repository KIND with Consult.
INITIAL seeds the local Consult filter."
  (let* ((selected
          (consult--read
           (gh-search--repository-list-backend context kind)
           :prompt (format "Repository %s: " kind)
           :initial initial :require-match t :sort t :category 'gh-search
           :lookup #'consult--lookup-member))
         (resource (get-text-property 0 'gh-resource selected)))
    (gh-resource-open resource)))

;;;###autoload
(defun gh-consult-search (kind &optional context initial options)
  "Dynamically search GitHub resource KIND.
CONTEXT supplies host and optional repository scope; INITIAL seeds input and
OPTIONS contains API filters."
  (interactive
   (list (intern (completing-read "Search type: "
                                  '("repos" "issues" "prs" "code" "commits")
                                  nil t))))
  (setq context (gh-context-resolve context))
  (gh-search--consult context kind options initial))

(defun gh-repository-consult-search (kind &optional context initial)
  "Search repository resource KIND with Consult.
CONTEXT defaults to the current repository and INITIAL seeds the search.
Issue, Pull Request, code, and commit searches use GitHub search.  Action,
Release, and branch searches fetch repository data and narrow it locally."
  (setq context (gh-context-resolve (or context gh-buffer-context) t))
  (pcase-exhaustive kind
    ((or 'issues 'prs 'code 'commits)
     (gh-consult-search kind context initial
                        (list :repo (gh-context-repository context))))
    ((or 'actions 'releases 'branches)
     (gh-search--consult-repository-list context kind initial))))

(defun gh-search-repositories () (interactive) (gh-consult-search 'repos))
(defun gh-search-issues () (interactive) (gh-consult-search 'issues))
(defun gh-search-prs () (interactive) (gh-consult-search 'prs))
(defun gh-search-code () (interactive) (gh-consult-search 'code))
(defun gh-search-commits () (interactive) (gh-consult-search 'commits))

(transient-define-prefix gh-search-dispatch ()
  "Global GitHub search."
  [["Search"
    ("r" "Repositories" gh-search-repositories)
    ("i" "Issues" gh-search-issues)
    ("p" "Pull requests" gh-search-prs)
    ("c" "Code" gh-search-code)
    ("m" "Commits" gh-search-commits)]])

;;;###autoload
(defun gh-repository-search-dispatch (&optional context)
  "Search within repository CONTEXT."
  (interactive)
  (setq context (gh-context-resolve (or context gh-buffer-context) t))
  (let* ((kind (intern
                (completing-read
                 "Repository search type: "
                 '("issues" "prs" "actions" "releases" "branches"
                   "code" "commits")
                 nil t))))
    (gh-repository-consult-search kind context)))

(gh-candidate-register
 'repository-search
 :open (lambda (resource)
         (gh-repository-search-dispatch (plist-get resource :context))))

(provide 'gh-search)
;;; gh-search.el ends here
