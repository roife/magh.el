;;; gh-search.el --- Cancellable dynamic GitHub search -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; Author: gh.el contributors
;; Keywords: tools, vc, github
;; Package-Requires: ((emacs "29.1") (consult "2.0") (transient "0.7.0"))

;;; Commentary:

;; Consult-powered global and repository-scoped searches.  Each input change
;; cancels the previous gh process; generation checks discard late results.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'transient)
(require 'gh-api)
(require 'gh-candidate)
(require 'gh-ui)

(declare-function consult--async-pipeline "consult")
(declare-function consult--async-min-input "consult")
(declare-function consult--async-throttle "consult")
(declare-function consult--read "consult")

(defvar gh-search--generation 0
  "Global diagnostic generation incremented for each search request.")

(defun gh-search--repository-name (data)
  "Extract OWNER/NAME from search DATA."
  (let ((repository (gh-core--alist-get 'repository data)))
    (or (gh-core--alist-get 'nameWithOwner repository)
        (gh-core--alist-get 'fullName repository)
        (gh-core--alist-get 'nameWithOwner data)
        (gh-core--alist-get 'fullName data))))

(defun gh-search--resource (base-context kind data)
  "Convert KIND search DATA to a native resource."
  (let* ((kind (if (eq kind 'repos) 'repository kind))
         (kind (if (eq kind 'prs) 'pr kind))
         (repo (or (gh-search--repository-name data)
                   (and base-context (gh-context-repository base-context))))
         (context (if repo
                      (gh-context-from-repository
                       repo (and base-context (gh-context-host base-context)))
                    base-context)))
    (pcase kind
      ('repository
       (let ((name (or (gh-core--alist-get 'fullName data) repo)))
         (gh-resource-create 'repository
                             (gh-context-from-repository
                              name (and base-context
                                        (gh-context-host base-context)))
                             :name name :title name
                             :url (gh-core--alist-get 'url data) :data data)))
      ('issues
       (gh-resource-create
        (if (gh-core--alist-get 'isPullRequest data) 'pr 'issue) context
        :number (gh-core--alist-get 'number data)
        :title (gh-core--alist-get 'title data)
        :url (gh-core--alist-get 'url data) :data data))
      ('pr
       (gh-resource-create 'pr context :number (gh-core--alist-get 'number data)
                           :title (gh-core--alist-get 'title data)
                           :url (gh-core--alist-get 'url data) :data data))
      ('code
       (let ((path (gh-core--alist-get 'path data))
             (sha (gh-core--alist-get 'sha data)))
         (gh-resource-create
          'file (gh-context-copy context :ref sha :path path)
          :path path :ref sha :fragment (gh-core--alist-get 'textMatches data)
          :url (gh-core--alist-get 'url data) :data data)))
      ('commits
       (gh-resource-create 'commit context :sha (gh-core--alist-get 'sha data)
                           :title (gh-core--alist-get
                                   'message (gh-core--alist-get 'commit data))
                           :url (gh-core--alist-get 'url data) :data data))
      (_ (gh-resource-create kind context :data data)))))

(defun gh-search--format (resource)
  "Format search RESOURCE as a completion candidate."
  (let ((data (plist-get resource :data)))
    (pcase (plist-get resource :kind)
      ('repository
       (gh-ui--row
        (gh-ui--styled (plist-get resource :repository) 'gh-repository)
        (gh-ui--styled
         (format "★%s" (or (gh-core--alist-get 'stargazersCount data) 0))
         'gh-permission)
        (gh-core--alist-get 'description data)))
      ((or 'issue 'pr)
       (let ((state (gh-core--alist-get 'state data)))
         (gh-ui--row
          (gh-ui--styled (upcase state) (gh-core--state-face state))
          (concat
           (or (gh-ui--styled (plist-get resource :repository)
                              'gh-repository) "")
           (or (gh-ui--styled (format "#%s" (plist-get resource :number))
                              'gh-resource-number) ""))
          (gh-ui--styled (plist-get resource :title) 'gh-resource-title))))
      ('file
       (let* ((matches (gh-core--alist-get 'textMatches data))
              (fragment (and matches
                             (gh-core--alist-get 'fragment (car matches)))))
         (gh-ui--row
          (gh-ui--styled (plist-get resource :repository) 'gh-repository)
          (gh-ui--styled (plist-get resource :path) 'gh-file)
          (string-trim (or fragment "")))))
      ('commit
       (gh-ui--row
        (gh-ui--styled (plist-get resource :repository) 'gh-repository)
        (gh-ui--styled
         (substring (plist-get resource :sha)
                    0 (min 10 (length (plist-get resource :sha))))
         'gh-hash)
        (gh-ui--styled
         (car (split-string (or (plist-get resource :title) "") "\n"))
         'gh-resource-title)))
      (_ (gh-ui--styled (gh-resource-title resource) 'gh-resource-title)))))

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
             (cl-incf gh-search--generation)
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
  (require 'consult)
  (let* ((collection
          (consult--async-pipeline
           (consult--async-min-input gh-search-minimum-input)
           (consult--async-throttle 0 gh-search-debounce)
           (gh-search--async-backend context kind options)))
         (selected
          (consult--read
           collection :prompt (format "GitHub %s search: " kind)
           :initial initial :require-match t :sort nil :category 'gh-search
           :state (gh-candidate--consult-state)))
         (resource (and selected (get-text-property 0 'gh-resource selected))))
    (when resource (gh-resource-open resource))))

(defun gh-search--fallback (context kind options initial)
  "Run non-dynamic fallback search when Consult is unavailable."
  (let ((query (read-string (format "GitHub %s search: " kind) initial)))
    (unless (< (length query) gh-search-minimum-input)
      (gh-api--search
       context kind query
       (lambda (items)
         (let* ((resources (mapcar
                            (lambda (item) (gh-search--resource context kind item))
                            items))
                (resource (gh-candidate-read
                           "Result: " resources :formatter #'gh-search--format
                           :preview t)))
           (when resource (gh-resource-open resource))))
       #'gh-core--user-error nil options))))

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
  (if (require 'consult nil t)
      (gh-search--consult context kind options initial)
    (gh-search--fallback context kind options initial)))

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
                 '("issues" "prs" "code" "commits" "releases" "workflows")
                 nil t)))
         (repo (gh-context-repository context)))
    (pcase kind
      ((or 'issues 'prs 'code 'commits)
       (gh-consult-search kind context nil (list :repo repo)))
      ('releases (gh-resource-open (gh-resource-create 'release-list context)))
      ('workflows (gh-resource-open (gh-resource-create 'workflow-list context))))))

(gh-candidate-register
 'repository-search
 :open (lambda (resource)
         (gh-repository-search-dispatch (plist-get resource :context))))

(provide 'gh-search)
;;; gh-search.el ends here
