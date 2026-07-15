;;; gh-search.el --- Cancellable dynamic GitHub search -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; Author: gh.el contributors
;; Keywords: tools, vc, github
;; Package-Requires: ((emacs "31.1") (consult "2.0") (transient "0.7.0"))

;;; Commentary:

;; Consult-powered global and repository-scoped searches.  Each input change
;; cancels the previous gh process; generation checks discard late results.

;;; Code:

(require 'cl-lib)
(require 'consult)
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
                 ((or 'issues 'code 'commits) kind)))
         (repo (if (eq kind 'repository)
                   (alist-get 'fullName data)
                 (alist-get 'nameWithOwner (alist-get 'repository data))))
         (context (gh-context-from-repository
                   repo (gh-context-host base-context))))
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
                           :url (alist-get 'url data) :data data)))))

(defun gh-search--format (resource)
  "Format search RESOURCE as a completion candidate."
  (let ((data (plist-get resource :data)))
    (pcase-exhaustive (plist-get resource :kind)
      ('repository
       (gh-ui--row
        (gh-ui--styled (plist-get resource :repository) 'gh-repository)
        (gh-ui--styled
         (format "★%s" (alist-get 'stargazersCount data))
         'gh-permission)
        (alist-get 'description data)))
      ((or 'issue 'pr)
       (let ((state (alist-get 'state data)))
         (gh-ui--row
          (gh-ui--styled (upcase state) (gh-core--state-face state))
          (concat
           (gh-ui--styled (plist-get resource :repository) 'gh-repository)
           (gh-ui--styled (format "#%s" (plist-get resource :number))
                          'gh-resource-number))
          (gh-ui--styled (plist-get resource :title) 'gh-resource-title))))
      ('file
       (let* ((matches (alist-get 'textMatches data))
              (fragment (and matches
                             (alist-get 'fragment (car matches)))))
         (gh-ui--row
          (gh-ui--styled (plist-get resource :repository) 'gh-repository)
          (gh-ui--styled (plist-get resource :path) 'gh-file)
          (string-trim (or fragment "")))))
      ('commit
       (gh-ui--row
        (gh-ui--styled (plist-get resource :repository) 'gh-repository)
        (gh-ui--styled (substring (plist-get resource :sha) 0 10) 'gh-hash)
        (gh-ui--styled
         (car (split-string (plist-get resource :title) "\n"))
         'gh-resource-title))))))

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
    (pcase-exhaustive kind
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
