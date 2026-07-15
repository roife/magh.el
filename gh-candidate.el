;;; gh-candidate.el --- Structured candidates and native navigation -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; Author: gh.el contributors
;; Keywords: tools, vc, github

;;; Commentary:

;; Candidate display text is deliberately separate from resource action data.
;; Consult, Embark, section RET actions, and previews all
;; consume the same resource plists.  This module does not query GitHub and does
;; not depend on any resource renderer.

;;; Code:

(require 'browse-url)
(require 'cl-lib)
(require 'consult)
(require 'subr-x)
(require 'url-parse)
(require 'gh-core)

(defvar gh-candidate--actions (make-hash-table :test #'eq)
  "Map resource kinds to action plists.")

(defvar gh-candidate--preview-buffers nil
  "Buffers created by the current candidate preview session.")

(defvar-local gh-buffer-preview-p nil
  "Whether the current buffer is a disposable Consult preview.")

(defun gh-candidate-register (kind &rest actions)
  "Register ACTIONS for resource KIND.
ACTIONS is a plist whose supported keys are :open, :preview, and :dispatch.
Action functions receive a resource plist."
  (puthash kind actions gh-candidate--actions)
  kind)

(defun gh-resource-create (kind context &rest properties)
  "Create a structured resource plist of KIND in CONTEXT.
PROPERTIES are copied into the returned plist."
  (append (list :kind kind :context context
                :repository (gh-context-repository context))
          properties))

(defun gh-resource-url (resource)
  "Return RESOURCE web URL, deriving one from structured fields when needed."
  (or (plist-get resource :url)
      (let ((context (plist-get resource :context)))
        (pcase (plist-get resource :kind)
          ('repository (gh-context-web-url context))
          ('issue (gh-context-web-url
                   context (format "issues/%s" (plist-get resource :number))))
          ('pr (gh-context-web-url
                context (format "pull/%s" (plist-get resource :number))))
          ('commit (gh-context-web-url
                    context (format "commit/%s" (plist-get resource :sha))))
          ('release (gh-context-web-url
                     context (format "releases/tag/%s"
                                     (plist-get resource :tag))))
          ('run (gh-context-web-url
                 context (format "actions/runs/%s" (plist-get resource :id))))
          ('workflow (gh-context-web-url
                      context (format "actions/workflows/%s"
                                      (or (plist-get resource :path)
                                          (plist-get resource :id)))))
          ('branch
           (gh-context-web-url
            context (format "tree/%s"
                            (or (plist-get resource :name)
                                (gh-context-ref context)))))
          ((or 'file 'tree)
           (gh-context-web-url
            context
            (format "%s/%s/%s"
                    (if (eq (plist-get resource :kind) 'tree) "tree" "blob")
                    (or (plist-get resource :ref)
                        (gh-context-ref context) "HEAD")
                    (or (plist-get resource :path) ""))))))))

(defun gh-resource-title (resource)
  "Return a useful title for RESOURCE."
  (or (plist-get resource :title)
      (plist-get resource :name)
      (plist-get resource :repository)
      (plist-get resource :path)
      (plist-get resource :tag)
      (plist-get resource :sha)
      (and (plist-get resource :number)
           (format "#%s" (plist-get resource :number)))
      (and (plist-get resource :id)
           (format "%s" (plist-get resource :id)))
      (symbol-name (plist-get resource :kind))))

(defun gh-candidate--action (resource action)
  "Return ACTION function for RESOURCE."
  (let* ((kind (plist-get resource :kind))
         (override (and (eq action :open) (alist-get kind gh-resource-actions)))
         (registered (plist-get (gethash kind gh-candidate--actions) action)))
    (or override registered)))

(defun gh-resource-open (resource)
  "Open RESOURCE using its native registered action."
  (interactive (list (gh-candidate-at-point)))
  (if-let* ((action (gh-candidate--action resource :open)))
      (funcall action resource)
    (gh-resource-browse resource)))

(defun gh-resource-preview (resource)
  "Preview RESOURCE using its registered preview or open action."
  (when-let* ((action (or (gh-candidate--action resource :preview)
                          (gh-candidate--action resource :open))))
    (let ((buffer (funcall action resource)))
      (when (bufferp buffer)
        (cl-pushnew buffer gh-candidate--preview-buffers)))))

(defun gh-resource-browse (resource)
  "Explicitly browse RESOURCE on GitHub."
  (interactive (list (gh-candidate-at-point)))
  (unless resource (user-error "No GitHub resource at point"))
  (if-let* ((url (gh-resource-url resource)))
      (browse-url url)
    (user-error "Resource has no web URL")))

(defun gh-resource-copy-url (resource)
  "Copy RESOURCE URL to the kill ring."
  (interactive (list (gh-candidate-at-point)))
  (if-let* ((url (gh-resource-url resource)))
      (progn (kill-new url) (message "Copied %s" url) url)
    (user-error "Resource has no web URL")))

(defun gh-resource-copy-title (resource)
  "Copy RESOURCE title to the kill ring."
  (interactive (list (gh-candidate-at-point)))
  (unless resource (user-error "No GitHub resource at point"))
  (let ((title (gh-resource-title resource)))
    (kill-new title)
    (message "Copied %s" title)
    title))

(defun gh-resource-org-link (resource)
  "Return and copy an Org link for RESOURCE."
  (interactive (list (gh-candidate-at-point)))
  (let ((url (gh-resource-url resource)))
    (unless url (user-error "Resource has no web URL"))
    (let ((link (format "[[%s][%s]]" url (gh-resource-title resource))))
      (kill-new link)
      (when (called-interactively-p 'interactive)
        (message "Copied %s" link))
      link)))

(defun gh-candidate-at-point ()
  "Return the structured GitHub resource at point."
  (or (get-text-property (point) 'gh-resource)
      (get-text-property (line-beginning-position) 'gh-resource)
      (and (> (point) (point-min))
           (get-text-property (1- (point)) 'gh-resource))))

(defun gh-candidate-string (display resource &optional category index)
  "Return DISPLAY carrying RESOURCE and completion CATEGORY.
INDEX is encoded invisibly to keep duplicate display rows distinct."
  (let* ((suffix (and index (format "\0%d" index)))
         (string (concat display suffix)))
    (when suffix
      (add-text-properties (length display) (length string)
                           '(invisible t intangible t) string))
    (add-text-properties
     0 (length string)
     (list 'gh-resource resource
           'category (or category
                         (intern (format "gh-%s"
                                         (plist-get resource :kind))))
           'consult--candidate resource)
     string)
    string))

(defun gh-candidate--cleanup-previews ()
  "Kill transient buffers created during candidate preview."
  (dolist (buffer gh-candidate--preview-buffers)
    (when (and (buffer-live-p buffer)
               (buffer-local-value 'gh-buffer-preview-p buffer))
      (kill-buffer buffer)))
  (setq gh-candidate--preview-buffers nil))

(defun gh-candidate--consult-state ()
  "Return a Consult state function for native resource previews."
  (lambda (action candidate)
    (when (eq action 'preview)
      (when-let* ((resource (and (stringp candidate)
                                 (get-text-property 0 'gh-resource candidate))))
        (gh-resource-preview resource)))))

(cl-defun gh-candidate-read
    (prompt resources &key formatter category preview initial group sort)
  "Read one of RESOURCES and return its structured plist.
FORMATTER receives a resource and returns display text."
  (let* ((formatter (or formatter #'gh-resource-title))
         (candidates
          (cl-loop for resource in resources
                   for index from 0
                   collect (gh-candidate-string
                            (funcall formatter resource) resource category index))))
    (unwind-protect
        (let ((selected
               (consult--read candidates :prompt prompt :require-match t
                              :initial initial :sort sort :group group
                              :state (and preview
                                          (gh-candidate--consult-state)))))
          (get-text-property 0 'gh-resource selected))
      (gh-candidate--cleanup-previews))))

(defun gh-candidate-select-and-open
    (prompt resources &optional formatter preview)
  "Read a resource from RESOURCES and open it."
  (gh-resource-open
   (gh-candidate-read prompt resources
                      :formatter formatter :preview preview)))

(defun gh-resource-from-url (url &optional context)
  "Parse supported GitHub URL into a native resource plist.
CONTEXT supplies local navigation state when URL is relative to the same host."
  (when url
    (let* ((parsed (url-generic-parse-url url))
           (parts (split-string
                   (string-remove-prefix
                    "/" (car (url-path-and-query parsed)))
                   "/")))
      (when (and (member (url-type parsed) '("http" "https"))
                 (url-host parsed) (>= (length parts) 2)
                 (not (string-empty-p (car parts)))
                 (not (string-empty-p (cadr parts))))
        (pcase-let* ((`(,owner ,name . ,path) parts)
                     (host (if-let* ((port (url-portspec parsed)))
                               (format "%s:%s" (url-host parsed) port)
                             (url-host parsed)))
                     (suffix (string-join path "/"))
                     (repo-context
                      (gh-context-copy (or context (gh-context-create))
                                       :host host :owner owner :name name
                                       :repository (format "%s/%s" owner name))))
          (cond
           ((string-match "\\`issues/\\([0-9]+\\)" suffix)
            (gh-resource-create 'issue repo-context
                                :number (string-to-number (match-string 1 suffix))
                                :url url))
           ((string-match "\\`pull/\\([0-9]+\\)" suffix)
            (gh-resource-create 'pr repo-context
                                :number (string-to-number (match-string 1 suffix))
                                :url url))
           ((string-match "\\`commit/\\([[:xdigit:]]+\\)" suffix)
            (gh-resource-create 'commit repo-context :sha (match-string 1 suffix)
                                :url url))
           ((string-match "\\`releases/tag/\\(.+\\)" suffix)
            (gh-resource-create 'release repo-context :tag (match-string 1 suffix)
                                :url url))
           ((string-match "\\`actions/runs/\\([0-9]+\\)" suffix)
            (gh-resource-create 'run repo-context
                                :id (string-to-number (match-string 1 suffix))
                                :url url))
           ((string-match "\\`actions/workflows/\\([^/]+\\)" suffix)
            (gh-resource-create 'workflow repo-context
                                :id (url-unhex-string (match-string 1 suffix))
                                :url url))
           ((string-match "\\`\\(blob\\|tree\\)/\\([^/]+\\)/\\(.*\\)" suffix)
            (let ((kind (if (string= (match-string 1 suffix) "tree")
                            'tree 'file))
                  (ref (match-string 2 suffix))
                  (path (match-string 3 suffix)))
              (gh-resource-create kind
                                  (gh-context-copy repo-context
                                                   :ref ref :path path)
                                  :ref ref :path path :url url)))
           (t (gh-resource-create 'repository repo-context :url url))))))))

(defun gh-candidate--notification-resource (base data)
  "Create a structured notification resource from API DATA and BASE context."
  (let* ((context (gh-context-from-repository
                   (alist-get 'full_name (alist-get 'repository data))
                   (gh-context-host base)))
         (subject (alist-get 'subject data))
         (type (alist-get 'type subject))
         (title (alist-get 'title subject))
         (api-url (or (alist-get 'url subject) ""))
         (subject-resource
          (cond
           ((and (string= type "PullRequest")
                 (string-match "/pulls/\\([0-9]+\\)" api-url))
            (gh-resource-create 'pr context
                                :number (string-to-number (match-string 1 api-url))
                                :title title))
           ((and (string= type "Issue")
                 (string-match "/issues/\\([0-9]+\\)" api-url))
            (gh-resource-create 'issue context
                                :number (string-to-number (match-string 1 api-url))
                                :title title))
           ((and (member type '("Commit" "CommitComment"))
                 (string-match "/commits/\\([[:xdigit:]]+\\)" api-url))
            (gh-resource-create 'commit context :sha (match-string 1 api-url)
                                :title title))
           ((and (member type '("WorkflowRun" "CheckSuite"))
                 (string-match "/actions/runs/\\([0-9]+\\)" api-url))
            (gh-resource-create 'run context
                                :id (string-to-number (match-string 1 api-url))
                                :title title))
           ((and (string= type "Release")
                 (string-match "/releases/\\([0-9]+\\)" api-url))
            (gh-resource-create 'release-id context
                                :id (string-to-number (match-string 1 api-url))
                                :title title))
           (t (gh-resource-create 'repository context :title title)))))
    (gh-resource-create
     'notification context :id (alist-get 'id data)
     :title title :reason (alist-get 'reason data)
     :subject-type type :unread (alist-get 'unread data)
     :updated (alist-get 'updated_at data)
     :subject-resource subject-resource
     :url (format "https://%s/notifications"
                  (or (gh-context-host context) "github.com"))
     :data data)))

(provide 'gh-candidate)
;;; gh-candidate.el ends here
