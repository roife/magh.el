;;; magh-candidate.el --- Structured candidates and native navigation -*- lexical-binding: t; -*-

;; Copyright (C) 2026
;; SPDX-License-Identifier: GPL-2.0-only

;; Author: magh.el contributors
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
(require 'magh-core)

(defvar magh-candidate--actions (make-hash-table :test #'eq)
  "Map resource kinds to action plists.")

(defvar magh-candidate--preview-buffers nil
  "Buffers created by the current candidate preview session.")

(defvar-local magh-buffer-preview-p nil
  "Whether the current buffer is a disposable Consult preview.")

(defun magh-candidate-register (kind &rest actions)
  "Register ACTIONS for resource KIND.
ACTIONS is a plist whose supported keys are :open, :preview, and :dispatch.
Action functions receive a resource plist."
  (puthash kind actions magh-candidate--actions)
  kind)

(defun magh-resource-create (kind context &rest properties)
  "Create a structured resource plist of KIND in CONTEXT.
PROPERTIES are copied into the returned plist."
  (append (list :kind kind :context context
                :repository (magh-context-repository context))
          properties))

(defun magh-resource-url (resource)
  "Return RESOURCE web URL, deriving one from structured fields when needed."
  (or (plist-get resource :url)
      (let ((context (plist-get resource :context)))
        (pcase (plist-get resource :kind)
          ('repository (magh-context-web-url context))
          ('issue (magh-context-web-url
                   context (format "issues/%s" (plist-get resource :number))))
          ('pr (magh-context-web-url
                context (format "pull/%s" (plist-get resource :number))))
          ('commit (magh-context-web-url
                    context (format "commit/%s" (plist-get resource :sha))))
          ('release (magh-context-web-url
                     context (format "releases/tag/%s"
                                     (plist-get resource :tag))))
          ('run (magh-context-web-url
                 context (format "actions/runs/%s" (plist-get resource :id))))
          ('artifact (magh-context-web-url
                      context (format "actions/runs/%s"
                                      (plist-get resource :run-id))))
          ('discussion
           (magh-context-web-url
            context (format "discussions/%s" (plist-get resource :number))))
          ('workflow (magh-context-web-url
                      context (format "actions/workflows/%s"
                                      (or (plist-get resource :path)
                                          (plist-get resource :id)))))
          ('branch
           (magh-context-web-url
            context (format "tree/%s"
                            (or (plist-get resource :name)
                                (magh-context-ref context)))))
          ((or 'file 'tree)
           (magh-context-web-url
            context
            (format "%s/%s/%s"
                    (if (eq (plist-get resource :kind) 'tree) "tree" "blob")
                    (or (plist-get resource :ref)
                        (magh-context-ref context) "HEAD")
                    (or (plist-get resource :path) ""))))))))

(defun magh-resource-title (resource)
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

(defun magh-candidate--action (resource action)
  "Return ACTION function for RESOURCE."
  (let* ((kind (plist-get resource :kind))
         (override (and (eq action :open) (alist-get kind magh-resource-actions)))
         (registered (plist-get (gethash kind magh-candidate--actions) action)))
    (or override registered)))

(defun magh-resource-open (resource)
  "Open RESOURCE using its native registered action."
  (interactive (list (magh-candidate-at-point)))
  (if-let* ((action (magh-candidate--action resource :open)))
      (funcall action resource)
    (magh-resource-browse resource)))

(defun magh-resource-preview (resource)
  "Preview RESOURCE using its registered preview or open action."
  (when-let* ((action (or (magh-candidate--action resource :preview)
                          (magh-candidate--action resource :open))))
    (let ((buffer (funcall action resource)))
      (when (bufferp buffer)
        (cl-pushnew buffer magh-candidate--preview-buffers)))))

(defun magh-resource-browse (resource)
  "Explicitly browse RESOURCE on GitHub."
  (interactive (list (magh-candidate-at-point)))
  (unless resource (user-error "No GitHub resource at point"))
  (if-let* ((url (magh-resource-url resource)))
      (browse-url url)
    (user-error "Resource has no web URL")))

(defun magh-resource-copy-url (resource)
  "Copy RESOURCE URL to the kill ring."
  (interactive (list (magh-candidate-at-point)))
  (if-let* ((url (magh-resource-url resource)))
      (progn (kill-new url) (message "Copied %s" url) url)
    (user-error "Resource has no web URL")))

(defun magh-resource-copy-title (resource)
  "Copy RESOURCE title to the kill ring."
  (interactive (list (magh-candidate-at-point)))
  (unless resource (user-error "No GitHub resource at point"))
  (let ((title (magh-resource-title resource)))
    (kill-new title)
    (message "Copied %s" title)
    title))

(defun magh-resource-org-link (resource)
  "Return and copy an Org link for RESOURCE."
  (interactive (list (magh-candidate-at-point)))
  (let ((url (magh-resource-url resource)))
    (unless url (user-error "Resource has no web URL"))
    (let ((link (format "[[%s][%s]]" url (magh-resource-title resource))))
      (kill-new link)
      (when (called-interactively-p 'interactive)
        (message "Copied %s" link))
      link)))

(defun magh-candidate-at-point ()
  "Return the structured GitHub resource at point."
  (or (get-text-property (point) 'magh-resource)
      (get-text-property (line-beginning-position) 'magh-resource)
      (and (> (point) (point-min))
           (get-text-property (1- (point)) 'magh-resource))))

(defun magh-candidate-string (display resource &optional category index)
  "Return DISPLAY carrying RESOURCE and completion CATEGORY.
INDEX is encoded invisibly to keep duplicate display rows distinct."
  (let* ((suffix (and index (format "\0%d" index)))
         (string (concat display suffix)))
    (when suffix
      (add-text-properties (length display) (length string)
                           '(invisible t intangible t) string))
    (add-text-properties
     0 (length string)
     (list 'magh-resource resource
           'category (or category
                         (intern (format "magh-%s"
                                         (plist-get resource :kind))))
           'consult--candidate resource)
     string)
    string))

(defun magh-candidate--cleanup-previews ()
  "Kill transient buffers created during candidate preview."
  (dolist (buffer magh-candidate--preview-buffers)
    (when (and (buffer-live-p buffer)
               (buffer-local-value 'magh-buffer-preview-p buffer))
      (kill-buffer buffer)))
  (setq magh-candidate--preview-buffers nil))

(defun magh-candidate--consult-state ()
  "Return a Consult state function for native resource previews."
  (lambda (action candidate)
    (when (eq action 'preview)
      (when-let* ((resource (and (stringp candidate)
                                 (get-text-property 0 'magh-resource candidate))))
        (magh-resource-preview resource)))))

(cl-defun magh-candidate-read
    (prompt resources &key formatter category preview initial group sort)
  "Read one of RESOURCES and return its structured plist.
FORMATTER receives a resource and returns display text."
  (unless resources
    (user-error "No GitHub resources available"))
  (let* ((formatter (or formatter #'magh-resource-title))
         (candidates
          (cl-loop for resource in resources
                   for index from 0
                   collect (magh-candidate-string
                            (funcall formatter resource) resource category index))))
    (unwind-protect
        (let ((selected
               (consult--read candidates :prompt prompt :require-match t
                              :initial initial :sort sort :group group
                              :state (and preview
                                          (magh-candidate--consult-state)))))
          (get-text-property 0 'magh-resource selected))
      (magh-candidate--cleanup-previews))))

(defun magh-candidate-select-and-open
    (prompt resources &optional formatter preview)
  "Read a resource from RESOURCES and open it."
  (magh-resource-open
   (magh-candidate-read prompt resources
                      :formatter formatter :preview preview)))

(defun magh-resource-from-url (url &optional context)
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
                      (magh-context-copy (or context (magh-context-create))
                                       :host host :owner owner :name name
                                       :repository (format "%s/%s" owner name))))
          (cond
           ((string-match "\\`issues/\\([0-9]+\\)" suffix)
            (magh-resource-create 'issue repo-context
                                :number (string-to-number (match-string 1 suffix))
                                :url url))
           ((string-match "\\`pull/\\([0-9]+\\)" suffix)
            (magh-resource-create 'pr repo-context
                                :number (string-to-number (match-string 1 suffix))
                                :url url))
           ((string-match "\\`commit/\\([[:xdigit:]]+\\)" suffix)
            (magh-resource-create 'commit repo-context :sha (match-string 1 suffix)
                                :url url))
           ((string-match "\\`releases/tag/\\(.+\\)" suffix)
            (magh-resource-create 'release repo-context :tag (match-string 1 suffix)
                                :url url))
           ((string-match "\\`actions/runs/\\([0-9]+\\)" suffix)
            (magh-resource-create 'run repo-context
                                :id (string-to-number (match-string 1 suffix))
                                :url url))
           ((string-match "\\`discussions/\\([0-9]+\\)" suffix)
            (magh-resource-create
             'discussion repo-context
             :number (string-to-number (match-string 1 suffix)) :url url))
           ((string-match "\\`actions/workflows/\\([^/]+\\)" suffix)
            (magh-resource-create 'workflow repo-context
                                :id (url-unhex-string (match-string 1 suffix))
                                :url url))
           ((string-match "\\`\\(blob\\|tree\\)/\\([^/]+\\)/\\(.*\\)" suffix)
            (let ((kind (if (string= (match-string 1 suffix) "tree")
                            'tree 'file))
                  (ref (match-string 2 suffix))
                  (path (match-string 3 suffix)))
              (magh-resource-create kind
                                  (magh-context-copy repo-context
                                                   :ref ref :path path)
                                  :ref ref :path path :url url)))
           (t (magh-resource-create 'repository repo-context :url url))))))))

(defun magh-candidate--notification-resource (base data)
  "Create a structured notification resource from API DATA and BASE context."
  (let* ((context (magh-context-from-repository
                   (alist-get 'full_name (alist-get 'repository data))
                   (magh-context-host base)))
         (subject (alist-get 'subject data))
         (type (alist-get 'type subject))
         (title (alist-get 'title subject))
         (api-url (or (alist-get 'url subject) ""))
         (subject-resource
          (cond
           ((and (string= type "PullRequest")
                 (string-match "/pulls/\\([0-9]+\\)" api-url))
            (magh-resource-create 'pr context
                                :number (string-to-number (match-string 1 api-url))
                                :title title))
           ((and (string= type "Issue")
                 (string-match "/issues/\\([0-9]+\\)" api-url))
            (magh-resource-create 'issue context
                                :number (string-to-number (match-string 1 api-url))
                                :title title))
           ((and (member type '("Commit" "CommitComment"))
                 (string-match "/commits/\\([[:xdigit:]]+\\)" api-url))
            (magh-resource-create 'commit context :sha (match-string 1 api-url)
                                :title title))
           ((and (member type '("WorkflowRun" "CheckSuite"))
                 (string-match "/actions/runs/\\([0-9]+\\)" api-url))
            (magh-resource-create 'run context
                                :id (string-to-number (match-string 1 api-url))
                                :title title))
           ((and (string= type "Discussion")
                 (string-match "/discussions/\\([0-9]+\\)" api-url))
            (magh-resource-create
             'discussion context
             :number (string-to-number (match-string 1 api-url))
             :title title))
           ((and (string= type "Release")
                 (string-match "/releases/\\([0-9]+\\)" api-url))
            (magh-resource-create 'release-id context
                                :id (string-to-number (match-string 1 api-url))
                                :title title))
           (t (magh-resource-create 'repository context :title title)))))
    (magh-resource-create
     'notification context :id (alist-get 'id data)
     :title title :reason (alist-get 'reason data)
     :subject-type type :unread (alist-get 'unread data)
     :updated (alist-get 'updated_at data)
     :subject-resource subject-resource
     :url (format "https://%s/notifications"
                  (or (magh-context-host context) "github.com"))
     :data data)))

(provide 'magh-candidate)
;;; magh-candidate.el ends here
