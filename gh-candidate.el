;;; gh-candidate.el --- Structured candidates and native navigation -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; Author: gh.el contributors
;; Keywords: tools, vc, github
;; Package-Requires: ((emacs "29.1"))

;;; Commentary:

;; Candidate display text is deliberately separate from resource action data.
;; Consult, completion fallback, Embark, section RET actions, and previews all
;; consume the same resource plists.  This module does not query GitHub and does
;; not depend on any resource renderer.

;;; Code:

(require 'browse-url)
(require 'cl-lib)
(require 'subr-x)
(require 'gh-core)

(declare-function consult--read "consult")

(defvar gh-candidate--actions (make-hash-table :test #'eq)
  "Map resource kinds to action plists.")

(defvar gh-candidate--preview-buffers nil
  "Buffers created by the current candidate preview session.")

(defun gh-candidate-register (kind &rest actions)
  "Register ACTIONS for resource KIND.
ACTIONS is a plist whose supported keys include :open, :preview, :browse,
:copy, and :insert.  Action functions receive a resource plist."
  (unless (symbolp kind)
    (signal 'gh-invalid-input (list "Resource kind must be a symbol")))
  (puthash kind actions gh-candidate--actions)
  kind)

(defun gh-candidate-unregister (kind)
  "Remove registered actions for KIND."
  (remhash kind gh-candidate--actions))

(defun gh-candidate-actions (kind)
  "Return the registered action plist for KIND."
  (gethash kind gh-candidate--actions))

(defun gh-resource-create (kind context &rest properties)
  "Create a structured resource plist of KIND in CONTEXT.
PROPERTIES are copied into the returned plist."
  (append (list :kind kind :context context
                :repository (and context (gh-context-repository context)))
          properties))

(defun gh-resource-kind (resource)
  "Return RESOURCE kind."
  (plist-get resource :kind))

(defun gh-resource-context (resource)
  "Return RESOURCE context."
  (plist-get resource :context))

(defun gh-resource-url (resource)
  "Return RESOURCE web URL, deriving one from structured fields when needed."
  (or (plist-get resource :url)
      (let ((context (plist-get resource :context)))
        (when context
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
            ((or 'file 'tree)
             (gh-context-web-url
              context
              (format "%s/%s/%s"
                      (if (eq (plist-get resource :kind) 'tree) "tree" "blob")
                      (or (plist-get resource :ref)
                          (gh-context-ref context) "HEAD")
                      (or (plist-get resource :path) "")))))))))

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
      (symbol-name (or (plist-get resource :kind) 'resource))))

(defun gh-candidate--action (resource action)
  "Return ACTION function for RESOURCE."
  (let* ((kind (plist-get resource :kind))
         (override (and (eq action :open) (alist-get kind gh-resource-actions)))
         (registered (plist-get (gethash kind gh-candidate--actions) action)))
    (or override registered)))

(defun gh-resource-open (resource)
  "Open RESOURCE using its native registered action."
  (interactive (list (gh-candidate-at-point)))
  (unless resource (user-error "No GitHub resource at point"))
  (if-let* ((action (gh-candidate--action resource :open)))
      (funcall action resource)
    (if (gh-resource-url resource)
        (gh-resource-browse resource)
      (user-error "No native action registered for %s"
                  (plist-get resource :kind)))))

(defun gh-resource-preview (resource)
  "Preview RESOURCE using its registered preview or open action."
  (when resource
    (if-let* ((action (or (gh-candidate--action resource :preview)
                          (gh-candidate--action resource :open))))
        (let ((buffer (funcall action resource)))
          (when (bufferp buffer)
            (cl-pushnew buffer gh-candidate--preview-buffers)))
      nil)))

(defun gh-resource-browse (resource)
  "Explicitly browse RESOURCE on GitHub."
  (interactive (list (gh-candidate-at-point)))
  (unless resource (user-error "No GitHub resource at point"))
  (if-let* ((action (gh-candidate--action resource :browse)))
      (funcall action resource)
    (if-let* ((url (gh-resource-url resource)))
        (browse-url url)
      (user-error "Resource has no web URL"))))

(defun gh-resource-copy-url (resource)
  "Copy RESOURCE URL to the kill ring."
  (interactive (list (gh-candidate-at-point)))
  (if-let* ((url (and resource (gh-resource-url resource))))
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
  (let ((url (and resource (gh-resource-url resource))))
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
               (boundp 'gh-buffer-preview-p)
               (buffer-local-value 'gh-buffer-preview-p buffer))
      (kill-buffer buffer)))
  (setq gh-candidate--preview-buffers nil))

(defun gh-candidate--consult-state ()
  "Return a Consult state function for native resource previews."
  (lambda (action candidate)
    (pcase action
      ('preview
       (when-let* ((resource (and (stringp candidate)
                                  (get-text-property 0 'gh-resource candidate))))
         (gh-resource-preview resource)))
      ((or 'exit 'return) (gh-candidate--cleanup-previews)))))

(cl-defun gh-candidate-read
    (prompt resources &key formatter category preview initial group sort)
  "Read one of RESOURCES and return its structured plist.
FORMATTER receives a resource and returns display text.  Use Consult when it
is installed; otherwise use `completing-read'."
  (let* ((formatter (or formatter #'gh-resource-title))
         (candidates
          (cl-loop for resource in resources
                   for index from 0
                   collect (gh-candidate-string
                            (funcall formatter resource) resource category index)))
         (selected
          (if (and (require 'consult nil t) (fboundp 'consult--read))
              (consult--read candidates :prompt prompt :require-match t
                             :initial initial :sort (if (null sort) nil sort)
                             :group group
                             :state (and preview (gh-candidate--consult-state)))
            (completing-read prompt candidates nil t initial))))
    (prog1 (get-text-property 0 'gh-resource selected)
      (gh-candidate--cleanup-previews))))

(defun gh-candidate-select-and-open
    (prompt resources &optional formatter preview)
  "Read a resource from RESOURCES and open it."
  (let ((resource (gh-candidate-read prompt resources
                                     :formatter formatter :preview preview)))
    (when resource (gh-resource-open resource))))

(defun gh-resource-from-url (url &optional context)
  "Parse supported GitHub URL into a native resource plist.
CONTEXT supplies local navigation state when URL is relative to the same host."
  (when (and url
             (string-match
              "\\`https?://\\([^/]+\\)/\\([^/]+\\)/\\([^/]+\\)\\(?:/\\(.*\\)\\)?\\'"
              (car (split-string url "[?#]"))))
    (let* ((host (match-string 1 url))
           (owner (match-string 2 url))
           (name (match-string 3 url))
           (suffix (or (match-string 4 url) ""))
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
       ((string-match "\\`actions/workflows/\\([^/?#]+\\)" suffix)
        (gh-resource-create 'workflow repo-context
                            :id (url-unhex-string (match-string 1 suffix))
                            :url url))
       ((string-match "\\`\\(blob\\|tree\\)/\\([^/]+\\)/\\(.*\\)" suffix)
        (gh-resource-create (if (string= (match-string 1 suffix) "tree")
                                'tree 'file)
                            (gh-context-copy repo-context
                                             :ref (match-string 2 suffix)
                                             :path (match-string 3 suffix))
                            :ref (match-string 2 suffix)
                            :path (match-string 3 suffix) :url url))
       (t (gh-resource-create 'repository repo-context :url url))))))

(defun gh-candidate--notification-resource (base data)
  "Create a structured notification resource from API DATA and BASE context."
  (let* ((repository (gh-core--alist-get 'repository data))
         (name (or (gh-core--alist-get 'full_name repository)
                   (gh-core--alist-get 'nameWithOwner repository)))
         (context (if name
                      (gh-context-from-repository
                       name (and base (gh-context-host base)))
                    base))
         (subject (gh-core--alist-get 'subject data))
         (type (gh-core--alist-get 'type subject))
         (title (gh-core--alist-get 'title subject))
         (api-url (gh-core--alist-get 'url subject))
         (subject-resource
          (cond
           ((and (string= type "PullRequest")
                 (string-match "/pulls/\\([0-9]+\\)" (or api-url "")))
            (gh-resource-create 'pr context
                                :number (string-to-number (match-string 1 api-url))
                                :title title))
           ((and (string= type "Issue")
                 (string-match "/issues/\\([0-9]+\\)" (or api-url "")))
            (gh-resource-create 'issue context
                                :number (string-to-number (match-string 1 api-url))
                                :title title))
           ((and (member type '("Commit" "CommitComment"))
                 (string-match "/commits/\\([[:xdigit:]]+\\)" (or api-url "")))
            (gh-resource-create 'commit context :sha (match-string 1 api-url)
                                :title title))
           ((and (member type '("WorkflowRun" "CheckSuite"))
                 (string-match "/actions/runs/\\([0-9]+\\)" (or api-url "")))
            (gh-resource-create 'run context
                                :id (string-to-number (match-string 1 api-url))
                                :title title))
           ((and (string= type "Release")
                 (string-match "/releases/\\([0-9]+\\)" (or api-url "")))
            (gh-resource-create 'release-id context
                                :id (string-to-number (match-string 1 api-url))
                                :title title))
           (t (gh-resource-create 'repository context :title title)))))
    (gh-resource-create
     'notification context :id (gh-core--alist-get 'id data)
     :title title :reason (gh-core--alist-get 'reason data)
     :subject-type type :unread (gh-core--alist-get 'unread data)
     :updated (gh-core--alist-get 'updated_at data)
     :subject-resource subject-resource
     :url (format "https://%s/notifications"
                  (or (and context (gh-context-host context)) "github.com"))
     :data data)))

(provide 'gh-candidate)
;;; gh-candidate.el ends here
