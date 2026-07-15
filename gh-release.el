;;; gh-release.el --- GitHub Release management for gh.el -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; Author: gh.el contributors
;; Keywords: tools, vc, github
;; Package-Requires: ((emacs "31.1") (transient "0.7.0"))

;;; Commentary:

;; Consult-backed release selection, native details, structured create/edit,
;; generated notes, publication state, latest marking, deletion, and assets.

;;; Code:

(require 'subr-x)
(require 'transient)
(require 'gh-api)
(require 'gh-candidate)
(require 'gh-edit)
(require 'gh-ui)

(defvar-local gh-release--tag nil)
(defvar-local gh-release--dispatch-resource nil)

(defun gh-release--context (&optional context)
  "Resolve repository CONTEXT for Release commands."
  (gh-context-resolve (or context gh-buffer-context) t))

(defun gh-release--resource (context data)
  "Create Release resource from DATA."
  (let ((tag (alist-get 'tagName data)))
    (gh-resource-create
     'release context :tag tag
     :title (or (alist-get 'name data) tag)
     :url (or (alist-get 'url data)
              (gh-context-web-url context (format "releases/tag/%s" tag)))
     :data data)))

(defun gh-release--state (data)
  "Return display state for Release DATA."
  (cond ((alist-get 'isDraft data) "draft")
        ((alist-get 'isPrerelease data) "prerelease")
        (t "published")))

(defun gh-release--format-candidate (resource)
  "Format Release RESOURCE for selection."
  (let* ((data (plist-get resource :data))
         (state (gh-release--state data)))
    (gh-ui--row
     (gh-ui--styled (upcase state) (gh-core--state-face state))
     (gh-ui--styled (plist-get resource :tag) 'gh-tag)
     (gh-ui--styled (gh-resource-title resource) 'gh-resource-title)
     (gh-ui--styled
      (gh-core--date (or (alist-get 'publishedAt data)
                         (alist-get 'createdAt data)))
      'gh-date))))

;;;###autoload
(defun gh-release-list (&optional context)
  "Select a Release in CONTEXT with native preview."
  (interactive)
  (setq context (gh-release--context context))
  (gh-api--release-list
   context
   (lambda (items)
     (let* ((resources (mapcar (lambda (item)
                                 (gh-release--resource context item)) items))
            (resource (gh-candidate-read
                       "Release: " resources :category 'gh-release :preview t
                       :formatter #'gh-release--format-candidate)))
       (gh-resource-open resource)))
   #'gh-core--user-error))

(defun gh-release--asset-resource (context tag asset)
  "Create asset resource for ASSET belonging to TAG."
  (gh-resource-create
   'release-asset context :id (alist-get 'id asset)
   :tag tag :name (alist-get 'name asset)
   :title (alist-get 'name asset)
   :url (alist-get 'url asset)
   :data asset))

(defun gh-release--render-view (context data)
  "Render Release DATA in CONTEXT."
  (let* ((resource (gh-release--resource context data))
         (tag (alist-get 'tagName data))
         (target (alist-get 'targetCommitish data))
         (state (gh-release--state data))
         (target-resource
          (if (string-match-p "\\`[[:xdigit:]]\\{7,40\\}\\'" target)
              (gh-resource-create 'commit context :sha target)
            (gh-resource-create 'tree (gh-context-copy context :ref target)
                                :ref target :path ""))))
    (setq gh-release--tag tag)
    (insert (propertize tag 'font-lock-face 'gh-tag) " "
            (propertize (gh-resource-title resource)
                        'font-lock-face 'gh-resource-title) "\n")
    (add-text-properties (line-beginning-position 0) (point)
                         (list 'gh-resource resource))
    (gh-ui--insert-header "State" state (gh-core--state-face state))
    (gh-ui--insert-header "Author"
                          (gh-core--name (alist-get 'author data))
                          'gh-author)
    (gh-ui--insert-header "Target" target 'gh-branch target-resource)
    (gh-ui--insert-header "Published"
                          (gh-core--date (alist-get 'publishedAt data))
                          'gh-date)
    (insert "\n")
    (pcase-let ((`(,summary . ,body)
                 (gh-ui--message-parts (alist-get 'body data)
                                       "No release notes.")))
      (gh-ui--section (description 'description resource nil)
        (gh-ui--styled summary 'magit-diff-revision-summary)
        (when body (gh-ui--insert-markdown body context))))
    (let ((assets (alist-get 'assets data)))
      (gh-ui--section (assets 'assets nil nil)
        (format "Assets (%d)" (length assets))
        (dolist (asset assets)
          (let ((asset-resource (gh-release--asset-resource context tag asset)))
            (gh-ui--section (asset (alist-get 'id asset)
                                   asset-resource t)
              (gh-ui--styled (alist-get 'name asset) 'gh-file)
              (gh-ui--insert-header "Size"
                                    (format "%s bytes" (alist-get 'size asset)))
              (gh-ui--insert-header
               "Downloads" (alist-get 'downloadCount asset)))))))))

(defun gh-release--setup-view (context tag)
  "Install Release detail bindings for CONTEXT and TAG."
  (setq gh-release--tag tag
        gh-buffer-dispatch-function
        (lambda ()
          (setq gh-release--dispatch-resource
                (gh-resource-create 'release context :tag tag))
          (call-interactively #'gh-release-dispatch)))
  (local-set-key (kbd "E")
                 (lambda () (interactive) (gh-release-edit context tag)))
  (local-set-key (kbd "d")
                 (lambda () (interactive) (gh-release-download context tag))))

;;;###autoload
(defun gh-release-view (tag &optional context preview)
  "Open Release TAG in CONTEXT."
  (interactive (list (read-string "Release tag: ")))
  (setq context (gh-release--context context))
  (gh-ui--open-page
   (if preview
       (format "*gh preview: %s · %s*" (gh-context-repository context) tag)
     (format "*gh: %s · Release %s*" (gh-context-repository context) tag))
   context 'release tag
   (lambda (success error force)
     (gh-api--release-get context tag success error force))
   (lambda (data) (gh-release--render-view context data))
   :preview preview :setup (lambda () (gh-release--setup-view context tag))))

(defun gh-release-view-id (id &optional context preview)
  "Open Release numeric ID in CONTEXT, as used by notifications."
  (setq context (gh-release--context context))
  (gh-ui--open-page
   (if preview
       (format "*gh preview: %s · Release %s*"
               (gh-context-repository context) id)
     (format "*gh: %s · Release %s*" (gh-context-repository context) id))
   context 'release id
   (lambda (success error force)
     (gh-api--release-get-id context id success error force))
   (lambda (data) (gh-release--render-view context data))
   :preview preview
   :setup
   (lambda ()
     (setq gh-buffer-dispatch-function #'gh-release-dispatch)
     (local-set-key
      (kbd "E")
      (lambda () (interactive)
        (unless gh-release--tag (user-error "Release is still loading"))
        (gh-release-edit context gh-release--tag)))
     (local-set-key
      (kbd "d")
      (lambda () (interactive)
        (unless gh-release--tag (user-error "Release is still loading"))
        (gh-release-download context gh-release--tag))))))

;;; Structured create/edit

(defun gh-release--branch-fetch (context)
  "Return asynchronous branch completion provider for CONTEXT."
  (lambda (success error)
    (gh-api--repo-branches
     context
     (lambda (items)
       (funcall success (mapcar (lambda (item)
                                  (alist-get 'name item)) items)))
     error)))

(defun gh-release--fields (context &optional creating)
  "Return Release editor fields for CONTEXT."
  (append
   '((:name tag :required t) (:name title))
   `((:name target :completion-fetch ,(gh-release--branch-fetch context))
     (:name draft :type boolean)
     (:name prerelease :type boolean))
   (when creating '((:name generate-notes :type boolean)))))

(defun gh-release--open-create-editor (context values body)
  "Open Release creation editor with VALUES and BODY."
  (gh-edit-open
   (format "*gh: %s · New Release*" (gh-context-repository context))
   (gh-release--fields context t)
   (append values
           (list :target (or (gh-context-default-branch context) "main")
                 :draft :json-false :prerelease :json-false
                 :generate-notes :json-false))
   body
   (lambda (parsed parsed-body success error)
     (gh-api--release-create context (plist-put parsed :body parsed-body)
                             success error))
   :after-success
   (lambda (result)
     (let ((tag (or (and (string-match "/releases/tag/\\([^[:space:]]+\\)" result)
                         (url-unhex-string (match-string 1 result)))
                    (plist-get values :tag))))
       (when tag (gh-release-view tag context))))))

;;;###autoload
(defun gh-release-create (&optional context)
  "Create a Release in CONTEXT with a structured editor."
  (interactive)
  (setq context (gh-release--context context))
  (gh-release--open-create-editor context nil ""))

(defun gh-release-create-generated (tag target &optional context)
  "Generate editable notes for TAG at TARGET, then create a Release."
  (interactive (list (read-string "Tag: ")
                     (read-string "Target: "
                                  (or (and gh-buffer-context
                                           (gh-context-default-branch
                                            gh-buffer-context)) "main"))))
  (setq context (gh-release--context context))
  (gh-api--release-generate-notes
   context tag target nil
   (lambda (data)
     (gh-release--open-create-editor
      context (list :tag tag :title (alist-get 'name data)
                    :target target)
      (alist-get 'body data)))
   #'gh-core--user-error))

(defun gh-release--edit-values (data)
  "Convert Release DATA to editor values."
  (list :tag (alist-get 'tagName data)
        :title (alist-get 'name data)
        :target (alist-get 'targetCommitish data)
        :draft (if (alist-get 'isDraft data) t :json-false)
        :prerelease (if (alist-get 'isPrerelease data) t :json-false)))

(defun gh-release--open-edit-editor (context tag data)
  "Open structured editor for Release TAG using DATA."
  (gh-edit-open
   (format "*gh: %s · Edit Release %s*" (gh-context-repository context) tag)
   (gh-release--fields context) (gh-release--edit-values data)
   (alist-get 'body data)
   (lambda (values body success error)
     (gh-api--release-edit context tag (plist-put values :body body)
                           success error))))

;;;###autoload
(defun gh-release-edit (&optional context tag)
  "Edit Release TAG in CONTEXT."
  (interactive)
  (setq context (gh-release--context context)
        tag (or tag gh-release--tag (read-string "Release tag: ")))
  (if (and (eq gh-buffer-resource-kind 'release)
           (equal gh-buffer-resource-id tag) gh-ui--data)
      (gh-release--open-edit-editor context tag gh-ui--data)
    (gh-api--release-get
     context tag (lambda (data) (gh-release--open-edit-editor context tag data))
     #'gh-core--user-error)))

;;; Actions

(defun gh-release--current ()
  "Return (CONTEXT TAG) for current Release action."
  (let ((resource (or gh-release--dispatch-resource (gh-ui-resource-at-point))))
    (list (or (plist-get resource :context) gh-buffer-context)
          (or (plist-get resource :tag) gh-release--tag
              (and (eq gh-buffer-resource-kind 'release)
                   gh-buffer-resource-id)))))

(defun gh-release-publish (&optional context tag)
  "Publish draft Release TAG."
  (interactive)
  (pcase-let ((`(,current-context ,current-tag) (gh-release--current)))
    (setq context (gh-release--context (or context current-context))
          tag (or tag current-tag))
    (gh-api--release-edit
     context tag '(:draft :json-false)
     (lambda (_) (message "Published %s" tag)
       (when (derived-mode-p 'gh-section-mode) (gh-ui-refresh t)))
     #'gh-core--user-error)))

(defun gh-release-mark-latest (&optional context tag)
  "Mark Release TAG as latest."
  (interactive)
  (pcase-let ((`(,current-context ,current-tag) (gh-release--current)))
    (setq context (gh-release--context (or context current-context))
          tag (or tag current-tag))
    (gh-api--release-edit
     context tag '(:latest t)
     (lambda (_) (message "%s is now latest" tag)
       (when (derived-mode-p 'gh-section-mode) (gh-ui-refresh t)))
     #'gh-core--user-error)))

(defun gh-release-toggle-prerelease (&optional context tag)
  "Toggle prerelease state for Release TAG."
  (interactive)
  (pcase-let ((`(,current-context ,current-tag) (gh-release--current)))
    (setq context (gh-release--context (or context current-context))
          tag (or tag current-tag))
    (gh-api--release-get
     context tag
     (lambda (data)
       (let ((enable (not (alist-get 'isPrerelease data))))
         (gh-api--release-edit
          context tag (list :prerelease (if enable t :json-false))
          (lambda (_) (message "Prerelease %s" (if enable "enabled" "disabled"))
            (when (derived-mode-p 'gh-section-mode) (gh-ui-refresh t)))
          #'gh-core--user-error)))
     #'gh-core--user-error)))

(defun gh-release-delete (&optional context tag)
  "Delete Release TAG."
  (interactive)
  (pcase-let ((`(,current-context ,current-tag) (gh-release--current)))
    (setq context (gh-release--context (or context current-context))
          tag (or tag current-tag))
    (when (gh-core--confirm (format "Delete Release %s? " tag))
      (gh-api--release-delete
       context tag (lambda (_) (message "Deleted Release %s" tag))
       #'gh-core--user-error))))

(defun gh-release-download (&optional context tag patterns directory)
  "Download assets from Release TAG matching PATTERNS into DIRECTORY."
  (interactive)
  (pcase-let ((`(,current-context ,current-tag) (gh-release--current)))
    (setq context (gh-release--context (or context current-context))
          tag (or tag current-tag)
          patterns (or patterns
                       (let ((text (read-string "Asset glob (empty for all): ")))
                         (unless (string-empty-p text) (list text))))
          directory (or directory
                        (read-directory-name "Download to: "
                                             (or gh-download-directory
                                                 default-directory))))
    (make-directory directory t)
    (gh-api--release-download
     context tag patterns directory
     (lambda (_) (message "Downloaded Release assets to %s" directory)
       (dired directory))
     #'gh-core--user-error)))

(defun gh-release-upload (files &optional clobber context tag)
  "Upload FILES to Release TAG, replacing assets when CLOBBER."
  (interactive (list (list (read-file-name "Asset: ")) current-prefix-arg))
  (pcase-let ((`(,current-context ,current-tag) (gh-release--current)))
    (setq context (gh-release--context (or context current-context))
          tag (or tag current-tag))
    (gh-api--release-upload
     context tag files clobber
     (lambda (_) (message "Uploaded %d asset(s)" (length files))
       (when (derived-mode-p 'gh-section-mode) (gh-ui-refresh t)))
     #'gh-core--user-error)))

(transient-define-prefix gh-release-dispatch ()
  "Release actions."
  [["View/Edit"
    ("g" "Refresh" gh-ui-refresh)
    ("E" "Edit" gh-release-edit)
    ("b" "Browse" gh-ui-browse)]
   ["Publish"
    ("p" "Publish draft" gh-release-publish)
    ("r" "Toggle prerelease" gh-release-toggle-prerelease)
    ("l" "Mark latest" gh-release-mark-latest)]
   ["Assets"
    ("d" "Download" gh-release-download)
    ("u" "Upload" gh-release-upload)]
   ["Danger"
    ("D" "Delete" gh-release-delete)]])

;;; Candidate registration

(gh-candidate-register
 'release
 :open (lambda (resource)
         (gh-release-view (plist-get resource :tag)
                          (plist-get resource :context)))
 :preview (lambda (resource)
            (gh-release-view (plist-get resource :tag)
                             (plist-get resource :context) t))
 :dispatch (lambda (resource)
             (setq gh-release--dispatch-resource resource)
             (call-interactively #'gh-release-dispatch)))

(gh-candidate-register
 'release-list :open (lambda (resource)
                       (gh-release-list (plist-get resource :context))))

(gh-candidate-register
 'release-id
 :open (lambda (resource)
         (gh-release-view-id (plist-get resource :id)
                             (plist-get resource :context)))
 :preview (lambda (resource)
            (gh-release-view-id (plist-get resource :id)
                                (plist-get resource :context) t)))

(gh-candidate-register
 'release-asset
 :open (lambda (resource)
         (gh-release-download (plist-get resource :context)
                              (plist-get resource :tag)
                              (list (plist-get resource :name)) nil)))

(provide 'gh-release)
;;; gh-release.el ends here
