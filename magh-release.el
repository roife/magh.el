;;; magh-release.el --- GitHub Release management for magh.el -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; Author: magh.el contributors
;; Keywords: tools, vc, github

;;; Commentary:

;; Consult-backed release selection, native details, structured create/edit,
;; generated notes, publication state, latest marking, deletion, and assets.

;;; Code:

(require 'subr-x)
(require 'transient)
(require 'magh-api)
(require 'magh-candidate)
(require 'magh-edit)
(require 'magh-ui)

(defvar-local magh-release--tag nil)
(defvar-local magh-release--dispatch-resource nil)

(defun magh-release--context (&optional context)
  "Resolve repository CONTEXT for Release commands."
  (magh-context-resolve (or context magh-buffer-context) t))

(defun magh-release--resource (context data)
  "Create Release resource from DATA."
  (let ((tag (alist-get 'tagName data)))
    (magh-resource-create
     'release context :tag tag
     :title (or (alist-get 'name data) tag)
     :url (or (alist-get 'url data)
              (magh-context-web-url context (format "releases/tag/%s" tag)))
     :data data)))

(defun magh-release--state (data)
  "Return display state for Release DATA."
  (cond ((alist-get 'isDraft data) "draft")
        ((alist-get 'isPrerelease data) "prerelease")
        (t "published")))

(defun magh-release--format-candidate (resource)
  "Format Release RESOURCE for selection."
  (let* ((data (plist-get resource :data))
         (state (magh-release--state data)))
    (magh-ui--row
     (magh-ui--styled (upcase state) (magh-core--state-face state))
     (magh-ui--styled (plist-get resource :tag) 'magh-tag)
     (magh-ui--styled (magh-resource-title resource) 'magh-resource-title)
     (magh-ui--styled
      (magh-core--date (or (alist-get 'publishedAt data)
                         (alist-get 'createdAt data)))
      'magh-date))))

;;;###autoload
(defun magh-release-list (&optional context)
  "Select a Release in CONTEXT with native preview."
  (interactive (list (magh-context-read-repository)))
  (setq context (magh-release--context context))
  (magh-api--release-list
   context
   (lambda (items)
     (let* ((resources (mapcar (lambda (item)
                                 (magh-release--resource context item)) items))
            (resource (magh-candidate-read
                       "Release: " resources :category 'magh-release :preview t
                       :formatter #'magh-release--format-candidate)))
       (magh-resource-open resource)))
   #'magh-core--user-error))

(defun magh-release--asset-resource (context tag asset)
  "Create asset resource for ASSET belonging to TAG."
  (magh-resource-create
   'release-asset context :id (alist-get 'id asset)
   :tag tag :name (alist-get 'name asset)
   :title (alist-get 'name asset)
   :url (alist-get 'url asset)
   :data asset))

(defun magh-release--render-view (context data)
  "Render Release DATA in CONTEXT."
  (let* ((resource (magh-release--resource context data))
         (tag (alist-get 'tagName data))
         (target (alist-get 'targetCommitish data))
         (state (magh-release--state data))
         (target-resource
          (if (string-match-p "\\`[[:xdigit:]]\\{7,40\\}\\'" target)
              (magh-resource-create 'commit context :sha target)
            (magh-resource-create 'tree (magh-context-copy context :ref target)
                                :ref target :path ""))))
    (setq magh-release--tag tag)
    (insert (propertize tag 'font-lock-face 'magh-tag) " "
            (propertize (magh-resource-title resource)
                        'font-lock-face 'magh-resource-title) "\n")
    (add-text-properties (line-beginning-position 0) (point)
                         (list 'magh-resource resource))
    (magh-ui--insert-header "State" state (magh-core--state-face state))
    (magh-ui--insert-header "Author"
                          (magh-core--name (alist-get 'author data))
                          'magh-author)
    (magh-ui--insert-header "Target" target 'magh-branch target-resource)
    (magh-ui--insert-header "Published"
                          (magh-core--date (alist-get 'publishedAt data))
                          'magh-date)
    (insert "\n")
    (magh-ui--section (description 'description resource nil)
      "Release notes"
      (let ((body (alist-get 'body data)))
        (magh-ui--insert-markdown
         (if (string-empty-p (string-trim (or body "")))
             "No release notes."
           body)
         context)))
    (let ((assets (alist-get 'assets data)))
      (magh-ui--section (assets 'assets nil nil)
        (format "Assets (%d)" (length assets))
        (dolist (asset assets)
          (let ((asset-resource (magh-release--asset-resource context tag asset)))
            (magh-ui--section (asset (alist-get 'id asset)
                                   asset-resource t)
              (magh-ui--styled (alist-get 'name asset) 'magh-file)
              (magh-ui--insert-header "Size"
                                    (format "%s bytes" (alist-get 'size asset)))
              (magh-ui--insert-header
               "Downloads" (alist-get 'downloadCount asset)))))))))

(defun magh-release--setup-view (context tag)
  "Install Release detail bindings for CONTEXT and TAG."
  (setq magh-release--tag tag
        magh-buffer-dispatch-function
        (lambda ()
          (setq magh-release--dispatch-resource
                (magh-resource-create 'release context :tag tag))
          (call-interactively #'magh-release-dispatch)))
  (local-set-key (kbd "E")
                 (lambda () (interactive) (magh-release-edit context tag)))
  (local-set-key (kbd "d")
                 (lambda () (interactive) (magh-release-download context tag))))

;;;###autoload
(defun magh-release-view (tag &optional context preview)
  "Open Release TAG in CONTEXT."
  (interactive (list (read-string "Release tag: ")))
  (setq context (magh-release--context context))
  (magh-ui--open-page
   (if preview
       (format "*magh preview: %s · %s*" (magh-context-repository context) tag)
     (format "*magh: %s · Release %s*" (magh-context-repository context) tag))
   context 'release tag
   (lambda (success error force)
     (magh-api--release-get context tag success error force))
   (lambda (data) (magh-release--render-view context data))
   :preview preview :setup (lambda () (magh-release--setup-view context tag))))

(defun magh-release-view-id (id &optional context preview)
  "Open Release numeric ID in CONTEXT, as used by notifications."
  (setq context (magh-release--context context))
  (magh-ui--open-page
   (if preview
       (format "*magh preview: %s · Release %s*"
               (magh-context-repository context) id)
     (format "*magh: %s · Release %s*" (magh-context-repository context) id))
   context 'release id
   (lambda (success error force)
     (magh-api--release-get-id context id success error force))
   (lambda (data) (magh-release--render-view context data))
   :preview preview
   :setup
   (lambda ()
     (setq magh-buffer-dispatch-function #'magh-release-dispatch)
     (local-set-key
      (kbd "E")
      (lambda () (interactive)
        (unless magh-release--tag (user-error "Release is still loading"))
        (magh-release-edit context magh-release--tag)))
     (local-set-key
      (kbd "d")
      (lambda () (interactive)
        (unless magh-release--tag (user-error "Release is still loading"))
        (magh-release-download context magh-release--tag))))))

;;; Structured create/edit

(defun magh-release--fields (context &optional creating)
  "Return Release editor fields for CONTEXT."
  (append
   (list '(:name tag :required t)
         (append '(:name title)
                 (unless creating '(:allow-empty t))))
   `((:name target
      :completion-fetch
      ,(magh-edit--completion-fetcher
        #'magh-api--repo-branches context 'name))
     (:name draft :type boolean)
     (:name prerelease :type boolean))
   (when creating '((:name generate-notes :type boolean)))))

(defun magh-release--open-create-editor (context values body)
  "Open Release creation editor with VALUES and BODY."
  (magh-edit-open
   (format "*magh: %s · New Release*" (magh-context-repository context))
   (magh-release--fields context t)
   (append values
           (list :target (or (magh-context-default-branch context) "main")
                 :draft :json-false :prerelease :json-false
                 :generate-notes :json-false))
   body
   (lambda (parsed parsed-body success error)
     (magh-api--release-create context (plist-put parsed :body parsed-body)
                             success error))
   :after-success
   (lambda (result)
     (let ((tag (or (and (string-match "/releases/tag/\\([^[:space:]]+\\)" result)
                         (url-unhex-string (match-string 1 result)))
                    (plist-get values :tag))))
       (when tag (magh-release-view tag context))))))

;;;###autoload
(defun magh-release-create (&optional context)
  "Create a Release in CONTEXT with a structured editor."
  (interactive (list (magh-context-read-repository)))
  (setq context (magh-release--context context))
  (magh-release--open-create-editor context nil ""))

(defun magh-release-create-generated (tag target &optional context)
  "Generate editable notes for TAG at TARGET, then create a Release."
  (interactive (list (read-string "Tag: ")
                     (read-string "Target: "
                                  (or (and magh-buffer-context
                                           (magh-context-default-branch
                                            magh-buffer-context)) "main"))))
  (setq context (magh-release--context context))
  (magh-api--release-generate-notes
   context tag target nil
   (lambda (data)
     (magh-release--open-create-editor
      context (list :tag tag :title (alist-get 'name data)
                    :target target)
      (alist-get 'body data)))
   #'magh-core--user-error))

(defun magh-release--edit-values (data)
  "Convert Release DATA to editor values."
  (list :tag (alist-get 'tagName data)
        :title (alist-get 'name data)
        :target (alist-get 'targetCommitish data)
        :draft (or (alist-get 'isDraft data) :json-false)
        :prerelease (or (alist-get 'isPrerelease data) :json-false)))

(defun magh-release--open-edit-editor (context tag data)
  "Open structured editor for Release TAG using DATA."
  (magh-edit-open
   (format "*magh: %s · Edit Release %s*" (magh-context-repository context) tag)
   (magh-release--fields context) (magh-release--edit-values data)
   (alist-get 'body data)
   (lambda (values body success error)
     (magh-api--release-edit context tag (plist-put values :body body)
                           success error))))

;;;###autoload
(defun magh-release-edit (&optional context tag)
  "Edit Release TAG in CONTEXT."
  (interactive)
  (setq context (magh-release--context context)
        tag (or tag magh-release--tag (read-string "Release tag: ")))
  (if (and (eq magh-buffer-resource-kind 'release)
           (equal magh-buffer-resource-id tag) magh-ui--data)
      (magh-release--open-edit-editor context tag magh-ui--data)
    (magh-api--release-get
     context tag (lambda (data) (magh-release--open-edit-editor context tag data))
     #'magh-core--user-error)))

;;; Actions

(defun magh-release--current ()
  "Return (CONTEXT TAG) for current Release action."
  (let ((resource (or magh-release--dispatch-resource (magh-ui-resource-at-point))))
    (list (or (plist-get resource :context) magh-buffer-context)
          (or (plist-get resource :tag) magh-release--tag
              (and (eq magh-buffer-resource-kind 'release)
                   magh-buffer-resource-id)))))

(defun magh-release-publish (&optional context tag)
  "Publish draft Release TAG."
  (interactive)
  (pcase-let ((`(,current-context ,current-tag) (magh-release--current)))
    (setq context (magh-release--context (or context current-context))
          tag (or tag current-tag))
    (magh-api--release-edit
     context tag '(:draft :json-false)
     (lambda (_) (message "Published %s" tag)
       (when (derived-mode-p 'magh-section-mode) (magh-ui-refresh t)))
     #'magh-core--user-error)))

(defun magh-release-mark-latest (&optional context tag)
  "Mark Release TAG as latest."
  (interactive)
  (pcase-let ((`(,current-context ,current-tag) (magh-release--current)))
    (setq context (magh-release--context (or context current-context))
          tag (or tag current-tag))
    (magh-api--release-edit
     context tag '(:latest t)
     (lambda (_) (message "%s is now latest" tag)
       (when (derived-mode-p 'magh-section-mode) (magh-ui-refresh t)))
     #'magh-core--user-error)))

(defun magh-release-toggle-prerelease (&optional context tag)
  "Toggle prerelease state for Release TAG."
  (interactive)
  (pcase-let ((`(,current-context ,current-tag) (magh-release--current)))
    (setq context (magh-release--context (or context current-context))
          tag (or tag current-tag))
    (magh-api--release-get
     context tag
     (lambda (data)
       (let ((enable (not (alist-get 'isPrerelease data))))
         (magh-api--release-edit
          context tag (list :prerelease (or enable :json-false))
          (lambda (_) (message "Prerelease %s" (if enable "enabled" "disabled"))
            (when (derived-mode-p 'magh-section-mode) (magh-ui-refresh t)))
          #'magh-core--user-error)))
     #'magh-core--user-error)))

(defun magh-release-delete (&optional context tag)
  "Delete Release TAG."
  (interactive)
  (pcase-let ((`(,current-context ,current-tag) (magh-release--current)))
    (setq context (magh-release--context (or context current-context))
          tag (or tag current-tag))
    (when (magh-core--confirm (format "Delete Release %s? " tag))
      (magh-api--release-delete
       context tag (lambda (_) (message "Deleted Release %s" tag))
       #'magh-core--user-error))))

(defun magh-release-download (&optional context tag patterns directory)
  "Download assets from Release TAG matching PATTERNS into DIRECTORY."
  (interactive)
  (pcase-let ((`(,current-context ,current-tag) (magh-release--current)))
    (setq context (magh-release--context (or context current-context))
          tag (or tag current-tag)
          patterns (or patterns
                       (let ((text (read-string "Asset glob (empty for all): ")))
                         (unless (string-empty-p text) (list text))))
          directory (or directory
                        (read-directory-name "Download to: "
                                             (or magh-download-directory
                                                 default-directory))))
    (make-directory directory t)
    (magh-api--release-download
     context tag patterns directory
     (lambda (_) (message "Downloaded Release assets to %s" directory)
       (dired directory))
     #'magh-core--user-error)))

(defun magh-release-upload (files &optional clobber context tag)
  "Upload FILES to Release TAG, replacing assets when CLOBBER."
  (interactive (list (list (read-file-name "Asset: ")) current-prefix-arg))
  (pcase-let ((`(,current-context ,current-tag) (magh-release--current)))
    (setq context (magh-release--context (or context current-context))
          tag (or tag current-tag))
    (magh-api--release-upload
     context tag files clobber
     (lambda (_) (message "Uploaded %d asset(s)" (length files))
       (when (derived-mode-p 'magh-section-mode) (magh-ui-refresh t)))
     #'magh-core--user-error)))

(transient-define-prefix magh-release-dispatch ()
  "Release actions."
  [["View/Edit"
    ("g" "Refresh" magh-ui-refresh)
    ("E" "Edit" magh-release-edit)
    ("b" "Browse" magh-ui-browse)]
   ["Publish"
    ("p" "Publish draft" magh-release-publish)
    ("r" "Toggle prerelease" magh-release-toggle-prerelease)
    ("l" "Mark latest" magh-release-mark-latest)]
   ["Assets"
    ("d" "Download" magh-release-download)
    ("u" "Upload" magh-release-upload)]
   ["Danger"
    ("D" "Delete" magh-release-delete)]])

;;; Candidate registration

(magh-candidate-register
 'release
 :open (lambda (resource)
         (magh-release-view (plist-get resource :tag)
                          (plist-get resource :context)))
 :preview (lambda (resource)
            (magh-release-view (plist-get resource :tag)
                             (plist-get resource :context) t))
 :dispatch (lambda (resource)
             (setq magh-release--dispatch-resource resource)
             (call-interactively #'magh-release-dispatch)))

(magh-candidate-register
 'release-list :open (lambda (resource)
                       (magh-release-list (plist-get resource :context))))

(magh-candidate-register
 'release-id
 :open (lambda (resource)
         (magh-release-view-id (plist-get resource :id)
                             (plist-get resource :context)))
 :preview (lambda (resource)
            (magh-release-view-id (plist-get resource :id)
                                (plist-get resource :context) t)))

(magh-candidate-register
 'release-asset
 :open (lambda (resource)
         (magh-release-download (plist-get resource :context)
                              (plist-get resource :tag)
                              (list (plist-get resource :name)) nil)))

(provide 'magh-release)
;;; magh-release.el ends here
