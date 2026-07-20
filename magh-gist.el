;;; magh-gist.el --- Native Gist pages and editing -*- lexical-binding: t; -*-

;; Copyright (C) 2026
;; SPDX-License-Identifier: GPL-2.0-only

;; Author: magh.el contributors
;; Keywords: tools, vc, github

;;; Commentary:

;; Native Gist lists, detail and file pages, creation, metadata editing, and
;; per-file content management.

;;; Code:

(require 'subr-x)
(require 'transient)
(require 'magh-api)
(require 'magh-candidate)
(require 'magh-edit)
(require 'magh-ui)

(defvar-local magh-gist--id nil)
(defvar-local magh-gist--dispatch-resource nil)

;;; Gists

(defun magh-gist--resource (context data)
  "Create Gist resource from DATA."
  (let ((id (alist-get 'id data)))
    (magh-resource-create
     'gist context :id id
     :title (or (alist-get 'description data) id)
     :url (alist-get 'html_url data) :data data)))

(defun magh-gist--render-gists (context data)
  "Render Gist list DATA."
  (insert (propertize "Gists\n\n" 'font-lock-face 'magh-resource-title))
  (if data
      (dolist (gist data)
        (let ((resource (magh-gist--resource context gist)))
          (magh-ui--section (gist (plist-get resource :id) resource t)
            (magh-ui--styled (magh-resource-title resource) 'magh-resource-title)
            (magh-ui--insert-header "ID" (plist-get resource :id))
            (magh-ui--insert-header "Visibility"
                                  (if (alist-get 'public gist)
                                      "public" "secret")
                                  'magh-permission)
            (magh-ui--insert-header
             "Updated" (magh-core--date (alist-get 'updated_at gist))
             'magh-date))))
    (insert (propertize "No gists found.\n" 'font-lock-face 'shadow))))

;;;###autoload
(defun magh-gist-list ()
  "Open native list of current account Gists."
  (interactive)
  (let ((context (magh-context-resolve)))
    (magh-ui--open-page
     "*magh: Gists*" context 'gist-list 'viewer
     (lambda (success error force)
       (magh-api--gist-list context success error force))
     (lambda (data) (magh-gist--render-gists context data))
     :setup (lambda ()
              (setq magh-buffer-dispatch-function
                    #'magh-gist-list-dispatch)))))

(defun magh-gist--files (data)
  "Return file alists from Gist DATA."
  (mapcar #'cdr (alist-get 'files data)))

(defun magh-gist--file-resource (context id file data)
  "Create Gist file resource."
  (magh-resource-create
   'gist-file context :id id :path (alist-get 'filename file)
   :title (alist-get 'filename file)
   :url (alist-get 'html_url data) :data file))

(defun magh-gist--render-gist (context data)
  "Render Gist DATA."
  (let* ((id (alist-get 'id data))
         (resource (magh-gist--resource context data))
         (start (point)))
    (insert (propertize (or (alist-get 'description data) id)
                        'font-lock-face 'magh-resource-title) "\n")
    (add-text-properties start (point) (list 'magh-resource resource))
    (magh-ui--insert-header "ID" id)
    (magh-ui--insert-header
     "Visibility" (if (magh-api--true-p (alist-get 'public data))
                      "public" "secret") 'magh-permission)
    (magh-ui--insert-header
     "Updated" (magh-core--date (alist-get 'updated_at data)) 'magh-date)
    (insert "\n")
    (dolist (file (magh-gist--files data))
      (let ((resource (magh-gist--file-resource context id file data)))
        (magh-ui--section (gist-file (plist-get resource :path) resource nil)
          (magh-ui--styled (plist-get resource :path) 'magh-file)
          (magh-ui--insert-header "Language" (alist-get 'language file))
          (magh-ui--insert-header "Size" (alist-get 'size file))
          (insert (or (alist-get 'content file)
                      "Content is truncated; press RET to open the file.") "\n"))))))

(defun magh-gist--setup-gist (id)
  "Install Gist detail state for ID."
  (setq magh-gist--id id
        magh-buffer-dispatch-function #'magh-gist-dispatch)
  (local-set-key (kbd "E") #'magh-gist-edit-metadata)
  (local-set-key (kbd "+") #'magh-gist-file-add))

(defun magh-gist-view (id &optional context preview)
  "Open Gist ID."
  (interactive (list (read-string "Gist ID: ")))
  (setq context (magh-context-resolve context))
  (magh-ui--open-page
   (if preview (format "*magh preview: Gist %s*" id)
     (format "*magh: Gist %s*" id))
   context 'gist id
   (lambda (success error force)
     (magh-api--gist-get context id success error force))
   (lambda (data) (magh-gist--render-gist context data))
   :preview preview :setup (lambda () (magh-gist--setup-gist id))))

(defun magh-gist-file-view (id path &optional context)
  "Open PATH in Gist ID as a read-only source buffer."
  (setq context (magh-context-resolve context))
  (let ((buffer (get-buffer-create (format "*magh: Gist %s · %s*" id path))))
    (with-current-buffer buffer
      (special-mode)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (propertize "Loading Gist file…\n"
                            'font-lock-face 'magh-loading)))
      (magh-api--gist-file-raw
       context id path
       (lambda (content)
         (let ((inhibit-read-only t))
           (erase-buffer) (insert content)
           (goto-char (point-min))
           (let ((buffer-file-name path)) (set-auto-mode))
           (setq buffer-read-only t)
           (set-buffer-modified-p nil)))
       (lambda (error)
         (let ((inhibit-read-only t))
           (erase-buffer) (insert (magh-error-message error) "\n")))))
    (funcall magh-display-buffer-function buffer)
    buffer))

(defun magh-gist--prefill ()
  "Return (FILENAME CONTENT) from the active region or file buffer."
  (let* ((file-buffer buffer-file-name)
         (filename (if file-buffer (file-name-nondirectory file-buffer)
                     "gist.txt"))
         (content
          (cond
           ((use-region-p)
            (buffer-substring-no-properties (region-beginning) (region-end)))
           (file-buffer
            (buffer-substring-no-properties (point-min) (point-max)))
           (t ""))))
    (list filename content)))

;;;###autoload
(defun magh-gist-create ()
  "Create a single-file Gist, defaulting to secret visibility."
  (interactive)
  (pcase-let* ((`(,filename ,content) (magh-gist--prefill))
               (context (magh-context-resolve)))
    (magh-edit-open
     "*magh: New Gist*"
     '((:name filename :required t)
       (:name description :allow-empty t)
       (:name public :type boolean))
     (list :filename filename :description "" :public :json-false) content
     (lambda (values body success error)
       (let ((filename (string-trim (or (plist-get values :filename) ""))))
         (if (string-empty-p filename)
             (funcall error
                      (magh-core--error 'magh-invalid-input
                                        "Gist filename is required"))
           (magh-api--gist-create
            context
            (list :description (plist-get values :description)
                  :public (plist-get values :public)
                  :files (list (cons filename (list :content body))))
            success error))))
     :after-success
     (lambda (gist)
       (when-let* ((id (alist-get 'id gist)))
         (magh-gist-view id context))))))

(defun magh-gist--current-gist-resource ()
  "Return current Gist or Gist file resource."
  (let ((point-resource (magh-ui-resource-at-point)))
    (or (and (memq (plist-get point-resource :kind) '(gist gist-file))
             point-resource)
        magh-gist--dispatch-resource
        (and magh-gist--id
             (magh-resource-create
              'gist magh-buffer-context :id magh-gist--id)))))

(defun magh-gist--data ()
  "Return loaded Gist metadata on a detail page."
  (and (listp magh-ui--data) (alist-get 'files magh-ui--data) magh-ui--data))

(defun magh-gist--with-gist (context id function)
  "Call FUNCTION with complete Gist metadata for ID."
  (if-let* ((data (and (equal id (alist-get 'id (magh-gist--data)))
                       (magh-gist--data))))
      (funcall function data)
    (magh-api--gist-get context id function #'magh-core--user-error t)))

;;;###autoload
(defun magh-gist-edit-metadata (&optional context id)
  "Edit the description of Gist ID.
Gist visibility cannot be converted after creation."
  (interactive)
  (let ((resource (magh-gist--current-gist-resource)))
    (setq context (magh-context-resolve
                   (or context (plist-get resource :context)))
          id (or id (plist-get resource :id) magh-gist--id
                 (read-string "Gist ID: ")))
    (magh-gist--with-gist
     context id
     (lambda (data)
       (magh-edit-open
        (format "*magh: Gist %s · Metadata*" id)
        '((:name description :allow-empty t))
        (list :description (or (alist-get 'description data) "")) ""
        (lambda (values _body success error)
          (magh-api--gist-update
           context id (list :description (plist-get values :description))
           success error)))))))

(defun magh-gist--file-context (&optional resource)
  "Return (RESOURCE CONTEXT ID PATH) for a Gist file action."
  (setq resource (or resource (magh-gist--current-gist-resource)))
  (unless (eq (plist-get resource :kind) 'gist-file)
    (user-error "No Gist file selected"))
  (list resource (magh-context-resolve (plist-get resource :context))
        (plist-get resource :id) (plist-get resource :path)))

(defun magh-gist--open-gist-content-editor
    (context id filename content title)
  "Open content editor for FILENAME in Gist ID."
  (magh-edit-open
   title nil nil content
   (lambda (_values body success error)
     (magh-api--gist-update
      context id (list :files
                       (list (cons filename (list :content body))))
      success error))))

;;;###autoload
(defun magh-gist-file-edit (&optional resource)
  "Edit full content of selected Gist file RESOURCE."
  (interactive)
  (pcase-let ((`(,_ ,context ,id ,path)
               (magh-gist--file-context resource)))
    (magh-api--gist-file-raw
     context id path
     (lambda (content)
       (magh-gist--open-gist-content-editor
        context id path content
        (format "*magh: Gist %s · Edit %s*" id path)))
     #'magh-core--user-error t)))

;;;###autoload
(defun magh-gist-file-add (&optional context id)
  "Add a file to Gist ID."
  (interactive)
  (let ((resource (magh-gist--current-gist-resource)))
    (setq context (magh-context-resolve
                   (or context (plist-get resource :context)))
          id (or id (plist-get resource :id) magh-gist--id
                 (read-string "Gist ID: ")))
    (magh-gist--with-gist
     context id
     (lambda (data)
       (let ((names (mapcar (lambda (file) (alist-get 'filename file))
                            (magh-gist--files data))))
         (magh-edit-open
          (format "*magh: Gist %s · Add File*" id)
          '((:name filename :required t)) nil ""
          (lambda (values body success error)
            (let ((filename
                   (string-trim (or (plist-get values :filename) ""))))
              (cond
               ((string-empty-p filename)
                (funcall error
                         (magh-core--error 'magh-invalid-input
                                           "Gist filename is required")))
               ((member filename names)
                (funcall error
                         (magh-core--error
                          'magh-invalid-input
                          (format "Gist already contains %s" filename))))
               (t
                (magh-api--gist-update
                 context id
                 (list :files
                       (list (cons filename (list :content body))))
                 success error)))))))))))

;;;###autoload
(defun magh-gist-file-rename (&optional resource new-name)
  "Rename selected Gist file RESOURCE to NEW-NAME."
  (interactive)
  (pcase-let ((`(,_ ,context ,id ,path)
               (magh-gist--file-context resource)))
    (magh-gist--with-gist
     context id
     (lambda (data)
       (let ((names (mapcar (lambda (file) (alist-get 'filename file))
                            (magh-gist--files data))))
         (setq new-name (string-trim
                         (or new-name (read-string "New filename: " path))))
         (cond
          ((string-empty-p new-name)
           (user-error "Gist filename cannot be empty"))
          ((and (not (string= new-name path)) (member new-name names))
           (user-error "Gist already contains %s" new-name))
          ((string= new-name path)
           (user-error "Filename is unchanged"))
          (t
           (magh-api--gist-update
            context id
            (list :files
                  (list (cons path (list :filename new-name))))
            (magh-ui--refresh-message "Renamed %s to %s" path new-name)
            #'magh-core--user-error))))))))

;;;###autoload
(defun magh-gist-file-remove (&optional resource)
  "Remove selected Gist file RESOURCE, preserving at least one file."
  (interactive)
  (pcase-let ((`(,_ ,context ,id ,path)
               (magh-gist--file-context resource)))
    (magh-gist--with-gist
     context id
     (lambda (data)
       (when (<= (length (magh-gist--files data)) 1)
         (user-error "Cannot remove the last file from a Gist"))
       (when (magh-core--confirm (format "Remove %s from this Gist? " path))
         (magh-api--gist-file-remove
          context id path
          (magh-ui--refresh-message "Removed %s" path)
          #'magh-core--user-error))))))

(transient-define-prefix magh-gist-dispatch ()
  "Gist commands."
  [["Gist"
    ("g" "Refresh" magh-ui-refresh)
    ("E" "Edit description" magh-gist-edit-metadata)
    ("b" "Browse" magh-ui-browse)]
   ["Files"
    ("+" "Add" magh-gist-file-add)
    ("e" "Edit content" magh-gist-file-edit)
    ("r" "Rename" magh-gist-file-rename)
    ("D" "Remove" magh-gist-file-remove)]])

(transient-define-prefix magh-gist-list-dispatch ()
  "Gist list commands."
  [["View"
    ("g" "Refresh" magh-ui-refresh)]
   ["Create"
    ("c" "New Gist" magh-gist-create)]])


;;; Candidate registration

(magh-candidate-register
 'gist
 :open (lambda (resource)
         (magh-gist-view (plist-get resource :id) (plist-get resource :context)))
 :preview (lambda (resource)
            (magh-gist-view (plist-get resource :id)
                            (plist-get resource :context) t))
 :dispatch (lambda (resource)
             (setq magh-gist--dispatch-resource resource)
             (call-interactively #'magh-gist-dispatch)))

(magh-candidate-register
 'gist-file
 :open (lambda (resource)
         (magh-gist-file-view (plist-get resource :id)
                              (plist-get resource :path)
                              (plist-get resource :context)))
 :dispatch (lambda (resource)
             (setq magh-gist--dispatch-resource resource)
             (call-interactively #'magh-gist-dispatch)))

(magh-candidate-register 'gist-list :open (lambda (_resource) (magh-gist-list)))

(provide 'magh-gist)
;;; magh-gist.el ends here
