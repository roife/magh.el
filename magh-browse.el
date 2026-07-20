;;; magh-browse.el --- Remote repository tree and file browser -*- lexical-binding: t; -*-

;; Copyright (C) 2026
;; SPDX-License-Identifier: GPL-2.0-only

;; Author: magh.el contributors
;; Keywords: tools, vc, github

;;; Commentary:

;; Dired-like, clone-free remote directory navigation and read-only source
;; buffers.  Contents are fetched asynchronously through magh-api.

;;; Code:

(require 'cl-lib)
(require 'dired)
(require 'seq)
(require 'magh-api)
(require 'magh-candidate)
(require 'magh-ui)

(defvar-local magh-browse--generation 0)
(defvar-local magh-browse--ref nil)
(defvar-local magh-browse--path nil)
(defvar magh-browse--temporary-clones nil)

(defvar-keymap magh-tree-mode-map
  :parent special-mode-map
  "RET" #'magh-browse-visit
  "^" #'magh-browse-parent
  "r" #'magh-browse-select-ref
  "H" #'magh-browse-history
  "b" #'magh-browse-web
  "C" #'magh-repo-clone-temporary
  "g" #'magh-browse-refresh
  "q" #'quit-window
  "n" #'next-line
  "p" #'previous-line)

(define-derived-mode magh-tree-mode special-mode "magh-tree"
  "Major mode for a remote GitHub repository directory."
  :group 'magh
  (setq-local truncate-lines t)
  (hl-line-mode 1))

(defvar-keymap magh-remote-file-mode-map
  :parent special-mode-map
  "C-c C-o" #'magh-browse-web
  "C-c C-c" #'magh-repo-clone-temporary
  "H" #'magh-browse-history
  "^" #'magh-browse-parent
  "q" #'quit-window)

(define-minor-mode magh-remote-file-mode
  "Minor mode for read-only source files fetched from GitHub."
  :lighter " GitHub"
  :keymap magh-remote-file-mode-map
  (setq buffer-read-only magh-remote-file-mode))

(defun magh-browse--buffer-name (context ref path)
  "Return remote tree buffer name for CONTEXT, REF, and PATH."
  (format "*magh: %s · %s:%s*" (magh-context-repository context)
          ref path))

(defun magh-browse--item-resource (context ref item)
  "Create a native resource from remote content ITEM."
  (let* ((type (alist-get 'type item))
         (path (alist-get 'path item))
         (kind (if (string= type "dir") 'tree 'file)))
    (magh-resource-create
     kind (magh-context-copy context :ref ref :path path)
     :path path :ref ref :title (alist-get 'name item)
     :url (alist-get 'html_url item) :data item)))

(defun magh-browse--render-tree (context ref path data)
  "Render remote tree DATA."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (insert (propertize (magh-context-repository context)
                        'font-lock-face 'magh-repository)
            " · " (propertize ref 'font-lock-face 'magh-branch) ":"
            (or (magh-ui--styled path 'magh-file) "") "\n\n")
    (dolist (item (sort (copy-sequence data)
                        (lambda (a b)
                          (let ((ta (alist-get 'type a))
                                (tb (alist-get 'type b)))
                            (if (equal ta tb)
                                (string< (alist-get 'name a)
                                         (alist-get 'name b))
                              (string= ta "dir"))))))
      (let* ((resource (magh-browse--item-resource context ref item))
             (start (point))
             (type (alist-get 'type item))
             (name (alist-get 'name item))
             (size (alist-get 'size item)))
        (insert
         (magh-ui--row
          (magh-ui--styled type 'magh-permission)
          (and (not (string= type "dir"))
               (magh-ui--styled (format "%s bytes" size) 'magh-permission))
          (magh-ui--styled (concat name (if (string= type "dir") "/" ""))
                         (if (string= type "dir") 'magh-branch 'magh-file)))
         "\n")
        (add-text-properties start (point) (list 'magh-resource resource))))
    (goto-char (point-min))
    (forward-line 2)))

(defun magh-browse--render-error (error)
  "Render browse ERROR."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (insert (propertize (magh-error-message error)
                        'font-lock-face 'magh-error) "\n"
            "Press g to retry.\n")))

;;;###autoload
(defun magh-browse-repository (&optional context ref path)
  "Browse repository CONTEXT remotely at REF and PATH."
  (interactive (list (magh-context-read-repository)))
  (setq context (magh-ui--repository-context context)
        ref (or ref (magh-context-ref context)
                (magh-context-default-branch context) "HEAD")
        path (or path (magh-context-path context) ""))
  (let ((buffer (get-buffer-create (magh-browse--buffer-name context ref path))))
    (with-current-buffer buffer
      (unless (derived-mode-p 'magh-tree-mode) (magh-tree-mode))
      (setq magh-buffer-context (magh-context-copy context :ref ref :path path)
            magh-browse--ref ref magh-browse--path path)
      (magh-browse-refresh))
    (funcall magh-display-buffer-function buffer)
    buffer))

(defun magh-browse-refresh (&optional force)
  "Refresh current remote tree asynchronously."
  (interactive "P")
  (cl-incf magh-browse--generation)
  (let ((generation magh-browse--generation)
        (context magh-buffer-context)
        (ref magh-browse--ref)
        (path magh-browse--path))
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert (propertize "Loading remote repository tree…\n"
                          'font-lock-face 'magh-loading)))
    (magh-api--content-get
     context path ref
     (lambda (data)
       (when (= generation magh-browse--generation)
         (if (alist-get 'type data)
             (magh-browse--open-file-data context ref path data)
           (magh-browse--render-tree context ref path data))))
     (lambda (error)
       (when (= generation magh-browse--generation)
         (magh-browse--render-error error)))
     force)))

(defun magh-browse-visit ()
  "Visit remote resource at point."
  (interactive)
  (let ((resource (magh-candidate-at-point)))
    (unless resource (user-error "No remote entry at point"))
    (magh-resource-open resource)))

(defun magh-browse-parent ()
  "Open the parent remote directory."
  (interactive)
  (let ((directory
         (file-name-directory (directory-file-name magh-browse--path))))
    (magh-browse-repository
     (magh-ui--repository-context) magh-browse--ref
     (if directory (directory-file-name directory) ""))))

(defun magh-browse-select-ref ()
  "Asynchronously select a branch or tag and reopen the current path."
  (interactive)
  (let ((context (magh-ui--repository-context))
        (path magh-browse--path))
    (magh-core--collect-async
     (list
      (cons 'branches (lambda (ok fail)
                        (magh-api--repo-branches context ok fail)))
      (cons 'tags (lambda (ok fail)
                    (magh-api--repo-tags context ok fail))))
     (lambda (result)
       (let* ((refs
               (append
                (mapcar (lambda (item)
                          (let ((name (alist-get 'name item)))
                            (cons (magh-ui--row
                                   (magh-ui--styled "branch" 'magh-permission)
                                   (magh-ui--styled name 'magh-branch))
                                  name)))
                        (alist-get 'branches result))
                (mapcar (lambda (item)
                          (let ((name (alist-get 'name item)))
                            (cons (magh-ui--row
                                   (magh-ui--styled "tag" 'magh-permission)
                                   (magh-ui--styled name 'magh-tag))
                                  name)))
                        (alist-get 'tags result))))
              (choice (completing-read "Ref: " refs nil t))
              (ref (cdr (assoc choice refs))))
         (magh-browse-repository context ref path)))
     #'magh-core--user-error)))

(defun magh-browse-history ()
  "Open Commit history for current remote path."
  (interactive)
  (let* ((resource (magh-candidate-at-point))
         (context (or (plist-get resource :context) magh-buffer-context))
         (path (or (plist-get resource :path) (magh-context-path context)))
         (ref (or (plist-get resource :ref) (magh-context-ref context))))
    (magh-resource-open (magh-resource-create 'commit-list context
                                          :path path :ref ref))))

(defun magh-browse-web ()
  "Explicitly browse current remote entry on GitHub."
  (interactive)
  (let ((resource (or (magh-candidate-at-point)
                      (magh-resource-create
                       (if (derived-mode-p 'magh-tree-mode) 'tree 'file)
                       magh-buffer-context :path (magh-context-path magh-buffer-context)
                       :ref (magh-context-ref magh-buffer-context)))))
    (magh-resource-browse resource)))

;;; Remote files

(defun magh-browse--display-file (buffer context ref path text &optional line)
  "Display TEXT in remote file BUFFER with suitable major mode."
  (with-current-buffer buffer
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert text)
      (goto-char (point-min))
      (let ((buffer-file-name path)) (set-auto-mode))
      (setq magh-buffer-context (magh-context-copy context :ref ref :path path)
            magh-browse--ref ref magh-browse--path path)
      (magh-remote-file-mode 1)
      (set-buffer-modified-p nil)
      (when line
        (goto-char (point-min))
        (forward-line (1- line))))))

(defun magh-browse--open-file-data (context ref path data &optional line buffer)
  "Open remote file DATA, using raw fallback when necessary."
  (let ((buffer (or buffer
                    (get-buffer-create
                     (magh-browse--buffer-name context ref path)))))
    (if (equal (alist-get 'encoding data) "base64")
        (magh-browse--display-file
         buffer context ref path (magh-api--decode-content data) line)
      (with-current-buffer buffer
        (magh-api--content-raw
         context path ref
         (lambda (text)
           (magh-browse--display-file buffer context ref path text line))
         (lambda (error) (magh-browse--render-error error)))))
    (funcall magh-display-buffer-function buffer)
    buffer))

(defun magh-browse-file (context path ref &optional line)
  "Open remote PATH at REF and optional LINE in CONTEXT."
  (setq context (magh-ui--repository-context context)
        ref (or ref (magh-context-ref context) "HEAD"))
  (let ((buffer (get-buffer-create
                 (magh-browse--buffer-name context ref path))))
    (with-current-buffer buffer
      (special-mode)
      (setq magh-buffer-context (magh-context-copy context :ref ref :path path)
            magh-browse--ref ref magh-browse--path path)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (propertize "Loading remote file…\n"
                            'font-lock-face 'magh-loading)))
      (magh-api--content-get
       context path ref
       (lambda (data) (magh-browse--open-file-data context ref path data line buffer))
       (lambda (error) (magh-browse--render-error error))))
    (funcall magh-display-buffer-function buffer)
    buffer))

;;; Temporary clones

;;;###autoload
(defun magh-repo-clone-temporary (&optional context)
  "Shallow clone repository CONTEXT into a tracked temporary directory."
  (interactive)
  (setq context (magh-ui--repository-context context))
  (make-directory magh-temporary-clone-directory t)
  (let* ((base (replace-regexp-in-string
                "/" "--" (magh-context-repository context)))
         (directory (make-temp-file
                     (expand-file-name (concat base "-")
                                       magh-temporary-clone-directory)
                     t))
         (ref (magh-context-ref context)))
    (magh-api--repo-clone
     context directory
     (lambda (_)
       (cl-pushnew directory magh-browse--temporary-clones :test #'equal)
       (dired directory))
     #'magh-core--user-error
     (append '("--depth" "1")
             (when (and ref (not (string= ref "HEAD")))
               (list "--branch" ref))))))

;;;###autoload
(defun magh-clean-temporary-clones ()
  "Delete temporary repository clones created by magh.el."
  (interactive)
  (let ((existing (seq-filter #'file-directory-p magh-browse--temporary-clones)))
    (when (and existing
               (magh-core--confirm
                (format "Delete %d magh.el temporary clone(s)? " (length existing))))
      (dolist (directory existing)
        (delete-directory directory t))
      (setq magh-browse--temporary-clones nil)
      (message "Deleted %d temporary clone(s)" (length existing)))))

;;; Candidate registration

(magh-candidate-register
 'tree
 :open (lambda (resource)
         (magh-browse-repository (plist-get resource :context)
                               (plist-get resource :ref)
                               (plist-get resource :path))))

(magh-candidate-register
 'file
 :open (lambda (resource)
         (magh-browse-file (plist-get resource :context)
                         (plist-get resource :path)
                         (plist-get resource :ref)
                         (plist-get resource :line))))

(provide 'magh-browse)
;;; magh-browse.el ends here
