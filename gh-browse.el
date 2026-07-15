;;; gh-browse.el --- Remote repository tree and file browser -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; Author: gh.el contributors
;; Keywords: tools, vc, github
;; Package-Requires: ((emacs "29.1"))

;;; Commentary:

;; Dired-like, clone-free remote directory navigation and read-only source
;; buffers.  Contents are fetched asynchronously through gh-api.

;;; Code:

(require 'base64)
(require 'cl-lib)
(require 'dired)
(require 'seq)
(require 'subr-x)
(require 'gh-api)
(require 'gh-candidate)
(require 'gh-ui)

(defvar-local gh-browse--generation 0)
(defvar-local gh-browse--ref nil)
(defvar-local gh-browse--path nil)
(defvar-local gh-browse--source-buffer nil)
(defvar gh-browse--temporary-clones nil)

(defvar-keymap gh-tree-mode-map
  :parent special-mode-map
  "RET" #'gh-browse-visit
  "^" #'gh-browse-parent
  "r" #'gh-browse-select-ref
  "H" #'gh-browse-history
  "b" #'gh-browse-web
  "C" #'gh-repo-clone-temporary
  "g" #'gh-browse-refresh
  "q" #'quit-window
  "n" #'next-line
  "p" #'previous-line)

(define-derived-mode gh-tree-mode special-mode "gh-tree"
  "Major mode for a remote GitHub repository directory."
  :group 'gh
  (setq-local truncate-lines t)
  (hl-line-mode 1))

(defvar-keymap gh-remote-file-mode-map
  :parent special-mode-map
  "C-c C-o" #'gh-browse-web
  "C-c C-c" #'gh-repo-clone-temporary
  "H" #'gh-browse-history
  "^" #'gh-browse-parent
  "q" #'quit-window)

(define-minor-mode gh-remote-file-mode
  "Minor mode for read-only source files fetched from GitHub."
  :lighter " GitHub"
  :keymap gh-remote-file-mode-map
  (when gh-remote-file-mode
    (setq buffer-read-only t)))

(defun gh-browse--context (&optional context)
  "Resolve repository CONTEXT for browsing."
  (gh-context-resolve (or context gh-buffer-context) t))

(defun gh-browse--buffer-name (context ref path)
  "Return remote tree buffer name for CONTEXT, REF, and PATH."
  (format "*gh: %s · %s:%s*" (gh-context-repository context)
          (or ref "HEAD") (or path "")))

(defun gh-browse--file-buffer-name (context ref path)
  "Return remote file buffer name."
  (format "*gh: %s · %s:%s*" (gh-context-repository context) ref path))

(defun gh-browse--parent-path (path)
  "Return parent of repository PATH."
  (let ((directory (file-name-directory (directory-file-name (or path "")))))
    (if directory (directory-file-name directory) "")))

(defun gh-browse--item-resource (context ref item)
  "Create a native resource from remote content ITEM."
  (let* ((type (gh-core--alist-get 'type item))
         (path (gh-core--alist-get 'path item))
         (kind (if (string= type "dir") 'tree 'file)))
    (gh-resource-create
     kind (gh-context-copy context :ref ref :path path)
     :path path :ref ref :title (gh-core--alist-get 'name item)
     :url (gh-core--alist-get 'html_url item) :data item)))

(defun gh-browse--render-tree (context ref path data)
  "Render remote tree DATA."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (insert (propertize (gh-context-repository context)
                        'font-lock-face 'gh-repository)
            " · " (propertize ref 'font-lock-face 'gh-branch) ":"
            (or (gh-ui--styled path 'gh-file) "") "\n\n")
    (dolist (item (sort (copy-sequence data)
                        (lambda (a b)
                          (let ((ta (gh-core--alist-get 'type a))
                                (tb (gh-core--alist-get 'type b)))
                            (if (equal ta tb)
                                (string< (gh-core--alist-get 'name a)
                                         (gh-core--alist-get 'name b))
                              (string= ta "dir"))))))
      (let* ((resource (gh-browse--item-resource context ref item))
             (start (point))
             (type (gh-core--alist-get 'type item))
             (name (gh-core--alist-get 'name item))
             (size (gh-core--alist-get 'size item)))
        (insert
         (gh-ui--row
          (gh-ui--styled type 'gh-permission)
          (and (not (string= type "dir"))
               (gh-ui--styled (format "%s bytes" (or size 0)) 'gh-permission))
          (gh-ui--styled (concat name (if (string= type "dir") "/" ""))
                         (if (string= type "dir") 'gh-branch 'gh-file)))
         "\n")
        (add-text-properties start (point) (list 'gh-resource resource))))
    (goto-char (point-min))
    (forward-line 2)))

(defun gh-browse--render-error (error)
  "Render browse ERROR."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (insert (propertize (gh-error-message error)
                        'font-lock-face 'gh-error) "\n"
            "Press g to retry.\n")))

;;;###autoload
(defun gh-browse-repository (&optional context ref path)
  "Browse repository CONTEXT remotely at REF and PATH."
  (interactive)
  (setq context (gh-browse--context context)
        ref (or ref (gh-context-ref context)
                (gh-context-default-branch context) "HEAD")
        path (or path (gh-context-path context) ""))
  (let ((buffer (get-buffer-create (gh-browse--buffer-name context ref path)))
        (source (current-buffer)))
    (with-current-buffer buffer
      (gh-tree-mode)
      (setq gh-buffer-context (gh-context-copy context :ref ref :path path)
            gh-browse--ref ref gh-browse--path path
            gh-browse--source-buffer source)
      (gh-browse-refresh))
    (funcall gh-display-buffer-function buffer)
    buffer))

(defun gh-browse-refresh (&optional force)
  "Refresh current remote tree asynchronously."
  (interactive "P")
  (cl-incf gh-browse--generation)
  (let ((generation gh-browse--generation)
        (context gh-buffer-context)
        (ref gh-browse--ref)
        (path gh-browse--path))
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert (propertize "Loading remote repository tree…\n"
                          'font-lock-face 'gh-loading)))
    (gh-api--content-list
     context path ref
     (lambda (data)
       (when (and (= generation gh-browse--generation)
                  (equal ref gh-browse--ref) (equal path gh-browse--path))
         (if (and (listp data) (gh-core--alist-get 'type data))
             (gh-browse--open-file-data context ref path data)
           (gh-browse--render-tree context ref path data))))
     (lambda (error)
       (when (= generation gh-browse--generation)
         (gh-browse--render-error error)))
     force)))

(defun gh-browse-visit ()
  "Visit remote resource at point."
  (interactive)
  (let ((resource (gh-candidate-at-point)))
    (unless resource (user-error "No remote entry at point"))
    (gh-resource-open resource)))

(defun gh-browse-parent ()
  "Open the parent remote directory."
  (interactive)
  (let* ((context (gh-browse--context))
         (path (or (and (derived-mode-p 'gh-tree-mode) gh-browse--path)
                   (gh-context-path context) ""))
         (parent (gh-browse--parent-path path)))
    (gh-browse-repository context (or gh-browse--ref (gh-context-ref context))
                          parent)))

(defun gh-browse-select-ref ()
  "Asynchronously select a branch or tag and reopen the current path."
  (interactive)
  (let ((context (gh-browse--context))
        (path gh-browse--path))
    (gh-core--collect-async
     (list
      (cons 'branches (lambda (ok fail)
                        (gh-api--repo-branches context ok fail)))
      (cons 'tags (lambda (ok fail)
                    (gh-api--repo-tags context ok fail))))
     (lambda (result)
       (let* ((refs
               (append
                (mapcar (lambda (item)
                          (let ((name (gh-core--alist-get 'name item)))
                            (cons (gh-ui--row
                                   (gh-ui--styled "branch" 'gh-permission)
                                   (gh-ui--styled name 'gh-branch))
                                  name)))
                        (alist-get 'branches result))
                (mapcar (lambda (item)
                          (let ((name (gh-core--alist-get 'name item)))
                            (cons (gh-ui--row
                                   (gh-ui--styled "tag" 'gh-permission)
                                   (gh-ui--styled name 'gh-tag))
                                  name)))
                        (alist-get 'tags result))))
              (choice (completing-read "Ref: " refs nil t))
              (ref (cdr (assoc choice refs))))
         (gh-browse-repository context ref path)))
     #'gh-core--user-error)))

(defun gh-browse-history ()
  "Open Commit history for current remote path."
  (interactive)
  (let* ((resource (gh-candidate-at-point))
         (context (or (plist-get resource :context) gh-buffer-context))
         (path (or (plist-get resource :path) (gh-context-path context)))
         (ref (or (plist-get resource :ref) (gh-context-ref context))))
    (gh-resource-open (gh-resource-create 'commit-list context
                                          :path path :ref ref))))

(defun gh-browse-web ()
  "Explicitly browse current remote entry on GitHub."
  (interactive)
  (let ((resource (or (gh-candidate-at-point)
                      (gh-resource-create
                       (if (derived-mode-p 'gh-tree-mode) 'tree 'file)
                       gh-buffer-context :path (gh-context-path gh-buffer-context)
                       :ref (gh-context-ref gh-buffer-context)))))
    (gh-resource-browse resource)))

;;; Remote files

(defun gh-browse--display-file (buffer context ref path text &optional line)
  "Display TEXT in remote file BUFFER with suitable major mode."
  (with-current-buffer buffer
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert text)
      (goto-char (point-min))
      (let ((buffer-file-name path)) (set-auto-mode))
      (setq gh-buffer-context (gh-context-copy context :ref ref :path path)
            gh-browse--ref ref gh-browse--path path)
      (gh-remote-file-mode 1)
      (set-buffer-modified-p nil)
      (when line
        (goto-char (point-min)) (forward-line (1- (max 1 line)))))))

(defun gh-browse--open-file-data (context ref path data &optional line buffer)
  "Open remote file DATA, using raw fallback when necessary."
  (let ((buffer (or buffer
                    (get-buffer-create
                     (gh-browse--file-buffer-name context ref path)))))
    (if-let* ((encoded (gh-core--alist-get 'content data)))
        (gh-browse--display-file
         buffer context ref path
         (decode-coding-string
          (base64-decode-string (replace-regexp-in-string "\n" "" encoded))
          'utf-8)
         line)
      (with-current-buffer buffer
        (gh-api--content-raw
         context path ref
         (lambda (text)
           (gh-browse--display-file buffer context ref path text line))
         (lambda (error) (gh-browse--render-error error)))))
    (funcall gh-display-buffer-function buffer)
    buffer))

(defun gh-browse-file (context path ref &optional line)
  "Open remote PATH at REF and optional LINE in CONTEXT."
  (setq context (gh-browse--context context)
        ref (or ref (gh-context-ref context) "HEAD"))
  (let ((buffer (get-buffer-create
                 (gh-browse--file-buffer-name context ref path))))
    (with-current-buffer buffer
      (special-mode)
      (setq gh-buffer-context (gh-context-copy context :ref ref :path path)
            gh-browse--ref ref gh-browse--path path)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (propertize "Loading remote file…\n"
                            'font-lock-face 'gh-loading)))
      (gh-api--content-get
       context path ref
       (lambda (data) (gh-browse--open-file-data context ref path data line buffer))
       (lambda (error) (gh-browse--render-error error))))
    (funcall gh-display-buffer-function buffer)
    buffer))

;;; Temporary clones

;;;###autoload
(defun gh-repo-clone-temporary (&optional context)
  "Shallow clone repository CONTEXT into a tracked temporary directory."
  (interactive)
  (setq context (gh-browse--context context))
  (make-directory gh-temporary-clone-directory t)
  (let* ((base (replace-regexp-in-string
                "/" "--" (gh-context-repository context)))
         (directory (make-temp-file
                     (expand-file-name (concat base "-")
                                       gh-temporary-clone-directory)
                     t))
         (ref (gh-context-ref context)))
    (delete-directory directory)
    (gh-api--repo-clone
     context directory
     (lambda (_)
       (cl-pushnew directory gh-browse--temporary-clones :test #'equal)
       (dired directory))
     #'gh-core--user-error
     (append '("--depth" "1")
             (when (and ref (not (string= ref "HEAD")))
               (list "--branch" ref))))))

;;;###autoload
(defun gh-clean-temporary-clones ()
  "Delete temporary repository clones created by gh.el."
  (interactive)
  (let ((existing (seq-filter #'file-directory-p gh-browse--temporary-clones)))
    (when (and existing
               (gh-core--confirm
                (format "Delete %d gh.el temporary clone(s)? " (length existing))))
      (dolist (directory existing)
        (delete-directory directory t))
      (setq gh-browse--temporary-clones nil)
      (message "Deleted %d temporary clone(s)" (length existing)))))

;;; Candidate registration

(gh-candidate-register
 'tree
 :open (lambda (resource)
         (gh-browse-repository (plist-get resource :context)
                               (plist-get resource :ref)
                               (plist-get resource :path))))

(gh-candidate-register
 'file
 :open (lambda (resource)
         (gh-browse-file (plist-get resource :context)
                         (plist-get resource :path)
                         (plist-get resource :ref)
                         (plist-get resource :line))))

(provide 'gh-browse)
;;; gh-browse.el ends here
