;;; gh-embark.el --- Optional structured Embark actions for gh.el -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; Author: gh.el contributors
;; Keywords: tools, vc, github, convenience
;; Package-Requires: ((emacs "29.1"))

;;; Commentary:

;; This optional module exposes the same structured resource actions used by
;; Consult and section RET.  It has no hard Embark dependency: loading the file
;; is harmless, while enabling `gh-embark-mode' checks that Embark is installed.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'gh-api)
(require 'gh-actions)
(require 'gh-candidate)
(require 'gh-core)
(require 'gh-issue)
(require 'gh-notify)
(require 'gh-pr)
(require 'gh-repo)

(declare-function embark--target-buffer "embark" ())
(defvar embark-keymap-alist)

(defgroup gh-embark nil
  "Optional Embark actions for gh.el candidates."
  :group 'gh)

(defvar gh-embark--saved-keymaps nil
  "Embark category entries replaced by `gh-embark-mode'.")

(defvar gh-embark-mode nil)

(defconst gh-embark--category-maps
  '((gh-repository . gh-embark-repository-map)
    (gh-file . gh-embark-file-map)
    (gh-tree . gh-embark-file-map)
    (gh-issue . gh-embark-issue-map)
    (gh-pr . gh-embark-pr-map)
    (gh-release . gh-embark-resource-map)
    (gh-workflow . gh-embark-resource-map)
    (gh-run . gh-embark-run-map)
    (gh-branch . gh-embark-resource-map)
    (gh-commit . gh-embark-resource-map)
    (gh-notification . gh-embark-notification-map))
  "Mapping from completion categories to Embark action maps.")

(defun gh-embark--resource (target)
  "Extract a structured resource from Embark TARGET."
  (or (and (listp target) (plist-get target :kind) target)
      (and (stringp target) (get-text-property 0 'gh-resource target))
      (user-error "Embark target does not carry a GitHub resource")))

(defun gh-embark-open (target)
  "Open GitHub resource represented by TARGET."
  (gh-resource-open (gh-embark--resource target)))

(defun gh-embark-browse (target)
  "Browse GitHub resource represented by TARGET."
  (gh-resource-browse (gh-embark--resource target)))

(defun gh-embark-copy-url (target)
  "Copy TARGET's GitHub URL."
  (gh-resource-copy-url (gh-embark--resource target)))

(defun gh-embark-copy-title (target)
  "Copy TARGET's display title."
  (gh-resource-copy-title (gh-embark--resource target)))

(defun gh-embark-copy-org-link (target)
  "Copy TARGET as an Org link."
  (gh-resource-org-link (gh-embark--resource target)))

(defun gh-embark--target-marker ()
  "Return insertion marker in the buffer from which Embark was invoked."
  (let ((buffer
         (or (and (fboundp 'embark--target-buffer)
                  (embark--target-buffer))
             (and (minibufferp) (minibuffer-selected-window)
                  (window-buffer (minibuffer-selected-window)))
             (current-buffer))))
    (unless (buffer-live-p buffer)
      (user-error "Embark source buffer is no longer live"))
    (with-current-buffer buffer
      (when buffer-read-only
        (user-error "Embark source buffer is read-only"))
      (copy-marker (point) t))))

(defun gh-embark--insert (text)
  "Insert TEXT into the Embark source buffer."
  (let ((marker (gh-embark--target-marker)))
    (with-current-buffer (marker-buffer marker)
      (goto-char marker)
      (insert text))
    (set-marker marker nil)))

(defun gh-embark-insert-title (target)
  "Insert TARGET's title at the original buffer point."
  (gh-embark--insert (gh-resource-title (gh-embark--resource target))))

(defun gh-embark-insert-url (target)
  "Insert TARGET's URL at the original buffer point."
  (let* ((resource (gh-embark--resource target))
         (url (gh-resource-url resource)))
    (unless url (user-error "Resource has no GitHub URL"))
    (gh-embark--insert url)))

(defun gh-embark--repository-context (target)
  "Return repository context carried by TARGET."
  (let* ((resource (gh-embark--resource target))
         (context (plist-get resource :context)))
    (unless (and context (gh-context-repository context))
      (user-error "This action requires a repository resource"))
    context))

(defun gh-embark-copy-https-url (target)
  "Copy TARGET repository's HTTPS clone URL."
  (let* ((context (gh-embark--repository-context target))
         (url (concat (gh-context-web-url context) ".git")))
    (kill-new url)
    (message "Copied %s" url)))

(defun gh-embark-copy-ssh-url (target)
  "Copy TARGET repository's SSH clone URL."
  (let* ((context (gh-embark--repository-context target))
         (url (format "git@%s:%s.git"
                      (or (gh-context-host context) "github.com")
                      (gh-context-repository context))))
    (kill-new url)
    (message "Copied %s" url)))

(defun gh-embark-copy-straight-snippet (target)
  "Copy a straight.el `use-package' snippet for repository TARGET."
  (let* ((context (gh-embark--repository-context target))
         (repo (gh-context-repository context))
         (name (gh-context-name context))
         (host (or (gh-context-host context) "github.com"))
         (recipe (if (string= host "github.com")
                     (format "(:host github :repo %S)" repo)
                   (format "(:type git :repo %S)"
                           (concat (gh-context-web-url context) ".git"))))
         (snippet (format "(use-package %s\n  :straight %s)" name recipe)))
    (kill-new snippet)
    (message "Copied straight.el recipe for %s" repo)))

(defun gh-embark-clone (target)
  "Clone repository TARGET after reading a destination."
  (let* ((context (gh-embark--repository-context target))
         (repo (gh-context-repository context))
         (directory
          (read-directory-name "Clone into: " nil nil nil
                               (file-name-nondirectory repo))))
    (gh-repository-clone repo directory)))

(defun gh-embark-fork (target)
  "Fork repository TARGET."
  (gh-repository-fork (gh-embark--repository-context target)))

(defun gh-embark--toggle-list-value (variable value label)
  "Toggle VALUE in list VARIABLE and describe it using LABEL."
  (let ((values (symbol-value variable)))
    (if (member value values)
        (progn
          (set variable (delete value values))
          (message "Removed %s from %s" value label))
      (set variable (append values (list value)))
      (message "Added %s to %s" value label))))

(defun gh-embark-toggle-known-repository (target)
  "Toggle repository TARGET in `gh-known-repositories'."
  (let ((context (gh-embark--repository-context target)))
    (gh-embark--toggle-list-value
     'gh-known-repositories (gh-context-repository context)
     "known repositories")))

(defun gh-embark-toggle-favorite-organization (target)
  "Toggle TARGET owner in `gh-favorite-organizations'."
  (let ((context (gh-embark--repository-context target)))
    (gh-embark--toggle-list-value
     'gh-favorite-organizations (gh-context-owner context)
     "favorite organizations")))

(defun gh-embark-toggle-workflow-template-repository (target)
  "Toggle TARGET in `gh-workflow-template-repositories'."
  (let ((context (gh-embark--repository-context target)))
    (gh-embark--toggle-list-value
     'gh-workflow-template-repositories (gh-context-repository context)
     "workflow template repositories")))

(defun gh-embark--decode-content (data)
  "Decode GitHub Contents API DATA."
  (let ((content (or (gh-core--alist-get 'content data) ""))
        (encoding (gh-core--alist-get 'encoding data)))
    (if (equal encoding "base64")
        (decode-coding-string
         (base64-decode-string
          (replace-regexp-in-string "[\r\n]" "" content))
         'utf-8)
      content)))

(defun gh-embark--remote-content (target consumer)
  "Fetch remote file TARGET asynchronously and call CONSUMER with its text."
  (let* ((resource (gh-embark--resource target))
         (context (plist-get resource :context))
         (path (plist-get resource :path))
         (ref (plist-get resource :ref)))
    (unless (and (eq (plist-get resource :kind) 'file) context path)
      (user-error "This action requires a remote file"))
    (gh-api--content-get
     context path ref
     (lambda (data) (funcall consumer (gh-embark--decode-content data)))
     #'gh-core--user-error)))

(defun gh-embark-insert-remote-file (target)
  "Fetch TARGET and insert its contents at the original point."
  (let ((marker (gh-embark--target-marker)))
    (gh-embark--remote-content
     target
     (lambda (content)
       (unwind-protect
           (if (not (marker-buffer marker))
               (message "Embark insertion buffer was closed")
             (condition-case error
                 (with-current-buffer (marker-buffer marker)
                   (goto-char marker)
                   (insert content))
               (error
                (message "Cannot insert remote file: %s"
                         (error-message-string error)))))
         (set-marker marker nil))))))

(defun gh-embark-copy-remote-file (target)
  "Fetch TARGET and copy its contents asynchronously."
  (gh-embark--remote-content
   target (lambda (content)
            (kill-new content)
            (message "Copied remote file contents"))))

(defun gh-embark-edit-topic (target)
  "Edit Issue or Pull Request TARGET."
  (let* ((resource (gh-embark--resource target))
         (context (plist-get resource :context))
         (number (plist-get resource :number)))
    (pcase (plist-get resource :kind)
      ('issue (gh-issue-edit context number))
      ('pr (gh-pr-edit context number))
      (_ (user-error "Target is not an Issue or Pull Request")))))

(defun gh-embark-close-topic (target)
  "Close Issue or Pull Request TARGET."
  (let* ((resource (gh-embark--resource target))
         (context (plist-get resource :context))
         (number (plist-get resource :number)))
    (pcase (plist-get resource :kind)
      ('issue (gh-issue-close context number))
      ('pr (gh-pr-close context number))
      (_ (user-error "Target is not an Issue or Pull Request")))))

(defun gh-embark-reopen-topic (target)
  "Reopen Issue or Pull Request TARGET."
  (let* ((resource (gh-embark--resource target))
         (context (plist-get resource :context))
         (number (plist-get resource :number)))
    (pcase (plist-get resource :kind)
      ('issue (gh-issue-reopen context number))
      ('pr (gh-pr-reopen context number))
      (_ (user-error "Target is not an Issue or Pull Request")))))

(defun gh-embark-rerun (target)
  "Rerun Actions Run TARGET."
  (let ((resource (gh-embark--resource target)))
    (gh-run-rerun nil (plist-get resource :context) (plist-get resource :id))))

(defun gh-embark-cancel-run (target)
  "Cancel Actions Run TARGET."
  (let ((resource (gh-embark--resource target)))
    (gh-run-cancel (plist-get resource :context) (plist-get resource :id))))

(defun gh-embark-notification-read (target)
  "Mark Notification TARGET read."
  (gh-notification-mark-read (gh-embark--resource target)))

(defun gh-embark-notification-unread (target)
  "Use the documented web fallback to mark Notification TARGET unread."
  (gh-notification-mark-unread (gh-embark--resource target)))

(defun gh-embark-notification-subscribe (target)
  "Subscribe to Notification TARGET."
  (gh-notification-subscribe (gh-embark--resource target)))

(defun gh-embark-notification-unsubscribe (target)
  "Unsubscribe from Notification TARGET."
  (gh-notification-unsubscribe (gh-embark--resource target)))

(defvar-keymap gh-embark-resource-map
  :doc "Common actions for structured gh.el resources."
  "RET" #'gh-embark-open
  "o" #'gh-embark-browse
  "w" #'gh-embark-copy-url
  "y" #'gh-embark-copy-title
  "l" #'gh-embark-copy-org-link
  "i" #'gh-embark-insert-title
  "u" #'gh-embark-insert-url)

(defvar-keymap gh-embark-repository-map
  :doc "Embark actions for gh.el repository resources."
  :parent gh-embark-resource-map
  "c" #'gh-embark-clone
  "f" #'gh-embark-fork
  "h" #'gh-embark-copy-https-url
  "s" #'gh-embark-copy-ssh-url
  "p" #'gh-embark-copy-straight-snippet
  "k" #'gh-embark-toggle-known-repository
  "O" #'gh-embark-toggle-favorite-organization
  "T" #'gh-embark-toggle-workflow-template-repository)

(defvar-keymap gh-embark-file-map
  :doc "Embark actions for gh.el remote file resources."
  :parent gh-embark-resource-map
  "I" #'gh-embark-insert-remote-file
  "Y" #'gh-embark-copy-remote-file)

(defvar-keymap gh-embark-issue-map
  :doc "Embark actions for gh.el Issue resources."
  :parent gh-embark-resource-map
  "e" #'gh-embark-edit-topic
  "x" #'gh-embark-close-topic
  "r" #'gh-embark-reopen-topic)

(defvar-keymap gh-embark-pr-map
  :doc "Embark actions for gh.el Pull Request resources."
  :parent gh-embark-resource-map
  "e" #'gh-embark-edit-topic
  "x" #'gh-embark-close-topic
  "r" #'gh-embark-reopen-topic)

(defvar-keymap gh-embark-run-map
  :doc "Embark actions for gh.el Actions Run resources."
  :parent gh-embark-resource-map
  "r" #'gh-embark-rerun
  "x" #'gh-embark-cancel-run)

(defvar-keymap gh-embark-notification-map
  :doc "Embark actions for gh.el Notification resources."
  :parent gh-embark-resource-map
  "r" #'gh-embark-notification-read
  "u" #'gh-embark-notification-unread
  "s" #'gh-embark-notification-subscribe
  "U" #'gh-embark-notification-unsubscribe)

(defun gh-embark--install ()
  "Install all gh.el category maps into `embark-keymap-alist'."
  (setq gh-embark--saved-keymaps nil)
  (dolist (entry gh-embark--category-maps)
    (let* ((category (car entry))
           (existing (assq category embark-keymap-alist)))
      (push (list category (and existing t) (cdr existing))
            gh-embark--saved-keymaps)
      (setf (alist-get category embark-keymap-alist) (cdr entry)))))

(defun gh-embark--uninstall ()
  "Restore Embark category maps replaced by this integration."
  (dolist (saved gh-embark--saved-keymaps)
    (pcase-let ((`(,category ,present ,value) saved))
      (if present
          (setf (alist-get category embark-keymap-alist) value)
        (setq embark-keymap-alist
              (assq-delete-all category embark-keymap-alist)))))
  (setq gh-embark--saved-keymaps nil))

;;;###autoload
(define-minor-mode gh-embark-mode
  "Register structured gh.el candidate categories with Embark."
  :global t
  :group 'gh-embark
  (if gh-embark-mode
      (if (require 'embark nil t)
          (gh-embark--install)
        (setq gh-embark-mode nil)
        (user-error "Embark is not installed"))
    (when (featurep 'embark)
      (gh-embark--uninstall))))

(provide 'gh-embark)
;;; gh-embark.el ends here
