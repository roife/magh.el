;;; magh-embark.el --- Optional structured Embark actions for magh.el -*- lexical-binding: t; -*-

;; Copyright (C) 2026
;; SPDX-License-Identifier: GPL-2.0-only

;; Author: magh.el contributors
;; Keywords: tools, vc, github, convenience

;;; Commentary:

;; This optional module exposes the same structured resource actions used by
;; Consult and section RET.  It has no hard Embark dependency: loading the file
;; is harmless, while enabling `magh-embark-mode' checks that Embark is installed.

;;; Code:

(require 'magh-api)
(require 'magh-actions)
(require 'magh-candidate)
(require 'magh-core)
(require 'magh-issue)
(require 'magh-notify)
(require 'magh-pr)
(require 'magh-repo)

(declare-function embark--target-buffer "embark" ())
(defvar embark-keymap-alist)

(defgroup magh-embark nil
  "Optional Embark actions for magh.el candidates."
  :group 'magh)

(defvar magh-embark--saved-keymaps nil
  "Embark category entries replaced by `magh-embark-mode'.")

(defconst magh-embark--category-maps
  '((magh-repository . magh-embark-repository-map)
    (magh-file . magh-embark-file-map)
    (magh-tree . magh-embark-file-map)
    (magh-issue . magh-embark-topic-map)
    (magh-pr . magh-embark-topic-map)
    (magh-release . magh-embark-resource-map)
    (magh-workflow . magh-embark-resource-map)
    (magh-run . magh-embark-run-map)
    (magh-branch . magh-embark-resource-map)
    (magh-commit . magh-embark-resource-map)
    (magh-notification . magh-embark-notification-map))
  "Mapping from completion categories to Embark action maps.")

(defun magh-embark--resource (target)
  "Extract a structured resource from Embark TARGET."
  (or (and (listp target) (plist-get target :kind) target)
      (and (stringp target) (get-text-property 0 'magh-resource target))
      (user-error "Embark target does not carry a GitHub resource")))

(defun magh-embark-open (target)
  "Open GitHub resource represented by TARGET."
  (magh-resource-open (magh-embark--resource target)))

(defun magh-embark-browse (target)
  "Browse GitHub resource represented by TARGET."
  (magh-resource-browse (magh-embark--resource target)))

(defun magh-embark-copy-url (target)
  "Copy TARGET's GitHub URL."
  (magh-resource-copy-url (magh-embark--resource target)))

(defun magh-embark-copy-title (target)
  "Copy TARGET's display title."
  (magh-resource-copy-title (magh-embark--resource target)))

(defun magh-embark-copy-org-link (target)
  "Copy TARGET as an Org link."
  (magh-resource-org-link (magh-embark--resource target)))

(defun magh-embark--target-marker ()
  "Return insertion marker in the buffer from which Embark was invoked."
  (with-current-buffer (embark--target-buffer)
    (when buffer-read-only
      (user-error "Embark source buffer is read-only"))
    (copy-marker (point) t)))

(defun magh-embark--insert (text)
  "Insert TEXT into the Embark source buffer."
  (let ((marker (magh-embark--target-marker)))
    (with-current-buffer (marker-buffer marker)
      (goto-char marker)
      (insert text))
    (set-marker marker nil)))

(defun magh-embark-insert-title (target)
  "Insert TARGET's title at the original buffer point."
  (magh-embark--insert (magh-resource-title (magh-embark--resource target))))

(defun magh-embark-insert-url (target)
  "Insert TARGET's URL at the original buffer point."
  (let* ((resource (magh-embark--resource target))
         (url (magh-resource-url resource)))
    (unless url (user-error "Resource has no GitHub URL"))
    (magh-embark--insert url)))

(defun magh-embark--repository-context (target)
  "Return repository context carried by TARGET."
  (let* ((resource (magh-embark--resource target))
         (context (plist-get resource :context)))
    (unless (and context (magh-context-repository context))
      (user-error "This action requires a repository resource"))
    context))

(defun magh-embark-copy-https-url (target)
  "Copy TARGET repository's HTTPS clone URL."
  (let* ((context (magh-embark--repository-context target))
         (url (concat (magh-context-web-url context) ".git")))
    (kill-new url)
    (message "Copied %s" url)))

(defun magh-embark-copy-ssh-url (target)
  "Copy TARGET repository's SSH clone URL."
  (let* ((context (magh-embark--repository-context target))
         (url (format "git@%s:%s.git"
                      (or (magh-context-host context) "github.com")
                      (magh-context-repository context))))
    (kill-new url)
    (message "Copied %s" url)))

(defun magh-embark-copy-straight-snippet (target)
  "Copy a straight.el `use-package' snippet for repository TARGET."
  (let* ((context (magh-embark--repository-context target))
         (repo (magh-context-repository context))
         (name (magh-context-name context))
         (host (or (magh-context-host context) "github.com"))
         (recipe (if (string= host "github.com")
                     (format "(:host github :repo %S)" repo)
                   (format "(:type git :repo %S)"
                           (concat (magh-context-web-url context) ".git"))))
         (snippet (format "(use-package %s\n  :straight %s)" name recipe)))
    (kill-new snippet)
    (message "Copied straight.el recipe for %s" repo)))

(defun magh-embark-clone (target)
  "Clone repository TARGET after reading a destination."
  (let* ((context (magh-embark--repository-context target))
         (repo (magh-context-repository context))
         (directory
          (read-directory-name "Clone into: " nil nil nil
                               (file-name-nondirectory repo))))
    (magh-repository-clone repo directory)))

(defun magh-embark-fork (target)
  "Fork repository TARGET."
  (magh-repository-fork (magh-embark--repository-context target)))

(defun magh-embark--toggle-list-value (variable value label)
  "Toggle VALUE in list VARIABLE and describe it using LABEL."
  (let ((values (symbol-value variable)))
    (if (member value values)
        (progn
          (set variable (delete value values))
          (message "Removed %s from %s" value label))
      (set variable (append values (list value)))
      (message "Added %s to %s" value label))))

(defun magh-embark-toggle-known-repository (target)
  "Toggle repository TARGET in `magh-known-repositories'."
  (let ((context (magh-embark--repository-context target)))
    (magh-embark--toggle-list-value
     'magh-known-repositories (magh-context-repository context)
     "known repositories")))

(defun magh-embark-toggle-favorite-organization (target)
  "Toggle TARGET owner in `magh-favorite-organizations'."
  (let ((context (magh-embark--repository-context target)))
    (magh-embark--toggle-list-value
     'magh-favorite-organizations (magh-context-owner context)
     "favorite organizations")))

(defun magh-embark-toggle-workflow-template-repository (target)
  "Toggle TARGET in `magh-workflow-template-repositories'."
  (let ((context (magh-embark--repository-context target)))
    (magh-embark--toggle-list-value
     'magh-workflow-template-repositories (magh-context-repository context)
     "workflow template repositories")))

(defun magh-embark--remote-content (target consumer)
  "Fetch remote file TARGET asynchronously and call CONSUMER with its text."
  (let* ((resource (magh-embark--resource target))
         (context (plist-get resource :context))
         (path (plist-get resource :path))
         (ref (plist-get resource :ref)))
    (unless (and (eq (plist-get resource :kind) 'file) context path)
      (user-error "This action requires a remote file"))
    (magh-api--content-get
     context path ref
     (lambda (data) (funcall consumer (magh-api--decode-content data)))
     #'magh-core--user-error)))

(defun magh-embark-insert-remote-file (target)
  "Fetch TARGET and insert its contents at the original point."
  (let ((marker (magh-embark--target-marker)))
    (magh-embark--remote-content
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

(defun magh-embark-copy-remote-file (target)
  "Fetch TARGET and copy its contents asynchronously."
  (magh-embark--remote-content
   target (lambda (content)
            (kill-new content)
            (message "Copied remote file contents"))))

(defun magh-embark-edit-topic (target)
  "Edit Issue or Pull Request TARGET."
  (let* ((resource (magh-embark--resource target))
         (context (plist-get resource :context))
         (number (plist-get resource :number)))
    (pcase (plist-get resource :kind)
      ('issue (magh-issue-edit context number))
      ('pr (magh-pr-edit context number))
      (_ (user-error "Target is not an Issue or Pull Request")))))

(defun magh-embark-close-topic (target)
  "Close Issue or Pull Request TARGET."
  (let* ((resource (magh-embark--resource target))
         (context (plist-get resource :context))
         (number (plist-get resource :number)))
    (pcase (plist-get resource :kind)
      ('issue (magh-issue-close context number))
      ('pr (magh-pr-close context number))
      (_ (user-error "Target is not an Issue or Pull Request")))))

(defun magh-embark-reopen-topic (target)
  "Reopen Issue or Pull Request TARGET."
  (let* ((resource (magh-embark--resource target))
         (context (plist-get resource :context))
         (number (plist-get resource :number)))
    (pcase (plist-get resource :kind)
      ('issue (magh-issue-reopen context number))
      ('pr (magh-pr-reopen context number))
      (_ (user-error "Target is not an Issue or Pull Request")))))

(defun magh-embark-rerun (target)
  "Rerun Actions Run TARGET."
  (let ((resource (magh-embark--resource target)))
    (magh-run-rerun nil (plist-get resource :context) (plist-get resource :id))))

(defun magh-embark-cancel-run (target)
  "Cancel Actions Run TARGET."
  (let ((resource (magh-embark--resource target)))
    (magh-run-cancel (plist-get resource :context) (plist-get resource :id))))

(defun magh-embark-notification-read (target)
  "Mark Notification TARGET read."
  (magh-notification-mark-read (magh-embark--resource target)))

(defun magh-embark-notification-unread (target)
  "Use the documented web fallback to mark Notification TARGET unread."
  (magh-notification-mark-unread (magh-embark--resource target)))

(defun magh-embark-notification-subscribe (target)
  "Subscribe to Notification TARGET."
  (magh-notification-subscribe (magh-embark--resource target)))

(defun magh-embark-notification-unsubscribe (target)
  "Unsubscribe from Notification TARGET."
  (magh-notification-unsubscribe (magh-embark--resource target)))

(defvar-keymap magh-embark-resource-map
  :doc "Common actions for structured magh.el resources."
  "RET" #'magh-embark-open
  "o" #'magh-embark-browse
  "w" #'magh-embark-copy-url
  "y" #'magh-embark-copy-title
  "l" #'magh-embark-copy-org-link
  "i" #'magh-embark-insert-title
  "u" #'magh-embark-insert-url)

(defvar-keymap magh-embark-repository-map
  :doc "Embark actions for magh.el repository resources."
  :parent magh-embark-resource-map
  "c" #'magh-embark-clone
  "f" #'magh-embark-fork
  "h" #'magh-embark-copy-https-url
  "s" #'magh-embark-copy-ssh-url
  "p" #'magh-embark-copy-straight-snippet
  "k" #'magh-embark-toggle-known-repository
  "O" #'magh-embark-toggle-favorite-organization
  "T" #'magh-embark-toggle-workflow-template-repository)

(defvar-keymap magh-embark-file-map
  :doc "Embark actions for magh.el remote file resources."
  :parent magh-embark-resource-map
  "I" #'magh-embark-insert-remote-file
  "Y" #'magh-embark-copy-remote-file)

(defvar-keymap magh-embark-topic-map
  :doc "Embark actions for magh.el Issue and Pull Request resources."
  :parent magh-embark-resource-map
  "e" #'magh-embark-edit-topic
  "x" #'magh-embark-close-topic
  "r" #'magh-embark-reopen-topic)

(defvar-keymap magh-embark-run-map
  :doc "Embark actions for magh.el Actions Run resources."
  :parent magh-embark-resource-map
  "r" #'magh-embark-rerun
  "x" #'magh-embark-cancel-run)

(defvar-keymap magh-embark-notification-map
  :doc "Embark actions for magh.el Notification resources."
  :parent magh-embark-resource-map
  "r" #'magh-embark-notification-read
  "u" #'magh-embark-notification-unread
  "s" #'magh-embark-notification-subscribe
  "U" #'magh-embark-notification-unsubscribe)

(defun magh-embark--install ()
  "Install all magh.el category maps into `embark-keymap-alist'."
  (setq magh-embark--saved-keymaps nil)
  (dolist (entry magh-embark--category-maps)
    (let ((category (car entry)))
      (push (cons category (alist-get category embark-keymap-alist))
            magh-embark--saved-keymaps)
      (setf (alist-get category embark-keymap-alist) (cdr entry)))))

(defun magh-embark--uninstall ()
  "Restore Embark category maps replaced by this integration."
  (dolist (saved magh-embark--saved-keymaps)
    (let ((category (car saved))
          (keymap (cdr saved)))
      (if keymap
          (setf (alist-get category embark-keymap-alist) keymap)
        (setq embark-keymap-alist
              (assq-delete-all category embark-keymap-alist)))))
  (setq magh-embark--saved-keymaps nil))

;;;###autoload
(define-minor-mode magh-embark-mode
  "Register structured magh.el candidate categories with Embark."
  :global t
  :group 'magh-embark
  (if magh-embark-mode
      (if (require 'embark nil t)
          (magh-embark--install)
        (setq magh-embark-mode nil)
        (user-error "Embark is not installed"))
    (magh-embark--uninstall)))

(provide 'magh-embark)
;;; magh-embark.el ends here
