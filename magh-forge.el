;;; magh-forge.el --- Optional Forge topic bridge for magh.el -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; Author: magh.el contributors
;; Keywords: tools, vc, github

;;; Commentary:

;; This bridge is inert unless explicitly enabled and Forge is installed.
;; Issue and Pull Request candidates can then open in Forge.  Repositories that
;; were not already known to Forge are recorded separately so cleanup never
;; removes repositories belonging to the user's own Forge setup.

;;; Code:

(require 'cl-lib)
(require 'eieio)
(require 'subr-x)
(require 'magh-candidate)
(require 'magh-core)
(require 'magh-issue)
(require 'magh-pr)

(declare-function forge--pull-topic "forge-commands" (repo topic))
(declare-function forge-current-topic "forge-topic" (&optional demand))
(declare-function forge-get-url "forge-commands" (object))
(declare-function forge-get-issue "forge-issue" (repo number))
(declare-function forge-get-pullreq "forge-pullreq" (repo number))
(declare-function forge-get-repository "forge-repo" (&rest arguments))
(declare-function forge-remove-repository "forge-commands" (repository))
(declare-function forge-visit-topic "forge-commands" (topic))
(declare-function magh-pr-review-mode "magh-pr-review" (&optional arg))
(defvar magh-pr-review-mode nil)

(defgroup magh-forge nil
  "Optional Forge interoperability for magh.el."
  :group 'magh)

(defcustom magh-forge-topic-wait-seconds 15
  "Maximum time to wait for Forge to import a selected topic."
  :type 'number)

(defvar magh-forge--added-repositories nil
  "Repository web URLs added to Forge by magh.el itself.")

(defvar magh-forge--saved-actions nil
  "Original `magh-resource-actions' entries restored when the mode stops.")

(defvar magh-forge-mode nil)

(defun magh-forge--ensure-repository (context)
  "Return a Forge repository for CONTEXT, inserting a selective entry if needed."
  (let* ((url (magh-context-web-url context))
         (known (forge-get-repository url nil :known?))
         (repo (or known (forge-get-repository url nil :insert!))))
    (unless repo
      (user-error "Forge does not recognize %s; customize `forge-alist'" url))
    (unless known
      (cl-pushnew url magh-forge--added-repositories :test #'equal)
      ;; This entry exists to import individually selected topics, not to
      ;; maintain a full duplicate of magh.el's API cache.
      (eieio-oset repo 'selective-p t))
    repo))

(defun magh-forge--topic (repo kind number)
  "Return Forge topic NUMBER of KIND in REPO, if already imported."
  (pcase-exhaustive kind
    ('issue (forge-get-issue repo number))
    ('pr (forge-get-pullreq repo number))))

(defun magh-forge--open-native (resource)
  "Open RESOURCE in magh.el without consulting action overrides."
  (pcase-exhaustive (plist-get resource :kind)
    ('issue (magh-issue-view (plist-get resource :number)
                           (plist-get resource :context)))
    ('pr (magh-pr-view (plist-get resource :number)
                     (plist-get resource :context)))))

(defun magh-forge--wait-for-topic (repo kind number resource deadline)
  "Open imported Forge topic, polling REPO until DEADLINE.
KIND, NUMBER, and RESOURCE identify the selected magh.el candidate."
  (condition-case error
      (if-let* ((topic (magh-forge--topic repo kind number)))
          (forge-visit-topic topic)
        (if (< (float-time) deadline)
            (run-at-time 0.15 nil #'magh-forge--wait-for-topic
                         repo kind number resource deadline)
          (message "Forge did not import %s #%s in time; opening magh.el"
                   kind number)
          (magh-forge--open-native resource)))
    (error
     (message "Forge topic import failed: %s; opening magh.el"
              (error-message-string error))
     (magh-forge--open-native resource))))

(defun magh-forge-open-resource (resource)
  "Import ISSUE/PR RESOURCE into Forge when needed, then open it there."
  (unless magh-forge-mode
    (user-error "Enable `magh-forge-mode' first"))
  (let* ((kind (plist-get resource :kind))
         (number (plist-get resource :number))
         (context (plist-get resource :context)))
    (unless (and (memq kind '(issue pr)) number context)
      (user-error "Forge bridge supports Issue and Pull Request resources"))
    (let* ((repo (magh-forge--ensure-repository context))
           (topic (magh-forge--topic repo kind number)))
      (if topic
          (forge-visit-topic topic)
        (message "Importing %s #%s into Forge…" kind number)
        ;; Forge's single-topic request is asynchronous.  Its public command
        ;; assumes a current Magit repository, so this bridge calls the same
        ;; generic operation with the structured repository object.
        (forge--pull-topic repo number)
        (magh-forge--wait-for-topic
         repo kind number resource
         (+ (float-time) magh-forge-topic-wait-seconds))))))

;;;###autoload
(defun magh-forge-open-current-topic-in-magh ()
  "Open the current Forge Issue or Pull Request in magh.el."
  (interactive)
  (unless (require 'forge nil t)
    (user-error "Forge is not installed"))
  (let* ((topic (forge-current-topic t))
         (resource (magh-resource-from-url (forge-get-url topic))))
    (pcase (plist-get resource :kind)
      ('issue (magh-issue-view (plist-get resource :number)
                             (plist-get resource :context)))
      ('pr (magh-pr-view (plist-get resource :number)
                       (plist-get resource :context)))
      (_ (user-error "Current Forge topic is not a GitHub Issue or Pull Request")))))

;;;###autoload
(defun magh-forge-remove-repository (&optional repository-url)
  "Remove REPOSITORY-URL only if magh.el added it to Forge."
  (interactive
   (list
    (if magh-forge--added-repositories
        (completing-read "Remove magh.el-added Forge repository: "
                         magh-forge--added-repositories nil t)
      (user-error "magh.el has not added any Forge repositories"))))
  (unless (member repository-url magh-forge--added-repositories)
    (user-error "Refusing to remove a Forge repository not added by magh.el"))
  (when-let* ((repo (forge-get-repository repository-url nil :known?)))
    (forge-remove-repository repo))
  (setq magh-forge--added-repositories
        (delete repository-url magh-forge--added-repositories))
  (message "Removed magh.el-added Forge repository %s" repository-url))

;;;###autoload
(defun magh-forge-remove-added-repositories ()
  "Remove every Forge repository entry that magh.el itself added."
  (interactive)
  (unless magh-forge--added-repositories
    (user-error "magh.el has not added any Forge repositories"))
  (when (or (not (called-interactively-p 'interactive))
            (yes-or-no-p
             (format "Remove %d magh.el-added Forge repositories? "
                     (length magh-forge--added-repositories))))
    (dolist (url (copy-sequence magh-forge--added-repositories))
      (magh-forge-remove-repository url))))

(defun magh-forge--save-and-set-action (kind)
  "Save KIND's current open override and replace it with the Forge bridge."
  (push (cons kind (alist-get kind magh-resource-actions))
        magh-forge--saved-actions)
  (setf (alist-get kind magh-resource-actions) #'magh-forge-open-resource))

(defun magh-forge--restore-actions ()
  "Restore resource overrides saved when `magh-forge-mode' was enabled."
  (dolist (saved magh-forge--saved-actions)
    (let ((kind (car saved))
          (action (cdr saved)))
      (if action
          (setf (alist-get kind magh-resource-actions) action)
        (setq magh-resource-actions (assq-delete-all kind magh-resource-actions)))))
  (setq magh-forge--saved-actions nil))

;;;###autoload
(define-minor-mode magh-forge-mode
  "Use Forge topic buffers as the default Issue and Pull Request viewer."
  :global t
  :group 'magh-forge
  (if magh-forge-mode
      (condition-case error
          (progn
            (unless (require 'forge nil t)
              (error "Forge is not installed"))
            (require 'forge-commands)
            (require 'forge-issue)
            (require 'forge-pullreq)
            (when magh-pr-review-mode
              (magh-pr-review-mode -1))
            (setq magh-forge--saved-actions nil)
            (magh-forge--save-and-set-action 'issue)
            (magh-forge--save-and-set-action 'pr))
        (error
         (setq magh-forge-mode nil)
         (magh-forge--restore-actions)
         (user-error "%s" (error-message-string error))))
    (magh-forge--restore-actions)))

(provide 'magh-forge)
;;; magh-forge.el ends here
