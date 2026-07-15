;;; gh-forge.el --- Optional Forge topic bridge for gh.el -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; Author: gh.el contributors
;; Keywords: tools, vc, github
;; Package-Requires: ((emacs "29.1"))

;;; Commentary:

;; This bridge is inert unless explicitly enabled and Forge is installed.
;; Issue and Pull Request candidates can then open in Forge.  Repositories that
;; were not already known to Forge are recorded separately so cleanup never
;; removes repositories belonging to the user's own Forge setup.

;;; Code:

(require 'cl-lib)
(require 'eieio)
(require 'subr-x)
(require 'gh-candidate)
(require 'gh-core)
(require 'gh-issue)
(require 'gh-pr)

(declare-function forge--pull-topic "forge-commands" (repo topic))
(declare-function forge-current-topic "forge-topic" (&optional demand))
(declare-function forge-get-issue "forge-issue" (repo number))
(declare-function forge-get-pullreq "forge-pullreq" (repo number))
(declare-function forge-get-repository "forge-repo" (&rest arguments))
(declare-function forge-issue-p "forge-issue" (object))
(declare-function forge-pullreq-p "forge-pullreq" (object))
(declare-function forge-remove-repository "forge-commands" (repository))
(declare-function forge-visit-topic "forge-commands" (topic))
(declare-function gh-pr-review-mode "gh-pr-review" (&optional arg))
(defvar gh-pr-review-mode)

(defgroup gh-forge nil
  "Optional Forge interoperability for gh.el."
  :group 'gh)

(defcustom gh-forge-topic-wait-seconds 15
  "Maximum time to wait for Forge to import a selected topic."
  :type 'number)

(defvar gh-forge--added-repositories nil
  "Repository web URLs added to Forge by gh.el itself.")

(defvar gh-forge--saved-actions nil
  "Original `gh-resource-actions' entries restored when the mode stops.")

(defvar gh-forge-mode nil)

(defun gh-forge--oref (object slot)
  "Read SLOT from optional Forge OBJECT without a compile-time class dependency."
  (eieio-oref object slot))

(defun gh-forge--oset (object slot value)
  "Set SLOT of optional Forge OBJECT to VALUE."
  (eieio-oset object slot value))

(defun gh-forge--repository-url (context)
  "Return repository web URL for CONTEXT."
  (gh-context-web-url context))

(defun gh-forge--ensure-repository (context)
  "Return a Forge repository for CONTEXT, inserting a selective entry if needed."
  (let* ((url (gh-forge--repository-url context))
         (known (forge-get-repository url nil :known?))
         (repo (or known (forge-get-repository url nil :insert!))))
    (unless repo
      (user-error "Forge does not recognize %s; customize `forge-alist'" url))
    (unless known
      (cl-pushnew url gh-forge--added-repositories :test #'equal)
      ;; This entry exists to import individually selected topics, not to
      ;; maintain a full duplicate of gh.el's API cache.
      (when (slot-exists-p repo 'selective-p)
        (gh-forge--oset repo 'selective-p t)))
    repo))

(defun gh-forge--topic (repo kind number)
  "Return Forge topic NUMBER of KIND in REPO, if already imported."
  (pcase kind
    ('issue (forge-get-issue repo number))
    ('pr (forge-get-pullreq repo number))))

(defun gh-forge--open-native (resource)
  "Open RESOURCE in gh.el without consulting action overrides."
  (pcase (plist-get resource :kind)
    ('issue (gh-issue-view (plist-get resource :number)
                           (plist-get resource :context)))
    ('pr (gh-pr-view (plist-get resource :number)
                     (plist-get resource :context)))))

(defun gh-forge--wait-for-topic (repo kind number resource deadline)
  "Open imported Forge topic, polling REPO until DEADLINE.
KIND, NUMBER, and RESOURCE identify the selected gh.el candidate."
  (condition-case error
      (if-let* ((topic (gh-forge--topic repo kind number)))
          (forge-visit-topic topic)
        (if (< (float-time) deadline)
            (run-at-time 0.15 nil #'gh-forge--wait-for-topic
                         repo kind number resource deadline)
          (message "Forge did not import %s #%s in time; opening gh.el"
                   kind number)
          (gh-forge--open-native resource)))
    (error
     (message "Forge topic import failed: %s; opening gh.el"
              (error-message-string error))
     (gh-forge--open-native resource))))

(defun gh-forge-open-resource (resource)
  "Import ISSUE/PR RESOURCE into Forge when needed, then open it there."
  (unless gh-forge-mode
    (user-error "Enable `gh-forge-mode' first"))
  (let* ((kind (plist-get resource :kind))
         (number (plist-get resource :number))
         (context (plist-get resource :context)))
    (unless (and (memq kind '(issue pr)) number context)
      (user-error "Forge bridge supports Issue and Pull Request resources"))
    (let* ((repo (gh-forge--ensure-repository context))
           (topic (gh-forge--topic repo kind number)))
      (if topic
          (forge-visit-topic topic)
        (message "Importing %s #%s into Forge…" kind number)
        ;; Forge's single-topic request is asynchronous.  Its public command
        ;; assumes a current Magit repository, so this bridge calls the same
        ;; generic operation with the structured repository object.
        (forge--pull-topic repo number)
        (gh-forge--wait-for-topic
         repo kind number resource
         (+ (float-time) gh-forge-topic-wait-seconds))))))

;;;###autoload
(defun gh-forge-open-current-topic-in-gh ()
  "Open the current Forge Issue or Pull Request in gh.el."
  (interactive)
  (unless (require 'forge nil t)
    (user-error "Forge is not installed"))
  (let* ((topic (forge-current-topic t))
         (repo (forge-get-repository topic))
           (context
          (gh-context-normalize
           (gh-context-create :host (gh-forge--oref repo 'githost)
                              :owner (gh-forge--oref repo 'owner)
                              :name (gh-forge--oref repo 'name))))
         (number (gh-forge--oref topic 'number)))
    (cond
     ((forge-issue-p topic) (gh-issue-view number context))
     ((forge-pullreq-p topic) (gh-pr-view number context))
     (t (user-error "Current Forge topic is not an Issue or Pull Request")))))

;;;###autoload
(defun gh-forge-remove-repository (&optional repository-url)
  "Remove REPOSITORY-URL only if gh.el added it to Forge."
  (interactive
   (list
    (if gh-forge--added-repositories
        (completing-read "Remove gh.el-added Forge repository: "
                         gh-forge--added-repositories nil t)
      (user-error "gh.el has not added any Forge repositories"))))
  (unless (member repository-url gh-forge--added-repositories)
    (user-error "Refusing to remove a Forge repository not added by gh.el"))
  (when-let* ((repo (forge-get-repository repository-url nil :known?)))
    (forge-remove-repository repo))
  (setq gh-forge--added-repositories
        (delete repository-url gh-forge--added-repositories))
  (message "Removed gh.el-added Forge repository %s" repository-url))

;;;###autoload
(defun gh-forge-remove-added-repositories ()
  "Remove every Forge repository entry that gh.el itself added."
  (interactive)
  (unless gh-forge--added-repositories
    (user-error "gh.el has not added any Forge repositories"))
  (when (or (not (called-interactively-p 'interactive))
            (yes-or-no-p
             (format "Remove %d gh.el-added Forge repositories? "
                     (length gh-forge--added-repositories))))
    (dolist (url (copy-sequence gh-forge--added-repositories))
      (gh-forge-remove-repository url))))

(defun gh-forge--save-and-set-action (kind)
  "Save KIND's current open override and replace it with the Forge bridge."
  (let ((cell (assq kind gh-resource-actions)))
    (push (list kind (and cell t) (cdr cell)) gh-forge--saved-actions)
    (setf (alist-get kind gh-resource-actions) #'gh-forge-open-resource)))

(defun gh-forge--restore-actions ()
  "Restore resource overrides saved when `gh-forge-mode' was enabled."
  (dolist (saved gh-forge--saved-actions)
    (pcase-let ((`(,kind ,present ,value) saved))
      (if present
          (setf (alist-get kind gh-resource-actions) value)
        (setq gh-resource-actions (assq-delete-all kind gh-resource-actions)))))
  (setq gh-forge--saved-actions nil))

;;;###autoload
(define-minor-mode gh-forge-mode
  "Use Forge topic buffers as the default Issue and Pull Request viewer."
  :global t
  :group 'gh-forge
  (if gh-forge-mode
      (condition-case error
          (progn
            (unless (require 'forge nil t)
              (error "Forge is not installed"))
            (require 'forge-commands)
            (require 'forge-issue)
            (require 'forge-pullreq)
            (when (and (boundp 'gh-pr-review-mode) gh-pr-review-mode
                       (fboundp 'gh-pr-review-mode))
              (gh-pr-review-mode -1))
            (setq gh-forge--saved-actions nil)
            (gh-forge--save-and-set-action 'issue)
            (gh-forge--save-and-set-action 'pr))
        (error
         (setq gh-forge-mode nil)
         (gh-forge--restore-actions)
         (user-error "%s" (error-message-string error))))
    (gh-forge--restore-actions)))

(provide 'gh-forge)
;;; gh-forge.el ends here
