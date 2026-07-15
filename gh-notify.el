;;; gh-notify.el --- GitHub notifications for gh.el -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; Author: gh.el contributors
;; Keywords: tools, vc, github
;; Package-Requires: ((emacs "31.1") (consult "2.0") (transient "0.7.0"))

;;; Commentary:

;; Structured notification candidates, native preview and navigation, read and
;; subscription mutations, configurable grouping, and documented web fallback
;; for the unsupported single-thread unread operation.

;;; Code:

(require 'transient)
(require 'url-util)
(require 'gh-api)
(require 'gh-candidate)
(require 'gh-ui)

(defun gh-notify--format (resource)
  "Format Notification RESOURCE."
  (gh-ui--row
   (gh-ui--styled (if (plist-get resource :unread) "unread" "read")
                  (if (plist-get resource :unread)
                      'gh-pending-state 'gh-draft-state))
   (gh-ui--styled (plist-get resource :reason) 'warning)
   (gh-ui--styled (plist-get resource :subject-type) 'gh-permission)
   (gh-ui--styled (plist-get resource :repository) 'gh-repository)
   (gh-ui--styled (plist-get resource :title) 'gh-resource-title)))

(defun gh-notify--group (candidate transform)
  "Group notification CANDIDATE according to user configuration."
  (if transform
      candidate
    (let ((resource (get-text-property 0 'gh-resource candidate)))
      (pcase gh-notifications-group-by
        ('repository (plist-get resource :repository))
        ('reason (plist-get resource :reason))
        ('type (plist-get resource :subject-type))
        ('state (if (plist-get resource :unread) "Unread" "Read"))
        ('date (substring (plist-get resource :updated) 0 10))
        (_ nil)))))

;;;###autoload
(defun gh-notifications (&optional unread-only)
  "Select a GitHub notification.
UNREAD-ONLY defaults to `gh-notifications-unread-only'."
  (interactive)
  (let ((context (gh-context-resolve))
        (unread-only (if (null unread-only)
                         gh-notifications-unread-only unread-only)))
    (message "Fetching GitHub notifications…")
    (gh-api--notification-list
     context unread-only
     (lambda (items)
       (let* ((resources (mapcar (lambda (item)
                                   (gh-candidate--notification-resource
                                    context item))
                                 items))
              (resource
               (gh-candidate-read
                (if unread-only "Unread notification: " "Notification: ")
                resources :formatter #'gh-notify--format
                :category 'gh-notification :preview t
                :group #'gh-notify--group :sort nil)))
         (gh-resource-open resource)))
     #'gh-core--user-error)))

(defun gh-notify--coerce (resource)
  "Return notification RESOURCE or one at point."
  (or resource (gh-candidate-at-point)
      (user-error "No notification selected")))

(defun gh-notification-mark-read (&optional resource callback)
  "Mark notification RESOURCE read, then call CALLBACK."
  (interactive)
  (setq resource (gh-notify--coerce resource))
  (gh-api--notification-read
   (plist-get resource :context) (plist-get resource :id)
   (lambda (_)
     (plist-put resource :unread nil)
     (if callback (funcall callback resource)
       (message "Notification marked read")))
   #'gh-core--user-error))

;;;###autoload
(defun gh-notification-mark-unread (&optional resource)
  "Open GitHub inbox fallback for marking RESOURCE unread."
  (interactive)
  (setq resource (gh-notify--coerce resource))
  (let* ((context (plist-get resource :context))
         (repo (plist-get resource :repository))
         (query (url-hexify-string (format "repo:%s is:read" repo)))
         (url (format "https://%s/notifications?query=%s"
                      (or (gh-context-host context) "github.com") query)))
    (browse-url url)))

(defun gh-notification-subscribe (&optional resource)
  "Subscribe to notification thread RESOURCE."
  (interactive)
  (setq resource (gh-notify--coerce resource))
  (gh-api--notification-subscription
   (plist-get resource :context) (plist-get resource :id) t
   (lambda (_) (message "Subscribed to notification thread"))
   #'gh-core--user-error))

(defun gh-notification-unsubscribe (&optional resource)
  "Unsubscribe from notification thread RESOURCE."
  (interactive)
  (setq resource (gh-notify--coerce resource))
  (gh-api--notification-subscription
   (plist-get resource :context) (plist-get resource :id) nil
   (lambda (_) (message "Unsubscribed from notification thread"))
   #'gh-core--user-error))

(defun gh-notify--open (resource)
  "Open notification RESOURCE and mark it read when necessary."
  (let ((subject (plist-get resource :subject-resource)))
    (if (plist-get resource :unread)
        (gh-notification-mark-read
         resource (lambda (_) (gh-resource-open subject)))
      (gh-resource-open subject))))

(defun gh-notify--preview (resource)
  "Preview notification RESOURCE without changing read state."
  (gh-resource-preview (plist-get resource :subject-resource)))

(defun gh-notifications-toggle-unread ()
  "Toggle unread-only notification filtering and reopen selection."
  (interactive)
  (setq gh-notifications-unread-only (not gh-notifications-unread-only))
  (gh-notifications gh-notifications-unread-only))

(defun gh-notifications-cycle-group ()
  "Cycle notification grouping and reopen selection."
  (interactive)
  (setq gh-notifications-group-by
        (pcase gh-notifications-group-by
          ('repository 'reason) ('reason 'type) ('type 'state)
          ('state 'date) ('date nil) (_ 'repository)))
  (gh-notifications gh-notifications-unread-only))

(transient-define-prefix gh-notifications-dispatch ()
  "Notification selection and settings."
  [["Open"
    ("RET" "Select notifications" gh-notifications)]
   ["Filter"
    ("u" "Toggle unread/all" gh-notifications-toggle-unread)
    ("g" "Cycle grouping" gh-notifications-cycle-group)]])

(gh-candidate-register
 'notification
 :open #'gh-notify--open
 :preview #'gh-notify--preview
 :dispatch (lambda (_resource)
             (call-interactively #'gh-notifications-dispatch)))

(gh-candidate-register
 'notification-list :open (lambda (_resource) (gh-notifications)))

(provide 'gh-notify)
;;; gh-notify.el ends here
