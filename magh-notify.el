;;; magh-notify.el --- GitHub notifications for magh.el -*- lexical-binding: t; -*-

;; Copyright (C) 2026
;; SPDX-License-Identifier: GPL-2.0-only

;; Author: magh.el contributors
;; Keywords: tools, vc, github

;;; Commentary:

;; Structured notification candidates, native preview and navigation, read and
;; subscription mutations, configurable grouping, and documented web fallback
;; for the unsupported single-thread unread operation.

;;; Code:

(require 'transient)
(require 'url-util)
(require 'magh-api)
(require 'magh-candidate)
(require 'magh-ui)

(defun magh-notify--format (resource)
  "Format Notification RESOURCE."
  (magh-ui--row
   (magh-ui--styled (if (plist-get resource :unread) "unread" "read")
                  (if (plist-get resource :unread)
                      'magh-pending-state 'magh-draft-state))
   (magh-ui--styled (plist-get resource :reason) 'warning)
   (magh-ui--styled (plist-get resource :subject-type) 'magh-permission)
   (magh-ui--styled (plist-get resource :repository) 'magh-repository)
   (magh-ui--styled (plist-get resource :title) 'magh-resource-title)))

(defun magh-notify--group (candidate transform)
  "Group notification CANDIDATE according to user configuration."
  (if transform
      candidate
    (let ((resource (get-text-property 0 'magh-resource candidate)))
      (pcase magh-notifications-group-by
        ('repository (plist-get resource :repository))
        ('reason (plist-get resource :reason))
        ('type (plist-get resource :subject-type))
        ('state (if (plist-get resource :unread) "Unread" "Read"))
        ('date (substring (plist-get resource :updated) 0 10))
        (_ nil)))))

;;;###autoload
(defun magh-notifications (&optional unread-only)
  "Select a GitHub notification.
UNREAD-ONLY defaults to `magh-notifications-unread-only'."
  (interactive)
  (let ((context (magh-context-resolve))
        (unread-only (if (null unread-only)
                         magh-notifications-unread-only unread-only)))
    (message "Fetching GitHub notifications…")
    (magh-api--notification-list
     context unread-only
     (lambda (items)
       (let* ((resources (mapcar (lambda (item)
                                   (magh-candidate--notification-resource
                                    context item))
                                 items))
              (resource
               (magh-candidate-read
                (if unread-only "Unread notification: " "Notification: ")
                resources :formatter #'magh-notify--format
                :category 'magh-notification :preview t
                :group #'magh-notify--group :sort nil)))
         (magh-resource-open resource)))
     #'magh-core--user-error)))

(defun magh-notify--coerce (resource)
  "Return notification RESOURCE or one at point."
  (or resource (magh-candidate-at-point)
      (user-error "No notification selected")))

(defun magh-notification-mark-read (&optional resource callback)
  "Mark notification RESOURCE read, then call CALLBACK."
  (interactive)
  (setq resource (magh-notify--coerce resource))
  (magh-api--notification-read
   (plist-get resource :context) (plist-get resource :id)
   (lambda (_)
     (plist-put resource :unread nil)
     (if callback (funcall callback resource)
       (message "Notification marked read")))
   #'magh-core--user-error))

;;;###autoload
(defun magh-notification-mark-unread (&optional resource)
  "Open GitHub inbox fallback for marking RESOURCE unread."
  (interactive)
  (setq resource (magh-notify--coerce resource))
  (let* ((context (plist-get resource :context))
         (repo (plist-get resource :repository))
         (query (url-hexify-string (format "repo:%s is:read" repo)))
         (url (format "https://%s/notifications?query=%s"
                      (or (magh-context-host context) "github.com") query)))
    (browse-url url)))

(defun magh-notification-subscribe (&optional resource)
  "Subscribe to notification thread RESOURCE."
  (interactive)
  (setq resource (magh-notify--coerce resource))
  (magh-api--notification-subscription
   (plist-get resource :context) (plist-get resource :id) t
   (lambda (_) (message "Subscribed to notification thread"))
   #'magh-core--user-error))

(defun magh-notification-unsubscribe (&optional resource)
  "Unsubscribe from notification thread RESOURCE."
  (interactive)
  (setq resource (magh-notify--coerce resource))
  (magh-api--notification-subscription
   (plist-get resource :context) (plist-get resource :id) nil
   (lambda (_) (message "Unsubscribed from notification thread"))
   #'magh-core--user-error))

(defun magh-notify--open (resource)
  "Open notification RESOURCE and mark it read when necessary."
  (let ((subject (plist-get resource :subject-resource)))
    (if (plist-get resource :unread)
        (magh-notification-mark-read
         resource (lambda (_) (magh-resource-open subject)))
      (magh-resource-open subject))))

(defun magh-notifications-toggle-unread ()
  "Toggle unread-only notification filtering and reopen selection."
  (interactive)
  (setq magh-notifications-unread-only (not magh-notifications-unread-only))
  (magh-notifications magh-notifications-unread-only))

(defun magh-notifications-cycle-group ()
  "Cycle notification grouping and reopen selection."
  (interactive)
  (setq magh-notifications-group-by
        (pcase magh-notifications-group-by
          ('repository 'reason) ('reason 'type) ('type 'state)
          ('state 'date) ('date nil) (_ 'repository)))
  (magh-notifications magh-notifications-unread-only))

;;;###autoload
(transient-define-prefix magh-notifications-dispatch ()
  "Notification selection and settings."
  [["Open"
    ("RET" "Select notifications" magh-notifications)]
   ["Filter"
    ("u" "Toggle unread/all" magh-notifications-toggle-unread)
    ("g" "Cycle grouping" magh-notifications-cycle-group)]])

(magh-candidate-register
 'notification
 :open #'magh-notify--open
 :preview (lambda (resource)
            (magh-resource-preview (plist-get resource :subject-resource)))
 :dispatch (lambda (_resource)
             (call-interactively #'magh-notifications-dispatch)))

(magh-candidate-register
 'notification-list :open (lambda (_resource) (magh-notifications)))

(provide 'magh-notify)
;;; magh-notify.el ends here
