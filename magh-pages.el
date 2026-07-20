;;; magh-pages.el --- User Status and profile pages -*- lexical-binding: t; -*-

;; Copyright (C) 2026
;; SPDX-License-Identifier: GPL-2.0-only

;; Author: magh.el contributors
;; Keywords: tools, vc, github

;;; Commentary:

;; Account-level status aggregation and native user profile pages.

;;; Code:

(require 'subr-x)
(require 'magh-api)
(require 'magh-candidate)
(require 'magh-topic)
(require 'magh-ui)

(declare-function magh-notifications "magh-notify")
(declare-function magh-review-requests "magh-pr")
(declare-function magh-search-dispatch "magh-search")

(defun magh-pages--insert-topic (base kind data)
  "Insert account topic KIND from DATA."
  (let* ((repository (alist-get 'repository data))
         (name (alist-get 'nameWithOwner repository))
         (context (magh-context-from-repository name (magh-context-host base)))
         (resource (magh-topic--resource kind context data))
         (values (magh-topic--row-values kind data)))
    (magh-ui--section (topic (list kind (plist-get resource :repository)
                                 (plist-get resource :number)) resource t)
      (magh-ui--format-row values '(:state :identifier :title))
      (magh-topic--insert-metadata kind data))))

;;; User status

(defun magh-pages--fetch-user-status (context success _error force)
  "Fetch current account status aggregates."
  (magh-core--collect-async-settled
   (list
    (cons 'user (lambda (ok fail)
                  (magh-api--user-get context nil ok fail force)))
    (cons 'notifications (lambda (ok fail)
                           (magh-api--notification-list context t ok fail force)))
    (cons 'review-requests (lambda (ok fail)
                             (magh-api--review-requests context ok fail force)))
    (cons 'assigned-issues (lambda (ok fail)
                             (magh-api--assigned-issues context ok fail force)))
    (cons 'assigned-prs (lambda (ok fail)
                          (magh-api--assigned-prs context ok fail force)))
    (cons 'my-prs (lambda (ok fail)
                    (magh-api--my-prs context ok fail force)))
    (cons 'repositories (lambda (ok fail)
                          (magh-api--user-repositories context nil ok fail force 10))))
   success))

(defun magh-pages--insert-notification (base data)
  "Insert notification summary DATA without marking it read."
  (let* ((notification (magh-candidate--notification-resource base data))
         (resource (plist-get notification :subject-resource)))
    (magh-ui--section (notification (plist-get notification :id)
                                   resource t)
      (magh-ui--row
       (magh-ui--styled (if (plist-get notification :unread) "unread" "read")
                      (if (plist-get notification :unread)
                          'magh-pending-state 'magh-draft-state))
       (magh-ui--styled (plist-get notification :subject-type) 'magh-permission)
       (magh-ui--styled (plist-get notification :repository) 'magh-repository)
       (magh-ui--styled (plist-get notification :title) 'magh-resource-title))
      (magh-ui--insert-header "Repository"
                            (plist-get notification :repository)
                            'magh-repository)
      (magh-ui--insert-header "Updated"
                            (magh-core--date (plist-get notification :updated))
                            'magh-date))))

(defun magh-pages--repo-resource (base data)
  "Create repository resource from DATA."
  (let* ((name (alist-get 'nameWithOwner data))
         (context (magh-context-from-repository name (magh-context-host base))))
    (magh-resource-create 'repository context :name name :title name
                        :url (alist-get 'url data) :data data)))

(defun magh-pages--render-user-status (context result)
  "Render current account status RESULT."
  (let* ((user (magh-batch-value result 'user))
         (notifications (magh-batch-value result 'notifications))
         (review-requests (magh-batch-value result 'review-requests))
         (assigned-issues (magh-batch-value result 'assigned-issues))
         (assigned-prs (magh-batch-value result 'assigned-prs))
         (my-prs (magh-batch-value result 'my-prs))
         (repositories (magh-batch-value result 'repositories))
         (login (alist-get 'login user))
         (user-resource
          (and user
               (magh-resource-create 'user context :login login
                                     :title login
                                     :url (alist-get 'html_url user)))))
    (if-let* ((error (magh-batch-error result 'user)))
        (progn
          (magh-ui--insert-header "User" "Unavailable" 'magh-error)
          (insert "\n")
          (magh-ui--section (account 'account nil nil)
            "Account"
            (magh-ui--insert-request-error error)))
      (magh-ui--insert-header
       "User" (format "%s (@%s)" (or (alist-get 'name user) login) login)
       'magh-author user-resource)
      (magh-ui--insert-header "Company" (alist-get 'company user))
      (magh-ui--insert-header "Location" (alist-get 'location user))
      (magh-ui--insert-header
       "Follows" (format "%s followers, %s following"
                         (alist-get 'followers user)
                         (alist-get 'following user)))
      (magh-ui--insert-header "Bio" (alist-get 'bio user))
      (insert "\n"))
    (magh-ui--section (notifications 'notifications
                                   (magh-resource-create 'notification-list context) t)
      "Notifications"
      (if-let* ((error (magh-batch-error result 'notifications)))
          (magh-ui--insert-request-error error)
        (dolist (item notifications)
          (magh-pages--insert-notification context item))))
    (magh-ui--section (status 'status nil nil)
      "Status"
      (magh-ui--section (review-requests 'review-requests nil nil)
        "Review requests"
        (if-let* ((error (magh-batch-error result 'review-requests)))
            (magh-ui--insert-request-error error)
          (dolist (item review-requests)
            (magh-pages--insert-topic context 'pr item))))
      (magh-ui--section (assigned-issues 'assigned-issues nil nil)
        "Assigned issues"
        (if-let* ((error (magh-batch-error result 'assigned-issues)))
            (magh-ui--insert-request-error error)
          (dolist (item assigned-issues)
            (magh-pages--insert-topic context 'issue item))))
      (magh-ui--section (assigned-prs 'assigned-prs nil nil)
        "Assigned pull requests"
        (if-let* ((error (magh-batch-error result 'assigned-prs)))
            (magh-ui--insert-request-error error)
          (dolist (item assigned-prs)
            (magh-pages--insert-topic context 'pr item))))
      (magh-ui--section (my-prs 'my-prs nil nil)
        "My pull requests"
        (if-let* ((error (magh-batch-error result 'my-prs)))
            (magh-ui--insert-request-error error)
          (dolist (item my-prs)
            (magh-pages--insert-topic context 'pr item)))))
    (magh-ui--section (repositories 'repositories nil nil)
      "Repositories"
      (if-let* ((error (magh-batch-error result 'repositories)))
          (magh-ui--insert-request-error error)
        (dolist (repo repositories)
          (let ((resource (magh-pages--repo-resource context repo)))
            (magh-ui--section (repository (plist-get resource :repository) resource t)
              (magh-ui--row
               (magh-ui--styled
                (and (alist-get 'visibility repo)
                     (downcase (alist-get 'visibility repo)))
                'magh-permission)
               (magh-ui--styled (plist-get resource :repository) 'magh-repository))
              (magh-ui--insert-header "Permission"
                                    (alist-get 'viewerPermission repo)
                                    'magh-permission)
              (magh-ui--insert-header "Updated"
                                    (magh-core--date (alist-get 'updatedAt repo))
                                    'magh-date)
              (magh-ui--section (description 'description resource nil)
                "Description"
                (let ((description (alist-get 'description repo)))
                  (magh-ui--insert-markdown
                   (if (string-empty-p (string-trim (or description "")))
                       "No description."
                     description)
                   context))))))))))

(defun magh-pages--setup-user-status ()
  "Install User Status bindings."
  (local-set-key (kbd "n") #'magit-section-forward)
  (local-set-key (kbd "N") #'magh-notifications)
  (local-set-key (kbd "R") #'magh-review-requests)
  (local-set-key (kbd "/") #'magh-search-dispatch))

;;;###autoload
(defun magh-user-status ()
  "Open current GitHub account status."
  (interactive)
  (let ((context (magh-context-resolve)))
    (magh-ui--open-page
     "*magh: User Status*" context 'user-status 'viewer
     (lambda (success error force)
       (magh-pages--fetch-user-status context success error force))
     (lambda (data) (magh-pages--render-user-status context data))
     :setup #'magh-pages--setup-user-status)))

;;; User profile

(defun magh-pages--fetch-profile (context login success error force)
  "Fetch LOGIN profile and repositories."
  (magh-core--collect-async
   (list
    (cons 'user (lambda (ok fail)
                  (magh-api--user-get context login ok fail force)))
    (cons 'repositories (lambda (ok fail)
                          (magh-api--user-repositories context login ok fail force 20)))
    (cons 'issues (lambda (ok fail)
                    (magh-api--search context 'issues "" ok fail force
                                    (list :author login))))
    (cons 'prs (lambda (ok fail)
                 (magh-api--search context 'prs "" ok fail force
                                 (list :author login)))))
   success error))

(defun magh-pages--render-profile (context login result)
  "Render LOGIN profile RESULT."
  (let ((user (alist-get 'user result))
        (repositories (alist-get 'repositories result))
        (issues (alist-get 'issues result))
        (prs (alist-get 'prs result)))
    (magh-ui--insert-header "User" (format "%s (@%s)"
                                         (or (alist-get 'name user) login)
                                         login) 'magh-author)
    (magh-ui--insert-header "Bio" (alist-get 'bio user))
    (magh-ui--insert-header "Company" (alist-get 'company user))
    (magh-ui--insert-header "Location" (alist-get 'location user))
    (insert "\n")
    (magh-ui--section (repositories 'repositories nil nil)
      "Repositories"
      (dolist (repo repositories)
        (let ((resource (magh-pages--repo-resource context repo)))
          (magh-ui--insert-resource-line
           (magh-ui--row
            (magh-ui--styled (plist-get resource :repository) 'magh-repository)
            (alist-get 'description repo))
           resource))))
    (magh-ui--section (issues 'issues nil t)
      "Issues"
      (dolist (item issues) (magh-pages--insert-topic context 'issue item)))
    (magh-ui--section (pull-requests 'pull-requests nil t)
      "Pull requests"
      (dolist (item prs) (magh-pages--insert-topic context 'pr item)))))

(defun magh-user-profile (login &optional context preview)
  "Open GitHub user LOGIN profile."
  (interactive (list (read-string "GitHub user: ")))
  (setq context (magh-context-resolve context))
  (magh-ui--open-page
   (if preview (format "*magh preview: @%s*" login)
     (format "*magh: User @%s*" login))
   context 'user login
   (lambda (success error force)
     (magh-pages--fetch-profile context login success error force))
   (lambda (data) (magh-pages--render-profile context login data))
   :preview preview))

;;; Candidate registration

(magh-candidate-register
 'user
 :open (lambda (resource)
         (magh-user-profile (plist-get resource :login)
                          (plist-get resource :context)))
 :preview (lambda (resource)
            (magh-user-profile (plist-get resource :login)
                             (plist-get resource :context) t)))

(provide 'magh-pages)
;;; magh-pages.el ends here
