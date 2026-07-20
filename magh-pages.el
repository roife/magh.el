;;; magh-pages.el --- User Status, profiles, and Gist pages -*- lexical-binding: t; -*-

;; Copyright (C) 2026
;; SPDX-License-Identifier: GPL-2.0-only

;; Author: magh.el contributors
;; Keywords: tools, vc, github

;;; Commentary:

;; Account-level aggregation and smaller native pages which do not warrant a
;; dedicated resource lifecycle module.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'transient)
(require 'magh-api)
(require 'magh-candidate)
(require 'magh-edit)
(require 'magh-ui)

(declare-function magh-notifications "magh-notify")
(declare-function magh-review-requests "magh-pr")
(declare-function magh-search-dispatch "magh-search")

(defvar-local magh-pages--gist-id nil)
(defvar-local magh-pages--gist-dispatch-resource nil)

(defun magh-pages--insert-topic (base kind data)
  "Insert account topic KIND from DATA."
  (let* ((repository (alist-get 'repository data))
         (name (alist-get 'nameWithOwner repository))
         (context (magh-context-from-repository name (magh-context-host base)))
         (resource (magh-resource-create
                    kind context :number (alist-get 'number data)
                    :title (alist-get 'title data)
                    :url (alist-get 'url data) :data data))
         (state (alist-get 'state data))
         (author (magh-core--name (alist-get 'author data)))
         (comment-count (magh-core--comments-count data)))
    (magh-ui--section (topic (list kind (plist-get resource :repository)
                                 (plist-get resource :number)) resource t)
      (magh-ui--row
       (magh-ui--styled (upcase state) (magh-core--state-face state))
       (magh-ui--styled (format "#%s" (plist-get resource :number))
                      'magh-resource-number)
       (magh-ui--styled (plist-get resource :title) 'magh-resource-title))
      (magh-ui--insert-header "Author" author 'magh-author)
      (magh-ui--insert-header "Labels"
                            (magh-core--names (alist-get 'labels data))
                            'magh-label)
      (when (eq kind 'issue)
        (magh-ui--insert-header
         "Assigned" (magh-core--names (alist-get 'assignees data))
         'magh-author))
      (magh-ui--insert-header "Comments" comment-count)
      (magh-ui--insert-header "Updated"
                            (magh-core--date (alist-get 'updatedAt data))
                            'magh-date))))

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

;;; Gists

(defun magh-pages--gist-resource (context data)
  "Create Gist resource from DATA."
  (let ((id (alist-get 'id data)))
    (magh-resource-create
     'gist context :id id
     :title (or (alist-get 'description data) id)
     :url (alist-get 'html_url data) :data data)))

(defun magh-pages--render-gists (context data)
  "Render Gist list DATA."
  (insert (propertize "Gists\n\n" 'font-lock-face 'magh-resource-title))
  (if data
      (dolist (gist data)
        (let ((resource (magh-pages--gist-resource context gist)))
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
     (lambda (data) (magh-pages--render-gists context data))
     :setup (lambda ()
              (setq magh-buffer-dispatch-function
                    #'magh-gist-list-dispatch)))))

(defun magh-pages--gist-files (data)
  "Return file alists from Gist DATA."
  (mapcar #'cdr (alist-get 'files data)))

(defun magh-pages--gist-file-resource (context id file data)
  "Create Gist file resource."
  (magh-resource-create
   'gist-file context :id id :path (alist-get 'filename file)
   :title (alist-get 'filename file)
   :url (alist-get 'html_url data) :data file))

(defun magh-pages--render-gist (context data)
  "Render Gist DATA."
  (let* ((id (alist-get 'id data))
         (resource (magh-pages--gist-resource context data))
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
    (dolist (file (magh-pages--gist-files data))
      (let ((resource (magh-pages--gist-file-resource context id file data)))
        (magh-ui--section (gist-file (plist-get resource :path) resource nil)
          (magh-ui--styled (plist-get resource :path) 'magh-file)
          (magh-ui--insert-header "Language" (alist-get 'language file))
          (magh-ui--insert-header "Size" (alist-get 'size file))
          (insert (or (alist-get 'content file)
                      "Content is truncated; press RET to open the file.") "\n"))))))

(defun magh-pages--setup-gist (id)
  "Install Gist detail state for ID."
  (setq magh-pages--gist-id id
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
   (lambda (data) (magh-pages--render-gist context data))
   :preview preview :setup (lambda () (magh-pages--setup-gist id))))

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

(defun magh-pages--gist-prefill ()
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
  (pcase-let* ((`(,filename ,content) (magh-pages--gist-prefill))
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

(defun magh-pages--current-gist-resource ()
  "Return current Gist or Gist file resource."
  (let ((point-resource (magh-ui-resource-at-point)))
    (or (and (memq (plist-get point-resource :kind) '(gist gist-file))
             point-resource)
        magh-pages--gist-dispatch-resource
        (and magh-pages--gist-id
             (magh-resource-create
              'gist magh-buffer-context :id magh-pages--gist-id)))))

(defun magh-pages--gist-data ()
  "Return loaded Gist metadata on a detail page."
  (and (listp magh-ui--data) (alist-get 'files magh-ui--data) magh-ui--data))

(defun magh-pages--with-gist (context id function)
  "Call FUNCTION with complete Gist metadata for ID."
  (if-let* ((data (and (equal id (alist-get 'id (magh-pages--gist-data)))
                       (magh-pages--gist-data))))
      (funcall function data)
    (magh-api--gist-get context id function #'magh-core--user-error t)))

;;;###autoload
(defun magh-gist-edit-metadata (&optional context id)
  "Edit the description of Gist ID.
Gist visibility cannot be converted after creation."
  (interactive)
  (let ((resource (magh-pages--current-gist-resource)))
    (setq context (magh-context-resolve
                   (or context (plist-get resource :context)))
          id (or id (plist-get resource :id) magh-pages--gist-id
                 (read-string "Gist ID: ")))
    (magh-pages--with-gist
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

(defun magh-pages--gist-file-context (&optional resource)
  "Return (RESOURCE CONTEXT ID PATH) for a Gist file action."
  (setq resource (or resource (magh-pages--current-gist-resource)))
  (unless (eq (plist-get resource :kind) 'gist-file)
    (user-error "No Gist file selected"))
  (list resource (magh-context-resolve (plist-get resource :context))
        (plist-get resource :id) (plist-get resource :path)))

(defun magh-pages--open-gist-content-editor
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
               (magh-pages--gist-file-context resource)))
    (magh-api--gist-file-raw
     context id path
     (lambda (content)
       (magh-pages--open-gist-content-editor
        context id path content
        (format "*magh: Gist %s · Edit %s*" id path)))
     #'magh-core--user-error t)))

;;;###autoload
(defun magh-gist-file-add (&optional context id)
  "Add a file to Gist ID."
  (interactive)
  (let ((resource (magh-pages--current-gist-resource)))
    (setq context (magh-context-resolve
                   (or context (plist-get resource :context)))
          id (or id (plist-get resource :id) magh-pages--gist-id
                 (read-string "Gist ID: ")))
    (magh-pages--with-gist
     context id
     (lambda (data)
       (let ((names (mapcar (lambda (file) (alist-get 'filename file))
                            (magh-pages--gist-files data))))
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
               (magh-pages--gist-file-context resource)))
    (magh-pages--with-gist
     context id
     (lambda (data)
       (let ((names (mapcar (lambda (file) (alist-get 'filename file))
                            (magh-pages--gist-files data))))
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
            (lambda (_) (message "Renamed %s to %s" path new-name)
              (magh-ui--refresh-if-page))
            #'magh-core--user-error))))))))

;;;###autoload
(defun magh-gist-file-remove (&optional resource)
  "Remove selected Gist file RESOURCE, preserving at least one file."
  (interactive)
  (pcase-let ((`(,_ ,context ,id ,path)
               (magh-pages--gist-file-context resource)))
    (magh-pages--with-gist
     context id
     (lambda (data)
       (when (<= (length (magh-pages--gist-files data)) 1)
         (user-error "Cannot remove the last file from a Gist"))
       (when (magh-core--confirm (format "Remove %s from this Gist? " path))
         (magh-api--gist-file-remove
          context id path
          (lambda (_) (message "Removed %s" path)
            (magh-ui--refresh-if-page))
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
 'user
 :open (lambda (resource)
         (magh-user-profile (plist-get resource :login)
                          (plist-get resource :context)))
 :preview (lambda (resource)
            (magh-user-profile (plist-get resource :login)
                             (plist-get resource :context) t)))

(magh-candidate-register
 'gist
 :open (lambda (resource)
         (magh-gist-view (plist-get resource :id) (plist-get resource :context)))
 :preview (lambda (resource)
            (magh-gist-view (plist-get resource :id)
                          (plist-get resource :context) t))
 :dispatch (lambda (resource)
             (setq magh-pages--gist-dispatch-resource resource)
             (call-interactively #'magh-gist-dispatch)))

(magh-candidate-register
 'gist-file
 :open (lambda (resource)
         (magh-gist-file-view (plist-get resource :id)
                            (plist-get resource :path)
                            (plist-get resource :context)))
 :dispatch (lambda (resource)
             (setq magh-pages--gist-dispatch-resource resource)
             (call-interactively #'magh-gist-dispatch)))

(magh-candidate-register 'gist-list :open (lambda (_resource) (magh-gist-list)))

(provide 'magh-pages)
;;; magh-pages.el ends here
