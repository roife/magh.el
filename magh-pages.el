;;; magh-pages.el --- User Status, profiles, and Gist pages -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; Author: magh.el contributors
;; Keywords: tools, vc, github

;;; Commentary:

;; Account-level aggregation and smaller native pages which do not warrant a
;; dedicated resource lifecycle module.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'magh-api)
(require 'magh-candidate)
(require 'magh-ui)

(declare-function magh-notifications "magh-notify")
(declare-function magh-review-requests "magh-pr")
(declare-function magh-search-dispatch "magh-search")

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

(defun magh-pages--fetch-user-status (context success error force)
  "Fetch current account status aggregates."
  (magh-core--collect-async
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
   success error))

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
  (let* ((user (alist-get 'user result))
         (notifications (alist-get 'notifications result))
         (review-requests (alist-get 'review-requests result))
         (assigned-issues (alist-get 'assigned-issues result))
         (assigned-prs (alist-get 'assigned-prs result))
         (my-prs (alist-get 'my-prs result))
         (repositories (alist-get 'repositories result))
         (login (alist-get 'login user))
         (user-resource (magh-resource-create 'user context :login login
                                            :title login
                                            :url (alist-get 'html_url user))))
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
    (insert "\n")
    (magh-ui--section (notifications 'notifications
                                   (magh-resource-create 'notification-list context) t)
      "Notifications"
      (dolist (item notifications)
        (magh-pages--insert-notification context item)))
    (magh-ui--section (status 'status nil nil)
      "Status"
      (magh-ui--section (review-requests 'review-requests nil nil)
        "Review requests"
        (dolist (item review-requests) (magh-pages--insert-topic context 'pr item)))
      (magh-ui--section (assigned-issues 'assigned-issues nil nil)
        "Assigned issues"
        (dolist (item assigned-issues) (magh-pages--insert-topic context 'issue item)))
      (magh-ui--section (assigned-prs 'assigned-prs nil nil)
        "Assigned pull requests"
        (dolist (item assigned-prs) (magh-pages--insert-topic context 'pr item)))
      (magh-ui--section (my-prs 'my-prs nil nil)
        "My pull requests"
        (dolist (item my-prs) (magh-pages--insert-topic context 'pr item))))
    (magh-ui--section (repositories 'repositories nil nil)
      "Repositories"
      (dolist (repo repositories)
        (let ((resource (magh-pages--repo-resource context repo)))
          (magh-ui--section (repository (plist-get resource :repository) resource t)
            (magh-ui--row
             (magh-ui--styled
              (downcase (alist-get 'visibility repo))
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
                 context)))))))))

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
     (lambda (data) (magh-pages--render-gists context data)))))

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
  (let ((id (alist-get 'id data)))
    (insert (propertize (or (alist-get 'description data) id)
                        'font-lock-face 'magh-resource-title) "\n\n")
    (dolist (file (magh-pages--gist-files data))
      (let ((resource (magh-pages--gist-file-resource context id file data)))
        (magh-ui--section (gist-file (plist-get resource :path) resource nil)
          (magh-ui--styled (plist-get resource :path) 'magh-file)
          (magh-ui--insert-header "Language" (alist-get 'language file))
          (magh-ui--insert-header "Size" (alist-get 'size file))
          (insert (or (alist-get 'content file)
                      "Content is truncated; press RET to open the file.") "\n"))))))

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
   :preview preview))

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
      (magh-api--gist-get
       context id
       (lambda (data)
         (let* ((file (cl-find path (magh-pages--gist-files data)
                               :key (lambda (item)
                                      (alist-get 'filename item))
                               :test #'string=))
                (content (or (alist-get 'content file) ""))
                (inhibit-read-only t))
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
                          (plist-get resource :context) t)))

(magh-candidate-register
 'gist-file
 :open (lambda (resource)
         (magh-gist-file-view (plist-get resource :id)
                            (plist-get resource :path)
                            (plist-get resource :context))))

(magh-candidate-register 'gist-list :open (lambda (_resource) (magh-gist-list)))

(provide 'magh-pages)
;;; magh-pages.el ends here
