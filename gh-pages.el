;;; gh-pages.el --- User Status, profiles, and Gist pages -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; Author: gh.el contributors
;; Keywords: tools, vc, github
;; Package-Requires: ((emacs "29.1"))

;;; Commentary:

;; Account-level aggregation and smaller native pages which do not warrant a
;; dedicated resource lifecycle module.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'gh-api)
(require 'gh-candidate)
(require 'gh-ui)

(declare-function gh-notifications "gh-notify")
(declare-function gh-review-requests "gh-pr")
(declare-function gh-search-dispatch "gh-search")

(defun gh-pages--repo-name (data)
  "Extract repository name from DATA."
  (let ((repo (gh-core--alist-get 'repository data)))
    (or (gh-core--alist-get 'nameWithOwner repo)
        (gh-core--alist-get 'fullName repo)
        (gh-core--alist-get 'nameWithOwner data)
        (gh-core--alist-get 'fullName data))))

(defun gh-pages--context-for (base data)
  "Build repository context for DATA using BASE host."
  (if-let* ((name (gh-pages--repo-name data)))
      (gh-context-from-repository name (gh-context-host base))
    base))

(defun gh-pages--topic-resource (base kind data)
  "Create KIND topic resource from search DATA."
  (let ((context (gh-pages--context-for base data)))
    (gh-resource-create
     kind context :number (gh-core--alist-get 'number data)
     :title (gh-core--alist-get 'title data)
     :url (gh-core--alist-get 'url data) :data data)))

(defun gh-pages--insert-topic (base kind data)
  "Insert account topic KIND from DATA."
  (let* ((resource (gh-pages--topic-resource base kind data))
         (state (if (gh-core--alist-get 'isDraft data)
                    "DRAFT" (or (gh-core--alist-get 'state data) "")))
         (author (gh-core--name (gh-core--alist-get 'author data)))
         (review (and (eq kind 'pr)
                      (gh-core--alist-get 'reviewDecision data)))
         (comments (gh-core--alist-get 'comments data))
         (comment-count
          (or (gh-core--alist-get 'commentsCount data)
              (gh-core--alist-get 'totalCount comments)
              (and (listp comments) (length comments)) 0)))
    (gh-ui--section (topic (list kind (plist-get resource :repository)
                                 (plist-get resource :number)) resource t)
      (gh-ui--row
       (gh-ui--styled (upcase state) (gh-core--state-face state))
       (gh-ui--styled (format "#%s" (plist-get resource :number))
                      'gh-resource-number)
       (gh-ui--styled (plist-get resource :title) 'gh-resource-title)
       (gh-ui--styled author 'gh-author)
       (and review (gh-ui--styled review (gh-core--state-face review)))
       (gh-ui--styled
        (gh-core--date (gh-core--alist-get 'updatedAt data)) 'gh-date))
      (when (eq kind 'pr)
        (gh-ui--insert-header
         "Branches" (format "%s → %s"
                            (or (gh-core--alist-get 'headRefName data) "")
                            (or (gh-core--alist-get 'baseRefName data) ""))
         'gh-branch))
      (gh-ui--insert-header "Labels"
                            (gh-core--names (gh-core--alist-get 'labels data))
                            'gh-label)
      (when (eq kind 'issue)
        (gh-ui--insert-header
         "Assigned" (gh-core--names (gh-core--alist-get 'assignees data))
         'gh-author))
      (gh-ui--insert-header "Comments" comment-count))))

;;; User status

(defun gh-pages--fetch-user-status (context success error force)
  "Fetch current account status aggregates."
  (gh-core--collect-async
   (list
    (cons 'user (lambda (ok fail)
                  (gh-api--user-get context nil ok fail force)))
    (cons 'notifications (lambda (ok fail)
                           (gh-api--notification-list context t ok fail force)))
    (cons 'review-requests (lambda (ok fail)
                             (gh-api--review-requests context ok fail force)))
    (cons 'assigned-issues (lambda (ok fail)
                             (gh-api--assigned-issues context ok fail force)))
    (cons 'assigned-prs (lambda (ok fail)
                          (gh-api--assigned-prs context ok fail force)))
    (cons 'my-prs (lambda (ok fail)
                    (gh-api--my-prs context ok fail force)))
    (cons 'repositories (lambda (ok fail)
                          (gh-api--user-repositories context nil ok fail force 10))))
   success error))

(defun gh-pages--insert-notification (base data index)
  "Insert notification summary DATA at INDEX without marking it read."
  (let* ((notification (gh-candidate--notification-resource base data))
         (resource (plist-get notification :subject-resource)))
    (gh-ui--section (notification (or (plist-get notification :id) index)
                                   resource t)
      (gh-ui--row
       (gh-ui--styled (if (plist-get notification :unread) "unread" "read")
                      (if (plist-get notification :unread)
                          'gh-pending-state 'gh-draft-state))
       (gh-ui--styled (plist-get notification :subject-type) 'gh-permission)
       (gh-ui--styled (plist-get notification :repository) 'gh-repository)
       (gh-ui--styled (plist-get notification :title) 'gh-resource-title))
      (gh-ui--insert-header "Repository"
                            (plist-get notification :repository)
                            'gh-repository)
      (gh-ui--insert-header "Updated"
                            (gh-core--date (plist-get notification :updated))
                            'gh-date))))

(defun gh-pages--repo-resource (base data)
  "Create repository resource from DATA."
  (let* ((name (or (gh-core--alist-get 'nameWithOwner data)
                   (gh-core--alist-get 'fullName data)))
         (context (gh-context-from-repository name (gh-context-host base))))
    (gh-resource-create 'repository context :name name :title name
                        :url (gh-core--alist-get 'url data) :data data)))

(defun gh-pages--render-user-status (context result)
  "Render current account status RESULT."
  (let* ((user (alist-get 'user result))
         (notifications (alist-get 'notifications result))
         (review-requests (alist-get 'review-requests result))
         (assigned-issues (alist-get 'assigned-issues result))
         (assigned-prs (alist-get 'assigned-prs result))
         (my-prs (alist-get 'my-prs result))
         (repositories (alist-get 'repositories result))
         (login (gh-core--alist-get 'login user))
         (user-resource (gh-resource-create 'user context :login login
                                            :title login
                                            :url (gh-core--alist-get 'html_url user))))
    (gh-ui--insert-header
     "User" (format "%s (@%s)" (or (gh-core--alist-get 'name user) login) login)
     'gh-author user-resource)
    (gh-ui--insert-header "Company" (gh-core--alist-get 'company user))
    (gh-ui--insert-header "Location" (gh-core--alist-get 'location user))
    (gh-ui--insert-header
     "Follows" (format "%s followers, %s following"
                       (or (gh-core--alist-get 'followers user) 0)
                       (or (gh-core--alist-get 'following user) 0)))
    (gh-ui--insert-header "Bio" (gh-core--alist-get 'bio user))
    (insert "\n")
    (gh-ui--section (notifications 'notifications
                                   (gh-resource-create 'notification-list context) t)
      (format "Notifications (%d)" (length notifications))
      (cl-loop for item in notifications for index from 1
               do (gh-pages--insert-notification context item index)))
    (gh-ui--section (status 'status nil nil)
      "Status"
      (gh-ui--section (review-requests 'review-requests nil nil)
        (format "Review requests (%d)" (length review-requests))
        (dolist (item review-requests) (gh-pages--insert-topic context 'pr item)))
      (gh-ui--section (assigned-issues 'assigned-issues nil nil)
        (format "Assigned issues (%d)" (length assigned-issues))
        (dolist (item assigned-issues) (gh-pages--insert-topic context 'issue item)))
      (gh-ui--section (assigned-prs 'assigned-prs nil nil)
        (format "Assigned pull requests (%d)" (length assigned-prs))
        (dolist (item assigned-prs) (gh-pages--insert-topic context 'pr item)))
      (gh-ui--section (my-prs 'my-prs nil nil)
        (format "My pull requests (%d)" (length my-prs))
        (dolist (item my-prs) (gh-pages--insert-topic context 'pr item))))
    (gh-ui--section (repositories 'repositories nil nil)
      (format "Repositories (%d recent)" (length repositories))
      (dolist (repo repositories)
        (let ((resource (gh-pages--repo-resource context repo)))
          (gh-ui--section (repository (plist-get resource :repository) resource t)
            (gh-ui--row
             (gh-ui--styled
              (downcase (format "%s"
                                (or (gh-core--alist-get 'visibility repo)
                                    "public")))
              'gh-permission)
             (gh-ui--styled (plist-get resource :repository) 'gh-repository)
             (gh-ui--styled (gh-core--alist-get 'viewerPermission repo)
                            'gh-permission)
             (gh-ui--styled
              (gh-core--date (or (gh-core--alist-get 'updatedAt repo)
                                 (gh-core--alist-get 'pushedAt repo)))
              'gh-date))
            (gh-ui--insert-markdown
             (or (gh-core--alist-get 'description repo) "") context)))))))

(defun gh-pages--setup-user-status ()
  "Install User Status bindings."
  (local-set-key (kbd "n") #'magit-section-forward)
  (local-set-key (kbd "N") #'gh-notifications)
  (local-set-key (kbd "R") #'gh-review-requests)
  (local-set-key (kbd "/") #'gh-search-dispatch))

;;;###autoload
(defun gh-user-status ()
  "Open current GitHub account status."
  (interactive)
  (let ((context (gh-context-resolve)))
    (gh-ui--open-page
     "*gh: User Status*" context 'user-status 'viewer
     (lambda (success error force)
       (gh-pages--fetch-user-status context success error force))
     (lambda (data) (gh-pages--render-user-status context data))
     :setup #'gh-pages--setup-user-status)))

;;; User profile

(defun gh-pages--fetch-profile (context login success error force)
  "Fetch LOGIN profile and repositories."
  (gh-core--collect-async
   (list
    (cons 'user (lambda (ok fail)
                  (gh-api--user-get context login ok fail force)))
    (cons 'repositories (lambda (ok fail)
                          (gh-api--user-repositories context login ok fail force 20)))
    (cons 'issues (lambda (ok fail)
                    (gh-api--search context 'issues "" ok fail force
                                    (list :author login))))
    (cons 'prs (lambda (ok fail)
                 (gh-api--search context 'prs "" ok fail force
                                 (list :author login)))))
   success error))

(defun gh-pages--render-profile (context login result)
  "Render LOGIN profile RESULT."
  (let ((user (alist-get 'user result))
        (repositories (alist-get 'repositories result))
        (issues (alist-get 'issues result))
        (prs (alist-get 'prs result)))
    (gh-ui--insert-header "User" (format "%s (@%s)"
                                         (or (gh-core--alist-get 'name user) login)
                                         login) 'gh-author)
    (gh-ui--insert-header "Bio" (gh-core--alist-get 'bio user))
    (gh-ui--insert-header "Company" (gh-core--alist-get 'company user))
    (gh-ui--insert-header "Location" (gh-core--alist-get 'location user))
    (insert "\n")
    (gh-ui--section (repositories 'repositories nil nil)
      (format "Repositories (%d)" (length repositories))
      (dolist (repo repositories)
        (let ((resource (gh-pages--repo-resource context repo)))
          (gh-ui--insert-resource-line
           (gh-ui--row
            (gh-ui--styled (plist-get resource :repository) 'gh-repository)
            (gh-core--alist-get 'description repo))
           resource))))
    (gh-ui--section (issues 'issues nil t)
      (format "Issues (%d)" (length issues))
      (dolist (item issues) (gh-pages--insert-topic context 'issue item)))
    (gh-ui--section (pull-requests 'pull-requests nil t)
      (format "Pull requests (%d)" (length prs))
      (dolist (item prs) (gh-pages--insert-topic context 'pr item)))))

(defun gh-user-profile (login &optional context preview)
  "Open GitHub user LOGIN profile."
  (interactive (list (read-string "GitHub user: ")))
  (setq context (gh-context-resolve context))
  (gh-ui--open-page
   (if preview (format "*gh preview: @%s*" login)
     (format "*gh: User @%s*" login))
   context 'user login
   (lambda (success error force)
     (gh-pages--fetch-profile context login success error force))
   (lambda (data) (gh-pages--render-profile context login data))
   :preview preview))

;;; Gists

(defun gh-pages--gist-resource (context data)
  "Create Gist resource from DATA."
  (let ((id (gh-core--alist-get 'id data)))
    (gh-resource-create
     'gist context :id id
     :title (or (gh-core--alist-get 'description data) id)
     :url (gh-core--alist-get 'html_url data) :data data)))

(defun gh-pages--render-gists (context data)
  "Render Gist list DATA."
  (insert (propertize "Gists\n\n" 'font-lock-face 'gh-resource-title))
  (if data
      (dolist (gist data)
        (let ((resource (gh-pages--gist-resource context gist)))
          (gh-ui--section (gist (plist-get resource :id) resource t)
            (gh-ui--styled (gh-resource-title resource) 'gh-resource-title)
            (gh-ui--insert-header "ID" (plist-get resource :id))
            (gh-ui--insert-header "Visibility"
                                  (if (gh-core--alist-get 'public gist)
                                      "public" "secret")
                                  'gh-permission)
            (gh-ui--insert-header
             "Updated" (gh-core--date (gh-core--alist-get 'updated_at gist))
             'gh-date))))
    (insert (propertize "No gists found.\n" 'font-lock-face 'shadow))))

;;;###autoload
(defun gh-gist-list ()
  "Open native list of current account Gists."
  (interactive)
  (let ((context (gh-context-resolve)))
    (gh-ui--open-page
     "*gh: Gists*" context 'gist-list 'viewer
     (lambda (success error force)
       (gh-api--gist-list context success error force))
     (lambda (data) (gh-pages--render-gists context data)))))

(defun gh-pages--gist-files (data)
  "Return normalized file alists from Gist DATA."
  (let ((files (gh-core--alist-get 'files data)))
    (mapcar
     (lambda (entry)
       (let ((file (cdr entry)))
         (if (gh-core--alist-get 'filename file) file
           (cons (cons 'filename (symbol-name (car entry))) file))))
     files)))

(defun gh-pages--gist-file-resource (context id file data)
  "Create Gist file resource."
  (gh-resource-create
   'gist-file context :id id :path (gh-core--alist-get 'filename file)
   :title (gh-core--alist-get 'filename file)
   :url (gh-core--alist-get 'html_url data) :data file))

(defun gh-pages--render-gist (context data)
  "Render Gist DATA."
  (let ((id (gh-core--alist-get 'id data)))
    (insert (propertize (or (gh-core--alist-get 'description data) id)
                        'font-lock-face 'gh-resource-title) "\n\n")
    (dolist (file (gh-pages--gist-files data))
      (let ((resource (gh-pages--gist-file-resource context id file data)))
        (gh-ui--section (gist-file (plist-get resource :path) resource nil)
          (gh-ui--styled (plist-get resource :path) 'gh-file)
          (gh-ui--insert-header "Language" (gh-core--alist-get 'language file))
          (gh-ui--insert-header "Size" (gh-core--alist-get 'size file))
          (insert (or (gh-core--alist-get 'content file)
                      "Content is truncated; press RET to open the file.") "\n"))))))

(defun gh-gist-view (id &optional context preview)
  "Open Gist ID."
  (interactive (list (read-string "Gist ID: ")))
  (setq context (gh-context-resolve context))
  (gh-ui--open-page
   (if preview (format "*gh preview: Gist %s*" id)
     (format "*gh: Gist %s*" id))
   context 'gist id
   (lambda (success error force)
     (gh-api--gist-get context id success error force))
   (lambda (data) (gh-pages--render-gist context data))
   :preview preview))

(defun gh-gist-file-view (id path &optional context)
  "Open PATH in Gist ID as a read-only source buffer."
  (setq context (gh-context-resolve context))
  (let ((buffer (get-buffer-create (format "*gh: Gist %s · %s*" id path))))
    (with-current-buffer buffer
      (special-mode)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (propertize "Loading Gist file…\n"
                            'font-lock-face 'gh-loading)))
      (gh-api--gist-get
       context id
       (lambda (data)
         (let* ((file (cl-find path (gh-pages--gist-files data)
                               :key (lambda (item)
                                      (gh-core--alist-get 'filename item))
                               :test #'string=))
                (content (or (gh-core--alist-get 'content file) ""))
                (inhibit-read-only t))
           (erase-buffer) (insert content)
           (goto-char (point-min))
           (let ((buffer-file-name path)) (set-auto-mode))
           (setq buffer-read-only t)
           (set-buffer-modified-p nil)))
       (lambda (error)
         (let ((inhibit-read-only t))
           (erase-buffer) (insert (gh-error-message error) "\n")))))
    (funcall gh-display-buffer-function buffer)
    buffer))

;;; Candidate registration

(gh-candidate-register
 'user
 :open (lambda (resource)
         (gh-user-profile (plist-get resource :login)
                          (plist-get resource :context)))
 :preview (lambda (resource)
            (gh-user-profile (plist-get resource :login)
                             (plist-get resource :context) t)))

(gh-candidate-register
 'gist
 :open (lambda (resource)
         (gh-gist-view (plist-get resource :id) (plist-get resource :context)))
 :preview (lambda (resource)
            (gh-gist-view (plist-get resource :id)
                          (plist-get resource :context) t)))

(gh-candidate-register
 'gist-file
 :open (lambda (resource)
         (gh-gist-file-view (plist-get resource :id)
                            (plist-get resource :path)
                            (plist-get resource :context))))

(gh-candidate-register 'gist-list :open (lambda (_resource) (gh-gist-list)))

(provide 'gh-pages)
;;; gh-pages.el ends here
