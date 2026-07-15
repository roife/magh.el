;;; gh-pr.el --- Pull Request and review workflow for gh.el -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; Author: gh.el contributors
;; Keywords: tools, vc, github
;; Package-Requires: ((emacs "31.1") (transient "0.7.0"))

;;; Commentary:

;; Pull Request list/detail pages, templates, structured editing, changed
;; files, checks, review collection/submission, checkout, merge, and state
;; actions.  Cross-resource navigation is registered through gh-candidate.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'transient)
(require 'gh-api)
(require 'gh-candidate)
(require 'gh-edit)
(require 'gh-ui)

(defvar-local gh-pr--state nil)
(defvar-local gh-pr--params nil)
(defvar-local gh-pr--limit nil)
(defvar-local gh-pr--dispatch-resource nil)
(defvar-local gh-pr--view-number nil)
(defvar gh-pr--review-comments (make-hash-table :test #'equal)
  "Collected review comment plists keyed by (HOST REPOSITORY NUMBER).")

(defun gh-pr--context (&optional context)
  "Resolve repository CONTEXT for a Pull Request command."
  (gh-context-resolve (or context gh-buffer-context) t))

(defun gh-pr--buffer-name (context &optional number suffix)
  "Return Pull Request buffer name for CONTEXT, NUMBER, and SUFFIX."
  (format "*gh: %s · %s%s*"
          (gh-context-repository context)
          (if number (format "PR #%s" number) "Pull requests")
          (if suffix (concat " · " suffix) "")))

(defun gh-pr--resource (context data)
  "Create Pull Request resource from DATA in CONTEXT."
  (gh-resource-create
   'pr context :number (alist-get 'number data)
   :title (alist-get 'title data)
   :url (alist-get 'url data)))

(defun gh-pr--row-values (data)
  "Return compact row values for Pull Request DATA."
  (let ((state (if (alist-get 'isDraft data)
                   "DRAFT" (alist-get 'state data)))
        (review (alist-get 'reviewDecision data)))
    (list :state (gh-ui--styled (upcase state) (gh-core--state-face state))
          :identifier (gh-ui--styled
                       (format "#%s" (alist-get 'number data))
                       'gh-resource-number)
          :title (gh-ui--styled (alist-get 'title data)
                                'gh-resource-title)
          :review (gh-ui--styled review (gh-core--state-face review)))))

(defun gh-pr--insert-row (context data)
  "Insert Pull Request DATA as a native section row."
  (let* ((resource (gh-pr--resource context data))
         (number (plist-get resource :number)))
    (gh-ui--section (pr number resource t)
      (gh-ui--format-row (gh-pr--row-values data))
      (gh-ui--insert-header
       "Branches" (format "%s → %s"
                          (alist-get 'headRefName data)
                          (alist-get 'baseRefName data))
       'gh-branch)
      (gh-ui--insert-header "Author"
                            (gh-core--name (alist-get 'author data))
                            'gh-author)
      (gh-ui--insert-header "Review"
                            (or (alist-get 'reviewDecision data) "—")
                            (gh-core--state-face
                             (alist-get 'reviewDecision data)))
      (gh-ui--insert-header "Labels"
                            (gh-core--names (alist-get 'labels data))
                            'gh-label)
      (gh-ui--insert-header "Comments"
                            (gh-core--comments-count data))
      (gh-ui--insert-header "Created"
                            (gh-core--date (alist-get 'createdAt data))
                            'gh-date)
      (gh-ui--insert-header "Updated"
                            (gh-core--date (alist-get 'updatedAt data))
                            'gh-date))))

(defun gh-pr--render-list (context state data)
  "Render Pull Request list DATA in CONTEXT for STATE."
  (gh-ui--insert-header "Repository" (gh-context-repository context)
                        'gh-repository (gh-resource-create 'repository context))
  (gh-ui--insert-header "Pull requests" state)
  (insert "\n")
  (if data
      (dolist (pr data) (gh-pr--insert-row context pr))
    (insert (propertize "No matching pull requests.\n"
                        'font-lock-face 'shadow)))
  (gh-ui--section (more 'more (gh-resource-create 'pr-more context) t)
    (format "Load more (current limit %d)" gh-pr--limit)
    (insert "Press RET to double the list limit.\n")))

;;;###autoload
(defun gh-pr-list (&optional context state params)
  "Open Pull Request list for CONTEXT, STATE, and PARAMS."
  (interactive)
  (setq context (gh-pr--context context)
        state (or state gh-default-pr-state))
  (let ((limit (or (plist-get params :limit) gh-list-limit)))
    (gh-ui--open-page
     (gh-pr--buffer-name context nil state) context 'pr-list state
     (lambda (success error force)
       (gh-api--pr-list context (append (list :state state :limit limit) params)
                        success error force))
     (lambda (data) (gh-pr--render-list context state data))
     :setup
     (lambda ()
       (setq gh-pr--state state gh-pr--params params gh-pr--limit limit)
       (local-set-key (kbd "c")
                      (lambda () (interactive) (gh-pr-create context)))
       (local-set-key (kbd "t") #'gh-pr-cycle-state)
       (setq gh-buffer-dispatch-function #'gh-pr-dispatch)))))

(defun gh-pr-load-more ()
  "Double current Pull Request list limit and refresh."
  (interactive)
  (let ((context gh-buffer-context)
        (state gh-pr--state)
        (params gh-pr--params)
        (limit (* 2 gh-pr--limit)))
    (setq gh-pr--limit limit
          gh-buffer-refresh-function
          (lambda (success error force)
            (gh-api--pr-list
             context (append (list :state state :limit limit) params)
             success error force)))
    (gh-ui-refresh t)))

(defun gh-pr-cycle-state ()
  "Cycle Pull Request list state."
  (interactive)
  (gh-pr-list gh-buffer-context
              (pcase gh-pr--state
                ("open" "closed") ("closed" "merged")
                ("merged" "all") (_ "open"))
              gh-pr--params))

;;; Details

(defun gh-pr--fetch-view (context number success error force)
  "Fetch all native detail data for Pull Request NUMBER."
  (gh-core--collect-async
   (list
    (cons 'pr (lambda (ok fail)
                (gh-api--pr-get context number ok fail force)))
    (cons 'commits (lambda (ok fail)
                     (gh-api--pr-commits context number ok fail force)))
    (cons 'files (lambda (ok fail)
                   (gh-api--pr-files context number ok fail force)))
    (cons 'review-comments
          (lambda (ok fail)
            (gh-api--pr-review-comments context number ok fail force))))
   success error))

(defun gh-pr--commit-resource (context commit)
  "Create Commit resource from COMMIT in CONTEXT."
  (gh-resource-create
   'commit context :sha (alist-get 'sha commit)
   :title (alist-get 'message (alist-get 'commit commit))
   :url (alist-get 'html_url commit)
   :data commit))

(defun gh-pr--file-resource (context file head-ref)
  "Create remote file resource for FILE at HEAD-REF."
  (let ((path (alist-get 'filename file)))
    (gh-resource-create
     'file (gh-context-copy context :ref head-ref :path path)
     :path path :ref head-ref
     :url (alist-get 'blob_url file))))

(defun gh-pr--head-context (context pr)
  "Return the head repository context for PR, falling back to CONTEXT."
  (if-let* ((repository
             (alist-get 'nameWithOwner (alist-get 'headRepository pr))))
      (gh-context-from-repository repository (gh-context-host context))
    context))

(defun gh-pr--head-ref (pr)
  "Return PR's immutable head ref when available."
  (or (alist-get 'headRefOid pr) (alist-get 'headRefName pr)))

(defun gh-pr--check-resource (context check)
  "Create Run or web CHECK resource."
  (let ((name (or (alist-get 'name check) (alist-get 'context check)))
        (url (or (alist-get 'detailsUrl check)
                 (alist-get 'targetUrl check))))
    (if (and url (string-match "/actions/runs/\\([0-9]+\\)" url))
        (gh-resource-create
         'run context :id (string-to-number (match-string 1 url))
         :title name :url url)
      (gh-resource-create
       'check context :title name :url url))))

(defun gh-pr--conversation-items (pr inline-comments)
  "Return sorted conversation items from PR and INLINE-COMMENTS."
  (sort
   (append
    (mapcar (lambda (item) (cons 'comment item))
            (alist-get 'comments pr))
    (mapcar (lambda (item) (cons 'review item))
            (alist-get 'reviews pr))
    (mapcar (lambda (item) (cons 'inline item)) inline-comments))
   (lambda (a b)
     (string< (or (alist-get 'createdAt (cdr a))
                  (alist-get 'submittedAt (cdr a))
                  (alist-get 'created_at (cdr a)) "")
              (or (alist-get 'createdAt (cdr b))
                  (alist-get 'submittedAt (cdr b))
                  (alist-get 'created_at (cdr b)) "")))))

(defun gh-pr--render-conversation (context items head-context head-ref)
  "Render Pull Request conversation ITEMS using HEAD-CONTEXT and HEAD-REF."
  (gh-ui--section (conversation 'conversation nil nil)
    (format "Conversation (%d)" (length items))
    (dolist (item items)
      (pcase-let* ((`(,kind . ,data) item)
                   (id (alist-get 'id data))
                   (author (gh-core--name
                            (or (alist-get 'author data)
                                (alist-get 'user data))))
                   (date (gh-core--date
                          (or (alist-get 'createdAt data)
                              (alist-get 'submittedAt data)
                              (alist-get 'created_at data))))
                   (state (alist-get 'state data))
                   (path (alist-get 'path data))
                   (line (or (alist-get 'line data)
                             (alist-get 'original_line data)))
                   (resource
                    (if path
                        (gh-resource-create
                         'file (gh-context-copy head-context
                                                :ref head-ref :path path)
                         :path path :ref head-ref :line line)
                      (gh-resource-create 'comment context :id id))))
        (gh-ui--section (comment id resource nil)
          (concat
           (gh-ui--styled
            (pcase kind ('review "Review") ('inline "Inline comment")
              (_ "Comment"))
            'gh-conversation-kind)
           " by " (or (gh-ui--styled author 'gh-author) "")
           (if state
               (concat " " (gh-ui--styled
                             state (gh-core--state-face state))) "")
           " · " (or (gh-ui--styled date 'gh-date) ""))
          (when path
            (gh-ui--insert-header "Location" (format "%s:%s" path line)
                                  'gh-file resource))
          (gh-ui--insert-markdown (alist-get 'body data) context))))))

(defun gh-pr--render-view (context result)
  "Render Pull Request detail RESULT in CONTEXT."
  (let* ((pr (alist-get 'pr result))
         (commits (alist-get 'commits result))
         (files (alist-get 'files result))
         (inline-comments (alist-get 'review-comments result))
         (checks (alist-get 'statusCheckRollup pr))
         (resource (gh-pr--resource context pr))
         (number (alist-get 'number pr))
         (head-name (alist-get 'headRefName pr))
         (head-context (gh-pr--head-context context pr))
         (head-ref (gh-pr--head-ref pr))
         (base-ref (alist-get 'baseRefName pr))
         (state (if (alist-get 'isDraft pr)
                    "DRAFT" (alist-get 'state pr))))
    (insert (propertize (format "#%s " number)
                        'font-lock-face 'gh-resource-number)
            (propertize (alist-get 'title pr)
                        'font-lock-face 'gh-resource-title) "\n")
    (add-text-properties (line-beginning-position 0) (point)
                         (list 'gh-resource resource))
    (gh-ui--insert-header "State" state (gh-core--state-face state))
    (gh-ui--insert-header "Author"
                          (gh-core--name (alist-get 'author pr))
                          'gh-author)
    (gh-ui--insert-header "Branches" (format "%s → %s" head-name base-ref)
                          'gh-branch)
    (let ((review (alist-get 'reviewDecision pr)))
      (gh-ui--insert-header "Review" (or review "—")
                            (and review (gh-core--state-face review))))
    (gh-ui--insert-header "Labels"
                          (gh-core--names (alist-get 'labels pr))
                          'gh-label)
    (insert "\n")
    (gh-ui--section (description 'description resource nil)
      "Description"
      (let ((body (alist-get 'body pr)))
        (gh-ui--insert-markdown
         (if (string-empty-p (string-trim (or body "")))
             "No description."
           body)
         context)))
    (gh-ui--section (commits 'commits
                             (gh-resource-create 'pr-commits context
                                                 :number number) nil)
      (format "Commits (%d)" (length commits))
      (dolist (commit commits)
        (let* ((commit-resource (gh-pr--commit-resource context commit))
               (sha (plist-get commit-resource :sha))
               (commit-data (alist-get 'commit commit))
               (message (car (split-string
                              (alist-get 'message commit-data) "\n")))
               (author (or (gh-core--name (alist-get 'author commit))
                           (gh-core--name (alist-get 'author commit-data)))))
          (gh-ui--section (commit sha commit-resource t)
            (gh-ui--row
             (gh-ui--styled (substring sha 0 10) 'gh-hash)
             (gh-ui--styled message 'gh-resource-title))
            (gh-ui--insert-header "Author" author 'gh-author)
            (gh-ui--insert-header
             "Committed" (gh-core--date
                           (alist-get 'date (alist-get 'committer commit-data)))
             'gh-date)))))
    (gh-ui--section (files 'files
                           (gh-resource-create 'pr-files context :number number) nil)
      (format "Files (%d)" (length files))
      (dolist (file files)
        (let ((file-resource
               (gh-pr--file-resource head-context file head-ref)))
          (gh-ui--section (file (alist-get 'filename file)
                                file-resource t)
            (gh-ui--row
             (gh-ui--styled (alist-get 'filename file) 'gh-file)
             (gh-ui--styled
              (format "+%s" (alist-get 'additions file))
              'gh-added)
             (gh-ui--styled
              (format "-%s" (alist-get 'deletions file))
              'gh-removed))))))
    (gh-ui--section (checks 'checks nil nil)
      (format "Checks (%d)" (length checks))
      (dolist (check checks)
        (let* ((check-resource (gh-pr--check-resource context check))
               (status (or (alist-get 'conclusion check)
                           (alist-get 'status check)))
               (name (gh-resource-title check-resource)))
          (gh-ui--section (check name check-resource t)
            (gh-ui--row
             (gh-ui--styled (upcase status) (gh-core--state-face status))
             (gh-ui--styled name 'gh-workflow))))))
    (gh-pr--render-conversation
     context (gh-pr--conversation-items pr inline-comments)
     head-context head-ref)))

(defun gh-pr--setup-view (context number)
  "Install detail keys for Pull Request NUMBER in CONTEXT."
  (setq gh-pr--view-number number)
  (local-set-key (kbd "E") (lambda () (interactive) (gh-pr-edit context number)))
  (local-set-key (kbd "C")
                 (lambda () (interactive) (gh-pr-view-commits context number)))
  (local-set-key (kbd "F")
                 (lambda () (interactive) (gh-pr-view-files context number)))
  (local-set-key (kbd "d")
                 (lambda () (interactive) (gh-pr-diff context number)))
  (setq gh-buffer-dispatch-function
        (lambda ()
          (setq gh-pr--dispatch-resource
                (gh-resource-create 'pr context :number number))
          (call-interactively #'gh-pr-dispatch))))

;;;###autoload
(defun gh-pr-view (number &optional context preview)
  "Open Pull Request NUMBER in CONTEXT.  PREVIEW creates a disposable page."
  (interactive (list (read-number "Pull Request number: ")))
  (setq context (gh-pr--context context))
  (gh-ui--open-page
   (if preview
       (format "*gh preview: %s#%s*" (gh-context-repository context) number)
     (gh-pr--buffer-name context number))
   context 'pr number
   (lambda (success error force)
     (gh-pr--fetch-view context number success error force))
   (lambda (data) (gh-pr--render-view context data))
   :preview preview :setup (lambda () (gh-pr--setup-view context number))))

;;; Commits, files, diff

;;;###autoload
(defun gh-pr-view-commits (&optional context number)
  "Select a commit belonging to Pull Request NUMBER."
  (interactive)
  (pcase-let ((`(,current-context ,current-number) (gh-pr--current)))
    (setq context (gh-pr--context (or context current-context))
          number (or number current-number))
    (gh-api--pr-commits
     context number
     (lambda (commits)
       (gh-candidate-select-and-open
        "Commit: "
        (mapcar (lambda (commit) (gh-pr--commit-resource context commit))
                commits)
        (lambda (item)
          (let ((message
                 (alist-get 'message
                            (alist-get 'commit (plist-get item :data)))))
            (gh-ui--row
             (gh-ui--styled (substring (plist-get item :sha) 0 10) 'gh-hash)
             (gh-ui--styled (car (split-string message "\n"))
                            'gh-resource-title))))
        t))
     #'gh-core--user-error)))

(defun gh-pr--render-files (context number result)
  "Render changed files RESULT for Pull Request NUMBER."
  (let* ((pr (alist-get 'pr result))
         (files (alist-get 'files result))
         (comments (alist-get 'review-comments result))
         (head-context (gh-pr--head-context context pr))
         (head-ref (gh-pr--head-ref pr)))
    (gh-ui--insert-header "Repository" (gh-context-repository context)
                          'gh-repository)
    (gh-ui--insert-header "Pull Request" (format "#%s changed files" number))
    (insert "\n")
    (dolist (file files)
      (let* ((path (alist-get 'filename file))
             (resource (gh-pr--file-resource head-context file head-ref))
             (file-comments
              (seq-filter (lambda (comment)
                            (equal (alist-get 'path comment) path))
                          comments)))
        (gh-ui--section (file path resource nil)
          (gh-ui--row
           (gh-ui--styled path 'gh-file)
           (gh-ui--styled
            (format "+%s" (alist-get 'additions file))
            'gh-added)
           (gh-ui--styled
            (format "-%s" (alist-get 'deletions file))
            'gh-removed))
          (if-let* ((patch (alist-get 'patch file)))
              (gh-ui--insert-diff patch)
            (insert (propertize "Binary or oversized diff unavailable.\n"
                                'font-lock-face 'shadow)))
          (dolist (comment file-comments)
            (let ((line (or (alist-get 'line comment)
                            (alist-get 'original_line comment))))
              (gh-ui--section (inline-comment
                               (alist-get 'id comment) resource nil)
                (gh-ui--row
                 "Inline comment by"
                 (gh-ui--styled
                  (gh-core--name (alist-get 'user comment))
                  'gh-author)
                 (gh-ui--styled (format "line %s" line) 'gh-permission))
                (gh-ui--insert-markdown
                 (alist-get 'body comment) context)))))))))

;;;###autoload
(defun gh-pr-view-files (&optional context number)
  "Open changed files page for Pull Request NUMBER."
  (interactive)
  (pcase-let ((`(,current-context ,current-number) (gh-pr--current)))
    (setq context (gh-pr--context (or context current-context))
          number (or number current-number))
    (gh-ui--open-page
     (gh-pr--buffer-name context number "Changed Files")
     context 'pr-files number
     (lambda (success error force)
       (gh-core--collect-async
        (list
         (cons 'pr (lambda (ok fail)
                     (gh-api--pr-get context number ok fail force)))
         (cons 'files (lambda (ok fail)
                        (gh-api--pr-files context number ok fail force)))
         (cons 'review-comments
               (lambda (ok fail)
                 (gh-api--pr-review-comments context number ok fail force))))
        success error))
     (lambda (data) (gh-pr--render-files context number data))
     :setup
     (lambda ()
       (setq gh-pr--view-number number
             gh-buffer-dispatch-function #'gh-pr-dispatch)
       (local-set-key (kbd "c") #'gh-pr-review-comment-add)
       (local-set-key (kbd "C") #'gh-pr-file-comment-add)))))

;;;###autoload
(defun gh-pr-diff (&optional context number)
  "Open complete diff for Pull Request NUMBER asynchronously."
  (interactive)
  (pcase-let ((`(,current-context ,current-number) (gh-pr--current)))
    (setq context (gh-pr--context (or context current-context))
          number (or number current-number))
    (let ((buffer (get-buffer-create (gh-pr--buffer-name context number "Diff"))))
      (with-current-buffer buffer
        (let ((inhibit-read-only t))
          (erase-buffer)
          (insert (propertize "Loading Pull Request diff…\n"
                              'font-lock-face 'gh-loading)))
        (diff-mode)
        (setq buffer-read-only t))
      (funcall gh-display-buffer-function buffer)
      (with-current-buffer buffer
        (gh-api--pr-diff
         context number
         (lambda (text)
           (let ((inhibit-read-only t))
             (erase-buffer) (insert text) (goto-char (point-min))
             (font-lock-ensure)))
         (lambda (error)
           (let ((inhibit-read-only t))
             (erase-buffer) (insert (gh-error-message error) "\n"))))))))

;;; Template and structured editing

;;;###autoload
(defun gh-pr-template-read (context callback)
  "Asynchronously read a Pull Request template in CONTEXT.
CALLBACK receives template text, or an empty string when no template exists."
  (setq context (gh-pr--context context))
  (let ((paths '("pull_request_template.md"
                 ".github/pull_request_template.md"
                 "docs/pull_request_template.md")))
    (cl-labels
        ((try (remaining)
           (if-let* ((path (car remaining)))
               (gh-api--content-get
                context path (gh-context-ref context)
                (lambda (data) (funcall callback (gh-api--decode-content data)))
                (lambda (_error) (try (cdr remaining))))
             (gh-api--content-list
              context ".github/PULL_REQUEST_TEMPLATE" (gh-context-ref context)
              (lambda (items)
                (let* ((files (seq-filter
                               (lambda (item)
                                 (string-suffix-p
                                  ".md" (alist-get 'name item)))
                               items))
                       (choice
                        (and files
                             (completing-read
                              "Pull Request template: "
                              (mapcar (lambda (item)
                                        (alist-get 'name item)) files)
                              nil t))))
                  (if choice
                      (let ((item
                             (seq-find
                              (lambda (entry)
                                (string= choice (alist-get 'name entry)))
                              files)))
                        (gh-api--content-get
                         context (alist-get 'path item)
                         (gh-context-ref context)
                         (lambda (data)
                           (funcall callback (gh-api--decode-content data)))
                         (lambda (_error) (funcall callback ""))))
                    (funcall callback ""))))
              (lambda (_error) (funcall callback ""))))))
      (try paths))))

(defun gh-pr--completion-fetchers (context)
  "Return async completion providers for CONTEXT."
  (let ((names
         (lambda (api key)
           (lambda (success error)
             (funcall api context
                      (lambda (items)
                        (funcall success
                                 (mapcar (lambda (item) (alist-get key item))
                                         items)))
                      error)))))
    (list :users (funcall names #'gh-api--repo-collaborators 'login)
          :labels (funcall names #'gh-api--repo-labels 'name)
          :milestones (funcall names #'gh-api--repo-milestones 'title)
          :branches (funcall names #'gh-api--repo-branches 'name)
          :projects (funcall names #'gh-api--project-list 'title))))

(defun gh-pr--editor-fields (context &optional creating)
  "Return Pull Request editor fields for CONTEXT."
  (let ((fetchers (gh-pr--completion-fetchers context)))
    (append
     `((:name title :required t)
       (:name base :required t :completion-fetch ,(plist-get fetchers :branches)))
     (when creating
       `((:name head :required t :completion-fetch ,(plist-get fetchers :branches))))
     `((:name reviewers :multiple t :completion-fetch ,(plist-get fetchers :users))
       (:name assignees :multiple t :completion-fetch ,(plist-get fetchers :users))
       (:name labels :multiple t :completion-fetch ,(plist-get fetchers :labels))
       (:name milestone :completion-fetch ,(plist-get fetchers :milestones))
       (:name projects :multiple t
        :completion-fetch ,(plist-get fetchers :projects)))
     (when creating '((:name draft :type boolean))))))

;;;###autoload
(defun gh-pr-create (&optional context)
  "Create a Pull Request in CONTEXT with a structured editor."
  (interactive)
  (setq context (gh-pr--context context))
  (gh-pr-template-read
   context
   (lambda (template)
     (gh-edit-open
      (format "*gh: %s · New Pull Request*" (gh-context-repository context))
      (gh-pr--editor-fields context t)
      (list :base (or (gh-context-default-branch context) "main")
            :head (or (gh-context-branch context) "") :draft :json-false)
      template
      (lambda (values body success error)
        (gh-api--pr-create context (plist-put values :body body) success error))
      :after-success
      (lambda (result)
        (when (string-match "/pull/\\([0-9]+\\)" result)
          (gh-pr-view (string-to-number (match-string 1 result)) context)))))))

(defun gh-pr--edit-values (data)
  "Convert Pull Request DATA to editor values."
  (list :title (alist-get 'title data)
        :base (alist-get 'baseRefName data)
        :reviewers (mapcar #'gh-core--name
                           (alist-get 'reviewRequests data))
        :assignees (mapcar #'gh-core--name (alist-get 'assignees data))
        :labels (mapcar #'gh-core--name (alist-get 'labels data))
        :milestone (gh-core--name (alist-get 'milestone data))
        :projects (mapcar #'gh-core--name (alist-get 'projectItems data))))

(defun gh-pr--open-edit-editor (context number data)
  "Open structured editor for Pull Request NUMBER using DATA."
  (let ((original (gh-pr--edit-values data)))
    (gh-edit-open
     (format "*gh: %s · Edit PR #%s*" (gh-context-repository context) number)
     (gh-pr--editor-fields context)
     original (alist-get 'body data)
     (lambda (values body success error)
       (let ((changes (list :title (plist-get values :title)
                            :base (plist-get values :base)
                            :body body :milestone (plist-get values :milestone))))
         (dolist (spec '((:reviewers :add-reviewers :remove-reviewers)
                         (:assignees :add-assignees :remove-assignees)
                         (:labels :add-labels :remove-labels)
                         (:projects :add-projects :remove-projects)))
           (let ((old (plist-get original (car spec)))
                 (new (plist-get values (car spec))))
             (setq changes (plist-put changes (nth 1 spec)
                                      (seq-difference new old #'string=)))
             (setq changes (plist-put changes (nth 2 spec)
                                      (seq-difference old new #'string=)))))
         (gh-api--pr-edit context number changes success error))))))

;;;###autoload
(defun gh-pr-edit (&optional context number)
  "Edit Pull Request NUMBER in CONTEXT."
  (interactive)
  (pcase-let ((`(,current-context ,current-number) (gh-pr--current)))
    (setq context (gh-pr--context (or context current-context))
          number (or number current-number (read-number "Pull Request number: ")))
    (let ((pr (alist-get 'pr gh-ui--data)))
      (if (equal (alist-get 'number pr) number)
          (gh-pr--open-edit-editor context number pr)
        (gh-api--pr-get
         context number (lambda (data) (gh-pr--open-edit-editor context number data))
         #'gh-core--user-error)))))

;;; Review collection

(defun gh-pr--review-key (context number)
  "Return review collection key for CONTEXT and NUMBER."
  (list (gh-context-host context) (gh-context-repository context) number))

(defun gh-pr--collect-review-comment (context number comment)
  "Store COMMENT for Pull Request NUMBER in CONTEXT."
  (let ((key (gh-pr--review-key context number)))
    (puthash key (append (gethash key gh-pr--review-comments) (list comment))
             gh-pr--review-comments)
    (message "Collected review comment (%d total)"
             (length (gethash key gh-pr--review-comments)))))

(defun gh-pr-review-comment-add
    (path line body &optional start-line side context number)
  "Collect a review comment BODY at PATH and LINE.
START-LINE makes a multi-line comment; SIDE defaults to RIGHT."
  (interactive
   (let* ((resource (gh-ui-resource-at-point))
          (path (or (plist-get resource :path) (read-string "Path: ")))
          (line (read-number "End line: " (or (plist-get resource :line) 1)))
          (start (read-number "Start line (same for one line): " line))
          (body (read-string "Review comment: ")))
     (list path line body (unless (= start line) start) "RIGHT")))
  (pcase-let ((`(,current-context ,current-number) (gh-pr--current)))
    (setq context (gh-pr--context (or context current-context))
          number (or number current-number))
    (let ((comment (list :path path :line line :side (or side "RIGHT")
                         :subject-type "LINE"
                          :body body)))
      (when start-line
        (setq comment (plist-put comment :start-line start-line)
              comment (plist-put comment :start-side (or side "RIGHT"))))
      (gh-pr--collect-review-comment context number comment))))

(defun gh-pr-file-comment-add (path body &optional context number)
  "Collect whole-file review comment BODY for PATH."
  (interactive
   (let ((resource (gh-ui-resource-at-point)))
     (list (or (plist-get resource :path) (read-string "Path: "))
           (read-string "File review comment: "))))
  (pcase-let ((`(,current-context ,current-number) (gh-pr--current)))
    (setq context (gh-pr--context (or context current-context))
          number (or number current-number))
    (gh-pr--collect-review-comment
     context number (list :path path :subject-type "FILE" :body body))))

;;;###autoload
(defun gh-pr-review-submit-collected (&optional context number)
  "Submit collected review comments for Pull Request NUMBER."
  (interactive)
  (pcase-let ((`(,current-context ,current-number) (gh-pr--current)))
    (setq context (gh-pr--context (or context current-context))
          number (or number current-number))
    (let* ((key (gh-pr--review-key context number))
           (comments (gethash key gh-pr--review-comments))
           (event-name (completing-read
                        "Review event: " '("COMMENT" "APPROVE" "REQUEST_CHANGES")
                        nil t nil nil "COMMENT"))
           (event (intern (downcase event-name)))
           (body (read-string "Review summary (optional): ")))
      (unless (or comments (not (string-empty-p body)))
        (user-error "No collected comments or review summary"))
      (gh-api--pr-review
       context number event body comments
       (lambda (_)
         (remhash key gh-pr--review-comments)
         (message "Review submitted")
         (when (derived-mode-p 'gh-section-mode) (gh-ui-refresh t)))
       #'gh-core--user-error))))

;;; Actions

(defun gh-pr--current ()
  "Return (CONTEXT NUMBER) for the current Pull Request action."
  (let ((resource (or gh-pr--dispatch-resource (gh-ui-resource-at-point))))
    (list (or (plist-get resource :context) gh-buffer-context)
          (or (plist-get resource :number) gh-pr--view-number))))

;;;###autoload
(defun gh-review-requests ()
  "Select a Pull Request requesting review from the current user."
  (interactive)
  (let ((context (gh-context-resolve)))
    (gh-api--review-requests
     context
     (lambda (items)
       (gh-candidate-select-and-open
        "Review request: "
        (mapcar
         (lambda (item)
           (let ((item-context
                  (gh-context-from-repository
                   (alist-get 'nameWithOwner (alist-get 'repository item))
                   (gh-context-host context))))
             (gh-pr--resource item-context item)))
         items)
        (lambda (item)
          (gh-ui--row
           (concat
            (gh-ui--styled (plist-get item :repository) 'gh-repository)
            (gh-ui--styled (format "#%s" (plist-get item :number))
                           'gh-resource-number))
           (gh-ui--styled (plist-get item :title) 'gh-resource-title)))
        t))
     #'gh-core--user-error)))

(defun gh-pr-comment (body &optional context number)
  "Add conversation BODY to Pull Request NUMBER."
  (interactive (list (read-string "Comment: ")))
  (pcase-let ((`(,current-context ,current-number) (gh-pr--current)))
    (setq context (gh-pr--context (or context current-context))
          number (or number current-number))
    (gh-api--pr-comment
     context number body
     (lambda (_) (message "Comment added")
       (when (derived-mode-p 'gh-section-mode) (gh-ui-refresh t)))
     #'gh-core--user-error)))

(defun gh-pr-close (&optional context number)
  "Close Pull Request NUMBER."
  (interactive)
  (pcase-let ((`(,current-context ,current-number) (gh-pr--current)))
    (setq context (gh-pr--context (or context current-context))
          number (or number current-number))
    (when (gh-core--confirm (format "Close Pull Request #%s? " number))
      (let ((comment (read-string "Closing comment (optional): "))
            (delete-branch (y-or-n-p "Delete branch after closing? ")))
        (gh-api--pr-close
         context number (unless (string-empty-p comment) comment) delete-branch
         (lambda (_) (message "Pull Request #%s closed" number)
           (when (derived-mode-p 'gh-section-mode) (gh-ui-refresh t)))
         #'gh-core--user-error)))))

(defun gh-pr-reopen (&optional context number)
  "Reopen Pull Request NUMBER."
  (interactive)
  (pcase-let ((`(,current-context ,current-number) (gh-pr--current)))
    (setq context (gh-pr--context (or context current-context))
          number (or number current-number))
    (let ((comment (read-string "Reopen comment (optional): ")))
      (gh-api--pr-reopen
       context number (unless (string-empty-p comment) comment)
       (lambda (_) (message "Pull Request #%s reopened" number)
         (when (derived-mode-p 'gh-section-mode) (gh-ui-refresh t)))
       #'gh-core--user-error))))

(defun gh-pr-checkout (&optional context number)
  "Checkout Pull Request NUMBER in the local worktree."
  (interactive)
  (pcase-let ((`(,current-context ,current-number) (gh-pr--current)))
    (setq context (gh-pr--context (or context current-context))
          number (or number current-number))
    (unless (gh-context-root context)
      (user-error "Checkout requires a local repository worktree"))
    (gh-api--pr-checkout
     context number (lambda (_) (message "Checked out Pull Request #%s" number))
     #'gh-core--user-error)))

(defun gh-pr-review (&optional context number)
  "Submit a summary-only review for Pull Request NUMBER."
  (interactive)
  (pcase-let ((`(,current-context ,current-number) (gh-pr--current)))
    (setq context (gh-pr--context (or context current-context))
          number (or number current-number))
    (let* ((event-name (completing-read
                        "Review: " '("COMMENT" "APPROVE" "REQUEST_CHANGES")
                        nil t))
           (event (intern (downcase event-name)))
           (body (read-string "Review body: ")))
      (gh-api--pr-review
       context number event body nil
       (lambda (_) (message "Review submitted")
         (when (derived-mode-p 'gh-section-mode) (gh-ui-refresh t)))
       #'gh-core--user-error))))

(defun gh-pr-merge (&optional context number)
  "Merge Pull Request NUMBER after prompting for strategy."
  (interactive)
  (pcase-let ((`(,current-context ,current-number) (gh-pr--current)))
    (setq context (gh-pr--context (or context current-context))
          number (or number current-number))
    (let ((method (intern (completing-read "Merge method: "
                                           '("merge" "squash" "rebase") nil t)))
          (delete-branch (y-or-n-p "Delete branch after merge? ")))
      (when (gh-core--confirm (format "Merge Pull Request #%s? " number))
        (gh-api--pr-merge
         context number method (list :delete-branch delete-branch)
         (lambda (_) (message "Pull Request #%s merged" number)
           (when (derived-mode-p 'gh-section-mode) (gh-ui-refresh t)))
         #'gh-core--user-error)))))

(defun gh-pr-lock (&optional unlock context number)
  "Lock Pull Request NUMBER, or UNLOCK with prefix argument."
  (interactive "P")
  (pcase-let ((`(,current-context ,current-number) (gh-pr--current)))
    (setq context (gh-pr--context (or context current-context))
          number (or number current-number))
    (gh-api--pr-lock
     context number (not unlock) nil
     (lambda (_) (message "Pull Request #%s %s" number
                          (if unlock "unlocked" "locked"))
       (when (derived-mode-p 'gh-section-mode) (gh-ui-refresh t)))
     #'gh-core--user-error)))

(defun gh-pr-ready (&optional draft context number)
  "Mark Pull Request NUMBER ready, or DRAFT with prefix argument."
  (interactive "P")
  (pcase-let ((`(,current-context ,current-number) (gh-pr--current)))
    (setq context (gh-pr--context (or context current-context))
          number (or number current-number))
    (gh-api--pr-ready
     context number (not draft)
     (lambda (_) (message "Pull Request #%s marked %s" number
                          (if draft "draft" "ready"))
       (when (derived-mode-p 'gh-section-mode) (gh-ui-refresh t)))
     #'gh-core--user-error)))

(defun gh-pr-auto-merge (&optional disable context number)
  "Enable auto-merge for Pull Request NUMBER, or DISABLE with prefix."
  (interactive "P")
  (pcase-let ((`(,current-context ,current-number) (gh-pr--current)))
    (setq context (gh-pr--context (or context current-context))
          number (or number current-number))
    (let ((method (and (not disable)
                       (intern (completing-read
                                "Auto-merge method: " '("squash" "merge" "rebase")
                                nil t)))))
      (gh-api--pr-auto-merge
       context number (not disable) method
       (lambda (_) (message "Auto-merge %s" (if disable "disabled" "enabled"))
         (when (derived-mode-p 'gh-section-mode) (gh-ui-refresh t)))
       #'gh-core--user-error))))

;;;###autoload
(defun gh-link-issue-pr (issue &optional context number)
  "Add `Closes #ISSUE' to Pull Request NUMBER body."
  (interactive (list (read-number "Issue number to close: ")))
  (pcase-let ((`(,current-context ,current-number) (gh-pr--current)))
    (setq context (gh-pr--context (or context current-context))
          number (or number current-number))
    (gh-api--pr-get
     context number
     (lambda (pr)
       (let* ((body (alist-get 'body pr))
              (marker (format "Closes #%s" issue)))
         (if (string-search marker body)
             (message "Pull Request already contains %s" marker)
           (gh-api--pr-edit
            context number (list :body (concat (string-trim-right body)
                                               "\n\n" marker "\n"))
            (lambda (_) (message "Linked Issue #%s" issue)
              (when (derived-mode-p 'gh-section-mode) (gh-ui-refresh t)))
            #'gh-core--user-error))))
     #'gh-core--user-error)))

(transient-define-prefix gh-pr-dispatch ()
  "Pull Request actions."
  [["View/Edit"
    ("g" "Refresh" gh-ui-refresh)
    ("E" "Edit" gh-pr-edit)
    ("c" "Comment" gh-pr-comment)
    ("d" "Diff" gh-pr-diff)
    ("F" "Changed files" gh-pr-view-files)
    ("C" "Commits" gh-pr-view-commits)]
   ["Review"
    ("v" "Review" gh-pr-review)
    ("V" "Submit collected" gh-pr-review-submit-collected)
    ("m" "Merge" gh-pr-merge)
    ("k" "Checkout" gh-pr-checkout)]
   ["State"
    ("x" "Close" gh-pr-close)
    ("o" "Reopen" gh-pr-reopen)
    ("r" "Ready / draft" gh-pr-ready)
    ("a" "Auto-merge" gh-pr-auto-merge)
    ("l" "Lock / unlock" gh-pr-lock)
    ("i" "Link Issue" gh-link-issue-pr)]])

;;; Candidate registration

(gh-candidate-register
 'pr
 :open (lambda (resource)
         (gh-pr-view (plist-get resource :number)
                     (plist-get resource :context)))
 :preview (lambda (resource)
            (gh-pr-view (plist-get resource :number)
                        (plist-get resource :context) t))
 :dispatch (lambda (resource)
             (setq gh-pr--dispatch-resource resource)
             (call-interactively #'gh-pr-dispatch)))

(gh-candidate-register
 'pr-list :open (lambda (resource) (gh-pr-list (plist-get resource :context))))
(gh-candidate-register
 'pr-more :open (lambda (_resource) (gh-pr-load-more)))
(gh-candidate-register
 'pr-commits
 :open (lambda (resource)
         (gh-pr-view-commits (plist-get resource :context)
                             (plist-get resource :number))))
(gh-candidate-register
 'pr-files
 :open (lambda (resource)
         (gh-pr-view-files (plist-get resource :context)
                           (plist-get resource :number))))

(provide 'gh-pr)
;;; gh-pr.el ends here
