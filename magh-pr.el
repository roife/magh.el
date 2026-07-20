;;; magh-pr.el --- Pull Request and review workflow for magh.el -*- lexical-binding: t; -*-

;; Copyright (C) 2026
;; SPDX-License-Identifier: GPL-2.0-only

;; Author: magh.el contributors
;; Keywords: tools, vc, github

;;; Commentary:

;; Pull Request list/detail pages, templates, structured editing, changed
;; files, checks, review collection/submission, checkout, merge, and state
;; actions.  Cross-resource navigation is registered through magh-candidate.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'transient)
(require 'magh-api)
(require 'magh-candidate)
(require 'magh-edit)
(require 'magh-ui)

(defvar-local magh-pr--state nil)
(defvar-local magh-pr--params nil)
(defvar-local magh-pr--limit nil)
(defvar-local magh-pr--dispatch-resource nil)
(defvar-local magh-pr--view-number nil)

(defun magh-pr--current (&optional context number)
  "Return resolved (CONTEXT NUMBER) for the current Pull Request action."
  (let ((resource (or magh-pr--dispatch-resource (magh-ui-resource-at-point))))
    (list (magh-ui--repository-context
           (or context (plist-get resource :context) magh-buffer-context))
          (or number (plist-get resource :number) magh-pr--view-number))))

(defun magh-pr--buffer-name (context &optional number suffix)
  "Return Pull Request buffer name for CONTEXT, NUMBER, and SUFFIX."
  (format "*magh: %s · %s%s*"
          (magh-context-repository context)
          (if number (format "PR #%s" number) "Pull requests")
          (if suffix (concat " · " suffix) "")))

(defun magh-pr--resource (context data)
  "Create Pull Request resource from DATA in CONTEXT."
  (magh-resource-create
   'pr context :number (alist-get 'number data)
   :title (alist-get 'title data)
   :url (alist-get 'url data)))

(defun magh-pr--row-values (data)
  "Return compact row values for Pull Request DATA."
  (let ((state (if (alist-get 'isDraft data)
                   "DRAFT" (alist-get 'state data)))
        (review (alist-get 'reviewDecision data)))
    (list :state (magh-ui--styled (upcase state) (magh-core--state-face state))
          :identifier (magh-ui--styled
                       (format "#%s" (alist-get 'number data))
                       'magh-resource-number)
          :title (magh-ui--styled (alist-get 'title data)
                                'magh-resource-title)
          :review (magh-ui--styled review (magh-core--state-face review)))))

(defun magh-pr--insert-row (context data)
  "Insert Pull Request DATA as a native section row."
  (let* ((resource (magh-pr--resource context data))
         (number (plist-get resource :number)))
    (magh-ui--section (pr number resource t)
      (magh-ui--format-row (magh-pr--row-values data)
                           '(:state :review :identifier :title))
      (magh-ui--insert-header
       "Branches" (format "%s → %s"
                          (alist-get 'headRefName data)
                          (alist-get 'baseRefName data))
       'magh-branch)
      (magh-ui--insert-header "Author"
                            (magh-core--name (alist-get 'author data))
                            'magh-author)
      (magh-ui--insert-header "Review"
                            (or (alist-get 'reviewDecision data) "—")
                            (magh-core--state-face
                             (alist-get 'reviewDecision data)))
      (magh-ui--insert-header "Labels"
                            (magh-core--names (alist-get 'labels data))
                            'magh-label)
      (magh-ui--insert-header "Comments"
                            (magh-core--comments-count data))
      (magh-ui--insert-header "Created"
                            (magh-core--date (alist-get 'createdAt data))
                            'magh-date)
      (magh-ui--insert-header "Updated"
                            (magh-core--date (alist-get 'updatedAt data))
                            'magh-date))))

(defun magh-pr--render-list (context state data)
  "Render Pull Request list DATA in CONTEXT for STATE."
  (let ((items (if (magh-page-p data) (magh-page-items data) data))
        (next (and (magh-page-p data) (magh-page-next data))))
    (magh-ui--insert-header "Repository" (magh-context-repository context)
                          'magh-repository
                          (magh-resource-create 'repository context))
    (magh-ui--insert-header "Pull requests" state)
    (insert "\n")
    (if items
        (dolist (pr items) (magh-pr--insert-row context pr))
      (insert (propertize "No matching pull requests.\n"
                          'font-lock-face 'shadow)))
    (if next
        (magh-ui--section (more 'more (magh-resource-create 'pr-more context) t)
          (format "Load next page (%d loaded)" (length items))
          (insert "Press RET to append more pull requests.\n"))
      (insert (propertize (format "End of list (%d items).\n" (length items))
                          'font-lock-face 'shadow)))))

;;;###autoload
(defun magh-pr-list (&optional context state params)
  "Open Pull Request list for CONTEXT, STATE, and PARAMS."
  (interactive (list (magh-context-read-repository)))
  (setq context (magh-ui--repository-context context)
        state (or state magh-default-pr-state))
  (let ((limit (or (plist-get params :limit) magh-list-limit)))
    (magh-ui--open-page
     (magh-pr--buffer-name context) context 'pr-list state
     (lambda (success error force)
       (magh-api--pr-page
        context (append (list :state state :limit limit) params)
        nil success error force))
     (lambda (data) (magh-pr--render-list context state data))
     :setup
     (lambda ()
       (setq magh-pr--state state magh-pr--params params magh-pr--limit limit)
       (local-set-key (kbd "c")
                      (lambda () (interactive) (magh-pr-create context)))
       (local-set-key (kbd "t") #'magh-pr-cycle-state)
       (setq magh-buffer-dispatch-function #'magh-pr-dispatch)))))

(defun magh-pr-load-more ()
  "Append the next page to the current Pull Request list."
  (interactive)
  (let ((context magh-buffer-context)
        (state magh-pr--state)
        (params magh-pr--params)
        (limit magh-pr--limit))
    (magh-ui--load-next-page
     (lambda (cursor success error)
       (magh-api--pr-page
        context (append (list :state state :limit limit) params)
        cursor success error))
     "pull requests")))

(defun magh-pr-cycle-state ()
  "Cycle Pull Request list state."
  (interactive)
  (magh-pr-list magh-buffer-context
              (pcase magh-pr--state
                ("open" "closed") ("closed" "merged")
                ("merged" "all") (_ "open"))
              magh-pr--params))

;;; Details

(defun magh-pr--fetch-view (context number success error force)
  "Fetch all native detail data for Pull Request NUMBER."
  (magh-core--collect-async
   (list
    (cons 'pr (lambda (ok fail)
                (magh-api--pr-get context number ok fail force)))
    (cons 'commits (lambda (ok fail)
                     (magh-api--pr-commits context number ok fail force)))
    (cons 'files (lambda (ok fail)
                   (magh-api--pr-files context number ok fail force)))
    (cons 'reviews (lambda (ok fail)
                     (magh-api--pr-reviews context number ok fail force)))
    (cons 'review-comments
          (lambda (ok fail)
            (magh-api--pr-review-comments context number ok fail force))))
   success error))

(defun magh-pr--commit-resource (context commit)
  "Create Commit resource from COMMIT in CONTEXT."
  (magh-resource-create
   'commit context :sha (alist-get 'sha commit)
   :title (alist-get 'message (alist-get 'commit commit))
   :url (alist-get 'html_url commit)
   :data commit))

(defun magh-pr--review-resource (context number &rest properties)
  "Create a Commit Review resource for Pull Request NUMBER.
PROPERTIES may identify a file, review, or inline comment within the review."
  (apply #'magh-resource-create 'commit-review context :number number properties))

(defun magh-pr--check-resource (context check)
  "Create Run or web CHECK resource."
  (let ((name (or (alist-get 'name check) (alist-get 'context check)))
        (url (or (alist-get 'detailsUrl check)
                 (alist-get 'targetUrl check))))
    (if (and url (string-match "/actions/runs/\\([0-9]+\\)" url))
        (magh-resource-create
         'run context :id (string-to-number (match-string 1 url))
         :title name :url url)
      (magh-resource-create
       'check context :title name :url url))))

(defun magh-pr--conversation-items (pr reviews inline-comments)
  "Return sorted conversation items from PR, REVIEWS, and INLINE-COMMENTS.
Inline comments belonging to a submitted review are stored on that review as
`inlineComments'.  Empty COMMENTED reviews are omitted."
  (let (associated review-items)
    (dolist (review reviews)
      (let* ((review-id (alist-get 'id review))
             (state (upcase (or (alist-get 'state review) "")))
             (body (string-trim (or (alist-get 'body review) "")))
             (comments
              (seq-filter
               (lambda (comment)
                 (equal (alist-get 'pull_request_review_id comment) review-id))
               inline-comments)))
        (unless (equal state "PENDING")
          (setq associated (append comments associated))
          (when (or comments (not (string-empty-p body))
                    (not (member state '("" "COMMENTED"))))
            (push (cons 'review
                        (cons (cons 'inlineComments comments) review))
                  review-items)))))
    (sort
     (append
      (mapcar (lambda (item) (cons 'comment item))
              (alist-get 'comments pr))
      (nreverse review-items)
      (mapcar (lambda (item) (cons 'inline item))
              (seq-remove (lambda (comment) (memq comment associated))
                          inline-comments)))
     (lambda (a b)
       (string< (or (alist-get 'createdAt (cdr a))
                    (alist-get 'submittedAt (cdr a))
                    (alist-get 'submitted_at (cdr a))
                    (alist-get 'created_at (cdr a)) "")
                (or (alist-get 'createdAt (cdr b))
                    (alist-get 'submittedAt (cdr b))
                    (alist-get 'submitted_at (cdr b))
                    (alist-get 'created_at (cdr b)) ""))))))

(defun magh-pr--insert-conversation-inline-comment (context number data)
  "Insert inline review comment DATA for Pull Request NUMBER in CONTEXT."
  (let* ((id (alist-get 'id data))
         (author (magh-core--name (alist-get 'user data)))
         (date (magh-core--date (alist-get 'created_at data)))
         (path (alist-get 'path data))
         (line (or (alist-get 'line data) (alist-get 'original_line data)))
         (resource
          (magh-pr--review-resource
           context number :comment-id id :path path :line line
           :side (or (alist-get 'side data)
                     (alist-get 'original_side data))))
         (heading
          (concat
           (magh-ui--styled "Inline comment" 'magh-conversation-kind)
           " by " (or (magh-ui--styled author 'magh-author) "")
           " · " (or (magh-ui--styled date 'magh-date) ""))))
    (magh-ui--section (inline-comment id resource nil)
      heading
      (when path
        (magh-ui--insert-header
         "Location" (format "%s:%s" path line) 'magh-file resource))
      (magh-ui--insert-markdown (alist-get 'body data) context))))

(defun magh-pr--render-conversation (context number items)
  "Render Pull Request NUMBER conversation ITEMS in CONTEXT."
  (magh-ui--section (conversation 'conversation nil nil)
    (format "Conversation (%d)" (length items))
    (dolist (item items)
      (pcase-let* ((`(,kind . ,data) item)
                   (id (alist-get 'id data))
                   (author (magh-core--name
                            (or (alist-get 'author data)
                                (alist-get 'user data))))
                   (date (magh-core--date
                          (or (alist-get 'createdAt data)
                              (alist-get 'submittedAt data)
                              (alist-get 'submitted_at data)
                              (alist-get 'created_at data))))
                   (state (alist-get 'state data))
                   (path (alist-get 'path data))
                   (line (or (alist-get 'line data)
                             (alist-get 'original_line data)))
                   (inline-comments (alist-get 'inlineComments data))
                   (resource
                    (pcase kind
                      ('review
                       (magh-pr--review-resource
                        context number :review-id id))
                      ('inline
                       (magh-pr--review-resource
                        context number :comment-id id :path path :line line
                        :side (or (alist-get 'side data)
                                  (alist-get 'original_side data))))
                      (_ (magh-resource-create 'comment context :id id))))
                   (heading
                    (concat
                     (magh-ui--styled
                      (pcase kind ('review "Review") ('inline "Inline comment")
                        (_ "Comment"))
                      'magh-conversation-kind)
                     " by " (or (magh-ui--styled author 'magh-author) "")
                     (if state
                         (concat " " (magh-ui--styled
                                       state (magh-core--state-face state))) "")
                     " · " (or (magh-ui--styled date 'magh-date) "")
                     (if (and (eq kind 'review) inline-comments)
                         (format " · %d inline comment%s"
                                 (length inline-comments)
                                 (if (length= inline-comments 1) "" "s"))
                       ""))))
        (pcase kind
          ('inline
           (magh-pr--insert-conversation-inline-comment context number data))
          ('review
           (magh-ui--section (comment (cons 'review id) resource t)
             heading
             (let ((body (or (alist-get 'body data) "")))
               (unless (string-empty-p (string-trim body))
                 (magh-ui--insert-markdown body context)
                 (when inline-comments (insert "\n"))))
             (dolist (comment inline-comments)
               (magh-pr--insert-conversation-inline-comment
                context number comment))))
          (_
           (magh-ui--section (comment id resource nil)
             heading
             (when path
               (magh-ui--insert-header
                "Location" (format "%s:%s" path line) 'magh-file resource))
             (magh-ui--insert-markdown (alist-get 'body data) context))))))))

(defun magh-pr--render-view (context result)
  "Render Pull Request detail RESULT in CONTEXT."
  (let* ((pr (alist-get 'pr result))
         (commits (alist-get 'commits result))
         (files (alist-get 'files result))
         (reviews (or (alist-get 'reviews result) (alist-get 'reviews pr)))
         (inline-comments (alist-get 'review-comments result))
         (checks (alist-get 'statusCheckRollup pr))
         (resource (magh-pr--resource context pr))
         (number (alist-get 'number pr))
         (head-name (alist-get 'headRefName pr))
         (base-ref (alist-get 'baseRefName pr))
         (state (if (alist-get 'isDraft pr)
                    "DRAFT" (alist-get 'state pr))))
    (insert (propertize (format "#%s " number)
                        'font-lock-face 'magh-resource-number)
            (propertize (alist-get 'title pr)
                        'font-lock-face 'magh-resource-title) "\n")
    (add-text-properties (line-beginning-position 0) (point)
                         (list 'magh-resource resource))
    (magh-ui--insert-header "State" state (magh-core--state-face state))
    (magh-ui--insert-header "Author"
                          (magh-core--name (alist-get 'author pr))
                          'magh-author)
    (magh-ui--insert-header "Branches" (format "%s → %s" head-name base-ref)
                          'magh-branch)
    (let ((review (alist-get 'reviewDecision pr)))
      (magh-ui--insert-header "Review" (or review "—")
                            (and review (magh-core--state-face review))))
    (magh-ui--insert-header "Labels"
                          (magh-core--names (alist-get 'labels pr))
                          'magh-label)
    (insert "\n")
    (magh-ui--section (description 'description resource nil)
      "Description"
      (let ((body (alist-get 'body pr)))
        (magh-ui--insert-markdown
         (if (string-empty-p (string-trim (or body "")))
             "No description."
           body)
         context)))
    (magh-ui--section (commits 'commits
                             (magh-resource-create 'pr-commits context
                                                 :number number) nil)
      (format "Commits (%d)" (length commits))
      (dolist (commit commits)
        (let* ((commit-resource (magh-pr--commit-resource context commit))
               (sha (plist-get commit-resource :sha))
               (commit-data (alist-get 'commit commit))
               (message (car (string-lines
                              (alist-get 'message commit-data))))
               (author (or (magh-core--name (alist-get 'author commit))
                           (magh-core--name (alist-get 'author commit-data)))))
          (magh-ui--section (commit sha commit-resource t)
            (magh-ui--row
             (magh-ui--styled (substring sha 0 10) 'magh-hash)
             (magh-ui--styled message 'magh-resource-title))
            (magh-ui--insert-header "Author" author 'magh-author)
            (magh-ui--insert-header
             "Committed" (magh-core--date
                           (alist-get 'date (alist-get 'committer commit-data)))
             'magh-date)))))
    (magh-ui--section (files 'files
                           (magh-pr--review-resource context number) nil)
      (format "Files (%d)" (length files))
      (dolist (file files)
        (let ((file-resource
               (magh-pr--review-resource
                context number :path (alist-get 'filename file))))
          (magh-ui--section (file (alist-get 'filename file)
                                file-resource t)
            (magh-ui--row
             (magh-ui--styled (alist-get 'filename file) 'magh-file)
             (magh-ui--styled
              (format "+%s" (alist-get 'additions file))
              'magh-added)
             (magh-ui--styled
              (format "-%s" (alist-get 'deletions file))
              'magh-removed))))))
    (magh-ui--section (checks 'checks nil nil)
      (format "Checks (%d)" (length checks))
      (dolist (check checks)
        (let* ((check-resource (magh-pr--check-resource context check))
               (status (or (alist-get 'conclusion check)
                           (alist-get 'status check)))
               (name (magh-resource-title check-resource)))
          (magh-ui--section (check name check-resource t)
            (magh-ui--row
             (magh-ui--styled (upcase status) (magh-core--state-face status))
             (magh-ui--styled name 'magh-workflow))))))
    (magh-pr--render-conversation
     context number (magh-pr--conversation-items pr reviews inline-comments))))

(defun magh-pr--setup-view (context number)
  "Install detail keys for Pull Request NUMBER in CONTEXT."
  (setq magh-pr--view-number number)
  (local-set-key (kbd "E") (lambda () (interactive) (magh-pr-edit context number)))
  (local-set-key (kbd "C")
                 (lambda () (interactive) (magh-pr-view-commits context number)))
  (local-set-key (kbd "F")
                 (lambda () (interactive) (magh-pr-view-files context number)))
  (local-set-key (kbd "d")
                 (lambda () (interactive) (magh-pr-diff context number)))
  (setq magh-buffer-dispatch-function
        (lambda ()
          (setq magh-pr--dispatch-resource
                (magh-resource-create 'pr context :number number))
          (call-interactively #'magh-pr-dispatch))))

;;;###autoload
(defun magh-pr-view (number &optional context preview)
  "Open Pull Request NUMBER in CONTEXT.  PREVIEW creates a disposable page."
  (interactive (list (read-number "Pull Request number: ")))
  (setq context (magh-ui--repository-context context))
  (magh-ui--open-page
   (if preview
       (format "*magh preview: %s#%s*" (magh-context-repository context) number)
     (magh-pr--buffer-name context number))
   context 'pr number
   (lambda (success error force)
     (magh-pr--fetch-view context number success error force))
   (lambda (data) (magh-pr--render-view context data))
   :preview preview :setup (lambda () (magh-pr--setup-view context number))))

;;; Commits, files, diff

;;;###autoload
(defun magh-pr-view-commits (&optional context number)
  "Select a commit belonging to Pull Request NUMBER."
  (interactive)
  (pcase-let ((`(,context ,number) (magh-pr--current context number)))
    (magh-api--pr-commits
     context number
     (lambda (commits)
       (magh-candidate-select-and-open
        "Commit: "
        (mapcar (lambda (commit) (magh-pr--commit-resource context commit))
                commits)
        (lambda (item)
          (let ((message
                 (alist-get 'message
                            (alist-get 'commit (plist-get item :data)))))
            (magh-ui--row
             (magh-ui--styled (substring (plist-get item :sha) 0 10) 'magh-hash)
             (magh-ui--styled (car (string-lines message))
                            'magh-resource-title))))
        t))
     #'magh-core--user-error)))

(defun magh-pr--render-files (context number result)
  "Render changed files RESULT for Pull Request NUMBER."
  (let* ((files (alist-get 'files result))
         (comments (alist-get 'review-comments result)))
    (magh-ui--insert-header "Repository" (magh-context-repository context)
                          'magh-repository)
    (magh-ui--insert-header "Pull Request" (format "#%s changed files" number))
    (insert "\n")
    (dolist (file files)
      (let* ((path (alist-get 'filename file))
             (resource (magh-pr--review-resource context number :path path))
             (file-comments
              (seq-filter (lambda (comment)
                            (equal (alist-get 'path comment) path))
                          comments)))
        (magh-ui--section (file path resource nil)
          (magh-ui--row
           (magh-ui--styled path 'magh-file)
           (magh-ui--styled
            (format "+%s" (alist-get 'additions file))
            'magh-added)
           (magh-ui--styled
            (format "-%s" (alist-get 'deletions file))
            'magh-removed))
          (if-let* ((patch (alist-get 'patch file)))
              (magh-ui--insert-diff patch)
            (insert (propertize "Binary or oversized diff unavailable.\n"
                                'font-lock-face 'shadow)))
          (dolist (comment file-comments)
            (let* ((line (or (alist-get 'line comment)
                             (alist-get 'original_line comment)))
                   (comment-resource
                   (magh-pr--review-resource
                    context number :comment-id (alist-get 'id comment)
                    :path path
                    :line line
                    :side (or (alist-get 'side comment)
                              (alist-get 'original_side comment)))))
              (magh-ui--section (inline-comment
                               (alist-get 'id comment) comment-resource nil)
                (magh-ui--row
                 (concat
                  (magh-ui--styled "Inline comment" 'magh-conversation-kind)
                  " by")
                 (magh-ui--styled
                  (magh-core--name (alist-get 'user comment))
                  'magh-author)
                 (magh-ui--styled (format "line %s" line) 'magh-permission))
                (magh-ui--insert-markdown
                 (alist-get 'body comment) context)))))))))

;;;###autoload
(defun magh-pr-view-files (&optional context number)
  "Open changed files page for Pull Request NUMBER."
  (interactive)
  (pcase-let ((`(,context ,number) (magh-pr--current context number)))
    (magh-ui--open-page
     (magh-pr--buffer-name context number "Changed Files")
     context 'pr-files number
     (lambda (success error force)
       (magh-core--collect-async
        (list
         (cons 'files (lambda (ok fail)
                        (magh-api--pr-files context number ok fail force)))
         (cons 'review-comments
               (lambda (ok fail)
                 (magh-api--pr-review-comments context number ok fail force))))
        success error))
     (lambda (data) (magh-pr--render-files context number data))
     :setup
     (lambda ()
       (setq magh-pr--view-number number
             magh-buffer-dispatch-function #'magh-pr-dispatch)
       (local-set-key (kbd "v") #'magh-pr-review)))))

;;;###autoload
(defun magh-pr-diff (&optional context number)
  "Open complete diff for Pull Request NUMBER asynchronously."
  (interactive)
  (pcase-let ((`(,context ,number) (magh-pr--current context number)))
    (let ((buffer (get-buffer-create (magh-pr--buffer-name context number "Diff"))))
      (with-current-buffer buffer
        (let ((inhibit-read-only t))
          (erase-buffer)
          (insert (propertize "Loading Pull Request diff…\n"
                              'font-lock-face 'magh-loading)))
        (diff-mode)
        (setq buffer-read-only t))
      (funcall magh-display-buffer-function buffer)
      (with-current-buffer buffer
        (magh-api--pr-diff
         context number
         (lambda (text)
           (let ((inhibit-read-only t))
             (erase-buffer) (insert text) (goto-char (point-min))
             (font-lock-ensure)))
         (lambda (error)
           (let ((inhibit-read-only t))
             (erase-buffer) (insert (magh-error-message error) "\n"))))))))

;;; Template and structured editing

;;;###autoload
(defun magh-pr-template-read (context callback)
  "Asynchronously read a Pull Request template in CONTEXT.
CALLBACK receives template text, or an empty string when no template exists."
  (setq context (magh-ui--repository-context context))
  (let ((paths '("pull_request_template.md"
                 ".github/pull_request_template.md"
                 "docs/pull_request_template.md")))
    (cl-labels
        ((try (remaining)
           (if-let* ((path (car remaining)))
               (magh-api--content-get
                context path (magh-context-ref context)
                (lambda (data) (funcall callback (magh-api--decode-content data)))
                (lambda (_error) (try (cdr remaining))))
             (magh-api--content-get
              context ".github/PULL_REQUEST_TEMPLATE" (magh-context-ref context)
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
                        (magh-api--content-get
                         context (alist-get 'path item)
                         (magh-context-ref context)
                         (lambda (data)
                           (funcall callback (magh-api--decode-content data)))
                         (lambda (_error) (funcall callback ""))))
                    (funcall callback ""))))
              (lambda (_error) (funcall callback ""))))))
      (try paths))))

(defun magh-pr--editor-fields (context &optional creating)
  "Return Pull Request editor fields for CONTEXT."
  (let ((branches (magh-edit--completion-fetcher
                   #'magh-api--repo-branches context 'name))
        (users (magh-edit--completion-fetcher
                #'magh-api--repo-collaborators context 'login))
        (labels (magh-edit--completion-fetcher
                 #'magh-api--repo-labels context 'name))
        (milestones (magh-edit--completion-fetcher
                     #'magh-api--repo-milestones context 'title))
        (projects (magh-edit--completion-fetcher
                   #'magh-api--project-list context 'title)))
    (append
     `((:name title :required t)
       (:name base :required t :completion-fetch ,branches))
     (when creating
       `((:name head :required t :completion-fetch ,branches)))
     `((:name reviewers :multiple t :completion-fetch ,users)
       (:name assignees :multiple t :completion-fetch ,users)
       (:name labels :multiple t :completion-fetch ,labels)
       (:name milestone :completion-fetch ,milestones)
       (:name projects :multiple t
        :completion-fetch ,projects))
     (when creating '((:name draft :type boolean))))))

;;;###autoload
(defun magh-pr-create (&optional context)
  "Create a Pull Request in CONTEXT with a structured editor."
  (interactive (list (magh-context-read-repository)))
  (setq context (magh-ui--repository-context context))
  (magh-pr-template-read
   context
   (lambda (template)
     (magh-edit-open
      (format "*magh: %s · New Pull Request*" (magh-context-repository context))
      (magh-pr--editor-fields context t)
      (list :base (or (magh-context-default-branch context) "main")
            :head (or (magh-context-branch context) "") :draft :json-false)
      template
      (lambda (values body success error)
        (magh-api--pr-create context (plist-put values :body body) success error))
      :after-success
      (lambda (result)
        (when (string-match "/pull/\\([0-9]+\\)" result)
          (magh-pr-view (string-to-number (match-string 1 result)) context)))))))

(defun magh-pr--edit-values (data)
  "Convert Pull Request DATA to editor values."
  (list :title (alist-get 'title data)
        :base (alist-get 'baseRefName data)
        :reviewers (mapcar #'magh-core--name
                           (alist-get 'reviewRequests data))
        :assignees (mapcar #'magh-core--name (alist-get 'assignees data))
        :labels (mapcar #'magh-core--name (alist-get 'labels data))
        :milestone (magh-core--name (alist-get 'milestone data))
        :projects (mapcar #'magh-core--name (alist-get 'projectItems data))))

(defun magh-pr--open-edit-editor (context number data)
  "Open structured editor for Pull Request NUMBER using DATA."
  (let ((original (magh-pr--edit-values data)))
    (magh-edit-open
     (format "*magh: %s · Edit PR #%s*" (magh-context-repository context) number)
     (magh-pr--editor-fields context)
     original (alist-get 'body data)
     (lambda (values body success error)
       (let* ((old-milestone (plist-get original :milestone))
              (new-milestone (plist-get values :milestone))
              (changes (list :title (plist-get values :title)
                             :base (plist-get values :base)
                             :body body)))
         (unless (equal old-milestone new-milestone)
           (setq changes
                 (if new-milestone
                     (plist-put changes :milestone new-milestone)
                   (plist-put changes :remove-milestone t))))
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
         (magh-api--pr-edit context number changes success error))))))

;;;###autoload
(defun magh-pr-edit (&optional context number)
  "Edit Pull Request NUMBER in CONTEXT."
  (interactive)
  (pcase-let ((`(,context ,number) (magh-pr--current context number)))
    (setq number (or number (read-number "Pull Request number: ")))
    (let ((pr (alist-get 'pr magh-ui--data)))
      (if (equal (alist-get 'number pr) number)
          (magh-pr--open-edit-editor context number pr)
        (magh-api--pr-get
         context number (lambda (data) (magh-pr--open-edit-editor context number data))
         #'magh-core--user-error)))))

;;; Actions

;;;###autoload
(defun magh-review-requests ()
  "Select a Pull Request requesting review from the current user."
  (interactive)
  (let ((context (magh-context-resolve)))
    (magh-api--review-requests
     context
     (lambda (items)
       (magh-candidate-select-and-open
        "Review request: "
        (mapcar
         (lambda (item)
           (let ((item-context
                  (magh-context-from-repository
                   (alist-get 'nameWithOwner (alist-get 'repository item))
                   (magh-context-host context))))
             (magh-pr--resource item-context item)))
         items)
        (lambda (item)
          (magh-ui--row
           (concat
            (magh-ui--styled (plist-get item :repository) 'magh-repository)
            (magh-ui--styled (format "#%s" (plist-get item :number))
                           'magh-resource-number))
           (magh-ui--styled (plist-get item :title) 'magh-resource-title)))
        t))
     #'magh-core--user-error)))

(defun magh-pr-comment (body &optional context number)
  "Add conversation BODY to Pull Request NUMBER."
  (interactive (list (read-string "Comment: ")))
  (pcase-let ((`(,context ,number) (magh-pr--current context number)))
    (magh-api--pr-comment
     context number body
     (lambda (_) (message "Comment added")
       (magh-ui--refresh-if-page))
     #'magh-core--user-error)))

(defun magh-pr-close (&optional context number)
  "Close Pull Request NUMBER."
  (interactive)
  (pcase-let ((`(,context ,number) (magh-pr--current context number)))
    (when (magh-core--confirm (format "Close Pull Request #%s? " number))
      (let ((comment (read-string "Closing comment (optional): "))
            (delete-branch (y-or-n-p "Delete branch after closing? ")))
        (magh-api--pr-close
         context number (unless (string-empty-p comment) comment) delete-branch
         (lambda (_) (message "Pull Request #%s closed" number)
           (magh-ui--refresh-if-page))
         #'magh-core--user-error)))))

(defun magh-pr-reopen (&optional context number)
  "Reopen Pull Request NUMBER."
  (interactive)
  (pcase-let ((`(,context ,number) (magh-pr--current context number)))
    (let ((comment (read-string "Reopen comment (optional): ")))
      (magh-api--pr-reopen
       context number (unless (string-empty-p comment) comment)
       (lambda (_) (message "Pull Request #%s reopened" number)
         (magh-ui--refresh-if-page))
       #'magh-core--user-error))))

(defun magh-pr-checkout (&optional context number)
  "Checkout Pull Request NUMBER in the local worktree."
  (interactive)
  (pcase-let ((`(,context ,number) (magh-pr--current context number)))
    (unless (magh-context-root context)
      (user-error "Checkout requires a local repository worktree"))
    (magh-api--pr-checkout
     context number (lambda (_) (message "Checked out Pull Request #%s" number))
     #'magh-core--user-error)))

(defun magh-pr-review (&optional context number)
  "Open the Commit Review page for the latest head of Pull Request NUMBER."
  (interactive)
  (pcase-let ((`(,context ,number) (magh-pr--current context number)))
    (magh-resource-open
     (magh-resource-create 'commit-review context :number number))))

(defun magh-pr-merge (&optional context number)
  "Merge Pull Request NUMBER after prompting for strategy."
  (interactive)
  (pcase-let ((`(,context ,number) (magh-pr--current context number)))
    (let ((method (intern (completing-read "Merge method: "
                                           '("merge" "squash" "rebase") nil t)))
          (delete-branch (y-or-n-p "Delete branch after merge? ")))
      (when (magh-core--confirm (format "Merge Pull Request #%s? " number))
        (magh-api--pr-merge
         context number method (list :delete-branch delete-branch)
         (lambda (_) (message "Pull Request #%s merged" number)
           (magh-ui--refresh-if-page))
         #'magh-core--user-error)))))

(defun magh-pr-lock (&optional unlock context number)
  "Lock Pull Request NUMBER, or UNLOCK with prefix argument."
  (interactive "P")
  (pcase-let ((`(,context ,number) (magh-pr--current context number)))
    (magh-api--pr-lock
     context number (not unlock) nil
     (lambda (_) (message "Pull Request #%s %s" number
                          (if unlock "unlocked" "locked"))
       (magh-ui--refresh-if-page))
     #'magh-core--user-error)))

(defun magh-pr-ready (&optional draft context number)
  "Mark Pull Request NUMBER ready, or DRAFT with prefix argument."
  (interactive "P")
  (pcase-let ((`(,context ,number) (magh-pr--current context number)))
    (magh-api--pr-ready
     context number (not draft)
     (lambda (_) (message "Pull Request #%s marked %s" number
                          (if draft "draft" "ready"))
       (magh-ui--refresh-if-page))
     #'magh-core--user-error)))

(defun magh-pr-auto-merge (&optional disable context number)
  "Enable auto-merge for Pull Request NUMBER, or DISABLE with prefix."
  (interactive "P")
  (pcase-let ((`(,context ,number) (magh-pr--current context number)))
    (let ((method (and (not disable)
                       (intern (completing-read
                                "Auto-merge method: " '("squash" "merge" "rebase")
                                nil t)))))
      (magh-api--pr-auto-merge
       context number (not disable) method
       (lambda (_) (message "Auto-merge %s" (if disable "disabled" "enabled"))
         (magh-ui--refresh-if-page))
       #'magh-core--user-error))))

;;;###autoload
(defun magh-link-issue-pr (issue &optional context number)
  "Add `Closes #ISSUE' to Pull Request NUMBER body."
  (interactive (list (read-number "Issue number to close: ")))
  (pcase-let ((`(,context ,number) (magh-pr--current context number)))
    (magh-api--pr-get
     context number
     (lambda (pr)
       (let* ((body (alist-get 'body pr))
              (marker (format "Closes #%s" issue)))
         (if (string-search marker body)
             (message "Pull Request already contains %s" marker)
           (magh-api--pr-edit
            context number (list :body (concat (string-trim-right body)
                                               "\n\n" marker "\n"))
            (lambda (_) (message "Linked Issue #%s" issue)
              (magh-ui--refresh-if-page))
            #'magh-core--user-error))))
     #'magh-core--user-error)))

(transient-define-prefix magh-pr-dispatch ()
  "Pull Request actions."
  [["View/Edit"
    ("g" "Refresh" magh-ui-refresh)
    ("E" "Edit" magh-pr-edit)
    ("c" "Comment" magh-pr-comment)
    ("d" "Diff" magh-pr-diff)
    ("F" "Changed files" magh-pr-view-files)
    ("C" "Commits" magh-pr-view-commits)]
   ["Review"
    ("v" "Review code" magh-pr-review)
    ("m" "Merge" magh-pr-merge)
    ("k" "Checkout" magh-pr-checkout)]
   ["State"
    ("x" "Close" magh-pr-close)
    ("o" "Reopen" magh-pr-reopen)
    ("r" "Ready / draft" magh-pr-ready)
    ("a" "Auto-merge" magh-pr-auto-merge)
    ("l" "Lock / unlock" magh-pr-lock)
    ("i" "Link Issue" magh-link-issue-pr)]])

;;; Candidate registration

(magh-candidate-register
 'pr
 :open (lambda (resource)
         (magh-pr-view (plist-get resource :number)
                     (plist-get resource :context)))
 :preview (lambda (resource)
            (magh-pr-view (plist-get resource :number)
                        (plist-get resource :context) t))
 :dispatch (lambda (resource)
             (setq magh-pr--dispatch-resource resource)
             (call-interactively #'magh-pr-dispatch)))

(magh-candidate-register
 'pr-list :open (lambda (resource) (magh-pr-list (plist-get resource :context))))
(magh-candidate-register
 'pr-more :open (lambda (_resource) (magh-pr-load-more)))
(magh-candidate-register
 'pr-commits
 :open (lambda (resource)
         (magh-pr-view-commits (plist-get resource :context)
                             (plist-get resource :number))))
(magh-candidate-register
 'pr-files
 :open (lambda (resource)
         (magh-pr-view-files (plist-get resource :context)
                           (plist-get resource :number))))

(provide 'magh-pr)
;;; magh-pr.el ends here
