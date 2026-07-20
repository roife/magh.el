;;; magh-issue.el --- Native Issue workflow for magh.el -*- lexical-binding: t; -*-

;; Copyright (C) 2026
;; SPDX-License-Identifier: GPL-2.0-only

;; Author: magh.el contributors
;; Keywords: tools, vc, github

;;; Commentary:

;; Issue lists, details, templates, structured create/edit, comments, state,
;; pin, lock, and linked development actions.

;;; Code:

(require 'seq)
(require 'subr-x)
(require 'transient)
(require 'magh-api)
(require 'magh-candidate)
(require 'magh-edit)
(require 'magh-ui)

(defvar-local magh-issue--state nil)
(defvar-local magh-issue--params nil)
(defvar-local magh-issue--limit nil)
(defvar-local magh-issue--dispatch-resource nil)

(defun magh-issue--resource (context data)
  "Create an Issue resource from DATA in CONTEXT."
  (magh-resource-create
   'issue context :number (alist-get 'number data)
   :title (alist-get 'title data)
   :url (alist-get 'url data)))

(defun magh-issue--buffer-name (context &optional number)
  "Return Issue buffer name for CONTEXT and optional NUMBER."
  (if number
      (format "*magh: %s · Issue #%s*" (magh-context-repository context) number)
    (format "*magh: %s · Issues*" (magh-context-repository context))))

(defun magh-issue--row-values (data)
  "Return display plist for Issue DATA."
  (let ((state (alist-get 'state data)))
    (list :state (magh-ui--styled (upcase state) (magh-core--state-face state))
          :identifier (magh-ui--styled
                       (format "#%s" (alist-get 'number data))
                       'magh-resource-number)
          :title (magh-ui--styled (alist-get 'title data)
                                'magh-resource-title))))

(defun magh-issue--insert-row (context data)
  "Insert a native Issue row from DATA."
  (let* ((resource (magh-issue--resource context data))
         (number (plist-get resource :number)))
    (magh-ui--section (issue number resource t)
      (magh-ui--format-row (magh-issue--row-values data))
      (magh-ui--insert-header "Author"
                            (magh-core--name (alist-get 'author data))
                            'magh-author)
      (magh-ui--insert-header "Labels"
                            (magh-core--names (alist-get 'labels data))
                            'magh-label)
      (magh-ui--insert-header "Assigned"
                            (magh-core--names (alist-get 'assignees data))
                            'magh-author)
      (magh-ui--insert-header "Comments"
                            (magh-core--comments-count data))
      (magh-ui--insert-header "Created"
                            (magh-core--date (alist-get 'createdAt data))
                            'magh-date)
      (magh-ui--insert-header "Updated"
                            (magh-core--date (alist-get 'updatedAt data))
                            'magh-date))))

(defun magh-issue--render-list (context state data)
  "Render Issue list DATA for CONTEXT and STATE."
  (let ((items (if (magh-page-p data) (magh-page-items data) data))
        (next (and (magh-page-p data) (magh-page-next data))))
    (magh-ui--insert-header "Repository" (magh-context-repository context)
                          'magh-repository
                          (magh-resource-create 'repository context))
    (magh-ui--insert-header "Issues" state)
    (insert "\n")
    (if items
        (dolist (issue items) (magh-issue--insert-row context issue))
      (insert (propertize "No matching issues.\n" 'font-lock-face 'shadow)))
    (if next
        (magh-ui--section (more 'more (magh-resource-create 'issue-more context) t)
          (format "Load next page (%d loaded)" (length items))
          (insert "Press RET to append more issues.\n"))
      (insert (propertize (format "End of list (%d items).\n" (length items))
                          'font-lock-face 'shadow)))))

;;;###autoload
(defun magh-issue-list (&optional context state params)
  "Open an asynchronous Issue list for CONTEXT, STATE, and PARAMS."
  (interactive (list (magh-context-read-repository)))
  (setq context (magh-ui--repository-context context)
        state (or state magh-default-issue-state))
  (let ((limit (or (plist-get params :limit) magh-list-limit)))
    (magh-ui--open-page
     (magh-issue--buffer-name context) context 'issue-list state
     (lambda (success error force)
       (magh-api--issue-page
        context (append (list :state state :limit limit) params)
        nil success error force))
     (lambda (data) (magh-issue--render-list context state data))
     :setup
     (lambda ()
       (setq magh-issue--state state magh-issue--params params magh-issue--limit limit)
       (local-set-key (kbd "c")
                      (lambda () (interactive) (magh-issue-create context)))
       (local-set-key (kbd "t") #'magh-issue-cycle-state)
       (setq magh-buffer-dispatch-function #'magh-issue-dispatch)))))

(defun magh-issue-load-more ()
  "Append the next page to the current Issue list."
  (interactive)
  (let ((context magh-buffer-context)
        (state magh-issue--state)
        (params magh-issue--params)
        (limit magh-issue--limit))
    (magh-ui--load-next-page
     (lambda (cursor success error)
       (magh-api--issue-page
        context (append (list :state state :limit limit) params)
        cursor success error))
     "issues")))

(defun magh-issue-cycle-state ()
  "Cycle Issue list state between open, closed, and all."
  (interactive)
  (let ((state (pcase magh-issue--state
                 ("open" "closed") ("closed" "all") (_ "open"))))
    (magh-issue-list magh-buffer-context state magh-issue--params)))

;;; Details

(defun magh-issue--render-comment (context comment)
  "Render COMMENT in CONTEXT."
  (let* ((id (alist-get 'id comment))
         (author (or (magh-core--name (alist-get 'author comment)) ""))
         (created (magh-core--date (alist-get 'createdAt comment)))
         (url (alist-get 'url comment))
         (resource (magh-resource-create 'comment context :id id :url url)))
    (magh-ui--section (comment id resource nil)
      (concat (magh-ui--styled "Comment" 'magh-conversation-kind)
              " by "
              (magh-ui--styled author 'magh-author)
              " · "
              (magh-ui--styled created 'magh-date))
      (magh-ui--insert-markdown (alist-get 'body comment) context))))

(defun magh-issue--render-view (context data)
  "Render Issue DATA in CONTEXT."
  (let* ((resource (magh-issue--resource context data))
         (number (alist-get 'number data))
         (title (alist-get 'title data))
         (state (alist-get 'state data)))
    (insert (propertize (format "#%s " number)
                        'font-lock-face 'magh-resource-number)
            (propertize title 'font-lock-face 'magh-resource-title) "\n")
    (add-text-properties (line-beginning-position 0) (point)
                         (list 'magh-resource resource))
    (magh-ui--insert-header "State" (upcase state) (magh-core--state-face state))
    (magh-ui--insert-header "Author"
                          (magh-core--name (alist-get 'author data))
                          'magh-author)
    (magh-ui--insert-header "Assignees"
                          (magh-core--names (alist-get 'assignees data))
                          'magh-author)
    (magh-ui--insert-header "Labels"
                          (magh-core--names (alist-get 'labels data))
                          'magh-label)
    (magh-ui--insert-header
     "Milestone" (magh-core--name (alist-get 'milestone data)))
    (magh-ui--insert-header
     "Dates" (format "created %s; updated %s"
                     (magh-core--date (alist-get 'createdAt data))
                     (magh-core--date (alist-get 'updatedAt data)))
     'magh-date)
    (insert "\n")
    (magh-ui--section (description 'description resource nil)
      "Description"
      (let ((body (alist-get 'body data)))
        (magh-ui--insert-markdown
         (if (string-empty-p (string-trim (or body "")))
             "No description."
           body)
         context)))
    (dolist (comment (alist-get 'comments data))
      (magh-issue--render-comment context comment))
    (when-let* ((closing-prs
                 (alist-get 'closedByPullRequestsReferences data)))
      (magh-ui--section (linked 'closing-pull-requests nil t)
        "Closed by pull requests"
        (dolist (pr closing-prs)
          (let ((pr-resource
                 (magh-resource-create
                  'pr context :number (alist-get 'number pr)
                  :title (alist-get 'title pr)
                  :url (alist-get 'url pr))))
            (magh-ui--insert-resource-line
             (magh-ui--row
              (magh-ui--styled (format "#%s" (alist-get 'number pr))
                             'magh-resource-number)
              (magh-ui--styled (alist-get 'title pr)
                             'magh-resource-title))
             pr-resource)))))))

(defun magh-issue--setup-view (context number)
  "Install Issue view bindings for CONTEXT and NUMBER."
  (local-set-key (kbd "E") (lambda () (interactive)
                             (magh-issue-edit context number)))
  (local-set-key (kbd "c") (lambda () (interactive)
                             (call-interactively #'magh-issue-comment)))
  (setq magh-buffer-dispatch-function
        (lambda ()
          (setq magh-issue--dispatch-resource
                (magh-resource-create 'issue context :number number))
          (call-interactively #'magh-issue-dispatch))))

;;;###autoload
(defun magh-issue-view (number &optional context preview)
  "Open Issue NUMBER in CONTEXT.  PREVIEW creates a disposable buffer."
  (interactive (list (read-number "Issue number: ")))
  (setq context (magh-ui--repository-context context))
  (magh-ui--open-page
   (if preview
       (format "*magh preview: %s#%s*" (magh-context-repository context) number)
     (magh-issue--buffer-name context number))
   context 'issue number
   (lambda (success error force)
     (magh-api--issue-get context number success error force))
   (lambda (data) (magh-issue--render-view context data))
   :preview preview :setup (lambda () (magh-issue--setup-view context number))))

;;; Templates and editing

(defun magh-issue--editor-fields (context)
  "Return Issue editor fields for CONTEXT."
  (let ((assignees (magh-edit--completion-fetcher
                    #'magh-api--repo-collaborators context 'login))
        (labels (magh-edit--completion-fetcher
                 #'magh-api--repo-labels context 'name))
        (milestones (magh-edit--completion-fetcher
                     #'magh-api--repo-milestones context 'title))
        (projects (magh-edit--completion-fetcher
                   #'magh-api--project-list context 'title)))
    `((:name title :required t)
      (:name assignees :multiple t
       :completion-fetch ,assignees)
      (:name labels :multiple t
       :completion-fetch ,labels)
      (:name milestone :completion-fetch ,milestones)
      (:name projects :multiple t
       :completion-fetch ,projects))))

(defun magh-issue--template-values (text)
  "Parse Markdown issue template TEXT into (VALUES BODY)."
  (let (values body)
    (if (string-match "\\`---[ \t]*\n\\([[:ascii:][:nonascii:]]*?\\)\n---[ \t]*\n" text)
        (let ((header (match-string 1 text)))
          (setq body (substring text (match-end 0)))
          (dolist (line (split-string header "\n" t))
            (when (string-match "^\\([[:alnum:]_-]+\\):[ \t]*\\(.*\\)$" line)
              (let ((key (intern (format ":%s" (match-string 1 line))))
                    (value (string-trim (match-string 2 line) "[ \t\"']+"
                                        "[ \t\"']+")))
                (when (memq key '(:title :labels :assignees))
                  (setq values
                        (plist-put values key
                                   (if (memq key '(:labels :assignees))
                                       (mapcar #'string-trim
                                               (split-string value "," t))
                                     value))))))))
      (setq body text))
    (list values body)))

(defun magh-issue--open-create-editor (context values body)
  "Open Issue creation editor in CONTEXT with VALUES and BODY."
  (magh-edit-open
   (format "*magh: %s · New Issue*" (magh-context-repository context))
   (magh-issue--editor-fields context) values body
   (lambda (parsed parsed-body success error)
     (magh-api--issue-create context (plist-put parsed :body parsed-body)
                           success error))
   :after-success
   (lambda (result)
     (when (string-match "/issues/\\([0-9]+\\)" result)
       (magh-issue-view (string-to-number (match-string 1 result)) context)))))

(defun magh-issue--choose-template (context items)
  "Choose a Markdown template from ITEMS and open it in CONTEXT."
  (let* ((files (seq-filter
                 (lambda (item)
                   (and (string= (alist-get 'type item) "file")
                        (string-suffix-p ".md" (alist-get 'name item))))
                 items))
         (choices (cons "No template" (mapcar (lambda (item)
                                                (alist-get 'name item))
                                              files)))
         (choice (completing-read "Issue template: " choices nil t)))
    (if (string= choice "No template")
        (magh-issue--open-create-editor context nil "")
      (let ((item (seq-find (lambda (entry)
                              (string= choice (alist-get 'name entry)))
                            files)))
        (magh-api--content-get
         context (alist-get 'path item) (magh-context-ref context)
         (lambda (file)
           (pcase-let ((`(,values ,body)
                        (magh-issue--template-values
                         (magh-api--decode-content file))))
             (magh-issue--open-create-editor context values body)))
         #'magh-core--user-error)))))

;;;###autoload
(defun magh-issue-create (&optional context)
  "Create an Issue in CONTEXT, using a remote template when available."
  (interactive (list (magh-context-read-repository)))
  (setq context (magh-ui--repository-context context))
  (message "Fetching Issue templates…")
  (magh-api--content-get
   context ".github/ISSUE_TEMPLATE" (magh-context-ref context)
   (lambda (items) (magh-issue--choose-template context items))
   (lambda (_error) (magh-issue--open-create-editor context nil ""))))

(defun magh-issue--edit-values (data)
  "Convert Issue DATA to structured edit values."
  (list :title (alist-get 'title data)
        :assignees (mapcar #'magh-core--name (alist-get 'assignees data))
        :labels (mapcar #'magh-core--name (alist-get 'labels data))
        :milestone (magh-core--name (alist-get 'milestone data))
        :projects (mapcar #'magh-core--name (alist-get 'projectItems data))))

(defun magh-issue--open-edit-editor (context number data)
  "Open editor for Issue NUMBER using DATA."
  (let ((original (magh-issue--edit-values data)))
    (magh-edit-open
     (format "*magh: %s · Edit Issue #%s*"
             (magh-context-repository context) number)
     (magh-issue--editor-fields context) original
     (alist-get 'body data)
     (lambda (values body success error)
       (let* ((old-milestone (plist-get original :milestone))
              (new-milestone (plist-get values :milestone))
              (changes (list :title (plist-get values :title) :body body)))
         (unless (equal old-milestone new-milestone)
           (setq changes
                 (if new-milestone
                     (plist-put changes :milestone new-milestone)
                   (plist-put changes :remove-milestone t))))
         (dolist (spec '((:assignees :add-assignees :remove-assignees)
                         (:labels :add-labels :remove-labels)
                         (:projects :add-projects :remove-projects)))
           (let ((old (plist-get original (car spec)))
                 (new (plist-get values (car spec))))
             (setq changes
                   (plist-put changes (nth 1 spec)
                              (seq-difference new old #'string=)))
             (setq changes
                   (plist-put changes (nth 2 spec)
                              (seq-difference old new #'string=)))))
         (magh-api--issue-edit context number changes success error))))))

;;;###autoload
(defun magh-issue-edit (&optional context number)
  "Edit Issue NUMBER in CONTEXT using a structured buffer."
  (interactive)
  (setq context (magh-ui--repository-context context)
        number (or number (and (eq magh-buffer-resource-kind 'issue)
                               magh-buffer-resource-id)
                   (read-number "Issue number: ")))
  (if (equal (alist-get 'number magh-ui--data) number)
      (magh-issue--open-edit-editor context number magh-ui--data)
    (magh-api--issue-get
     context number
     (lambda (data) (magh-issue--open-edit-editor context number data))
     #'magh-core--user-error)))

;;; Actions

(defun magh-issue--current ()
  "Return (CONTEXT NUMBER) for the current Issue action."
  (let ((resource (or magh-issue--dispatch-resource (magh-ui-resource-at-point))))
    (list (or (plist-get resource :context) magh-buffer-context)
          (or (plist-get resource :number)
              (and (eq magh-buffer-resource-kind 'issue) magh-buffer-resource-id)))))

;;;###autoload
(defun magh-issue-comment (body &optional context number)
  "Add BODY to Issue NUMBER in CONTEXT."
  (interactive (list (read-string "Comment: ")))
  (pcase-let ((`(,current-context ,current-number) (magh-issue--current)))
    (setq context (magh-ui--repository-context (or context current-context))
          number (or number current-number))
    (magh-api--issue-comment context number body
                           (lambda (_) (message "Comment added")
                             (magh-ui--refresh-if-page))
                           #'magh-core--user-error)))

(defun magh-issue-close (&optional context number)
  "Close Issue NUMBER in CONTEXT, prompting for reason and comment."
  (interactive)
  (pcase-let ((`(,current-context ,current-number) (magh-issue--current)))
    (setq context (magh-ui--repository-context (or context current-context))
          number (or number current-number))
    (when (magh-core--confirm (format "Close Issue #%s? " number))
      (let ((reason (completing-read "Reason: " '("completed" "not planned")
                                     nil t nil nil "completed"))
            (comment (read-string "Closing comment (optional): ")))
        (magh-api--issue-close
         context number reason (unless (string-empty-p comment) comment)
         (lambda (_) (message "Issue #%s closed" number)
           (magh-ui--refresh-if-page))
         #'magh-core--user-error)))))

(defun magh-issue-reopen (&optional context number)
  "Reopen Issue NUMBER in CONTEXT."
  (interactive)
  (pcase-let ((`(,current-context ,current-number) (magh-issue--current)))
    (setq context (magh-ui--repository-context (or context current-context))
          number (or number current-number))
    (let ((comment (read-string "Reopen comment (optional): ")))
      (magh-api--issue-reopen
       context number (unless (string-empty-p comment) comment)
       (lambda (_) (message "Issue #%s reopened" number)
         (magh-ui--refresh-if-page))
       #'magh-core--user-error))))

(defun magh-issue-pin (&optional unpin context number)
  "Pin Issue NUMBER, or UNPIN it with prefix argument."
  (interactive "P")
  (pcase-let ((`(,current-context ,current-number) (magh-issue--current)))
    (setq context (magh-ui--repository-context (or context current-context))
          number (or number current-number))
    (magh-api--issue-pin
     context number (not unpin)
     (lambda (_) (message "Issue #%s %s" number (if unpin "unpinned" "pinned"))
       (magh-ui--refresh-if-page))
     #'magh-core--user-error)))

(defun magh-issue-lock (&optional unlock context number)
  "Lock Issue NUMBER, or UNLOCK it with prefix argument."
  (interactive "P")
  (pcase-let ((`(,current-context ,current-number) (magh-issue--current)))
    (setq context (magh-ui--repository-context (or context current-context))
          number (or number current-number))
    (let ((reason (and (not unlock)
                       (completing-read "Lock reason (optional): "
                                        '("off-topic" "too heated" "resolved" "spam")
                                        nil t))))
      (magh-api--issue-lock
       context number (not unlock) (unless (string-empty-p (or reason "")) reason)
       (lambda (_) (message "Issue #%s %s" number
                            (if unlock "unlocked" "locked"))
         (magh-ui--refresh-if-page))
       #'magh-core--user-error))))

;;;###autoload
(defun magh-issue-develop (branch base checkout &optional context number)
  "Create linked development BRANCH from BASE for Issue NUMBER.
With CHECKOUT non-nil, check out the new branch."
  (interactive
   (list (read-string "Branch name: ")
         (read-string "Base: " (or (and magh-buffer-context
                                         (magh-context-default-branch
                                          magh-buffer-context)) "main"))
         (y-or-n-p "Checkout branch? ")))
  (pcase-let ((`(,current-context ,current-number) (magh-issue--current)))
    (setq context (magh-ui--repository-context (or context current-context))
          number (or number current-number))
    (magh-api--issue-develop
     context number branch base checkout
     (lambda (_) (message "Created linked branch %s" branch))
     #'magh-core--user-error)))

(transient-define-prefix magh-issue-dispatch ()
  "Issue actions."
  [["View/Edit"
    ("g" "Refresh" magh-ui-refresh)
    ("E" "Edit" magh-issue-edit)
    ("c" "Comment" magh-issue-comment)
    ("b" "Browse" magh-ui-browse)]
   ["State"
    ("x" "Close" magh-issue-close)
    ("o" "Reopen" magh-issue-reopen)
    ("p" "Pin / unpin" magh-issue-pin)
    ("l" "Lock / unlock" magh-issue-lock)]
   ["Develop"
    ("d" "Create linked branch" magh-issue-develop)]])

;;; Candidate registration

(magh-candidate-register
 'issue
 :open (lambda (resource)
         (magh-issue-view (plist-get resource :number)
                        (plist-get resource :context)))
 :preview (lambda (resource)
            (magh-issue-view (plist-get resource :number)
                           (plist-get resource :context) t))
 :dispatch (lambda (resource)
             (setq magh-issue--dispatch-resource resource)
             (call-interactively #'magh-issue-dispatch)))

(magh-candidate-register
 'issue-list
 :open (lambda (resource) (magh-issue-list (plist-get resource :context))))

(magh-candidate-register 'issue-more :open (lambda (_resource) (magh-issue-load-more)))

(provide 'magh-issue)
;;; magh-issue.el ends here
