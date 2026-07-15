;;; gh-issue.el --- Native Issue workflow for gh.el -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; Author: gh.el contributors
;; Keywords: tools, vc, github
;; Package-Requires: ((emacs "31.1") (transient "0.7.0"))

;;; Commentary:

;; Issue lists, details, templates, structured create/edit, comments, state,
;; pin, lock, and linked development actions.

;;; Code:

(require 'seq)
(require 'subr-x)
(require 'transient)
(require 'gh-api)
(require 'gh-candidate)
(require 'gh-edit)
(require 'gh-ui)

(defvar-local gh-issue--state nil)
(defvar-local gh-issue--params nil)
(defvar-local gh-issue--limit nil)
(defvar-local gh-issue--dispatch-resource nil)

(defun gh-issue--context (&optional context)
  "Resolve repository CONTEXT for an Issue command."
  (gh-context-resolve (or context gh-buffer-context) t))

(defun gh-issue--resource (context data)
  "Create an Issue resource from DATA in CONTEXT."
  (gh-resource-create
   'issue context :number (alist-get 'number data)
   :title (alist-get 'title data)
   :url (alist-get 'url data)))

(defun gh-issue--buffer-name (context &optional number state)
  "Return Issue buffer name for CONTEXT, NUMBER, and STATE."
  (if number
      (format "*gh: %s · Issue #%s*" (gh-context-repository context) number)
    (format "*gh: %s · Issues · %s*" (gh-context-repository context)
            state)))

(defun gh-issue--row-values (data)
  "Return display plist for Issue DATA."
  (let ((state (alist-get 'state data)))
    (list :state (gh-ui--styled (upcase state) (gh-core--state-face state))
          :identifier (gh-ui--styled
                       (format "#%s" (alist-get 'number data))
                       'gh-resource-number)
          :title (gh-ui--styled (alist-get 'title data)
                                'gh-resource-title))))

(defun gh-issue--insert-row (context data)
  "Insert a native Issue row from DATA."
  (let* ((resource (gh-issue--resource context data))
         (number (plist-get resource :number)))
    (gh-ui--section (issue number resource t)
      (gh-ui--format-row (gh-issue--row-values data))
      (gh-ui--insert-header "Author"
                            (gh-core--name (alist-get 'author data))
                            'gh-author)
      (gh-ui--insert-header "Labels"
                            (gh-core--names (alist-get 'labels data))
                            'gh-label)
      (gh-ui--insert-header "Assigned"
                            (gh-core--names (alist-get 'assignees data))
                            'gh-author)
      (gh-ui--insert-header "Comments"
                            (gh-core--comments-count data))
      (gh-ui--insert-header "Created"
                            (gh-core--date (alist-get 'createdAt data))
                            'gh-date)
      (gh-ui--insert-header "Updated"
                            (gh-core--date (alist-get 'updatedAt data))
                            'gh-date))))

(defun gh-issue--render-list (context state data)
  "Render Issue list DATA for CONTEXT and STATE."
  (gh-ui--insert-header "Repository" (gh-context-repository context)
                        'gh-repository
                        (gh-resource-create 'repository context))
  (gh-ui--insert-header "Issues" state)
  (insert "\n")
  (if data
      (dolist (issue data) (gh-issue--insert-row context issue))
    (insert (propertize "No matching issues.\n" 'font-lock-face 'shadow)))
  (gh-ui--section (more 'more (gh-resource-create 'issue-more context) t)
    (format "Load more (current limit %d)" gh-issue--limit)
    (insert "Press RET to double the list limit.\n")))

;;;###autoload
(defun gh-issue-list (&optional context state params)
  "Open an asynchronous Issue list for CONTEXT, STATE, and PARAMS."
  (interactive)
  (setq context (gh-issue--context context)
        state (or state gh-default-issue-state))
  (let ((limit (or (plist-get params :limit) gh-list-limit)))
    (gh-ui--open-page
     (gh-issue--buffer-name context nil state) context 'issue-list state
     (lambda (success error force)
       (gh-api--issue-list
        context (append (list :state state :limit limit) params)
        success error force))
     (lambda (data) (gh-issue--render-list context state data))
     :setup
     (lambda ()
       (setq gh-issue--state state gh-issue--params params gh-issue--limit limit)
       (local-set-key (kbd "c")
                      (lambda () (interactive) (gh-issue-create context)))
       (local-set-key (kbd "t") #'gh-issue-cycle-state)
       (setq gh-buffer-dispatch-function #'gh-issue-dispatch)))))

(defun gh-issue-load-more ()
  "Double the current Issue list limit and refresh."
  (interactive)
  (let ((context gh-buffer-context)
        (state gh-issue--state)
        (params gh-issue--params)
        (limit (* 2 gh-issue--limit)))
    (setq gh-issue--limit limit
          gh-buffer-refresh-function
          (lambda (success error force)
            (gh-api--issue-list
             context (append (list :state state :limit limit) params)
             success error force)))
    (gh-ui-refresh t)))

(defun gh-issue-cycle-state ()
  "Cycle Issue list state between open, closed, and all."
  (interactive)
  (let ((state (pcase gh-issue--state
                 ("open" "closed") ("closed" "all") (_ "open"))))
    (gh-issue-list gh-buffer-context state gh-issue--params)))

;;; Details

(defun gh-issue--render-comment (context comment)
  "Render COMMENT in CONTEXT."
  (let* ((id (alist-get 'id comment))
         (author (or (gh-core--name (alist-get 'author comment)) ""))
         (created (gh-core--date (alist-get 'createdAt comment)))
         (url (alist-get 'url comment))
         (resource (gh-resource-create 'comment context :id id :url url)))
    (gh-ui--section (comment id resource nil)
      (format "Comment by %s · %s"
              (propertize author 'font-lock-face 'gh-author)
              (propertize created 'font-lock-face 'gh-date))
      (gh-ui--insert-markdown (alist-get 'body comment) context))))

(defun gh-issue--render-view (context data)
  "Render Issue DATA in CONTEXT."
  (let* ((resource (gh-issue--resource context data))
         (number (alist-get 'number data))
         (title (alist-get 'title data))
         (state (alist-get 'state data)))
    (insert (propertize (format "#%s " number)
                        'font-lock-face 'gh-resource-number)
            (propertize title 'font-lock-face 'gh-resource-title) "\n")
    (add-text-properties (line-beginning-position 0) (point)
                         (list 'gh-resource resource))
    (gh-ui--insert-header "State" (upcase state) (gh-core--state-face state))
    (gh-ui--insert-header "Author"
                          (gh-core--name (alist-get 'author data))
                          'gh-author)
    (gh-ui--insert-header "Assignees"
                          (gh-core--names (alist-get 'assignees data))
                          'gh-author)
    (gh-ui--insert-header "Labels"
                          (gh-core--names (alist-get 'labels data))
                          'gh-label)
    (gh-ui--insert-header
     "Milestone" (gh-core--name (alist-get 'milestone data)))
    (gh-ui--insert-header
     "Dates" (format "created %s; updated %s"
                     (gh-core--date (alist-get 'createdAt data))
                     (gh-core--date (alist-get 'updatedAt data)))
     'gh-date)
    (insert "\n")
    (gh-ui--section (description 'description resource nil)
      "Description"
      (let ((body (alist-get 'body data)))
        (gh-ui--insert-markdown
         (if (string-empty-p (string-trim (or body "")))
             "No description."
           body)
         context)))
    (dolist (comment (alist-get 'comments data))
      (gh-issue--render-comment context comment))
    (when-let* ((closing-prs
                 (alist-get 'closedByPullRequestsReferences data)))
      (gh-ui--section (linked 'closing-pull-requests nil t)
        "Closed by pull requests"
        (dolist (pr closing-prs)
          (let ((pr-resource
                 (gh-resource-create
                  'pr context :number (alist-get 'number pr)
                  :title (alist-get 'title pr)
                  :url (alist-get 'url pr))))
            (gh-ui--insert-resource-line
             (gh-ui--row
              (gh-ui--styled (format "#%s" (alist-get 'number pr))
                             'gh-resource-number)
              (gh-ui--styled (alist-get 'title pr)
                             'gh-resource-title))
             pr-resource)))))))

(defun gh-issue--setup-view (context number)
  "Install Issue view bindings for CONTEXT and NUMBER."
  (local-set-key (kbd "E") (lambda () (interactive)
                             (gh-issue-edit context number)))
  (local-set-key (kbd "c") (lambda () (interactive)
                             (call-interactively #'gh-issue-comment)))
  (setq gh-buffer-dispatch-function
        (lambda ()
          (setq gh-issue--dispatch-resource
                (gh-resource-create 'issue context :number number))
          (call-interactively #'gh-issue-dispatch))))

;;;###autoload
(defun gh-issue-view (number &optional context preview)
  "Open Issue NUMBER in CONTEXT.  PREVIEW creates a disposable buffer."
  (interactive (list (read-number "Issue number: ")))
  (setq context (gh-issue--context context))
  (gh-ui--open-page
   (if preview
       (format "*gh preview: %s#%s*" (gh-context-repository context) number)
     (gh-issue--buffer-name context number))
   context 'issue number
   (lambda (success error force)
     (gh-api--issue-get context number success error force))
   (lambda (data) (gh-issue--render-view context data))
   :preview preview :setup (lambda () (gh-issue--setup-view context number))))

;;; Templates and editing

(defun gh-issue--completion-fetchers (context)
  "Return asynchronous field completion fetchers for CONTEXT."
  (let ((names
         (lambda (api key)
           (lambda (success error)
             (funcall api context
                      (lambda (items)
                        (funcall success
                                 (mapcar (lambda (item) (alist-get key item))
                                         items)))
                      error)))))
    (list :assignees (funcall names #'gh-api--repo-collaborators 'login)
          :labels (funcall names #'gh-api--repo-labels 'name)
          :milestones (funcall names #'gh-api--repo-milestones 'title)
          :projects (funcall names #'gh-api--project-list 'title))))

(defun gh-issue--editor-fields (context)
  "Return Issue editor fields for CONTEXT."
  (let ((fetchers (gh-issue--completion-fetchers context)))
    `((:name title :required t)
      (:name assignees :multiple t
       :completion-fetch ,(plist-get fetchers :assignees))
      (:name labels :multiple t
       :completion-fetch ,(plist-get fetchers :labels))
      (:name milestone :completion-fetch ,(plist-get fetchers :milestones))
      (:name projects :multiple t
       :completion-fetch ,(plist-get fetchers :projects)))))

(defun gh-issue--template-values (text)
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

(defun gh-issue--open-create-editor (context values body)
  "Open Issue creation editor in CONTEXT with VALUES and BODY."
  (gh-edit-open
   (format "*gh: %s · New Issue*" (gh-context-repository context))
   (gh-issue--editor-fields context) values body
   (lambda (parsed parsed-body success error)
     (gh-api--issue-create context (plist-put parsed :body parsed-body)
                           success error))
   :after-success
   (lambda (result)
     (when (string-match "/issues/\\([0-9]+\\)" result)
       (gh-issue-view (string-to-number (match-string 1 result)) context)))))

(defun gh-issue--choose-template (context items)
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
        (gh-issue--open-create-editor context nil "")
      (let ((item (seq-find (lambda (entry)
                              (string= choice (alist-get 'name entry)))
                            files)))
        (gh-api--content-get
         context (alist-get 'path item) (gh-context-ref context)
         (lambda (file)
           (pcase-let ((`(,values ,body)
                        (gh-issue--template-values
                         (gh-api--decode-content file))))
             (gh-issue--open-create-editor context values body)))
         #'gh-core--user-error)))))

;;;###autoload
(defun gh-issue-create (&optional context)
  "Create an Issue in CONTEXT, using a remote template when available."
  (interactive)
  (setq context (gh-issue--context context))
  (message "Fetching Issue templates…")
  (gh-api--content-list
   context ".github/ISSUE_TEMPLATE" (gh-context-ref context)
   (lambda (items) (gh-issue--choose-template context items))
   (lambda (_error) (gh-issue--open-create-editor context nil ""))))

(defun gh-issue--edit-values (data)
  "Convert Issue DATA to structured edit values."
  (list :title (alist-get 'title data)
        :assignees (mapcar #'gh-core--name (alist-get 'assignees data))
        :labels (mapcar #'gh-core--name (alist-get 'labels data))
        :milestone (gh-core--name (alist-get 'milestone data))
        :projects (mapcar #'gh-core--name (alist-get 'projectItems data))))

(defun gh-issue--open-edit-editor (context number data)
  "Open editor for Issue NUMBER using DATA."
  (let ((original (gh-issue--edit-values data)))
    (gh-edit-open
     (format "*gh: %s · Edit Issue #%s*"
             (gh-context-repository context) number)
     (gh-issue--editor-fields context) original
     (alist-get 'body data)
     (lambda (values body success error)
       (let ((changes (list :title (plist-get values :title) :body body
                            :milestone (plist-get values :milestone))))
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
         (gh-api--issue-edit context number changes success error))))))

;;;###autoload
(defun gh-issue-edit (&optional context number)
  "Edit Issue NUMBER in CONTEXT using a structured buffer."
  (interactive)
  (setq context (gh-issue--context context)
        number (or number (and (eq gh-buffer-resource-kind 'issue)
                               gh-buffer-resource-id)
                   (read-number "Issue number: ")))
  (if (equal (alist-get 'number gh-ui--data) number)
      (gh-issue--open-edit-editor context number gh-ui--data)
    (gh-api--issue-get
     context number
     (lambda (data) (gh-issue--open-edit-editor context number data))
     #'gh-core--user-error)))

;;; Actions

(defun gh-issue--current ()
  "Return (CONTEXT NUMBER) for the current Issue action."
  (let ((resource (or gh-issue--dispatch-resource (gh-ui-resource-at-point))))
    (list (or (plist-get resource :context) gh-buffer-context)
          (or (plist-get resource :number)
              (and (eq gh-buffer-resource-kind 'issue) gh-buffer-resource-id)))))

;;;###autoload
(defun gh-issue-comment (body &optional context number)
  "Add BODY to Issue NUMBER in CONTEXT."
  (interactive (list (read-string "Comment: ")))
  (pcase-let ((`(,current-context ,current-number) (gh-issue--current)))
    (setq context (gh-issue--context (or context current-context))
          number (or number current-number))
    (gh-api--issue-comment context number body
                           (lambda (_) (message "Comment added")
                             (when (derived-mode-p 'gh-section-mode)
                               (gh-ui-refresh t)))
                           #'gh-core--user-error)))

(defun gh-issue-close (&optional context number)
  "Close Issue NUMBER in CONTEXT, prompting for reason and comment."
  (interactive)
  (pcase-let ((`(,current-context ,current-number) (gh-issue--current)))
    (setq context (gh-issue--context (or context current-context))
          number (or number current-number))
    (when (gh-core--confirm (format "Close Issue #%s? " number))
      (let ((reason (completing-read "Reason: " '("completed" "not planned")
                                     nil t nil nil "completed"))
            (comment (read-string "Closing comment (optional): ")))
        (gh-api--issue-close
         context number reason (unless (string-empty-p comment) comment)
         (lambda (_) (message "Issue #%s closed" number)
           (when (derived-mode-p 'gh-section-mode) (gh-ui-refresh t)))
         #'gh-core--user-error)))))

(defun gh-issue-reopen (&optional context number)
  "Reopen Issue NUMBER in CONTEXT."
  (interactive)
  (pcase-let ((`(,current-context ,current-number) (gh-issue--current)))
    (setq context (gh-issue--context (or context current-context))
          number (or number current-number))
    (let ((comment (read-string "Reopen comment (optional): ")))
      (gh-api--issue-reopen
       context number (unless (string-empty-p comment) comment)
       (lambda (_) (message "Issue #%s reopened" number)
         (when (derived-mode-p 'gh-section-mode) (gh-ui-refresh t)))
       #'gh-core--user-error))))

(defun gh-issue-pin (&optional unpin context number)
  "Pin Issue NUMBER, or UNPIN it with prefix argument."
  (interactive "P")
  (pcase-let ((`(,current-context ,current-number) (gh-issue--current)))
    (setq context (gh-issue--context (or context current-context))
          number (or number current-number))
    (gh-api--issue-pin
     context number (not unpin)
     (lambda (_) (message "Issue #%s %s" number (if unpin "unpinned" "pinned"))
       (when (derived-mode-p 'gh-section-mode) (gh-ui-refresh t)))
     #'gh-core--user-error)))

(defun gh-issue-lock (&optional unlock context number)
  "Lock Issue NUMBER, or UNLOCK it with prefix argument."
  (interactive "P")
  (pcase-let ((`(,current-context ,current-number) (gh-issue--current)))
    (setq context (gh-issue--context (or context current-context))
          number (or number current-number))
    (let ((reason (and (not unlock)
                       (completing-read "Lock reason (optional): "
                                        '("off-topic" "too heated" "resolved" "spam")
                                        nil t))))
      (gh-api--issue-lock
       context number (not unlock) (unless (string-empty-p (or reason "")) reason)
       (lambda (_) (message "Issue #%s %s" number
                            (if unlock "unlocked" "locked"))
         (when (derived-mode-p 'gh-section-mode) (gh-ui-refresh t)))
       #'gh-core--user-error))))

;;;###autoload
(defun gh-issue-develop (branch base checkout &optional context number)
  "Create linked development BRANCH from BASE for Issue NUMBER.
With CHECKOUT non-nil, check out the new branch."
  (interactive
   (list (read-string "Branch name: ")
         (read-string "Base: " (or (and gh-buffer-context
                                         (gh-context-default-branch
                                          gh-buffer-context)) "main"))
         (y-or-n-p "Checkout branch? ")))
  (pcase-let ((`(,current-context ,current-number) (gh-issue--current)))
    (setq context (gh-issue--context (or context current-context))
          number (or number current-number))
    (gh-api--issue-develop
     context number branch base checkout
     (lambda (_) (message "Created linked branch %s" branch))
     #'gh-core--user-error)))

(transient-define-prefix gh-issue-dispatch ()
  "Issue actions."
  [["View/Edit"
    ("g" "Refresh" gh-ui-refresh)
    ("E" "Edit" gh-issue-edit)
    ("c" "Comment" gh-issue-comment)
    ("b" "Browse" gh-ui-browse)]
   ["State"
    ("x" "Close" gh-issue-close)
    ("o" "Reopen" gh-issue-reopen)
    ("p" "Pin / unpin" gh-issue-pin)
    ("l" "Lock / unlock" gh-issue-lock)]
   ["Develop"
    ("d" "Create linked branch" gh-issue-develop)]])

;;; Candidate registration

(gh-candidate-register
 'issue
 :open (lambda (resource)
         (gh-issue-view (plist-get resource :number)
                        (plist-get resource :context)))
 :preview (lambda (resource)
            (gh-issue-view (plist-get resource :number)
                           (plist-get resource :context) t))
 :dispatch (lambda (resource)
             (setq gh-issue--dispatch-resource resource)
             (call-interactively #'gh-issue-dispatch)))

(gh-candidate-register
 'issue-list
 :open (lambda (resource) (gh-issue-list (plist-get resource :context))))

(gh-candidate-register 'issue-more :open (lambda (_resource) (gh-issue-load-more)))

(provide 'gh-issue)
;;; gh-issue.el ends here
