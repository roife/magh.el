;;; magh-discussion.el --- GitHub Discussions frontend for magh.el -*- lexical-binding: t; -*-

;; Copyright (C) 2026
;; SPDX-License-Identifier: GPL-2.0-only

;; Author: magh.el contributors
;; Keywords: tools, vc, github

;;; Commentary:

;; Repository Discussion lists and details with bounded comment trees,
;; structured creation/editing, replies, Q&A answers, and close/reopen actions.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'transient)
(require 'magh-api)
(require 'magh-candidate)
(require 'magh-edit)
(require 'magh-ui)

(defvar-local magh-discussion--number nil)
(defvar-local magh-discussion--id nil)
(defvar-local magh-discussion--params nil)
(defvar-local magh-discussion--dispatch-resource nil)

(defun magh-discussion--resource (context data)
  "Create a Discussion resource from DATA."
  (magh-resource-create
   'discussion context :id (alist-get 'id data)
   :number (alist-get 'number data) :title (alist-get 'title data)
   :url (alist-get 'url data) :data data))

(defun magh-discussion--state (data)
  "Return display state for Discussion DATA."
  (cond ((magh-api--true-p (alist-get 'closed data)) "closed")
        ((magh-api--true-p (alist-get 'isAnswered data)) "answered")
        (t "open")))

(defun magh-discussion--insert-row (context data)
  "Insert Discussion DATA in CONTEXT."
  (let* ((resource (magh-discussion--resource context data))
         (state (magh-discussion--state data))
         (category (alist-get 'category data)))
    (magh-ui--section (discussion (alist-get 'number data) resource t)
      (magh-ui--row
       (magh-ui--styled (upcase state) (magh-core--state-face state))
       (magh-ui--styled (format "#%s" (alist-get 'number data))
                        'magh-resource-number)
       (magh-ui--styled (alist-get 'title data) 'magh-resource-title))
      (magh-ui--insert-header "Category" (alist-get 'name category))
      (magh-ui--insert-header
       "Author" (magh-core--name (alist-get 'author data)) 'magh-author)
      (magh-ui--insert-header
       "Comments" (magh-api--json-at data 'comments 'totalCount))
      (magh-ui--insert-header
       "Updated" (magh-core--date (alist-get 'updatedAt data)) 'magh-date))))

(defun magh-discussion--render-list (context data)
  "Render paginated Discussion DATA in CONTEXT."
  (let ((items (magh-page-items data)) (next (magh-page-next data)))
    (magh-ui--insert-header "Repository" (magh-context-repository context)
                            'magh-repository
                            (magh-resource-create 'repository context))
    (magh-ui--insert-header "Discussions" (length items))
    (insert "\n")
    (if items
        (dolist (discussion items)
          (magh-discussion--insert-row context discussion))
      (insert (propertize "No Discussions found.\n" 'font-lock-face 'shadow)))
    (if next
        (magh-ui--section
            (more 'more (magh-resource-create 'discussion-more context) t)
          (format "Load next page (%d loaded)" (length items))
          (insert "Press RET to append more Discussions.\n"))
      (insert (propertize
               (format "End of list (%d Discussions).\n" (length items))
               'font-lock-face 'shadow)))))

;;;###autoload
(defun magh-discussion-list (&optional context params)
  "Open a cursor-paginated Discussion list in CONTEXT."
  (interactive (list (magh-context-read-repository)))
  (setq context (magh-ui--repository-context context))
  (magh-ui--open-page
   (format "*magh: %s · Discussions*" (magh-context-repository context))
   context 'discussion-list (magh-context-repository context)
   (lambda (success error force)
     (magh-api--discussion-page context params nil success error force))
   (lambda (data) (magh-discussion--render-list context data))
   :setup
   (lambda ()
     (setq magh-discussion--params params
           magh-buffer-dispatch-function #'magh-discussion-list-dispatch)
     (local-set-key (kbd "c")
                    (lambda () (interactive)
                      (magh-discussion-create context))))))

(defun magh-discussion-load-more ()
  "Append the next Discussion page."
  (interactive)
  (let ((context magh-buffer-context) (params magh-discussion--params))
    (magh-ui--load-next-page
     (lambda (cursor success error)
       (magh-api--discussion-page
        context params cursor success error))
     "Discussions")))

(defun magh-discussion--comment-resource
    (context discussion-number comment &optional reply)
  "Create comment resource for COMMENT under DISCUSSION-NUMBER."
  (magh-resource-create
   'discussion-comment context :id (alist-get 'id comment)
   :discussion-number discussion-number :discussion-id magh-discussion--id
   :reply reply
   :answer (magh-api--true-p (alist-get 'isAnswer comment))
   :title (format "Comment by %s"
                  (or (magh-core--name (alist-get 'author comment)) "unknown"))
   :url (alist-get 'url comment) :data comment))

(defun magh-discussion--insert-reply (context number reply)
  "Insert REPLY under Discussion NUMBER."
  (let* ((resource (magh-discussion--comment-resource context number reply t))
         (author (or (magh-core--name (alist-get 'author reply)) "unknown")))
    (magh-ui--section (comment (alist-get 'id reply) resource nil)
      (concat (magh-ui--styled "Reply" 'magh-conversation-kind)
              " by " (magh-ui--styled author 'magh-author)
              " · "
              (magh-ui--styled
               (magh-core--date (alist-get 'createdAt reply)) 'magh-date))
      (magh-ui--insert-markdown (alist-get 'body reply) context))))

(defun magh-discussion--insert-truncation (resource label loaded total)
  "Insert a browseable truncation notice for LABEL."
  (when (> (or total 0) loaded)
    (magh-ui--insert-resource-line
     (propertize
      (format "%s truncated: showing %d of %d; press b for GitHub."
              label loaded total)
      'font-lock-face 'warning)
     resource)))

(defun magh-discussion--insert-comment (context number discussion comment)
  "Insert COMMENT and replies for DISCUSSION NUMBER."
  (let* ((resource
          (magh-discussion--comment-resource context number comment))
         (author (or (magh-core--name (alist-get 'author comment)) "unknown"))
         (replies (magh-api--json-at comment 'replies 'nodes))
         (reply-total (magh-api--json-at comment 'replies 'totalCount)))
    (magh-ui--section (comment (alist-get 'id comment) resource nil)
      (concat
       (magh-ui--styled
        (if (magh-api--true-p (alist-get 'isAnswer comment))
            "Answer" "Comment")
        'magh-conversation-kind)
       " by " (magh-ui--styled author 'magh-author)
       " · "
       (magh-ui--styled
        (magh-core--date (alist-get 'createdAt comment)) 'magh-date))
      (magh-ui--insert-markdown (alist-get 'body comment) context)
      (dolist (reply replies)
        (magh-discussion--insert-reply context number reply))
      (magh-discussion--insert-truncation
       discussion "Replies" (length replies) reply-total))))

(defun magh-discussion--render (context data)
  "Render Discussion DATA in CONTEXT."
  (let* ((resource (magh-discussion--resource context data))
         (number (alist-get 'number data))
         (state (magh-discussion--state data))
         (comments (magh-api--json-at data 'comments 'nodes))
         (comment-total (magh-api--json-at data 'comments 'totalCount))
         (category (alist-get 'category data)))
    (let ((start (point)))
      (insert
       (magh-ui--row
        (magh-ui--styled (upcase state) (magh-core--state-face state))
        (magh-ui--styled (format "#%s" number) 'magh-resource-number)
        (magh-ui--styled (alist-get 'title data) 'magh-resource-title))
       "\n")
      (add-text-properties start (point) (list 'magh-resource resource)))
    (magh-ui--insert-header "Category" (alist-get 'name category))
    (magh-ui--insert-header
     "Author" (magh-core--name (alist-get 'author data)) 'magh-author)
    (magh-ui--insert-header
     "Created" (magh-core--date (alist-get 'createdAt data)) 'magh-date)
    (magh-ui--insert-header
     "Updated" (magh-core--date (alist-get 'updatedAt data)) 'magh-date)
    (insert "\n")
    (magh-ui--section (description 'body resource nil)
      "Discussion"
      (magh-ui--insert-markdown (alist-get 'body data) context))
    (magh-ui--section (conversation 'comments nil nil)
      (format "Comments (%s)" (or comment-total 0))
      (if comments
          (dolist (comment comments)
            (magh-discussion--insert-comment context number resource comment))
        (insert (propertize "No comments.\n" 'font-lock-face 'shadow)))
      (magh-discussion--insert-truncation
       resource "Comments" (length comments) comment-total))))

(defun magh-discussion--setup (_context number)
  "Install Discussion NUMBER detail state."
  (setq magh-discussion--number number
        magh-buffer-dispatch-function #'magh-discussion-dispatch)
  (local-set-key (kbd "c") #'magh-discussion-comment)
  (local-set-key (kbd "r") #'magh-discussion-reply)
  (local-set-key (kbd "E") #'magh-discussion-edit))

;;;###autoload
(defun magh-discussion-view (number &optional context preview)
  "Open Discussion NUMBER in CONTEXT."
  (interactive (list (read-number "Discussion number: ")))
  (setq context (magh-ui--repository-context context))
  (magh-ui--open-page
   (if preview
       (format "*magh preview: %s · Discussion #%s*"
               (magh-context-repository context) number)
     (format "*magh: %s · Discussion #%s*"
             (magh-context-repository context) number))
   context 'discussion number
   (lambda (success error force)
     (magh-api--discussion-get context number success error force))
   (lambda (data)
     (setq magh-discussion--id (alist-get 'id data))
     (magh-discussion--render context data))
   :preview preview :setup (lambda () (magh-discussion--setup context number))))

(defun magh-discussion--categories (metadata)
  "Return category nodes from repository METADATA."
  (magh-api--json-at metadata 'discussionCategories 'nodes))

(defun magh-discussion--category-choices (categories)
  "Return editor choice names from CATEGORIES."
  (mapcar (lambda (category) (alist-get 'name category)) categories))

(defun magh-discussion--category-id (categories name)
  "Find category node ID in CATEGORIES by NAME."
  (alist-get
   'id (cl-find name categories :key (lambda (item) (alist-get 'name item))
                :test #'string=)))

(defun magh-discussion--open-create (context metadata)
  "Open Discussion creation editor using repository METADATA."
  (let ((categories (magh-discussion--categories metadata)))
    (unless categories
      (user-error "This repository has no Discussion categories"))
    (magh-edit-open
     (format "*magh: %s · New Discussion*"
             (magh-context-repository context))
     `((:name title :required t)
       (:name category :required t
        :choices ,(magh-discussion--category-choices categories)))
     (list :category (alist-get 'name (car categories))) ""
     (lambda (values body success error)
       (magh-api--discussion-create
        context
        (list :repository-id (alist-get 'id metadata)
              :category-id
              (magh-discussion--category-id
               categories (plist-get values :category))
              :title (plist-get values :title) :body body)
        success error))
     :after-success
     (lambda (result)
       (when-let* ((discussion
                    (magh-api--json-at
                     result 'data 'createDiscussion 'discussion))
                   (number (alist-get 'number discussion)))
         (magh-discussion-view number context))))))

;;;###autoload
(defun magh-discussion-create (&optional context)
  "Create a Discussion in CONTEXT using a structured buffer."
  (interactive (list (magh-context-read-repository)))
  (setq context (magh-ui--repository-context context))
  (magh-api--discussion-categories
   context (lambda (metadata)
             (magh-discussion--open-create context metadata))
   #'magh-core--user-error))

(defun magh-discussion--current ()
  "Return current Discussion resource."
  (or (and (eq (plist-get magh-discussion--dispatch-resource :kind)
               'discussion)
           magh-discussion--dispatch-resource)
      (and (listp magh-ui--data)
           (magh-discussion--resource magh-buffer-context magh-ui--data))))

(defun magh-discussion--open-edit (context data metadata)
  "Open editor for Discussion DATA using category METADATA."
  (let* ((categories (magh-discussion--categories metadata))
         (number (alist-get 'number data)))
    (magh-edit-open
     (format "*magh: %s · Edit Discussion #%s*"
             (magh-context-repository context) number)
     `((:name title :required t)
       (:name category :required t
        :choices ,(magh-discussion--category-choices categories)))
     (list :title (alist-get 'title data)
           :category (magh-api--json-at data 'category 'name))
     (alist-get 'body data)
     (lambda (values body success error)
       (magh-api--discussion-update
        context number (alist-get 'id data)
        (list :title (plist-get values :title) :body body
              :category-id
              (magh-discussion--category-id
               categories (plist-get values :category)))
        success error)))))

;;;###autoload
(defun magh-discussion-edit (&optional context number)
  "Edit Discussion NUMBER in CONTEXT."
  (interactive)
  (let* ((resource (magh-discussion--current))
         (data (or (plist-get resource :data) magh-ui--data)))
    (setq context (magh-ui--repository-context
                   (or context (plist-get resource :context)))
          number (or number (plist-get resource :number)
                     magh-discussion--number
                     (read-number "Discussion number: ")))
    (cl-labels
        ((with-data
           (discussion)
           (magh-api--discussion-categories
            context
            (lambda (metadata)
              (magh-discussion--open-edit context discussion metadata))
            #'magh-core--user-error)))
      (if (and data (alist-get 'body data))
          (with-data data)
        (magh-api--discussion-get
         context number #'with-data #'magh-core--user-error t)))))

(defun magh-discussion--body-editor (name submit)
  "Open a multiline body editor NAME using SUBMIT."
  (magh-edit-open
   name nil nil ""
   (lambda (_values body success error)
     (if (string-empty-p (string-trim body))
         (funcall error
                  (magh-core--error 'magh-invalid-input
                                    "Discussion comment body is required"))
       (funcall submit body success error)))))

;;;###autoload
(defun magh-discussion-comment (&optional context number discussion-id)
  "Add a top-level comment to Discussion NUMBER."
  (interactive)
  (let ((resource (magh-discussion--current)))
    (setq context (magh-ui--repository-context
                   (or context (plist-get resource :context)))
          number (or number (plist-get resource :number)
                     magh-discussion--number
                     (read-number "Discussion number: "))
          discussion-id (or discussion-id (plist-get resource :id)
                            magh-discussion--id))
    (cl-labels
        ((open-editor
           (id)
           (magh-discussion--body-editor
            (format "*magh: Discussion #%s · New Comment*" number)
            (lambda (body success error)
              (magh-api--discussion-comment
               context number id body success error)))))
      (if discussion-id
          (open-editor discussion-id)
        (magh-api--discussion-get
         context number
         (lambda (data) (open-editor (alist-get 'id data)))
         #'magh-core--user-error)))))

(defun magh-discussion--selected-comment ()
  "Return selected Discussion comment resource."
  (let* ((point-resource (magh-ui-resource-at-point))
         (resource
          (if (eq (plist-get point-resource :kind) 'discussion-comment)
              point-resource
            magh-discussion--dispatch-resource)))
    (unless (eq (plist-get resource :kind) 'discussion-comment)
      (user-error "No Discussion comment selected"))
    resource))

;;;###autoload
(defun magh-discussion-reply (&optional comment-resource)
  "Reply to selected COMMENT-RESOURCE."
  (interactive)
  (setq comment-resource
        (or comment-resource (magh-discussion--selected-comment)))
  (when (plist-get comment-resource :reply)
    (user-error "Select a top-level comment to reply"))
  (let ((context (plist-get comment-resource :context))
        (number (plist-get comment-resource :discussion-number))
        (discussion-id (plist-get comment-resource :discussion-id))
        (comment-id (plist-get comment-resource :id)))
    (magh-discussion--body-editor
     (format "*magh: Discussion #%s · Reply*" number)
     (lambda (body success error)
       (magh-api--discussion-comment
        context number discussion-id body success error comment-id)))))

;;;###autoload
(defun magh-discussion-answer-toggle (&optional comment-resource)
  "Mark selected COMMENT-RESOURCE as the Q&A answer, or unmark it."
  (interactive)
  (setq comment-resource
        (or comment-resource (magh-discussion--selected-comment)))
  (when (plist-get comment-resource :reply)
    (user-error "Select a top-level comment as the answer"))
  (let ((marked (not (plist-get comment-resource :answer))))
    (magh-api--discussion-mark-answer
     (plist-get comment-resource :context)
     (plist-get comment-resource :discussion-number)
     (plist-get comment-resource :id) marked
     (lambda (_)
       (message "Discussion answer %s" (if marked "marked" "unmarked"))
       (magh-ui--refresh-if-page))
     #'magh-core--user-error)))

;;;###autoload
(defun magh-discussion-close (&optional reason)
  "Close current Discussion with REASON."
  (interactive)
  (let* ((resource (magh-discussion--current))
         (data (plist-get resource :data)))
    (unless resource (user-error "This command requires a Discussion page"))
    (when (magh-api--true-p (alist-get 'closed data))
      (user-error "Discussion is already closed"))
    (setq reason
          (or reason
              (completing-read
               "Close reason: " '("RESOLVED" "OUTDATED" "DUPLICATE") nil t)))
    (when (magh-core--confirm
           (format "Close Discussion #%s as %s? "
                   (plist-get resource :number) reason))
      (magh-api--discussion-close
       (plist-get resource :context) (plist-get resource :number)
       (plist-get resource :id) reason
       (lambda (_) (message "Discussion closed")
         (magh-ui--refresh-if-page))
       #'magh-core--user-error))))

;;;###autoload
(defun magh-discussion-reopen ()
  "Reopen current Discussion."
  (interactive)
  (let* ((resource (magh-discussion--current))
         (data (plist-get resource :data)))
    (unless resource (user-error "This command requires a Discussion page"))
    (unless (magh-api--true-p (alist-get 'closed data))
      (user-error "Discussion is already open"))
    (magh-api--discussion-reopen
     (plist-get resource :context) (plist-get resource :number)
     (plist-get resource :id)
     (lambda (_) (message "Discussion reopened")
       (magh-ui--refresh-if-page))
     #'magh-core--user-error)))

(transient-define-prefix magh-discussion-list-dispatch ()
  "Discussion list commands."
  [["View"
    ("g" "Refresh" magh-ui-refresh)
    ("m" "Load more" magh-discussion-load-more)]
   ["Create"
    ("c" "New Discussion" magh-discussion-create)]])

(transient-define-prefix magh-discussion-dispatch ()
  "Discussion commands."
  [["Discussion"
    ("g" "Refresh" magh-ui-refresh)
    ("E" "Edit" magh-discussion-edit)
    ("b" "Browse" magh-ui-browse)]
   ["Conversation"
    ("c" "Comment" magh-discussion-comment)
    ("r" "Reply" magh-discussion-reply)
    ("a" "Mark / unmark answer" magh-discussion-answer-toggle)]
   ["State"
    ("x" "Close" magh-discussion-close)
    ("o" "Reopen" magh-discussion-reopen)]])

(magh-candidate-register
 'discussion
 :open (lambda (resource)
         (magh-discussion-view
          (plist-get resource :number) (plist-get resource :context)))
 :preview (lambda (resource)
            (magh-discussion-view
             (plist-get resource :number) (plist-get resource :context) t))
 :dispatch (lambda (resource)
             (setq magh-discussion--dispatch-resource resource)
             (call-interactively #'magh-discussion-dispatch)))

(magh-candidate-register
 'discussion-comment
 :open (lambda (resource) (magh-resource-browse resource))
 :dispatch (lambda (resource)
             (setq magh-discussion--dispatch-resource resource)
             (call-interactively #'magh-discussion-dispatch)))

(magh-candidate-register
 'discussion-list
 :open (lambda (resource)
         (magh-discussion-list (plist-get resource :context))))

(magh-candidate-register
 'discussion-more :open (lambda (_resource) (magh-discussion-load-more)))

(provide 'magh-discussion)
;;; magh-discussion.el ends here
