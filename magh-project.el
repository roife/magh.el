;;; magh-project.el --- GitHub Projects frontend for magh.el -*- lexical-binding: t; -*-

;; Copyright (C) 2026
;; SPDX-License-Identifier: GPL-2.0-only

;; Author: magh.el contributors
;; Keywords: tools, vc, github

;;; Commentary:

;; Owner-level Project lists and native Project details, including Project
;; metadata, field metadata, items, draft issues, and one-field-at-a-time
;; updates through the GitHub CLI.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'transient)
(require 'magh-api)
(require 'magh-candidate)
(require 'magh-edit)
(require 'magh-ui)

(defvar-local magh-project--owner nil)
(defvar-local magh-project--number nil)
(defvar-local magh-project--id nil)
(defvar-local magh-project--dispatch-resource nil)

(defun magh-project--default-owner (context)
  "Return the default Project owner for CONTEXT."
  (or (magh-context-owner context) "@me"))

(defun magh-project--resource (context owner data)
  "Create a Project resource for OWNER from DATA."
  (magh-resource-create
   'project context :owner owner :number (alist-get 'number data)
   :id (alist-get 'id data) :title (alist-get 'title data)
   :url (alist-get 'url data) :data data))

(defun magh-project--render-list (context owner projects)
  "Render PROJECTS owned by OWNER."
  (magh-ui--insert-header "Owner" owner 'magh-author)
  (magh-ui--insert-header "Projects" (length projects))
  (insert "\n")
  (if projects
      (dolist (project projects)
        (let* ((resource (magh-project--resource context owner project))
               (closed (magh-api--true-p (alist-get 'closed project))))
          (magh-ui--section
              (project (alist-get 'number project) resource t)
            (magh-ui--row
             (magh-ui--styled (if closed "CLOSED" "OPEN")
                              (if closed 'magh-closed-state 'magh-open-state))
             (magh-ui--styled
              (format "#%s" (alist-get 'number project))
              'magh-resource-number)
             (magh-ui--styled (alist-get 'title project)
                              'magh-resource-title))
            (magh-ui--insert-header
             "Visibility" (if (magh-api--true-p (alist-get 'public project))
                              "public" "private")
             'magh-permission)
            (magh-ui--insert-header
             "Description" (alist-get 'shortDescription project))
            (magh-ui--insert-header
             "Items" (magh-api--json-at project 'items 'totalCount))
            (magh-ui--insert-header
             "Fields" (magh-api--json-at project 'fields 'totalCount)))))
    (insert (propertize "No Projects found.\n" 'font-lock-face 'shadow))))

;;;###autoload
(defun magh-project-list (&optional context owner include-closed)
  "Open Projects for OWNER.
OWNER defaults to the current repository owner, or `@me' outside a repository.
With INCLUDE-CLOSED, include closed Projects."
  (interactive (list nil nil current-prefix-arg))
  (setq context (magh-context-resolve context)
        owner (or owner (magh-project--default-owner context)))
  (magh-ui--open-page
   (format "*magh: %s · Projects*" owner) context 'project-list owner
   (lambda (success error force)
     (magh-api--project-list
      context success error force owner include-closed))
   (lambda (data) (magh-project--render-list context owner data))
   :setup
   (lambda ()
     (setq magh-project--owner owner
           magh-buffer-dispatch-function #'magh-project-list-dispatch))))

(defun magh-project--fetch (context owner number success error force)
  "Fetch Project NUMBER, then its items and editable fields."
  (magh-api--project-get
   context owner number
   (lambda (project)
     (let ((project-id (alist-get 'id project)))
       (magh-core--collect-async-settled
        (list
         (cons 'items
               (lambda (ok fail)
                 (magh-api--project-items
                  context owner number ok fail force)))
         (cons 'fields
               (lambda (ok fail)
                 (if project-id
                     (magh-api--project-fields
                      context project-id ok fail force)
                   (funcall fail
                            (magh-core--error
                             'magh-api-error
                             "Project response has no node ID"))))))
        (lambda (result)
          (funcall
           success
           (magh-batch-result-create
            :values (cons (cons 'project project)
                          (magh-batch-result-values result))
            :errors (magh-batch-result-errors result)))))))
   error force))

(defun magh-project--field-options (field)
  "Return display option objects for FIELD."
  (or (alist-get 'options field)
      (append
       (magh-api--json-at field 'configuration 'iterations)
       (magh-api--json-at field 'configuration 'completedIterations))))

(defun magh-project--insert-field (context owner number field)
  "Insert FIELD metadata for Project NUMBER."
  (let ((resource
         (magh-resource-create
          'project-field context :owner owner :number number
          :id (alist-get 'id field) :title (alist-get 'name field)
          :field-type (alist-get 'dataType field) :data field)))
    (magh-ui--section (project-field (alist-get 'id field) resource t)
      (magh-ui--row
       (magh-ui--styled (alist-get 'name field) 'magh-resource-title)
       (magh-ui--styled (alist-get 'dataType field) 'magh-permission))
      (when-let* ((options (magh-project--field-options field)))
        (magh-ui--insert-header
         "Options"
         (mapconcat (lambda (option)
                      (or (alist-get 'name option)
                          (alist-get 'title option)))
                    options ", "))))))

(defun magh-project--item-content (item)
  "Return content object from Project ITEM."
  (alist-get 'content item))

(defun magh-project--item-resource (context owner number item)
  "Create a native resource from Project ITEM."
  (let* ((content (magh-project--item-content item))
         (subject (magh-resource-from-url (alist-get 'url content) context))
         (properties
          (list :project-context context :project-owner owner
                :project-number number :project-id magh-project--id
                :project-item-id (alist-get 'id item) :project-item item)))
    (if (memq (plist-get subject :kind) '(issue pr))
        (append subject properties)
      (apply #'magh-resource-create
             'project-item context
             (append
              (list :owner owner :number number :id (alist-get 'id item)
                    :title (or (alist-get 'title content) "Untitled draft")
                    :draft t :data item)
              properties)))))

(defun magh-project--field-value-text (value)
  "Return compact display text for a Project field VALUE."
  (cond
   ((or (stringp value) (numberp value)) (format "%s" value))
   ((and (listp value) (alist-get 'name value)) (alist-get 'name value))
   ((and (listp value) (alist-get 'title value)) (alist-get 'title value))
   ((and (listp value) (alist-get 'login value)) (alist-get 'login value))
   ((listp value) (mapconcat #'magh-project--field-value-text value ", "))
   (t nil)))

(defun magh-project--insert-item (context owner number item)
  "Insert Project ITEM."
  (let* ((content (magh-project--item-content item))
         (resource (magh-project--item-resource context owner number item))
         (type (or (alist-get 'type content) "DraftIssue")))
    (magh-ui--section (project-item (alist-get 'id item) resource t)
      (magh-ui--row
       (magh-ui--styled type 'magh-permission)
       (magh-ui--styled (alist-get 'title content) 'magh-resource-title))
      (when (alist-get 'number content)
        (magh-ui--insert-header
         "Number" (format "#%s" (alist-get 'number content))
         'magh-resource-number))
      (dolist (entry item)
        (unless (memq (car entry) '(id content type title))
          (when-let* ((text (magh-project--field-value-text (cdr entry))))
            (magh-ui--insert-header (symbol-name (car entry)) text)))))))

(defun magh-project--render (context owner number result)
  "Render Project NUMBER aggregate RESULT."
  (let* ((project (magh-batch-value result 'project))
         (resource (magh-project--resource context owner project))
         (closed (magh-api--true-p (alist-get 'closed project))))
    (let ((start (point)))
      (insert
       (magh-ui--row
        (magh-ui--styled (if closed "CLOSED" "OPEN")
                         (if closed 'magh-closed-state 'magh-open-state))
        (magh-ui--styled (format "#%s" number) 'magh-resource-number)
        (magh-ui--styled (alist-get 'title project) 'magh-resource-title))
       "\n")
      (add-text-properties start (point) (list 'magh-resource resource)))
    (magh-ui--insert-header "Owner" owner 'magh-author)
    (magh-ui--insert-header
     "Visibility" (if (magh-api--true-p (alist-get 'public project))
                      "public" "private") 'magh-permission)
    (magh-ui--insert-header "Description" (alist-get 'shortDescription project))
    (insert "\n")
    (magh-ui--section (readme 'readme resource nil)
      "README"
      (magh-ui--insert-markdown
       (if (string-empty-p (string-trim (or (alist-get 'readme project) "")))
           "No README."
         (alist-get 'readme project))
       context))
    (magh-ui--section (fields 'fields nil nil)
      "Fields"
      (if-let* ((field-error (magh-batch-error result 'fields)))
          (magh-ui--insert-request-error field-error)
        (dolist (field (magh-batch-value result 'fields))
          (magh-project--insert-field context owner number field))))
    (magh-ui--section (items 'items nil nil)
      "Items"
      (if-let* ((item-error (magh-batch-error result 'items)))
          (magh-ui--insert-request-error item-error)
        (let ((items (magh-batch-value result 'items)))
          (if items
              (dolist (item items)
                (magh-project--insert-item context owner number item))
            (insert (propertize "No items.\n" 'font-lock-face 'shadow))))))))

(defun magh-project--setup (_context owner number)
  "Install Project detail state for OWNER and NUMBER."
  (setq magh-project--owner owner
        magh-project--number number
        magh-buffer-dispatch-function #'magh-project-dispatch)
  (local-set-key (kbd "E") #'magh-project-edit)
  (local-set-key (kbd "+") #'magh-project-item-add))

;;;###autoload
(defun magh-project-view (number &optional owner context preview)
  "Open Project NUMBER for OWNER."
  (interactive (list (read-number "Project number: ")))
  (setq context (magh-context-resolve context)
        owner (or owner (magh-project--default-owner context)))
  (magh-ui--open-page
   (if preview
       (format "*magh preview: %s · Project #%s*" owner number)
     (format "*magh: %s · Project #%s*" owner number))
   context 'project (list owner number)
   (lambda (success error force)
     (magh-project--fetch context owner number success error force))
   (lambda (result)
     (setq magh-project--id
           (alist-get 'id (magh-batch-value result 'project)))
     (magh-project--render context owner number result))
   :preview preview
   :setup (lambda () (magh-project--setup context owner number))))

(defun magh-project--current-resource ()
  "Return current Project or Project item resource."
  (let ((point-resource (magh-ui-resource-at-point)))
    (or (and (or (memq (plist-get point-resource :kind)
                       '(project project-item project-field))
                 (plist-get point-resource :project-item-id))
             point-resource)
        magh-project--dispatch-resource
        (magh-resource-create
         'project magh-buffer-context :owner magh-project--owner
         :number magh-project--number :id magh-project--id))))

(defun magh-project--resolve-location (resource context owner number)
  "Resolve (CONTEXT OWNER NUMBER) from RESOURCE and explicit arguments."
  (setq context (magh-context-resolve
                 (or context (magh-project--item-context resource))))
  (list context
        (or owner (magh-project--item-owner resource)
            (magh-project--default-owner context))
        (or number (magh-project--item-project-number resource)
            (read-number "Project number: "))))

;;;###autoload
(defun magh-project-create (&optional context owner)
  "Create a Project for OWNER using a structured editor."
  (interactive)
  (setq context (magh-context-resolve context)
        owner (or owner (magh-project--default-owner context)))
  (magh-edit-open
   (format "*magh: %s · New Project*" owner)
   '((:name title :required t)) nil ""
   (lambda (values _body success error)
     (magh-api--project-create context owner values success error))
   :after-success
   (lambda (project)
     (when-let* ((number (alist-get 'number project)))
       (magh-project-view number owner context)))))

(defun magh-project--open-edit (context owner number project)
  "Open editor for PROJECT NUMBER in CONTEXT."
  (magh-edit-open
     (format "*magh: %s · Edit Project #%s*" owner number)
     '((:name title :required t)
       (:name description :allow-empty t)
       (:name visibility :choices ("PUBLIC" "PRIVATE")))
     (list :title (alist-get 'title project)
           :description (or (alist-get 'shortDescription project) "")
           :visibility (if (magh-api--true-p (alist-get 'public project))
                           "PUBLIC" "PRIVATE"))
     (or (alist-get 'readme project) "")
     (lambda (values body success error)
       (magh-api--project-edit
        context owner number (plist-put values :readme body) success error))))

;;;###autoload
(defun magh-project-edit (&optional context owner number)
  "Edit Project NUMBER metadata and README."
  (interactive)
  (let* ((resource (magh-project--current-resource))
         (project (or (magh-batch-value magh-ui--data 'project)
                      (plist-get resource :data))))
    (pcase-setq `(,context ,owner ,number)
                (magh-project--resolve-location resource context owner number))
    ;; List responses are not guaranteed to carry the full README.  Fetch the
    ;; detail before editing instead of risking an accidental clear.
    (if (and project (assq 'readme project))
        (magh-project--open-edit context owner number project)
      (magh-api--project-get
       context owner number
       (lambda (data) (magh-project--open-edit context owner number data))
       #'magh-core--user-error t))))

;;;###autoload
(defun magh-project-toggle-closed (&optional context owner number)
  "Close Project NUMBER, or reopen it when currently closed."
  (interactive)
  (let* ((resource (magh-project--current-resource))
         (project (or (magh-batch-value magh-ui--data 'project)
                      (plist-get resource :data)))
         (closed (magh-api--true-p (alist-get 'closed project))))
    (pcase-setq `(,context ,owner ,number)
                (magh-project--resolve-location resource context owner number))
    (when (or closed
              (magh-core--confirm (format "Close Project #%s? " number)))
      (magh-api--project-close
       context owner number (not closed)
       (lambda (_)
         (message "Project #%s %s" number (if closed "reopened" "closed"))
         (magh-ui--refresh-if-page))
       #'magh-core--user-error))))

;;;###autoload
(defun magh-project-item-add (&optional context owner number url)
  "Add the Issue or Pull Request URL to Project NUMBER."
  (interactive)
  (let ((resource (magh-project--current-resource)))
    (pcase-setq `(,context ,owner ,number)
                (magh-project--resolve-location resource context owner number))
    (setq url (or url (read-string "Issue or Pull Request URL: ")))
    (magh-api--project-item-add
     context owner number url
     (lambda (_) (message "Added item to Project #%s" number)
       (magh-ui--refresh-if-page))
     #'magh-core--user-error)))

;;;###autoload
(defun magh-project-draft-create (&optional context owner number)
  "Create a draft Issue in Project NUMBER."
  (interactive)
  (let ((resource (magh-project--current-resource)))
    (pcase-setq `(,context ,owner ,number)
                (magh-project--resolve-location resource context owner number))
    (magh-edit-open
     (format "*magh: %s · Project #%s Draft*" owner number)
     '((:name title :required t)) nil ""
     (lambda (values body success error)
       (magh-api--project-item-create
        context owner number (plist-put values :body body) success error)))))

(defun magh-project--selected-item ()
  "Return the selected Project item resource."
  (let ((resource (magh-project--current-resource)))
    (unless (or (eq (plist-get resource :kind) 'project-item)
                (plist-get resource :project-item-id))
      (user-error "No Project item selected"))
    resource))

(defun magh-project--item-owner (resource)
  "Return Project owner stored on RESOURCE."
  (or (plist-get resource :project-owner)
      (plist-get resource :owner) magh-project--owner))

(defun magh-project--item-context (resource)
  "Return Project navigation context stored on RESOURCE."
  (or (plist-get resource :project-context)
      (and (memq (plist-get resource :kind)
                 '(project project-item project-field))
           (plist-get resource :context))
      magh-buffer-context (plist-get resource :context)))

(defun magh-project--item-project-number (resource)
  "Return Project number stored on RESOURCE."
  (or (plist-get resource :project-number)
      (and (memq (plist-get resource :kind)
                 '(project project-item project-field))
           (plist-get resource :number))
      magh-project--number))

;;;###autoload
(defun magh-project-draft-edit (&optional resource)
  "Edit the selected draft Project item RESOURCE."
  (interactive)
  (setq resource (or resource (magh-project--selected-item)))
  (unless (plist-get resource :draft)
    (user-error "Only draft Project items can be edited here"))
  (let* ((context (magh-project--item-context resource))
         (owner (plist-get resource :owner))
         (number (plist-get resource :number))
         (item (plist-get resource :data))
         (content (magh-project--item-content item)))
    (magh-edit-open
     (format "*magh: %s · Edit Project Draft*" owner)
     '((:name title :required t))
     (list :title (alist-get 'title content)) (or (alist-get 'body content) "")
     (lambda (values body success error)
       (magh-api--project-draft-edit
        context owner number (plist-get resource :id)
        (plist-put values :body body) success error)))))

;;;###autoload
(defun magh-project-item-archive (&optional resource archived)
  "Archive selected Project item RESOURCE.
When ARCHIVED is explicitly nil with a non-nil RESOURCE, restore it."
  (interactive)
  (setq resource (or resource (magh-project--selected-item))
        archived (if (called-interactively-p 'interactive) t archived))
  (magh-api--project-item-archive
   (magh-project--item-context resource) (magh-project--item-owner resource)
   (magh-project--item-project-number resource)
   (or (plist-get resource :project-item-id) (plist-get resource :id)) archived
   (lambda (_) (message "Project item %s"
                    (if archived "archived" "restored"))
     (magh-ui--refresh-if-page))
   #'magh-core--user-error))

;;;###autoload
(defun magh-project-item-remove (&optional resource)
  "Remove selected Project item RESOURCE from its Project."
  (interactive)
  (setq resource (or resource (magh-project--selected-item)))
  (when (magh-core--confirm "Remove this item from the Project? ")
    (magh-api--project-item-remove
     (magh-project--item-context resource) (magh-project--item-owner resource)
     (magh-project--item-project-number resource)
     (or (plist-get resource :project-item-id) (plist-get resource :id))
     (lambda (_) (message "Removed item from Project")
       (magh-ui--refresh-if-page))
     #'magh-core--user-error)))

(defun magh-project--editable-fields ()
  "Return editable fields loaded on the current Project page."
  (seq-filter
   (lambda (field)
     (member (alist-get 'dataType field)
             '("TEXT" "NUMBER" "DATE" "SINGLE_SELECT" "ITERATION")))
   (magh-batch-value magh-ui--data 'fields)))

(defun magh-project--read-field-value (field)
  "Read and return (VALUE CLEAR) for FIELD."
  (let ((type (alist-get 'dataType field)))
    (if (member type '("SINGLE_SELECT" "ITERATION"))
        (let* ((options (magh-project--field-options field))
               (choices
                (cons (cons "<clear>" nil)
                      (mapcar
                       (lambda (option)
                         (cons (or (alist-get 'name option)
                                   (alist-get 'title option))
                               (alist-get 'id option)))
                       options)))
               (choice (completing-read "Value: " choices nil t)))
          (list (cdr (assoc choice choices)) (string= choice "<clear>")))
      (let ((text (read-string
                   (format "%s value (empty clears): " type))))
        (when (and (string= type "NUMBER")
                   (not (string-empty-p text))
                   (not (string-match-p
                         "\\`[+-]?[0-9]+\\(?:\\.[0-9]+\\)?\\'" text)))
          (user-error "NUMBER must be an integer or decimal"))
        (when (and (string= type "DATE")
                   (not (string-empty-p text))
                   (not (string-match-p
                         "\\`[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\'" text)))
          (user-error "DATE must use YYYY-MM-DD"))
        (list (if (string= type "NUMBER")
                  (and (not (string-empty-p text)) (string-to-number text))
                text)
              (string-empty-p text))))))

;;;###autoload
(defun magh-project-item-field-edit (&optional item-resource field)
  "Edit one FIELD on selected ITEM-RESOURCE."
  (interactive)
  (setq item-resource (or item-resource (magh-project--selected-item)))
  (let* ((fields (magh-project--editable-fields))
         (field
          (or field
              (let* ((choices
                      (mapcar (lambda (entry)
                                (cons (format "%s (%s)"
                                              (alist-get 'name entry)
                                              (alist-get 'dataType entry))
                                      entry))
                              fields))
                     (choice (completing-read "Field: " choices nil t)))
                (cdr (assoc choice choices)))))
         (selection (magh-project--read-field-value field)))
    (magh-api--project-field-edit
     (magh-project--item-context item-resource)
     (magh-project--item-owner item-resource)
     (magh-project--item-project-number item-resource)
     (or (plist-get item-resource :project-id) magh-project--id)
     (or (plist-get item-resource :project-item-id)
         (plist-get item-resource :id))
     (alist-get 'id field) (alist-get 'dataType field)
     (car selection) (cadr selection)
     (lambda (_) (message "Updated Project field %s" (alist-get 'name field))
       (magh-ui--refresh-if-page))
     #'magh-core--user-error)))

(transient-define-prefix magh-project-list-dispatch ()
  "Project list commands."
  [["View"
    ("g" "Refresh" magh-ui-refresh)]
   ["Create"
    ("c" "New Project" magh-project-create)]])

(transient-define-prefix magh-project-dispatch ()
  "Project commands."
  [["Project"
    ("g" "Refresh" magh-ui-refresh)
    ("E" "Edit" magh-project-edit)
    ("t" "Close / reopen" magh-project-toggle-closed)
    ("b" "Browse" magh-ui-browse)]
   ["Items"
    ("+" "Add Issue / PR" magh-project-item-add)
    ("c" "New draft" magh-project-draft-create)
    ("e" "Edit draft" magh-project-draft-edit)
    ("f" "Set field" magh-project-item-field-edit)]
   ["Manage item"
    ("a" "Archive" magh-project-item-archive)
    ("D" "Remove from Project" magh-project-item-remove)]])

(magh-candidate-register
 'project
 :open (lambda (resource)
         (magh-project-view
          (plist-get resource :number) (plist-get resource :owner)
          (plist-get resource :context)))
 :preview (lambda (resource)
            (magh-project-view
             (plist-get resource :number) (plist-get resource :owner)
             (plist-get resource :context) t))
 :dispatch (lambda (resource)
             (setq magh-project--dispatch-resource resource)
             (call-interactively #'magh-project-dispatch)))

(magh-candidate-register
 'project-item
 :open (lambda (resource) (magh-project-draft-edit resource))
 :dispatch (lambda (resource)
             (setq magh-project--dispatch-resource resource)
             (call-interactively #'magh-project-dispatch)))

(magh-candidate-register
 'project-field
 :dispatch (lambda (resource)
             (setq magh-project--dispatch-resource resource)
             (call-interactively #'magh-project-dispatch)))

(magh-candidate-register
 'project-list
 :open (lambda (resource)
         (magh-project-list
          (plist-get resource :context) (plist-get resource :owner))))

(provide 'magh-project)
;;; magh-project.el ends here
