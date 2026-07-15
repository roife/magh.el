;;; gh-actions.el --- GitHub Actions frontend for gh.el -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; Author: gh.el contributors
;; Keywords: tools, vc, github
;; Package-Requires: ((emacs "29.1") (transient "0.7.0"))

;;; Commentary:

;; Native workflow/run/job/step pages and asynchronous logs, watch, dispatch,
;; enable, disable, rerun, job rerun, and cancellation.

;;; Code:

(require 'base64)
(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'transient)
(require 'gh-api)
(require 'gh-candidate)
(require 'gh-ui)

(defvar-local gh-actions--params nil)
(defvar-local gh-actions--dispatch-resource nil)
(defvar-local gh-actions--run-id nil)
(defvar-local gh-actions--workflow-id nil)

(defun gh-actions--context (&optional context)
  "Resolve repository CONTEXT for Actions."
  (gh-context-resolve (or context gh-buffer-context) t))

(defun gh-actions--run-resource (context data)
  "Create Run resource from DATA."
  (let ((id (or (gh-core--alist-get 'databaseId data)
                (gh-core--alist-get 'id data))))
    (gh-resource-create
     'run context :id id
     :title (or (gh-core--alist-get 'displayTitle data)
                (gh-core--alist-get 'name data))
     :url (or (gh-core--alist-get 'url data)
              (gh-context-web-url context (format "actions/runs/%s" id)))
     :workflow-id (gh-core--alist-get 'workflowDatabaseId data)
     :data data)))

(defun gh-actions--run-state (data)
  "Return display state for Run DATA."
  (or (gh-core--alist-get 'conclusion data)
      (gh-core--alist-get 'status data) ""))

(defun gh-actions--insert-run (context data &optional workflow-page)
  "Insert Run DATA in CONTEXT.
When WORKFLOW-PAGE is non-nil, use the compact layout from the Workflow page."
  (let* ((resource (gh-actions--run-resource context data))
         (id (plist-get resource :id))
         (state (gh-actions--run-state data))
         (title (or (gh-core--alist-get 'displayTitle data) ""))
         (workflow (or (gh-core--alist-get 'workflowName data)
                       (gh-core--alist-get 'name data) ""))
         (branch (gh-core--alist-get 'headBranch data))
         (created (gh-core--date (gh-core--alist-get 'createdAt data))))
    (gh-ui--section (run id resource t)
      (gh-ui--row
       (gh-ui--styled (upcase state) (gh-core--state-face state))
       (gh-ui--styled title 'gh-resource-title)
       (if workflow-page
           (gh-ui--styled branch 'gh-branch)
         (gh-ui--styled workflow 'gh-workflow))
       (and (not workflow-page) (gh-ui--styled created 'gh-date)))
      (unless workflow-page
        (gh-ui--insert-header "Branch" branch 'gh-branch))
      (gh-ui--insert-header "Event" (gh-core--alist-get 'event data))
      (when workflow-page
        (gh-ui--insert-header "Created" created 'gh-date))
      (gh-ui--insert-header "Commit" (gh-core--alist-get 'headSha data)
                            'gh-hash))))

(defun gh-actions--render-list (context data)
  "Render Actions Run list DATA."
  (gh-ui--insert-header "Repository" (gh-context-repository context)
                        'gh-repository (gh-resource-create 'repository context))
  (gh-ui--insert-header "Actions" (format "%d runs" (length data)))
  (insert "\n")
  (if data
      (dolist (run data) (gh-actions--insert-run context run))
    (insert (propertize "No matching workflow runs.\n"
                        'font-lock-face 'shadow))))

(defun gh-actions--setup-list (context)
  "Install Actions list keys for CONTEXT."
  (local-set-key (kbd "W") (lambda () (interactive) (gh-workflow-list context)))
  (setq gh-buffer-dispatch-function #'gh-actions-dispatch))

;;;###autoload
(defun gh-run-list (&optional context params)
  "Open recent workflow runs in CONTEXT filtered by PARAMS."
  (interactive)
  (setq context (gh-actions--context context)
        params (copy-sequence params))
  (gh-ui--open-page
   (format "*gh: %s · Actions*" (gh-context-repository context))
   context 'run-list (gh-context-repository context)
   (lambda (success error force)
     (gh-api--run-list context params success error force))
   (lambda (data) (gh-actions--render-list context data))
   :setup (lambda ()
            (setq gh-actions--params params)
            (gh-actions--setup-list context))))

(defalias 'gh-actions #'gh-run-list)

;;; Run details

(defun gh-actions--job-resource (context run-id job)
  "Create a Job resource for JOB in RUN-ID."
  (gh-resource-create
   'job context :id (or (gh-core--alist-get 'databaseId job)
                        (gh-core--alist-get 'id job))
   :run-id run-id :title (gh-core--alist-get 'name job)
   :url (gh-core--alist-get 'url job) :data job))

(defun gh-actions--render-job (context run-id job)
  "Render JOB for RUN-ID."
  (let* ((resource (gh-actions--job-resource context run-id job))
         (id (plist-get resource :id))
         (state (or (gh-core--alist-get 'conclusion job)
                    (gh-core--alist-get 'status job) "")))
    (gh-ui--section (job id resource nil)
      (gh-ui--row
       (gh-ui--styled (upcase state) (gh-core--state-face state))
       (gh-ui--styled (or (gh-core--alist-get 'name job) "job")
                      'gh-resource-title))
      (dolist (step (gh-core--alist-get 'steps job))
        (let ((step-state (or (gh-core--alist-get 'conclusion step)
                              (gh-core--alist-get 'status step) "")))
          (gh-ui--section (step (or (gh-core--alist-get 'number step)
                                    (gh-core--alist-get 'name step))
                                resource t)
            (gh-ui--row
             (gh-ui--styled (upcase step-state)
                            (gh-core--state-face step-state))
             (gh-ui--styled (or (gh-core--alist-get 'name step) "step")
                            'gh-resource-title))))))))

(defun gh-actions--render-run (context data)
  "Render workflow Run DATA."
  (let* ((resource (gh-actions--run-resource context data))
         (id (plist-get resource :id))
         (state (gh-actions--run-state data))
         (workflow-resource
          (gh-resource-create
           'workflow context
           :id (gh-core--alist-get 'workflowDatabaseId data)
           :title (or (gh-core--alist-get 'workflowName data)
                      (gh-core--alist-get 'name data)))))
    (insert (propertize (upcase state)
                        'font-lock-face (gh-core--state-face state)) " "
            (propertize (or (gh-core--alist-get 'displayTitle data) "")
                        'font-lock-face 'gh-resource-title) "\n")
    (add-text-properties (line-beginning-position 0) (point)
                         (list 'gh-resource resource))
    (gh-ui--insert-header "Workflow"
                          (or (gh-core--alist-get 'workflowName data)
                              (gh-core--alist-get 'name data))
                          'gh-workflow workflow-resource)
    (gh-ui--insert-header "Status" (gh-core--alist-get 'status data)
                          (gh-core--state-face
                           (gh-core--alist-get 'status data)))
    (gh-ui--insert-header "Conclusion" (gh-core--alist-get 'conclusion data)
                          (gh-core--state-face state))
    (gh-ui--insert-header "Branch" (gh-core--alist-get 'headBranch data)
                          'gh-branch
                          (gh-resource-create
                           'tree (gh-context-copy
                                  context :ref (gh-core--alist-get 'headBranch data))
                           :ref (gh-core--alist-get 'headBranch data) :path ""))
    (gh-ui--insert-header "Commit" (gh-core--alist-get 'headSha data) 'gh-hash
                          (gh-resource-create
                           'commit context :sha (gh-core--alist-get 'headSha data)))
    (gh-ui--insert-header "Event" (gh-core--alist-get 'event data))
    (insert "\n")
    (dolist (job (gh-core--alist-get 'jobs data))
      (gh-actions--render-job context id job))))

(defun gh-actions--setup-run (context id)
  "Install Run detail keys for CONTEXT and ID."
  (setq gh-actions--run-id id
        gh-buffer-dispatch-function
        (lambda ()
          (setq gh-actions--dispatch-resource
                (gh-resource-create 'run context :id id))
          (call-interactively #'gh-run-dispatch)))
  (local-set-key (kbd "l") (lambda () (interactive) (gh-run-log context id)))
  (local-set-key (kbd "w") (lambda () (interactive) (gh-run-watch context id))))

(defun gh-run-view (id &optional context preview)
  "Open workflow Run ID in CONTEXT."
  (interactive (list (read-number "Run ID: ")))
  (setq context (gh-actions--context context))
  (gh-ui--open-page
   (if preview
       (format "*gh preview: %s · Run %s*"
               (gh-context-repository context) id)
     (format "*gh: %s · Run %s*" (gh-context-repository context) id))
   context 'run id
   (lambda (success error force)
     (gh-api--run-get context id success error force))
   (lambda (data) (gh-actions--render-run context data))
   :preview preview :setup (lambda () (gh-actions--setup-run context id))))

;;; Logs and watch

(defun gh-actions--text-buffer (name mode)
  "Create a read-only text buffer NAME in MODE."
  (let ((buffer (get-buffer-create name)))
    (with-current-buffer buffer
      (let ((inhibit-read-only t)) (erase-buffer))
      (funcall mode)
      (setq buffer-read-only t))
    buffer))

(defun gh-run-log (&optional context id job-id)
  "Open complete log for Run ID, optionally restricted to JOB-ID."
  (interactive)
  (setq context (gh-actions--context context)
        id (or id gh-actions--run-id
               (plist-get (gh-ui-resource-at-point) :run-id)))
  (let ((buffer (gh-actions--text-buffer
                 (format "*gh: %s · Run %s Log*"
                         (gh-context-repository context) id)
                 #'special-mode)))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (insert (propertize "Loading Actions log…\n"
                            'font-lock-face 'gh-loading)))
      (gh-api--run-log
       context id job-id
       (lambda (text)
         (let ((inhibit-read-only t))
           (erase-buffer)
           (gh-ui--insert-ansi text)
           (goto-char (point-min))))
       (lambda (error)
         (let ((inhibit-read-only t))
           (erase-buffer) (insert (gh-error-message error) "\n")))))
    (funcall gh-display-buffer-function buffer)))

(defun gh-run-watch (&optional context id)
  "Watch Run ID without blocking Emacs."
  (interactive)
  (setq context (gh-actions--context context)
        id (or id gh-actions--run-id))
  (let ((buffer (gh-actions--text-buffer
                 (format "*gh: %s · Watch Run %s*"
                         (gh-context-repository context) id)
                 #'special-mode)))
    (with-current-buffer buffer
      (gh-api--run-watch
       context id
       (lambda (_text)
         (let ((inhibit-read-only t))
           (goto-char (point-max))
           (insert "\nRun watch completed.\n")))
       (lambda (error)
         (let ((inhibit-read-only t))
           (goto-char (point-max))
           (insert "\n" (gh-error-message error) "\n")))
       (lambda (chunk _request)
         (when (buffer-live-p buffer)
           (with-current-buffer buffer
             (let ((inhibit-read-only t)
                   (start (point-max)))
               (goto-char start) (insert chunk)
               (ansi-color-apply-on-region start (point-max))
               (gh-ui--adopt-font-lock-faces start (point-max))))))))
    (funcall gh-display-buffer-function buffer)))

;;; Workflow

(defun gh-actions--workflow-resource (context data)
  "Create Workflow resource from DATA."
  (let ((id (gh-core--alist-get 'id data)))
    (gh-resource-create
     'workflow context :id id :name (gh-core--alist-get 'name data)
     :title (gh-core--alist-get 'name data) :path (gh-core--alist-get 'path data)
     :url (or (gh-core--alist-get 'html_url data)
              (gh-context-web-url context (format "actions/workflows/%s" id)))
     :data data)))

;;;###autoload
(defun gh-workflow-list (&optional context)
  "Select a workflow in CONTEXT with native preview."
  (interactive)
  (setq context (gh-actions--context context))
  (gh-api--workflow-list
   context
   (lambda (items)
     (let* ((resources (mapcar (lambda (item)
                                 (gh-actions--workflow-resource context item))
                               items))
            (resource
             (gh-candidate-read
              "Workflow: " resources :category 'gh-workflow :preview t
              :formatter
              (lambda (item)
                (let ((data (plist-get item :data)))
                  (gh-ui--row
                   (gh-ui--styled
                    (upcase (or (gh-core--alist-get 'state data) ""))
                    (gh-core--state-face
                     (gh-core--alist-get 'state data)))
                   (gh-ui--styled (gh-resource-title item) 'gh-workflow)
                   (gh-ui--styled (plist-get item :path) 'gh-file)))))))
       (when resource (gh-resource-open resource))))
   #'gh-core--user-error))

(defun gh-actions--fetch-workflow
    (context workflow ref success error force)
  "Fetch WORKFLOW details, YAML, and recent runs at REF."
  (gh-api--workflow-get
   context workflow
   (lambda (metadata)
     (let ((path (gh-core--alist-get 'path metadata)))
       (gh-core--collect-async
        (list
         (cons 'metadata (lambda (ok _fail) (funcall ok metadata)))
         (cons 'configuration
               (lambda (ok fail)
                 (gh-api--content-get context path ref ok fail force)))
         (cons 'runs
               (lambda (ok fail)
                 (gh-api--workflow-runs context workflow ref ok fail force))))
        success error)))
   error force))

(defun gh-actions--decode-yaml (content)
  "Decode workflow CONTENT JSON into text."
  (decode-coding-string
   (base64-decode-string
    (replace-regexp-in-string
     "\n" "" (or (gh-core--alist-get 'content content) "")))
   'utf-8))

(defun gh-actions--render-workflow (context _workflow ref result)
  "Render workflow RESULT in CONTEXT at REF."
  (let* ((metadata (alist-get 'metadata result))
         (configuration (alist-get 'configuration result))
         (runs (alist-get 'runs result))
         (resource (gh-actions--workflow-resource context metadata))
         (path (gh-core--alist-get 'path metadata)))
    (gh-ui--insert-header "Repository" (gh-context-repository context)
                          'gh-repository)
    (gh-ui--insert-header "Workflow" (gh-core--alist-get 'name metadata)
                          'gh-workflow resource)
    (gh-ui--insert-header "State" (gh-core--alist-get 'state metadata)
                          (gh-core--state-face
                           (gh-core--alist-get 'state metadata)))
    (gh-ui--insert-header
     "Path" path 'gh-file
     (gh-resource-create 'file (gh-context-copy context :ref ref :path path)
                         :path path :ref ref))
    (gh-ui--insert-header "Ref" ref 'gh-branch
                          (gh-resource-create 'tree
                                              (gh-context-copy context :ref ref)
                                              :ref ref :path ""))
    (insert "\n")
    (gh-ui--section (configuration 'configuration nil nil)
      "Configuration"
      (let ((start (point)))
        (insert (gh-actions--decode-yaml configuration))
        (unless (bolp) (insert "\n"))
        (when (fboundp 'yaml-mode)
          (add-text-properties start (point) '(font-lock-face default)))))
    (gh-ui--section (runs 'recent-runs nil nil)
      (format "Recent runs (%d)" (length runs))
      (dolist (run runs) (gh-actions--insert-run context run t)))))

(defun gh-workflow-view (workflow &optional context ref preview)
  "Open WORKFLOW details in CONTEXT at REF."
  (interactive (list (read-string "Workflow ID, name, or path: ")))
  (setq context (gh-actions--context context)
        ref (or ref (gh-context-ref context)
                (gh-context-default-branch context) "HEAD"))
  (gh-ui--open-page
   (if preview
       (format "*gh preview: %s · Workflow %s*"
               (gh-context-repository context) workflow)
     (format "*gh: %s · Workflow %s*"
             (gh-context-repository context) workflow))
   (gh-context-copy context :ref ref) 'workflow workflow
   (lambda (success error force)
     (gh-actions--fetch-workflow context workflow ref success error force))
   (lambda (data) (gh-actions--render-workflow context workflow ref data))
   :preview preview
   :setup (lambda ()
            (setq gh-actions--workflow-id workflow
                  gh-buffer-dispatch-function #'gh-workflow-dispatch))))

;;; Mutations

(defun gh-run-rerun (&optional failed-only context id)
  "Rerun Run ID; with FAILED-ONLY, rerun only failed jobs."
  (interactive "P")
  (setq context (gh-actions--context context)
        id (or id gh-actions--run-id
               (plist-get gh-actions--dispatch-resource :id)))
  (gh-api--run-rerun
   context id failed-only
   (lambda (_) (message "Rerun requested for %s" id)
     (when (derived-mode-p 'gh-section-mode) (gh-ui-refresh t)))
   #'gh-core--user-error))

;;;###autoload
(defun gh-run-rerun-job (&optional context job-id)
  "Choose or rerun JOB-ID in the current run."
  (interactive)
  (setq context (gh-actions--context context))
  (let* ((resource (gh-ui-resource-at-point))
         (job-id (or job-id (and (eq (plist-get resource :kind) 'job)
                                  (plist-get resource :id)))))
    (unless job-id
      (let* ((run (and (eq gh-buffer-resource-kind 'run) gh-ui--data))
             (jobs (and run (gh-core--alist-get 'jobs run)))
             (choices (mapcar (lambda (job)
                                (cons (gh-core--alist-get 'name job) job)) jobs))
             (choice (and choices
                          (completing-read "Job: " choices nil t)))
             (job (and choice (cdr (assoc choice choices)))))
        (setq job-id (and job (or (gh-core--alist-get 'databaseId job)
                                  (gh-core--alist-get 'id job))))))
    (unless job-id (user-error "No job selected"))
    (gh-api--run-rerun-job
     context job-id
     (lambda (_) (message "Rerun requested for job %s" job-id))
     #'gh-core--user-error)))

(defun gh-run-cancel (&optional context id)
  "Cancel Run ID."
  (interactive)
  (setq context (gh-actions--context context)
        id (or id gh-actions--run-id
               (plist-get gh-actions--dispatch-resource :id)))
  (when (gh-core--confirm (format "Cancel Actions run %s? " id))
    (gh-api--run-cancel
     context id (lambda (_) (message "Cancelled run %s" id)
                  (when (derived-mode-p 'gh-section-mode) (gh-ui-refresh t)))
     #'gh-core--user-error)))

(defun gh-workflow-toggle (&optional disable context workflow)
  "Enable WORKFLOW, or DISABLE with prefix argument."
  (interactive "P")
  (setq context (gh-actions--context context)
        workflow (or workflow gh-actions--workflow-id))
  (gh-api--workflow-enable
   context workflow (not disable)
   (lambda (_) (message "Workflow %s" (if disable "disabled" "enabled"))
     (when (derived-mode-p 'gh-section-mode) (gh-ui-refresh t)))
   #'gh-core--user-error))

(defun gh-workflow-dispatch-run (&optional context workflow)
  "Manually dispatch WORKFLOW with arbitrary key=value inputs."
  (interactive)
  (setq context (gh-actions--context context)
        workflow (or workflow gh-actions--workflow-id))
  (let ((ref (read-string "Ref: " (or (gh-context-ref context) "main")))
        inputs input)
    (while (not (string-empty-p
                 (setq input (read-string "Input key=value (empty to finish): "))))
      (unless (string-match "\\`\\([^=]+\\)=\\(.*\\)\\'" input)
        (user-error "Expected key=value"))
      (push (cons (match-string 1 input) (match-string 2 input)) inputs))
    (gh-api--workflow-dispatch
     context workflow ref (nreverse inputs)
     (lambda (_) (message "Workflow dispatched at %s" ref))
     #'gh-core--user-error)))

(transient-define-prefix gh-actions-dispatch ()
  "Actions list commands."
  [["View"
    ("g" "Refresh" gh-ui-refresh)
    ("W" "Workflows" gh-workflow-list)]])

(transient-define-prefix gh-run-dispatch ()
  "Workflow Run actions."
  [["Inspect"
    ("g" "Refresh" gh-ui-refresh)
    ("l" "Log" gh-run-log)
    ("w" "Watch" gh-run-watch)
    ("b" "Browse" gh-ui-browse)]
   ["Mutate"
    ("r" "Rerun" gh-run-rerun)
    ("j" "Rerun job" gh-run-rerun-job)
    ("x" "Cancel" gh-run-cancel)]])

(transient-define-prefix gh-workflow-dispatch ()
  "Workflow actions."
  [["Inspect"
    ("g" "Refresh" gh-ui-refresh)
    ("b" "Browse" gh-ui-browse)]
   ["Mutate"
    ("r" "Dispatch run" gh-workflow-dispatch-run)
    ("t" "Enable / disable" gh-workflow-toggle)]])

;;; Candidate registration

(gh-candidate-register
 'run
 :open (lambda (resource)
         (gh-run-view (plist-get resource :id) (plist-get resource :context)))
 :preview (lambda (resource)
            (gh-run-view (plist-get resource :id)
                         (plist-get resource :context) t))
 :dispatch (lambda (resource)
             (setq gh-actions--dispatch-resource resource)
             (call-interactively #'gh-run-dispatch)))

(gh-candidate-register
 'run-list :open (lambda (resource) (gh-run-list (plist-get resource :context))))

(gh-candidate-register
 'workflow
 :open (lambda (resource)
         (gh-workflow-view (or (plist-get resource :id)
                               (plist-get resource :path)
                               (plist-get resource :name))
                           (plist-get resource :context)))
 :preview (lambda (resource)
            (gh-workflow-view (or (plist-get resource :id)
                                  (plist-get resource :path)
                                  (plist-get resource :name))
                              (plist-get resource :context) nil t))
 :dispatch (lambda (resource)
             (setq gh-actions--workflow-id
                   (or (plist-get resource :id) (plist-get resource :path)))
             (call-interactively #'gh-workflow-dispatch)))

(gh-candidate-register
 'workflow-list :open (lambda (resource)
                        (gh-workflow-list (plist-get resource :context))))

(gh-candidate-register
 'job :open (lambda (resource)
              (gh-run-log (plist-get resource :context)
                          (plist-get resource :run-id)
                          (plist-get resource :id))))

(provide 'gh-actions)
;;; gh-actions.el ends here
