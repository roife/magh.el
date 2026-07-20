;;; magh-actions.el --- GitHub Actions frontend for magh.el -*- lexical-binding: t; -*-

;; Copyright (C) 2026
;; SPDX-License-Identifier: GPL-2.0-only

;; Author: magh.el contributors
;; Keywords: tools, vc, github

;;; Commentary:

;; Native workflow/run/job/step pages and asynchronous logs, watch, dispatch,
;; enable, disable, rerun, job rerun, and cancellation.

;;; Code:

(require 'subr-x)
(require 'transient)
(require 'magh-api)
(require 'magh-candidate)
(require 'magh-ui)

(defvar-local magh-actions--dispatch-resource nil)
(defvar-local magh-actions--run-id nil)
(defvar-local magh-actions--workflow-id nil)

(defun magh-actions--run-resource (context data)
  "Create Run resource from DATA."
  (magh-resource-create
   'run context :id (alist-get 'databaseId data)
   :title (alist-get 'displayTitle data)
   :url (alist-get 'url data)))

(defun magh-actions--run-state (data)
  "Return display state for Run DATA."
  (or (alist-get 'conclusion data)
      (alist-get 'status data)))

(defun magh-actions--insert-run (context data &optional workflow-page)
  "Insert Run DATA in CONTEXT.
When WORKFLOW-PAGE is non-nil, use the compact layout from the Workflow page."
  (let* ((resource (magh-actions--run-resource context data))
         (id (plist-get resource :id))
         (state (magh-actions--run-state data))
         (title (alist-get 'displayTitle data))
         (workflow (or (alist-get 'workflowName data)
                       (alist-get 'name data)))
         (branch (alist-get 'headBranch data))
         (created (magh-core--date (alist-get 'createdAt data))))
    (magh-ui--section (run id resource t)
      (magh-ui--row
       (magh-ui--styled (upcase state) (magh-core--state-face state))
       (magh-ui--styled title 'magh-resource-title)
       (if workflow-page
           (magh-ui--styled branch 'magh-branch)
         (magh-ui--styled workflow 'magh-workflow)))
      (unless workflow-page
        (magh-ui--insert-header "Branch" branch 'magh-branch))
      (magh-ui--insert-header "Event" (alist-get 'event data))
      (magh-ui--insert-header "Created" created 'magh-date)
      (magh-ui--insert-header "Commit" (alist-get 'headSha data)
                            'magh-hash))))

(defun magh-actions--render-list (context data)
  "Render Actions Run list DATA."
  (magh-ui--insert-header "Repository" (magh-context-repository context)
                        'magh-repository (magh-resource-create 'repository context))
  (magh-ui--insert-header "Actions" (format "%d runs" (length data)))
  (insert "\n")
  (if data
      (dolist (run data) (magh-actions--insert-run context run))
    (insert (propertize "No matching workflow runs.\n"
                        'font-lock-face 'shadow))))

;;;###autoload
(defun magh-run-list (&optional context params)
  "Open recent workflow runs in CONTEXT filtered by PARAMS."
  (interactive (list (magh-context-read-repository)))
  (setq context (magh-ui--repository-context context))
  (magh-ui--open-page
   (format "*magh: %s · Actions*" (magh-context-repository context))
   context 'run-list (magh-context-repository context)
   (lambda (success error force)
     (magh-api--run-list context params success error force))
   (lambda (data) (magh-actions--render-list context data))
   :setup
   (lambda ()
     (local-set-key (kbd "W")
                    (lambda () (interactive) (magh-workflow-list context)))
     (setq magh-buffer-dispatch-function #'magh-actions-dispatch))))

;;; Run details

(defun magh-actions--render-job (context run-id job)
  "Render JOB for RUN-ID."
  (let* ((resource (magh-resource-create
                    'job context :id (alist-get 'databaseId job)
                    :run-id run-id :title (alist-get 'name job)
                    :url (alist-get 'url job)))
         (id (plist-get resource :id))
         (state (or (alist-get 'conclusion job)
                    (alist-get 'status job))))
    (magh-ui--section (job id resource nil)
      (magh-ui--row
       (magh-ui--styled (upcase state) (magh-core--state-face state))
       (magh-ui--styled (alist-get 'name job)
                      'magh-resource-title))
      (dolist (step (alist-get 'steps job))
        (let ((step-state (or (alist-get 'conclusion step)
                              (alist-get 'status step))))
          (magh-ui--section (step (alist-get 'number step) resource t)
            (magh-ui--row
             (magh-ui--styled (upcase step-state)
                            (magh-core--state-face step-state))
             (magh-ui--styled (alist-get 'name step)
                            'magh-resource-title))))))))

(defun magh-actions--render-run (context data)
  "Render workflow Run DATA."
  (let* ((resource (magh-actions--run-resource context data))
         (id (plist-get resource :id))
         (state (magh-actions--run-state data))
         (workflow-resource
          (magh-resource-create
           'workflow context
           :id (alist-get 'workflowDatabaseId data)
           :title (or (alist-get 'workflowName data)
                      (alist-get 'name data)))))
    (insert (propertize (upcase state)
                        'font-lock-face (magh-core--state-face state)) " "
            (propertize (alist-get 'displayTitle data)
                        'font-lock-face 'magh-resource-title) "\n")
    (add-text-properties (line-beginning-position 0) (point)
                         (list 'magh-resource resource))
    (magh-ui--insert-header "Workflow"
                          (or (alist-get 'workflowName data)
                              (alist-get 'name data))
                          'magh-workflow workflow-resource)
    (magh-ui--insert-header "Status" (alist-get 'status data)
                          (magh-core--state-face
                           (alist-get 'status data)))
    (magh-ui--insert-header "Conclusion" (alist-get 'conclusion data)
                          (magh-core--state-face state))
    (magh-ui--insert-header "Branch" (alist-get 'headBranch data)
                          'magh-branch
                          (magh-resource-create
                           'tree (magh-context-copy
                                  context :ref (alist-get 'headBranch data))
                           :ref (alist-get 'headBranch data) :path ""))
    (magh-ui--insert-header "Commit" (alist-get 'headSha data) 'magh-hash
                          (magh-resource-create
                           'commit context :sha (alist-get 'headSha data)))
    (magh-ui--insert-header "Event" (alist-get 'event data))
    (magh-ui--insert-header "Created"
                          (magh-core--date (alist-get 'createdAt data))
                          'magh-date)
    (magh-ui--insert-header "Updated"
                          (magh-core--date (alist-get 'updatedAt data))
                          'magh-date)
    (insert "\n")
    (dolist (job (alist-get 'jobs data))
      (magh-actions--render-job context id job))))

(defun magh-actions--setup-run (context id)
  "Install Run detail keys for CONTEXT and ID."
  (setq magh-actions--run-id id
        magh-buffer-dispatch-function
        (lambda ()
          (setq magh-actions--dispatch-resource
                (magh-resource-create 'run context :id id))
          (call-interactively #'magh-run-dispatch)))
  (local-set-key (kbd "l") (lambda () (interactive) (magh-run-log context id)))
  (local-set-key (kbd "w") (lambda () (interactive) (magh-run-watch context id))))

(defun magh-run-view (id &optional context preview)
  "Open workflow Run ID in CONTEXT."
  (interactive (list (read-number "Run ID: ")))
  (setq context (magh-ui--repository-context context))
  (magh-ui--open-page
   (if preview
       (format "*magh preview: %s · Run %s*"
               (magh-context-repository context) id)
     (format "*magh: %s · Run %s*" (magh-context-repository context) id))
   context 'run id
   (lambda (success error force)
     (magh-api--run-get context id success error force))
   (lambda (data) (magh-actions--render-run context data))
   :preview preview :setup (lambda () (magh-actions--setup-run context id))))

;;; Logs and watch

(defun magh-actions--text-buffer (name)
  "Create a read-only text buffer NAME."
  (let ((buffer (get-buffer-create name)))
    (with-current-buffer buffer
      (let ((inhibit-read-only t)) (erase-buffer))
      (special-mode))
    buffer))

(defun magh-actions--short-log-time (payload)
  "Shorten an ISO timestamp prefix in Actions log PAYLOAD to HH:MM:SS."
  (if (string-match
       "\\`[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9]T\\([0-9][0-9]:[0-9][0-9]:[0-9][0-9]\\)\\(?:\\.[0-9]+\\)?Z\\(.*\\)\\'"
       payload)
      (concat (match-string 1 payload) (match-string 2 payload))
    payload))

(defun magh-actions--simplify-log (text)
  "Group repeated job and step columns in Actions log TEXT.
The `gh run view --log' output repeats both columns on every line.  Emit each
job and step once, shorten ISO timestamps, and preserve ANSI escapes."
  (let (last-job last-step lines)
    (dolist (line (split-string text "\n"))
      (if (string-match "\\`\\([^\t]+\\)\t\\([^\t]+\\)\t\\(.*\\)\\'" line)
          (let ((job (match-string 1 line))
                (step (match-string 2 line))
                (payload (magh-actions--short-log-time
                          (match-string 3 line))))
            (unless (equal job last-job)
              (when last-job (push "" lines))
              (push (magh-ui--styled job 'magh-workflow) lines)
              (setq last-job job last-step nil))
            (unless (equal step last-step)
              (push (concat "  " (magh-ui--styled step 'magh-resource-title)) lines)
              (setq last-step step))
            (push payload lines))
        (push line lines)))
    (string-join (nreverse lines) "\n")))

(defun magh-run-log (&optional context id job-id)
  "Open complete log for Run ID, optionally restricted to JOB-ID."
  (interactive)
  (setq context (magh-ui--repository-context context)
        id (or id magh-actions--run-id
               (plist-get magh-actions--dispatch-resource :id)))
  (let ((buffer (magh-actions--text-buffer
                 (format "*magh: %s · Run %s Log*"
                         (magh-context-repository context) id))))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (insert (propertize "Loading Actions log…\n"
                            'font-lock-face 'magh-loading)))
      (magh-api--run-log
       context id job-id
       (lambda (text)
         (let ((inhibit-read-only t))
           (erase-buffer)
           (magh-ui--insert-ansi (magh-actions--simplify-log text))
           (goto-char (point-min))))
       (lambda (error)
         (let ((inhibit-read-only t))
           (erase-buffer) (insert (magh-error-message error) "\n")))))
    (funcall magh-display-buffer-function buffer)))

(defun magh-run-watch (&optional context id)
  "Watch Run ID without blocking Emacs."
  (interactive)
  (setq context (magh-ui--repository-context context)
        id (or id magh-actions--run-id
               (plist-get magh-actions--dispatch-resource :id)))
  (let ((buffer (magh-actions--text-buffer
                 (format "*magh: %s · Watch Run %s*"
                         (magh-context-repository context) id))))
    (with-current-buffer buffer
      (magh-api--run-watch
       context id
       (lambda (_text)
         (let ((inhibit-read-only t))
           (goto-char (point-max))
           (insert "\nRun watch completed.\n")))
       (lambda (error)
         (let ((inhibit-read-only t))
           (goto-char (point-max))
           (insert "\n" (magh-error-message error) "\n")))
       (lambda (chunk _request)
         (when (buffer-live-p buffer)
           (with-current-buffer buffer
             (let ((inhibit-read-only t)
                   (start (point-max)))
               (goto-char start) (insert chunk)
               (ansi-color-apply-on-region start (point-max))
               (magh-ui--adopt-font-lock-faces start (point-max))))))))
    (funcall magh-display-buffer-function buffer)))

;;; Workflow

(defun magh-actions--workflow-resource (context data)
  "Create Workflow resource from DATA."
  (magh-resource-create
   'workflow context :id (alist-get 'id data)
   :title (alist-get 'name data) :path (alist-get 'path data)
   :url (alist-get 'html_url data)
   :data data))

;;;###autoload
(defun magh-workflow-list (&optional context)
  "Select a workflow in CONTEXT with native preview."
  (interactive (list (magh-context-read-repository)))
  (setq context (magh-ui--repository-context context))
  (magh-api--workflow-list
   context
   (lambda (items)
     (magh-candidate-select-and-open
      "Workflow: "
      (mapcar (lambda (item) (magh-actions--workflow-resource context item))
              items)
      (lambda (item)
        (let ((data (plist-get item :data)))
          (magh-ui--row
           (magh-ui--styled (upcase (alist-get 'state data))
                          (magh-core--state-face (alist-get 'state data)))
           (magh-ui--styled (magh-resource-title item) 'magh-workflow)
           (magh-ui--styled (plist-get item :path) 'magh-file))))
      t))
   #'magh-core--user-error))

(defun magh-actions--fetch-workflow
    (context workflow ref success error force)
  "Fetch WORKFLOW details, YAML, and recent runs at REF."
  (magh-api--workflow-get
   context workflow
   (lambda (metadata)
     (let ((path (alist-get 'path metadata)))
       (magh-core--collect-async
        (list
         (cons 'configuration
               (lambda (ok fail)
                 (magh-api--content-get context path ref ok fail force)))
         (cons 'runs
               (lambda (ok fail)
                 (magh-api--run-list
                  context (list :workflow (format "%s" workflow) :branch ref)
                  ok fail force))))
        (lambda (result)
          (funcall success (cons (cons 'metadata metadata) result)))
        error)))
   error force))

(defun magh-actions--render-workflow (context _workflow ref result)
  "Render workflow RESULT in CONTEXT at REF."
  (let* ((metadata (alist-get 'metadata result))
         (configuration (alist-get 'configuration result))
         (runs (alist-get 'runs result))
         (resource (magh-actions--workflow-resource context metadata))
         (path (alist-get 'path metadata)))
    (magh-ui--insert-header "Repository" (magh-context-repository context)
                          'magh-repository)
    (magh-ui--insert-header "Workflow" (alist-get 'name metadata)
                          'magh-workflow resource)
    (magh-ui--insert-header "State" (alist-get 'state metadata)
                          (magh-core--state-face
                           (alist-get 'state metadata)))
    (magh-ui--insert-header
     "Path" path 'magh-file
     (magh-resource-create 'file (magh-context-copy context :ref ref :path path)
                         :path path :ref ref))
    (magh-ui--insert-header "Ref" ref 'magh-branch
                          (magh-resource-create 'tree
                                              (magh-context-copy context :ref ref)
                                              :ref ref :path ""))
    (insert "\n")
    (magh-ui--section (configuration 'configuration nil nil)
      "Configuration"
      (insert (magh-api--decode-content configuration))
      (unless (bolp) (insert "\n")))
    (magh-ui--section (runs 'recent-runs nil nil)
      (format "Recent runs (%d)" (length runs))
      (dolist (run runs) (magh-actions--insert-run context run t)))))

(defun magh-workflow-view (workflow &optional context ref preview)
  "Open WORKFLOW details in CONTEXT at REF."
  (interactive (list (read-string "Workflow ID, name, or path: ")))
  (setq context (magh-ui--repository-context context)
        ref (or ref (magh-context-ref context)
                (magh-context-default-branch context) "HEAD"))
  (magh-ui--open-page
   (if preview
       (format "*magh preview: %s · Workflow %s*"
               (magh-context-repository context) workflow)
     (format "*magh: %s · Workflow %s*"
             (magh-context-repository context) workflow))
   (magh-context-copy context :ref ref) 'workflow workflow
   (lambda (success error force)
     (magh-actions--fetch-workflow context workflow ref success error force))
   (lambda (data) (magh-actions--render-workflow context workflow ref data))
   :preview preview
   :setup (lambda ()
            (setq magh-actions--workflow-id workflow
                  magh-buffer-dispatch-function #'magh-workflow-dispatch))))

;;; Mutations

(defun magh-run-rerun (&optional failed-only context id)
  "Rerun Run ID; with FAILED-ONLY, rerun only failed jobs."
  (interactive "P")
  (setq context (magh-ui--repository-context context)
        id (or id magh-actions--run-id
               (plist-get magh-actions--dispatch-resource :id)))
  (magh-api--run-rerun
   context id failed-only
   (lambda (_) (message "Rerun requested for %s" id)
     (magh-ui--refresh-if-page))
   #'magh-core--user-error))

;;;###autoload
(defun magh-run-rerun-job (&optional context job-id)
  "Choose or rerun JOB-ID in the current run."
  (interactive)
  (setq context (magh-ui--repository-context context))
  (let* ((resource (magh-ui-resource-at-point))
         (job-id (or job-id (and (eq (plist-get resource :kind) 'job)
                                  (plist-get resource :id)))))
    (unless job-id
      (let* ((run (and (eq magh-buffer-resource-kind 'run) magh-ui--data))
             (jobs (and run (alist-get 'jobs run)))
             (choices (mapcar (lambda (job)
                                (cons (alist-get 'name job) job)) jobs))
             (choice (and choices
                          (completing-read "Job: " choices nil t)))
             (job (and choice (cdr (assoc choice choices)))))
        (setq job-id (and job (alist-get 'databaseId job)))))
    (unless job-id (user-error "No job selected"))
    (magh-api--run-rerun-job
     context job-id
     (lambda (_) (message "Rerun requested for job %s" job-id))
     #'magh-core--user-error)))

(defun magh-run-cancel (&optional context id)
  "Cancel Run ID."
  (interactive)
  (setq context (magh-ui--repository-context context)
        id (or id magh-actions--run-id
               (plist-get magh-actions--dispatch-resource :id)))
  (when (magh-core--confirm (format "Cancel Actions run %s? " id))
    (magh-api--run-cancel
     context id (lambda (_) (message "Cancelled run %s" id)
                  (magh-ui--refresh-if-page))
     #'magh-core--user-error)))

(defun magh-workflow-toggle (&optional disable context workflow)
  "Enable WORKFLOW, or DISABLE with prefix argument."
  (interactive "P")
  (setq context (magh-ui--repository-context context)
        workflow (or workflow magh-actions--workflow-id))
  (magh-api--workflow-enable
   context workflow (not disable)
   (lambda (_) (message "Workflow %s" (if disable "disabled" "enabled"))
     (magh-ui--refresh-if-page))
   #'magh-core--user-error))

(defun magh-workflow-dispatch-run (&optional context workflow)
  "Manually dispatch WORKFLOW with arbitrary key=value inputs."
  (interactive)
  (setq context (magh-ui--repository-context context)
        workflow (or workflow magh-actions--workflow-id))
  (let ((ref (read-string "Ref: " (or (magh-context-ref context) "main")))
        inputs input)
    (while (not (string-empty-p
                 (setq input (read-string "Input key=value (empty to finish): "))))
      (push (magh-core--parse-key-value input) inputs))
    (magh-api--workflow-dispatch
     context workflow ref (nreverse inputs)
     (lambda (_) (message "Workflow dispatched at %s" ref))
     #'magh-core--user-error)))

(transient-define-prefix magh-actions-dispatch ()
  "Actions list commands."
  [["View"
    ("g" "Refresh" magh-ui-refresh)
    ("W" "Workflows" magh-workflow-list)]])

(transient-define-prefix magh-run-dispatch ()
  "Workflow Run actions."
  [["Inspect"
    ("g" "Refresh" magh-ui-refresh)
    ("l" "Log" magh-run-log)
    ("w" "Watch" magh-run-watch)
    ("b" "Browse" magh-ui-browse)]
   ["Mutate"
    ("r" "Rerun" magh-run-rerun)
    ("j" "Rerun job" magh-run-rerun-job)
    ("x" "Cancel" magh-run-cancel)]])

(transient-define-prefix magh-workflow-dispatch ()
  "Workflow actions."
  [["Inspect"
    ("g" "Refresh" magh-ui-refresh)
    ("b" "Browse" magh-ui-browse)]
   ["Mutate"
    ("r" "Dispatch run" magh-workflow-dispatch-run)
    ("t" "Enable / disable" magh-workflow-toggle)]])

;;; Candidate registration

(magh-candidate-register
 'run
 :open (lambda (resource)
         (magh-run-view (plist-get resource :id) (plist-get resource :context)))
 :preview (lambda (resource)
            (magh-run-view (plist-get resource :id)
                         (plist-get resource :context) t))
 :dispatch (lambda (resource)
             (setq magh-actions--dispatch-resource resource)
             (call-interactively #'magh-run-dispatch)))

(magh-candidate-register
 'run-list :open (lambda (resource) (magh-run-list (plist-get resource :context))))

(magh-candidate-register
 'workflow
 :open (lambda (resource)
         (magh-workflow-view (plist-get resource :id)
                           (plist-get resource :context)))
 :preview (lambda (resource)
            (magh-workflow-view (plist-get resource :id)
                              (plist-get resource :context) nil t))
 :dispatch (lambda (resource)
             (setq magh-actions--workflow-id (plist-get resource :id))
             (call-interactively #'magh-workflow-dispatch)))

(magh-candidate-register
 'workflow-list :open (lambda (resource)
                        (magh-workflow-list (plist-get resource :context))))

(magh-candidate-register
 'job :open (lambda (resource)
              (magh-run-log (plist-get resource :context)
                          (plist-get resource :run-id)
                          (plist-get resource :id))))

(provide 'magh-actions)
;;; magh-actions.el ends here
