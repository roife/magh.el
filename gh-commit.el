;;; gh-commit.el --- Commit history and details for gh.el -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; Author: gh.el contributors
;; Keywords: tools, vc, github
;; Package-Requires: ((emacs "29.1") (transient "0.7.0"))

;;; Commentary:

;; Native commit history, details, parents, changed files, diff, and comments.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'transient)
(require 'gh-api)
(require 'gh-candidate)
(require 'gh-ui)

(defvar-local gh-commit--params nil)
(defvar-local gh-commit--limit nil)
(defvar-local gh-commit--sha nil)

(defun gh-commit--context (&optional context)
  "Resolve repository CONTEXT for Commit commands."
  (gh-context-resolve (or context gh-buffer-context) t))

(defun gh-commit--resource (context data)
  "Create Commit resource from DATA."
  (let* ((sha (gh-core--alist-get 'sha data))
         (commit (gh-core--alist-get 'commit data))
         (message (or (gh-core--alist-get 'message commit)
                      (gh-core--alist-get 'message data) "")))
    (gh-resource-create
     'commit context :sha sha :title (car (split-string message "\n"))
     :url (or (gh-core--alist-get 'html_url data)
              (gh-core--alist-get 'url data)
              (gh-context-web-url context (format "commit/%s" sha)))
     :data data)))

(defun gh-commit--author (data)
  "Return useful author text from commit DATA."
  (or (gh-core--name (gh-core--alist-get 'author data))
      (gh-core--name (gh-core--alist-get
                      'author (gh-core--alist-get 'commit data))) ""))

(defun gh-commit--date (data)
  "Return authored date from commit DATA."
  (or (gh-core--alist-get 'date
                          (gh-core--alist-get
                           'author (gh-core--alist-get 'commit data)))
      (gh-core--alist-get 'committedDate data)))

(defun gh-commit--insert-row (context data)
  "Insert Commit DATA row."
  (let* ((resource (gh-commit--resource context data))
         (sha (plist-get resource :sha)))
    (gh-ui--section (commit sha resource t)
      (gh-ui--row
       (gh-ui--styled (substring sha 0 (min 10 (length sha))) 'gh-hash)
       (gh-ui--styled (gh-resource-title resource) 'gh-resource-title)
       (gh-ui--styled (gh-commit--author data) 'gh-author)
       (gh-ui--styled (gh-core--date (gh-commit--date data)) 'gh-date))
      (gh-ui--insert-header "SHA" sha 'gh-hash)
      (gh-ui--insert-header "Author" (gh-commit--author data) 'gh-author))))

(defun gh-commit--render-list (context params data)
  "Render commit list DATA using PARAMS."
  (gh-ui--insert-header "Repository" (gh-context-repository context)
                        'gh-repository (gh-resource-create 'repository context))
  (gh-ui--insert-header "History"
                        (or (plist-get params :path) (plist-get params :ref) "HEAD"))
  (insert "\n")
  (if data
      (dolist (commit data) (gh-commit--insert-row context commit))
    (insert (propertize "No commits found.\n" 'font-lock-face 'shadow)))
  (gh-ui--section (more 'more (gh-resource-create 'commit-more context) t)
    (format "Load more (current limit %d)" gh-commit--limit)
    (insert "Press RET to double the history limit.\n")))

;;;###autoload
(defun gh-commit-list (&optional context ref path)
  "Open commit history in CONTEXT, optionally restricted to REF and PATH."
  (interactive)
  (setq context (gh-commit--context context)
        ref (or ref (gh-context-ref context)))
  (let ((params (list :ref ref :path path :limit gh-list-limit)))
    (gh-ui--open-page
     (format "*gh: %s · History%s*" (gh-context-repository context)
             (if path (format " · %s" path) ""))
     (gh-context-copy context :ref ref :path path) 'commit-list
     (format "%s:%s" ref path)
     (lambda (success error force)
       (gh-api--commit-list context params success error force))
     (lambda (data) (gh-commit--render-list context params data))
     :setup (lambda ()
              (setq gh-commit--params params gh-commit--limit gh-list-limit)))))

(defalias 'gh-history #'gh-commit-list)

(defun gh-commit-load-more ()
  "Double the current commit history limit."
  (interactive)
  (let* ((context gh-buffer-context)
         (params (plist-put (copy-sequence gh-commit--params)
                            :limit (* 2 gh-commit--limit))))
    (setq gh-commit--limit (plist-get params :limit)
          gh-commit--params params
          gh-buffer-refresh-function
          (lambda (success error force)
            (gh-api--commit-list context params success error force)))
    (gh-ui-refresh t)))

;;; Details

(defun gh-commit--fetch-view (context sha success error force)
  "Fetch Commit SHA details, comments, and diff."
  (gh-core--collect-async
   (list
    (cons 'commit (lambda (ok fail)
                    (gh-api--commit-get context sha ok fail force)))
    (cons 'comments (lambda (ok fail)
                     (gh-api--commit-comments context sha ok fail force)))
    (cons 'diff (lambda (ok fail)
                 (gh-api--commit-diff context sha ok fail force))))
   success error))

(defun gh-commit--render-view (context result)
  "Render Commit detail RESULT."
  (let* ((data (alist-get 'commit result))
         (comments (alist-get 'comments result))
         (diff (alist-get 'diff result))
         (resource (gh-commit--resource context data))
         (sha (gh-core--alist-get 'sha data))
         (commit (gh-core--alist-get 'commit data))
         (author (gh-core--alist-get 'author commit))
         (committer (gh-core--alist-get 'committer commit))
         (stats (gh-core--alist-get 'stats data))
         (files (or (gh-core--alist-get 'files data) nil)))
    (insert (propertize sha 'font-lock-face 'gh-hash) "\n")
    (add-text-properties (line-beginning-position 0) (point)
                         (list 'gh-resource resource))
    (gh-ui--insert-header "Author"
                          (format "%s <%s>"
                                  (or (gh-core--alist-get 'name author) "")
                                  (or (gh-core--alist-get 'email author) ""))
                          'gh-author)
    (gh-ui--insert-header "AuthorDate"
                          (gh-core--date (gh-core--alist-get 'date author)) 'gh-date)
    (gh-ui--insert-header "Committer"
                          (format "%s <%s>"
                                  (or (gh-core--alist-get 'name committer) "")
                                  (or (gh-core--alist-get 'email committer) ""))
                          'gh-author)
    (gh-ui--insert-header "CommitDate"
                          (gh-core--date (gh-core--alist-get 'date committer))
                          'gh-date)
    (gh-ui--insert-header
     "Changes" (format "+%s -%s, %d files"
                       (or (gh-core--alist-get 'additions stats) 0)
                       (or (gh-core--alist-get 'deletions stats) 0)
                       (length files)))
    (dolist (parent (gh-core--alist-get 'parents data))
      (let ((parent-resource (gh-commit--resource context parent)))
        (gh-ui--insert-header "Parent" (gh-core--alist-get 'sha parent)
                              'gh-hash parent-resource)))
    (insert "\n")
    (gh-ui--section (message 'message resource nil)
      (car (split-string (or (gh-core--alist-get 'message commit) "") "\n"))
      (gh-ui--insert-markdown (or (gh-core--alist-get 'message commit) "") context))
    (gh-ui--section (changed-files 'changed-files nil nil)
      (format "Changed files (%d)" (length files))
      (dolist (file files)
        (let* ((path (gh-core--alist-get 'filename file))
               (file-resource
                (gh-resource-create
                 'file (gh-context-copy context :ref sha :path path)
                 :path path :ref sha :data file)))
          (gh-ui--section (file path file-resource t)
            (gh-ui--row
             (gh-ui--styled path 'gh-file)
             (gh-ui--styled (or (gh-core--alist-get 'status file) "modified")
                            'gh-permission)
             (gh-ui--styled (format "+%s"
                                    (or (gh-core--alist-get 'additions file) 0))
                            'gh-added)
             (gh-ui--styled (format "-%s"
                                    (or (gh-core--alist-get 'deletions file) 0))
                            'gh-removed))
            (when-let* ((patch (gh-core--alist-get 'patch file)))
              (gh-ui--insert-diff patch)))))
    (gh-ui--section (diff 'diff nil t)
      "Full diff"
      (gh-ui--insert-diff diff))
    (gh-ui--section (comments 'comments nil nil)
      (format "Comments (%d)" (length comments))
      (cl-loop for comment in comments
               for index from 1
               do (gh-ui--section
                      (comment (or (gh-core--alist-get 'id comment) index) nil nil)
                    (gh-ui--row
                     (gh-ui--styled
                      (gh-core--name (gh-core--alist-get 'user comment))
                      'gh-author)
                     (gh-ui--styled
                      (gh-core--date (gh-core--alist-get 'created_at comment))
                      'gh-date))
                    (gh-ui--insert-markdown
                     (or (gh-core--alist-get 'body comment) "") context)))))))

(defun gh-commit--setup-view (_context sha)
  "Install Commit view keys for SHA."
  (setq gh-commit--sha sha
        gh-buffer-dispatch-function #'gh-commit-dispatch)
  (local-set-key (kbd "b") #'gh-commit-browse-tree)
  (local-set-key (kbd "c") #'gh-commit-comment))

(defun gh-commit-browse-tree (&optional context sha)
  "Browse the repository tree at commit SHA in CONTEXT."
  (interactive)
  (setq context (gh-commit--context context)
        sha (or sha gh-commit--sha))
  (unless sha (user-error "No commit at point or in this buffer"))
  (gh-resource-open
   (gh-resource-create 'tree (gh-context-copy context :ref sha :path "")
                       :ref sha :path "")))

;;;###autoload
(defun gh-commit-view (sha &optional context preview)
  "Open Commit SHA in CONTEXT."
  (interactive (list (read-string "Commit SHA: ")))
  (setq context (gh-commit--context context))
  (gh-ui--open-page
   (if preview
       (format "*gh preview: %s · %.10s*" (gh-context-repository context) sha)
     (format "*gh: %s · Commit %.10s*" (gh-context-repository context) sha))
   (gh-context-copy context :ref sha) 'commit sha
   (lambda (success error force)
     (gh-commit--fetch-view context sha success error force))
   (lambda (data) (gh-commit--render-view context data))
   :preview preview :setup (lambda () (gh-commit--setup-view context sha))))

(defun gh-commit-comment (body &optional context sha path line)
  "Add BODY as a comment on Commit SHA, optionally at PATH and LINE."
  (interactive (list (read-string "Commit comment: ")))
  (setq context (gh-commit--context context)
        sha (or sha gh-commit--sha))
  (gh-api--commit-comment
   context sha body path line
   (lambda (_) (message "Commit comment added")
     (when (derived-mode-p 'gh-section-mode) (gh-ui-refresh t)))
   #'gh-core--user-error))

(transient-define-prefix gh-commit-dispatch ()
  "Commit actions."
  [["Inspect"
   ("g" "Refresh" gh-ui-refresh)
    ("b" "Browse tree" gh-commit-browse-tree)
    ("o" "Browse web" gh-ui-browse)]
   ["Comment"
    ("c" "Add comment" gh-commit-comment)]])

(gh-candidate-register
 'commit
 :open (lambda (resource)
         (gh-commit-view (plist-get resource :sha)
                         (plist-get resource :context)))
 :preview (lambda (resource)
            (gh-commit-view (plist-get resource :sha)
                            (plist-get resource :context) t)))

(gh-candidate-register
 'commit-list
 :open (lambda (resource)
         (let ((context (plist-get resource :context)))
           (gh-commit-list context (or (plist-get resource :ref)
                                       (gh-context-ref context))
                           (plist-get resource :path)))))

(gh-candidate-register 'commit-more :open (lambda (_resource)
                                            (gh-commit-load-more)))

(provide 'gh-commit)
;;; gh-commit.el ends here
