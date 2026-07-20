;;; magh-commit.el --- Commit history and details for magh.el -*- lexical-binding: t; -*-

;; Copyright (C) 2026
;; SPDX-License-Identifier: GPL-2.0-only

;; Author: magh.el contributors
;; Keywords: tools, vc, github

;;; Commentary:

;; Native commit history, details, parents, changed files, diff, and comments.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'transient)
(require 'magh-api)
(require 'magh-candidate)
(require 'magh-diff)
(require 'magh-ui)

(defvar-local magh-commit--params nil)
(defvar-local magh-commit--sha nil)

(defun magh-commit--resource (context data)
  "Create Commit resource from DATA."
  (let* ((sha (alist-get 'sha data))
         (commit (alist-get 'commit data))
         (message (alist-get 'message commit)))
    (magh-resource-create
     'commit context :sha sha
     :title (car (string-lines message))
     :url (alist-get 'html_url data)
     :data data)))

(defun magh-commit--author (data)
  "Return useful author text from commit DATA."
  (or (magh-core--name (alist-get 'author data))
      (magh-core--name (alist-get
                      'author (alist-get 'commit data)))))

(defun magh-commit--insert-row (context data)
  "Insert Commit DATA row."
  (let* ((resource (magh-commit--resource context data))
         (sha (plist-get resource :sha)))
    (magh-ui--section (commit sha resource t)
      (magh-ui--row
       (magh-ui--styled (substring sha 0 10) 'magh-hash)
       (magh-ui--styled (magh-resource-title resource) 'magh-resource-title)
       (magh-ui--styled (magh-commit--author data) 'magh-author)
       (magh-ui--styled
        (magh-core--date
         (alist-get 'date (alist-get 'author (alist-get 'commit data))))
        'magh-date))
      (magh-ui--insert-header "SHA" sha 'magh-hash)
      (magh-ui--insert-header "Author" (magh-commit--author data) 'magh-author))))

(defun magh-commit--render-list (context params data)
  "Render commit list DATA using PARAMS."
  (magh-ui--insert-header "Repository" (magh-context-repository context)
                          'magh-repository
                          (magh-resource-create 'repository context))
  (magh-ui--insert-header
   "History" (or (plist-get params :path) (plist-get params :ref) "HEAD"))
  (insert "\n")
  (magh-ui--insert-paged-items
   context data (lambda (commit) (magh-commit--insert-row context commit))
   'commit-more :empty-message "No commits found."
   :more-message "Press RET to append more commits."
   :end-format "End of history (%d commits)."))

;;;###autoload
(defun magh-commit-list (&optional context ref path)
  "Open commit history in CONTEXT, optionally restricted to REF and PATH."
  (interactive (list (magh-context-read-repository)))
  (setq context (magh-ui--repository-context context)
        ref (or ref (magh-context-ref context)))
  (let ((params (list :ref ref :path path :limit magh-list-limit)))
    (magh-ui--open-page
     (format "*magh: %s · History%s*" (magh-context-repository context)
             (if path (format " · %s" path) ""))
     (magh-context-copy context :ref ref :path path) 'commit-list
     (format "%s:%s" ref path)
     (lambda (success error force)
       (magh-api--commit-page context params nil success error force))
     (lambda (data) (magh-commit--render-list context params data))
     :setup (lambda () (setq magh-commit--params params)))))

(defun magh-commit-load-more ()
  "Append the next page to the current commit history."
  (interactive)
  (let ((context magh-buffer-context)
        (params magh-commit--params))
    (magh-ui--load-next-page
     (lambda (page success error)
       (magh-api--commit-page context params page success error))
     "commits")))

;;; Details

(defun magh-commit--fetch-view (context sha success error force)
  "Fetch Commit SHA details, comments, and diff."
  (magh-core--collect-async
   (list
    (cons 'commit (lambda (ok fail)
                    (magh-api--commit-get context sha ok fail force)))
    (cons 'comments (lambda (ok fail)
                     (magh-api--commit-comments context sha ok fail force)))
    (cons 'diff (lambda (ok fail)
                 (magh-api--commit-diff context sha ok fail force))))
   success error))

(defun magh-commit--commit-comment-resource (context sha comment)
  "Create a structured resource for COMMIT comment data."
  (magh-resource-create
   'commit-comment context :id (alist-get 'id comment) :sha sha
   :path (alist-get 'path comment) :position (alist-get 'position comment)
   :line (alist-get 'line comment) :data comment
   :url (alist-get 'html_url comment)))

(defun magh-commit--commit-comment-heading (comment &optional inline)
  "Return a heading for COMMIT COMMENT, labeled INLINE when anchored."
  (magh-ui--conversation-heading
   (if inline "Inline comment" "Comment")
   (magh-core--name (alist-get 'user comment))
   (magh-core--date (alist-get 'created_at comment))))

(defun magh-commit--insert-commit-comment-body (context comment)
  "Insert COMMIT COMMENT metadata and body in CONTEXT."
  (when (alist-get 'path comment)
    (magh-ui--insert-header
     "Location"
     (format "%s:%s"
             (alist-get 'path comment)
             (or (alist-get 'line comment)
                 (format "diff@%s" (alist-get 'position comment))))
     'magh-file))
  (magh-ui--insert-markdown (alist-get 'body comment) context))

(defun magh-commit--insert-commit-comment (context sha comment &optional inline)
  "Insert COMMIT comment, labeling it INLINE when anchored to a diff."
  (let ((resource (magh-commit--commit-comment-resource context sha comment))
        (heading (magh-commit--commit-comment-heading comment inline)))
    (if inline
        (magh-ui--section (inline-comment (alist-get 'id comment) resource nil)
          heading
          (magh-commit--insert-commit-comment-body context comment))
      (magh-ui--section (comment (alist-get 'id comment) resource nil)
        heading
        (magh-commit--insert-commit-comment-body context comment)))))

(defun magh-commit--insert-commit-patch
    (context sha path patch comments)
  "Insert PATCH for PATH and place matching commit COMMENTS inline."
  (let (inserted)
    (if (string-empty-p (or patch ""))
        (insert (propertize "Binary or oversized diff unavailable.\n"
                            'font-lock-face 'shadow))
      (magh-diff--insert-patch-records
       (magh-diff--parse-patch-lines
        patch context nil sha path 'commit-line)
       (lambda (record)
         (let ((start (point))
               (resource (plist-get record :resource)))
           (magh-ui--insert-diff (concat (plist-get record :text) "\n"))
           (when resource
             (add-text-properties start (point) (list 'magh-resource resource))
             (dolist (comment comments)
               (when (and (not (memq comment inserted))
                          (equal (alist-get 'position comment)
                                 (plist-get resource :position)))
                 (push comment inserted)
                 (magh-commit--insert-commit-comment
                  context sha comment t))))))))
    (let ((unmapped (seq-remove (lambda (comment) (memq comment inserted))
                                comments)))
      (when unmapped
        (magh-ui--section (unmapped-comments path nil nil)
          "Unmapped inline comments"
          (dolist (comment unmapped)
            (magh-commit--insert-commit-comment context sha comment t)))))))

(defun magh-commit--diff-heading-matches (regexp text)
  "Return heading matches for REGEXP in diff TEXT."
  (let ((position 0)
        matches)
    (while (string-match regexp text position)
      (push (list :start (match-beginning 0)
                  :end (match-end 0)
                  :heading (match-string 0 text))
            matches)
      (setq position (match-end 0)))
    (nreverse matches)))

(defun magh-commit--diff-heading-body-start (text heading)
  "Return body start in TEXT immediately after HEADING."
  (let ((end (plist-get heading :end)))
    (if (and (< end (length text)) (eq (aref text end) ?\n))
        (1+ end)
      end)))

(defun magh-commit--split-diff-hunks (text)
  "Split diff file body TEXT into its preamble and hunk records."
  (let* ((matches (magh-commit--diff-heading-matches "^@@+ .*$" text))
         (preamble (substring text 0 (or (plist-get (car matches) :start)
                                         (length text))))
         hunks)
    (while matches
      (let* ((heading (pop matches))
             (body-start (magh-commit--diff-heading-body-start text heading))
             (body-end (or (plist-get (car matches) :start) (length text))))
        (push (list :heading (plist-get heading :heading)
                    :body (substring text body-start body-end))
              hunks)))
    (list :preamble preamble :hunks (nreverse hunks))))

(defun magh-commit--split-full-diff (diff)
  "Split unified DIFF into file records containing hunk records."
  (let* ((text (magh-ui--normalize-newlines (or diff "")))
         (matches (magh-commit--diff-heading-matches "^diff --git .*$" text))
         files)
    (while matches
      (let* ((heading (pop matches))
             (body-start (magh-commit--diff-heading-body-start text heading))
             (body-end (or (plist-get (car matches) :start) (length text)))
             (parts (magh-commit--split-diff-hunks
                     (substring text body-start body-end))))
        (push (list :heading (plist-get heading :heading)
                    :preamble (plist-get parts :preamble)
                    :hunks (plist-get parts :hunks))
              files)))
    (nreverse files)))

(defun magh-commit--insert-full-diff (diff)
  "Insert DIFF as foldable file and hunk sections."
  (let ((files (magh-commit--split-full-diff diff)))
    (if (null files)
        (magh-ui--insert-diff diff)
      (cl-loop
       for file in files
       for file-index from 1
       do
       (magh-ui--section (diff-file file-index nil nil)
         (magh-ui--diff-file-heading (plist-get file :heading))
         (unless (string-empty-p (plist-get file :preamble))
           (magh-ui--insert-diff (plist-get file :preamble)))
         (cl-loop
          for hunk in (plist-get file :hunks)
          for hunk-index from 1
          do
          (magh-ui--section (diff-hunk (cons file-index hunk-index) nil nil)
            (magh-ui--diff-hunk-heading (plist-get hunk :heading))
            (unless (string-empty-p (plist-get hunk :body))
              (magh-ui--insert-diff (plist-get hunk :body))))))))))

(defun magh-commit--render-view (context result)
  "Render Commit detail RESULT."
  (let* ((data (alist-get 'commit result))
         (comments (alist-get 'comments result))
         (diff (alist-get 'diff result))
         (resource (magh-commit--resource context data))
         (sha (alist-get 'sha data))
         (commit (alist-get 'commit data))
         (author (alist-get 'author commit))
         (committer (alist-get 'committer commit))
         (stats (alist-get 'stats data))
         (files (alist-get 'files data))
         (file-paths (mapcar (lambda (file) (alist-get 'filename file)) files)))
    (insert (propertize sha 'font-lock-face 'magh-hash) "\n")
    (add-text-properties (line-beginning-position 0) (point)
                         (list 'magh-resource resource))
    (magh-ui--insert-header "Author"
                          (format "%s <%s>"
                                  (alist-get 'name author)
                                  (alist-get 'email author))
                          'magh-author)
    (magh-ui--insert-header "AuthorDate"
                          (magh-core--date (alist-get 'date author)) 'magh-date)
    (magh-ui--insert-header "Commit"
                          (format "%s <%s>"
                                  (alist-get 'name committer)
                                  (alist-get 'email committer))
                          'magh-author)
    (magh-ui--insert-header "CommitDate"
                          (magh-core--date (alist-get 'date committer))
                          'magh-date)
    (magh-ui--insert-header
     "Changes" (format "+%s -%s, %d files"
                       (alist-get 'additions stats)
                       (alist-get 'deletions stats)
                       (length files)))
    (dolist (parent (alist-get 'parents data))
      (let ((parent-resource (magh-commit--resource context parent)))
        (magh-ui--insert-header "Parent" (alist-get 'sha parent)
                              'magh-hash parent-resource)))
    (insert "\n")
    (let* ((message (string-trim
                     (magh-ui--normalize-newlines
                      (alist-get 'message commit))))
           (message (if (string-empty-p message) "(no message)" message))
           (newline (string-match "\n" message))
           (summary (if newline (substring message 0 newline) message))
           (body (and newline (string-trim-right
                               (substring message (1+ newline))))))
      (magh-ui--section (message 'message resource nil)
        (magh-ui--styled summary 'magit-diff-revision-summary)
        (unless (string-empty-p (or body ""))
          (insert body)
          (unless (bolp) (insert "\n")))))
    (magh-ui--section (changed-files 'changed-files nil nil)
      (format "Changed files (%d)" (length files))
      (dolist (file files)
        (let* ((path (alist-get 'filename file))
               (file-resource
                (magh-resource-create
                 'file (magh-context-copy context :ref sha :path path)
                 :path path :ref sha :data file)))
          (magh-ui--section (file path file-resource nil)
            (magh-ui--diff-file-heading
             (magh-ui--row
              (magh-ui--styled path 'magh-file)
              (magh-ui--styled (alist-get 'status file) 'magh-permission)
              (magh-ui--styled (format "+%s" (alist-get 'additions file))
                             'magh-added)
              (magh-ui--styled (format "-%s" (alist-get 'deletions file))
                             'magh-removed)))
            (magh-commit--insert-commit-patch
             context sha path (alist-get 'patch file)
             (seq-filter
              (lambda (comment) (equal (alist-get 'path comment) path))
              comments))))))
    (magh-ui--section (diff 'diff nil t)
      "Full diff"
      (magh-commit--insert-full-diff diff))
    (magh-ui--section (comments 'comments nil nil)
      (format "Comments (%d)" (length comments))
      (dolist (comment comments)
        (when (or (not (alist-get 'path comment))
                  (not (member (alist-get 'path comment) file-paths)))
          (magh-commit--insert-commit-comment context sha comment))))))

(defun magh-commit-browse-tree (&optional context sha)
  "Browse the repository tree at commit SHA in CONTEXT."
  (interactive)
  (setq context (magh-ui--repository-context context)
        sha (or sha magh-commit--sha))
  (unless sha (user-error "No commit at point or in this buffer"))
  (magh-resource-open
   (magh-resource-create 'tree (magh-context-copy context :ref sha :path "")
                       :ref sha :path "")))

;;;###autoload
(defun magh-commit-view (sha &optional context preview)
  "Open Commit SHA in CONTEXT."
  (interactive (list (read-string "Commit SHA: ")))
  (setq context (magh-ui--repository-context context))
  (magh-ui--open-page
   (if preview
       (format "*magh preview: %s · %.10s*" (magh-context-repository context) sha)
     (format "*magh: %s · Commit %.10s*" (magh-context-repository context) sha))
   (magh-context-copy context :ref sha) 'commit sha
   (lambda (success error force)
     (magh-commit--fetch-view context sha success error force))
   (lambda (data) (magh-commit--render-view context data))
   :preview preview
   :setup (lambda ()
            (setq magh-commit--sha sha
                  magh-buffer-dispatch-function #'magh-commit-dispatch)
            (local-set-key (kbd "b") #'magh-commit-browse-tree)
            (local-set-key (kbd "c") #'magh-commit-inline-comment)
            (local-set-key (kbd "C") #'magh-commit-comment))))

(defun magh-commit-comment (body &optional context sha path position)
  "Add BODY on Commit SHA, optionally at PATH and diff POSITION."
  (interactive (list (read-string "Commit comment: ")))
  (setq context (magh-ui--repository-context context)
        sha (or sha magh-commit--sha))
  (when (string-empty-p (string-trim (or body "")))
    (user-error "Commit comment cannot be empty"))
  (magh-api--commit-comment
   context sha body path position
   (magh-ui--refresh-message "Commit comment added")
   #'magh-core--user-error))

(defun magh-commit-inline-comment (body &optional context sha)
  "Add BODY as an inline comment at the selected Commit diff position."
  (interactive (list (read-string "Inline commit comment: ")))
  (let ((location (magh-diff--commit-selection)))
    (magh-commit-comment body context sha
                       (plist-get location :path)
                       (plist-get location :position))))

(transient-define-prefix magh-commit-dispatch ()
  "Commit actions."
  [["Inspect"
   ("g" "Refresh" magh-ui-refresh)
    ("b" "Browse tree" magh-commit-browse-tree)
    ("o" "Browse web" magh-ui-browse)]
   ["Comment"
    ("c" "Inline comment" magh-commit-inline-comment)
    ("C" "General comment" magh-commit-comment)]])

(magh-candidate-register
 'commit
 :open (lambda (resource)
         (magh-commit-view (plist-get resource :sha)
                         (plist-get resource :context)))
 :preview (lambda (resource)
            (magh-commit-view (plist-get resource :sha)
                            (plist-get resource :context) t)))

(magh-candidate-register
 'commit-list
 :open (lambda (resource)
         (let ((context (plist-get resource :context)))
           (magh-commit-list context (or (plist-get resource :ref)
                                       (magh-context-ref context))
                           (plist-get resource :path)))))

(magh-candidate-register 'commit-more :open (lambda (_resource)
                                            (magh-commit-load-more)))

(provide 'magh-commit)
;;; magh-commit.el ends here
