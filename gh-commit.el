;;; gh-commit.el --- Commit history and details for gh.el -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; Author: gh.el contributors
;; Keywords: tools, vc, github
;; Package-Requires: ((emacs "31.1") (transient "0.7.0"))

;;; Commentary:

;; Native commit history, details, parents, changed files, diff, and comments.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'transient)
(require 'gh-api)
(require 'gh-candidate)
(require 'gh-ui)

(defvar-local gh-commit--params nil)
(defvar-local gh-commit--limit nil)
(defvar-local gh-commit--sha nil)
(defvar-local gh-commit--review-number nil)
(defvar-local gh-commit--review-head nil)
(defvar-local gh-commit--review-base nil)

(defvar gh-commit--review-drafts (make-hash-table :test #'equal)
  "Local Pull Request review drafts keyed by host, repository, PR, and head.")

(defvar gh-commit--draft-sequence 0
  "Sequence used to give local review drafts stable identifiers.")

(defun gh-commit--context (&optional context)
  "Resolve repository CONTEXT for Commit commands."
  (gh-context-resolve (or context gh-buffer-context) t))

(defun gh-commit--resource (context data)
  "Create Commit resource from DATA."
  (let* ((sha (alist-get 'sha data))
         (commit (alist-get 'commit data))
         (message (alist-get 'message commit)))
    (gh-resource-create
     'commit context :sha sha
     :title (and message (car (split-string message "\n")))
     :url (alist-get 'html_url data)
     :data data)))

(defun gh-commit--author (data)
  "Return useful author text from commit DATA."
  (or (gh-core--name (alist-get 'author data))
      (gh-core--name (alist-get
                      'author (alist-get 'commit data)))))

(defun gh-commit--insert-row (context data)
  "Insert Commit DATA row."
  (let* ((resource (gh-commit--resource context data))
         (sha (plist-get resource :sha)))
    (gh-ui--section (commit sha resource t)
      (gh-ui--row
       (gh-ui--styled (substring sha 0 10) 'gh-hash)
       (gh-ui--styled (gh-resource-title resource) 'gh-resource-title)
       (gh-ui--styled (gh-commit--author data) 'gh-author)
       (gh-ui--styled
        (gh-core--date
         (alist-get 'date (alist-get 'author (alist-get 'commit data))))
        'gh-date))
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

(defun gh-commit-load-more ()
  "Double the current commit history limit."
  (interactive)
  (let* ((context gh-buffer-context)
         (params (plist-put gh-commit--params
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

(defun gh-commit--commit-comment-resource (context sha comment)
  "Create a structured resource for COMMIT comment data."
  (gh-resource-create
   'commit-comment context :id (alist-get 'id comment) :sha sha
   :path (alist-get 'path comment) :position (alist-get 'position comment)
   :line (alist-get 'line comment) :data comment
   :url (alist-get 'html_url comment)))

(defun gh-commit--commit-comment-heading (comment &optional inline)
  "Return a heading for COMMIT COMMENT, labeled INLINE when anchored."
  (gh-ui--row
   (concat
    (gh-ui--styled (if inline "Inline comment" "Comment")
                   'gh-conversation-kind)
    " by")
   (gh-ui--styled (gh-core--name (alist-get 'user comment)) 'gh-author)
   (gh-ui--styled (gh-core--date (alist-get 'created_at comment)) 'gh-date)))

(defun gh-commit--insert-commit-comment-body (context comment)
  "Insert COMMIT COMMENT metadata and body in CONTEXT."
  (when (alist-get 'path comment)
    (gh-ui--insert-header
     "Location"
     (format "%s:%s"
             (alist-get 'path comment)
             (or (alist-get 'line comment)
                 (format "diff@%s" (alist-get 'position comment))))
     'gh-file))
  (gh-ui--insert-markdown (alist-get 'body comment) context))

(defun gh-commit--insert-commit-comment (context sha comment &optional inline)
  "Insert COMMIT comment, labeling it INLINE when anchored to a diff."
  (let ((resource (gh-commit--commit-comment-resource context sha comment))
        (heading (gh-commit--commit-comment-heading comment inline)))
    (if inline
        (gh-ui--section (inline-comment (alist-get 'id comment) resource nil)
          heading
          (gh-commit--insert-commit-comment-body context comment))
      (gh-ui--section (comment (alist-get 'id comment) resource nil)
        heading
        (gh-commit--insert-commit-comment-body context comment)))))

(defun gh-commit--insert-commit-patch
    (context sha path patch comments)
  "Insert PATCH for PATH and place matching commit COMMENTS inline."
  (let (inserted)
    (if (string-empty-p (or patch ""))
        (insert (propertize "Binary or oversized diff unavailable.\n"
                            'font-lock-face 'shadow))
      (dolist (record
               (gh-commit--parse-patch-lines
                patch context nil sha path 'commit-line))
        (let ((start (point))
              (resource (plist-get record :resource)))
          (gh-ui--insert-diff (concat (plist-get record :text) "\n"))
          (when resource
            (add-text-properties start (point) (list 'gh-resource resource))
            (dolist (comment comments)
              (when (and (not (memq comment inserted))
                         (equal (alist-get 'position comment)
                                (plist-get resource :position)))
                (push comment inserted)
                (gh-commit--insert-commit-comment
                 context sha comment t)))))))
    (let ((unmapped (seq-remove (lambda (comment) (memq comment inserted))
                                comments)))
      (when unmapped
        (gh-ui--section (unmapped-comments path nil nil)
          "Unmapped inline comments"
          (dolist (comment unmapped)
            (gh-commit--insert-commit-comment context sha comment t)))))))

(defun gh-commit--diff-heading-matches (regexp text)
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

(defun gh-commit--diff-heading-body-start (text heading)
  "Return body start in TEXT immediately after HEADING."
  (let ((end (plist-get heading :end)))
    (if (and (< end (length text)) (eq (aref text end) ?\n))
        (1+ end)
      end)))

(defun gh-commit--split-diff-hunks (text)
  "Split diff file body TEXT into its preamble and hunk records."
  (let* ((matches (gh-commit--diff-heading-matches "^@@+ .*$" text))
         (preamble (substring text 0 (or (plist-get (car matches) :start)
                                         (length text))))
         hunks)
    (while matches
      (let* ((heading (pop matches))
             (body-start (gh-commit--diff-heading-body-start text heading))
             (body-end (or (plist-get (car matches) :start) (length text))))
        (push (list :heading (plist-get heading :heading)
                    :body (substring text body-start body-end))
              hunks)))
    (list :preamble preamble :hunks (nreverse hunks))))

(defun gh-commit--split-full-diff (diff)
  "Split unified DIFF into file records containing hunk records."
  (let* ((text (gh-ui--normalize-newlines (or diff "")))
         (matches (gh-commit--diff-heading-matches "^diff --git .*$" text))
         files)
    (while matches
      (let* ((heading (pop matches))
             (body-start (gh-commit--diff-heading-body-start text heading))
             (body-end (or (plist-get (car matches) :start) (length text)))
             (parts (gh-commit--split-diff-hunks
                     (substring text body-start body-end))))
        (push (list :heading (plist-get heading :heading)
                    :preamble (plist-get parts :preamble)
                    :hunks (plist-get parts :hunks))
              files)))
    (nreverse files)))

(defun gh-commit--insert-full-diff (diff)
  "Insert DIFF as foldable file and hunk sections."
  (let ((files (gh-commit--split-full-diff diff)))
    (if (null files)
        (gh-ui--insert-diff diff)
      (cl-loop
       for file in files
       for file-index from 1
       do
       (gh-ui--section (diff-file file-index nil nil)
         (gh-ui--fontified-string (plist-get file :heading) 'diff-mode)
         (unless (string-empty-p (plist-get file :preamble))
           (gh-ui--insert-diff (plist-get file :preamble)))
         (cl-loop
          for hunk in (plist-get file :hunks)
          for hunk-index from 1
          do
          (gh-ui--section (diff-hunk (cons file-index hunk-index) nil nil)
            (gh-ui--fontified-string (plist-get hunk :heading) 'diff-mode)
            (unless (string-empty-p (plist-get hunk :body))
              (gh-ui--insert-diff (plist-get hunk :body))))))))))

(defun gh-commit--render-view (context result)
  "Render Commit detail RESULT."
  (let* ((data (alist-get 'commit result))
         (comments (alist-get 'comments result))
         (diff (alist-get 'diff result))
         (resource (gh-commit--resource context data))
         (sha (alist-get 'sha data))
         (commit (alist-get 'commit data))
         (author (alist-get 'author commit))
         (committer (alist-get 'committer commit))
         (stats (alist-get 'stats data))
         (files (alist-get 'files data))
         (file-paths (mapcar (lambda (file) (alist-get 'filename file)) files)))
    (insert (propertize sha 'font-lock-face 'gh-hash) "\n")
    (add-text-properties (line-beginning-position 0) (point)
                         (list 'gh-resource resource))
    (gh-ui--insert-header "Author"
                          (format "%s <%s>"
                                  (alist-get 'name author)
                                  (alist-get 'email author))
                          'gh-author)
    (gh-ui--insert-header "AuthorDate"
                          (gh-core--date (alist-get 'date author)) 'gh-date)
    (gh-ui--insert-header "Commit"
                          (format "%s <%s>"
                                  (alist-get 'name committer)
                                  (alist-get 'email committer))
                          'gh-author)
    (gh-ui--insert-header "CommitDate"
                          (gh-core--date (alist-get 'date committer))
                          'gh-date)
    (gh-ui--insert-header
     "Changes" (format "+%s -%s, %d files"
                       (alist-get 'additions stats)
                       (alist-get 'deletions stats)
                       (length files)))
    (dolist (parent (alist-get 'parents data))
      (let ((parent-resource (gh-commit--resource context parent)))
        (gh-ui--insert-header "Parent" (alist-get 'sha parent)
                              'gh-hash parent-resource)))
    (insert "\n")
    (let* ((message (string-trim
                     (gh-ui--normalize-newlines
                      (alist-get 'message commit))))
           (message (if (string-empty-p message) "(no message)" message))
           (newline (string-match "\n" message))
           (summary (if newline (substring message 0 newline) message))
           (body (and newline (string-trim-right
                               (substring message (1+ newline))))))
      (gh-ui--section (message 'message resource nil)
        (gh-ui--styled summary 'magit-diff-revision-summary)
        (unless (string-empty-p (or body ""))
          (insert body)
          (unless (bolp) (insert "\n")))))
    (gh-ui--section (changed-files 'changed-files nil nil)
      (format "Changed files (%d)" (length files))
      (dolist (file files)
        (let* ((path (alist-get 'filename file))
               (file-resource
                (gh-resource-create
                 'file (gh-context-copy context :ref sha :path path)
                 :path path :ref sha :data file)))
          (gh-ui--section (file path file-resource nil)
            (gh-ui--row
             (gh-ui--styled path 'gh-file)
             (gh-ui--styled (alist-get 'status file) 'gh-permission)
             (gh-ui--styled (format "+%s" (alist-get 'additions file))
                            'gh-added)
             (gh-ui--styled (format "-%s" (alist-get 'deletions file))
                            'gh-removed))
            (gh-commit--insert-commit-patch
             context sha path (alist-get 'patch file)
             (seq-filter
              (lambda (comment) (equal (alist-get 'path comment) path))
              comments))))))
    (gh-ui--section (diff 'diff nil t)
      "Full diff"
      (gh-commit--insert-full-diff diff))
    (gh-ui--section (comments 'comments nil nil)
      (format "Comments (%d)" (length comments))
      (dolist (comment comments)
        (when (or (not (alist-get 'path comment))
                  (not (member (alist-get 'path comment) file-paths)))
          (gh-commit--insert-commit-comment context sha comment))))))

;;; Pull Request review

(defun gh-commit--review-key (context number head)
  "Return the local review key for NUMBER at HEAD in CONTEXT."
  (list (gh-context-host context) (gh-context-repository context) number head))

(defun gh-commit--review-drafts (context number head)
  "Return local drafts for Pull Request NUMBER at HEAD in CONTEXT."
  (gethash (gh-commit--review-key context number head)
           gh-commit--review-drafts))

(defun gh-commit--set-review-drafts (context number head drafts)
  "Store DRAFTS for Pull Request NUMBER at HEAD in CONTEXT."
  (let ((key (gh-commit--review-key context number head)))
    (if drafts
        (puthash key drafts gh-commit--review-drafts)
      (remhash key gh-commit--review-drafts))))

(defun gh-commit--stale-review-drafts (context number head)
  "Return stale draft entries for NUMBER excluding current HEAD."
  (let ((prefix (list (gh-context-host context)
                      (gh-context-repository context) number))
        entries)
    (maphash
     (lambda (key drafts)
       (when (and (equal prefix (seq-take key 3))
                  (not (equal head (nth 3 key))))
         (push (cons key drafts) entries)))
     gh-commit--review-drafts)
    (nreverse entries)))

(defun gh-commit--fetch-review (context number success error force)
  "Fetch the current review surface for Pull Request NUMBER."
  (gh-api--pr-get
   context number
   (lambda (pr)
     (let ((head (alist-get 'headRefOid pr))
           (base (alist-get 'baseRefOid pr)))
       (if (or (string-empty-p (or head ""))
               (string-empty-p (or base "")))
           (funcall error
                    (gh-core--error
                     'gh-api-error
                     "Pull Request has no base or head commit"))
         (gh-core--collect-async
          (list
           (cons 'commit
                 (lambda (ok fail)
                   (gh-api--commit-get context head ok fail force)))
           (cons 'comparison
                 (lambda (ok fail)
                   (gh-api--compare context base head ok fail force)))
           (cons 'threads
                 (lambda (ok fail)
                   (gh-api--pr-review-threads
                    context number ok fail force))))
          (lambda (result)
            (funcall success (cons (cons 'pr pr) result)))
          error))))
   error force))

(defun gh-commit--parse-patch-lines
    (patch context number head path &optional resource-kind)
  "Parse PATCH into display records with GitHub review coordinates."
  (let ((old-line 0)
        (new-line 0)
        (hunk 0)
        diff-position
        records)
    (dolist (text (split-string (gh-ui--normalize-newlines (or patch ""))
                                "\n" nil))
      (let (line side location)
        (cond
         ((string-match
           "^@@ -\\([0-9]+\\)\\(?:,[0-9]+\\)? +\\+\\([0-9]+\\)\\(?:,[0-9]+\\)? @@"
           text)
          (if diff-position
              (cl-incf diff-position)
            (setq diff-position 0))
          (setq old-line (string-to-number (match-string 1 text))
                new-line (string-to-number (match-string 2 text)))
          (cl-incf hunk))
         ((and (> hunk 0) (string-prefix-p "-" text)
               (not (string-prefix-p "---" text)))
          (cl-incf diff-position)
          (setq line old-line side "LEFT" location t)
          (cl-incf old-line))
         ((and (> hunk 0) (string-prefix-p "+" text)
               (not (string-prefix-p "+++" text)))
          (cl-incf diff-position)
          (setq line new-line side "RIGHT" location t)
          (cl-incf new-line))
         ((and (> hunk 0) (string-prefix-p " " text))
          (cl-incf diff-position)
          (setq line new-line side "RIGHT" location t)
          (cl-incf old-line)
          (cl-incf new-line))
         ((> hunk 0)
          (cl-incf diff-position)))
        (push
         (append
          (list :text text)
          (when location
            (list :resource
                  (gh-resource-create
                   (or resource-kind 'review-line)
                   context :number number :sha head :path path
                   :line line :side side :hunk hunk
                   :position diff-position))))
         records)))
    (nreverse records)))

(defun gh-commit--review-thread-resource (context number head thread)
  "Create a structured review resource from THREAD."
  (gh-resource-create
   'review-thread context
   :number number :sha head :path (alist-get 'path thread)
   :line (alist-get 'line thread) :side (alist-get 'side thread)
   :thread-id (alist-get 'id thread) :root-id (alist-get 'root_id thread)
   :resolved (alist-get 'is_resolved thread)
   :outdated (alist-get 'is_outdated thread)
   :can-reply (alist-get 'viewer_can_reply thread)
   :can-resolve (alist-get 'viewer_can_resolve thread)
   :can-unresolve (alist-get 'viewer_can_unresolve thread)
   :data thread))

(defun gh-commit--draft-resource (context number head key draft &optional stale)
  "Create a review draft resource for DRAFT stored below KEY."
  (gh-resource-create
   'review-draft context :number number :sha head :path (plist-get draft :path)
   :line (plist-get draft :line) :side (plist-get draft :side)
   :draft-id (plist-get draft :id) :draft-key key :stale stale :data draft))

(defun gh-commit--review-comment-heading (comment &optional reply)
  "Return a heading for review COMMENT, marking it as REPLY when non-nil."
  (let ((author (gh-core--name (alist-get 'user comment)))
        (date (or (alist-get 'created_at comment)
                  (alist-get 'updated_at comment))))
    (gh-ui--row
     (concat
      (gh-ui--styled (if reply "Reply" "Comment") 'gh-conversation-kind)
      " by")
     (gh-ui--styled author 'gh-author)
     (gh-ui--styled (gh-core--date date) 'gh-date))))

(defun gh-commit--insert-review-comment (context comment &optional reply)
  "Insert review COMMENT, marking it as REPLY when non-nil."
  (insert (gh-commit--review-comment-heading comment reply) "\n")
  (gh-ui--insert-markdown (alist-get 'body comment) context))

(defun gh-commit--insert-review-thread (context number head thread)
  "Insert normalized review THREAD for NUMBER at HEAD."
  (let* ((resource (gh-commit--review-thread-resource
                    context number head thread))
         (comments (alist-get 'comments thread))
         (state (cond ((alist-get 'is_outdated thread) "OUTDATED")
                      ((alist-get 'is_resolved thread) "RESOLVED")
                      (t "OPEN"))))
    (gh-ui--section (inline-comment
                     (or (alist-get 'id thread)
                         (alist-get 'root_id thread))
                     resource nil)
      (gh-ui--row
       (gh-ui--styled "Review thread" 'gh-conversation-kind)
       (gh-ui--styled state (gh-core--state-face state)))
      (when-let* ((first (car comments)))
        (gh-commit--insert-review-comment context first))
      (dolist (reply (cdr comments))
        (gh-ui--section (comment (alist-get 'id reply) resource nil)
          (gh-commit--review-comment-heading reply t)
          (gh-ui--insert-markdown (alist-get 'body reply) context))))))

(defun gh-commit--insert-review-draft
    (context number head key draft &optional stale)
  "Insert local review DRAFT, optionally marked STALE."
  (let ((resource (gh-commit--draft-resource
                   context number head key draft stale)))
    (gh-ui--section (inline-comment (plist-get draft :id) resource nil)
      (gh-ui--row
       (gh-ui--styled (if stale "Stale draft" "Draft review comment")
                      'gh-conversation-kind)
       (when stale
         (gh-ui--styled (format "%.10s" (nth 3 key)) 'gh-hash)))
      (gh-ui--insert-markdown (plist-get draft :body) context))))

(defun gh-commit--thread-at-location-p (thread path line side)
  "Return non-nil when THREAD is anchored at PATH, LINE, and SIDE."
  (and (not (alist-get 'is_outdated thread))
       (equal (alist-get 'path thread) path)
       (equal (alist-get 'line thread) line)
       (equal (upcase (or (alist-get 'side thread) "RIGHT")) side)))

(defun gh-commit--draft-at-location-p (draft path line side)
  "Return non-nil when DRAFT is anchored at PATH, LINE, and SIDE."
  (and (equal (plist-get draft :subject-type) "LINE")
       (equal (plist-get draft :path) path)
       (equal (plist-get draft :line) line)
       (equal (upcase (or (plist-get draft :side) "RIGHT")) side)))

(defun gh-commit--review-head-context (context pr head)
  "Return the repository context containing PR HEAD."
  (if-let* ((repository
             (alist-get 'nameWithOwner (alist-get 'headRepository pr))))
      (gh-context-copy
       (gh-context-from-repository repository (gh-context-host context))
       :ref head)
    (gh-context-copy context :ref head)))

(defun gh-commit--insert-review-file
    (context head-context number head file threads drafts draft-key)
  "Insert review FILE with THREADS and local DRAFTS."
  (let* ((path (alist-get 'filename file))
         (patch (alist-get 'patch file))
         (file-resource
          (gh-resource-create
           'file (gh-context-copy head-context :path path)
           :number number :sha head :path path :ref head :data file))
         (file-threads
          (seq-filter (lambda (thread)
                        (equal (alist-get 'path thread) path))
                      threads))
         (file-drafts
          (seq-filter (lambda (draft)
                        (equal (plist-get draft :path) path))
                      drafts))
         inserted-threads inserted-drafts)
    (gh-ui--section (file path file-resource nil)
      (gh-ui--row
       (gh-ui--styled path 'gh-file)
       (gh-ui--styled (or (alist-get 'status file) "modified") 'gh-permission)
       (gh-ui--styled (format "+%s" (or (alist-get 'additions file) 0))
                      'gh-added)
       (gh-ui--styled (format "-%s" (or (alist-get 'deletions file) 0))
                      'gh-removed))
      (dolist (thread file-threads)
        (when (equal (upcase (or (alist-get 'subject_type thread) "LINE"))
                     "FILE")
          (push thread inserted-threads)
          (gh-commit--insert-review-thread context number head thread)))
      (dolist (draft file-drafts)
        (when (equal (plist-get draft :subject-type) "FILE")
          (push draft inserted-drafts)
          (gh-commit--insert-review-draft
           context number head draft-key draft)))
      (if (string-empty-p (or patch ""))
          (insert (propertize "Binary or oversized diff unavailable.\n"
                              'font-lock-face 'shadow))
        (dolist (record
                 (gh-commit--parse-patch-lines
                  patch context number head path))
          (let ((start (point))
                (resource (plist-get record :resource)))
            (gh-ui--insert-diff (concat (plist-get record :text) "\n"))
            (when resource
              (add-text-properties start (point) (list 'gh-resource resource))
              (let ((line (plist-get resource :line))
                    (side (plist-get resource :side)))
                (dolist (thread file-threads)
                  (when (and (not (memq thread inserted-threads))
                             (gh-commit--thread-at-location-p
                              thread path line side))
                    (push thread inserted-threads)
                    (gh-commit--insert-review-thread
                     context number head thread)))
                (dolist (draft file-drafts)
                  (when (and (not (memq draft inserted-drafts))
                             (gh-commit--draft-at-location-p
                              draft path line side))
                    (push draft inserted-drafts)
                    (gh-commit--insert-review-draft
                     context number head draft-key draft))))))))
      (let ((unmapped-threads
             (seq-remove (lambda (thread) (memq thread inserted-threads))
                         file-threads))
            (unmapped-drafts
             (seq-remove (lambda (draft) (memq draft inserted-drafts))
                         file-drafts)))
        (when (or unmapped-threads unmapped-drafts)
          (gh-ui--section (outdated-review 'outdated-review nil nil)
            "Outdated or unmapped review comments"
            (dolist (thread unmapped-threads)
              (gh-commit--insert-review-thread context number head thread))
            (dolist (draft unmapped-drafts)
              (gh-commit--insert-review-draft
               context number head draft-key draft))))))))

(defun gh-commit--insert-review-summaries (context reviews)
  "Insert submitted Pull Request REVIEWS."
  (gh-ui--section (reviews 'reviews nil nil)
    (format "Reviews (%d)" (length reviews))
    (dolist (review reviews)
      (let ((id (or (alist-get 'id review)
                    (format "%s:%s"
                            (gh-core--name (alist-get 'author review))
                            (alist-get 'submittedAt review)))))
        (gh-ui--section (review id nil nil)
          (gh-ui--row
           (gh-ui--styled
            (or (alist-get 'state review) "COMMENTED")
            (gh-core--state-face (alist-get 'state review)))
           "by"
           (gh-ui--styled (gh-core--name (alist-get 'author review))
                          'gh-author)
           (gh-ui--styled (gh-core--date (alist-get 'submittedAt review))
                          'gh-date))
          (unless (string-empty-p (or (alist-get 'body review) ""))
            (gh-ui--insert-markdown (alist-get 'body review) context)))))))

(defun gh-commit--render-review (context number result)
  "Render Pull Request NUMBER review RESULT using the Commit page."
  (let* ((pr (alist-get 'pr result))
         (commit (alist-get 'commit result))
         (comparison (alist-get 'comparison result))
         (threads (alist-get 'threads result))
         (head (alist-get 'headRefOid pr))
         (base (alist-get 'baseRefOid pr))
         (files (alist-get 'files comparison))
         (draft-key (gh-commit--review-key context number head))
         (drafts (gh-commit--review-drafts context number head))
         (stale (gh-commit--stale-review-drafts context number head))
         (head-context (gh-commit--review-head-context context pr head))
         (pr-resource
          (gh-resource-create 'pr context :number number
                              :title (alist-get 'title pr)
                              :url (alist-get 'url pr))))
    (setq gh-commit--review-number number
          gh-commit--review-head head
          gh-commit--review-base base
          gh-commit--sha head)
    (let ((start (point)))
      (insert (propertize (format "PR #%s  " number)
                          'font-lock-face 'gh-resource-number)
              (propertize (alist-get 'title pr)
                          'font-lock-face 'gh-resource-title) "\n")
      (add-text-properties start (point) (list 'gh-resource pr-resource)))
    (gh-ui--insert-header "Repository" (gh-context-repository context)
                          'gh-repository)
    (gh-ui--insert-header "Review"
                          (or (alist-get 'reviewDecision pr) "REVIEW_REQUIRED")
                          (gh-core--state-face (alist-get 'reviewDecision pr)))
    (gh-ui--insert-header "Base" (format "%s · %.10s"
                                          (alist-get 'baseRefName pr) base)
                          'gh-branch)
    (gh-ui--insert-header "Head" (format "%s · %.10s"
                                          (alist-get 'headRefName pr) head)
                          'gh-branch)
    (when commit
      (gh-ui--insert-header
       "Commit"
       (car (split-string
             (alist-get 'message (alist-get 'commit commit)) "\n"))
       'gh-resource-title))
    (when stale
      (insert (propertize
               (format "Warning: %d draft set(s) belong to an older PR head.\n"
                       (length stale))
               'font-lock-face 'gh-error)))
    (insert "\n")
    (gh-commit--insert-review-summaries context (alist-get 'reviews pr))
    (gh-ui--section (changed-files 'changed-files nil nil)
      (format "Changed files (%d) · Local drafts (%d)"
              (length files) (length drafts))
      (dolist (file files)
        (gh-commit--insert-review-file
         context head-context number head file threads drafts draft-key)))
    (when stale
      (gh-ui--section (stale-drafts 'stale-drafts nil nil)
        "Stale drafts (cannot be submitted)"
        (dolist (entry stale)
          (dolist (draft (cdr entry))
            (gh-commit--insert-review-draft
             context number head (car entry) draft t)))))))

(defun gh-commit--diff-selection-resources (kind)
  "Return ordered diff-line resources of KIND at point or in the region."
  (let* ((beg (if (use-region-p) (region-beginning) (line-beginning-position)))
         (end (if (use-region-p)
                  (max beg (1- (region-end)))
                (line-end-position)))
         position
         resources)
    (save-excursion
      (goto-char beg)
      (setq position (line-beginning-position))
      (while (<= position end)
        (goto-char position)
        (let ((resource (gh-ui-resource-at-point)))
          (unless (eq (plist-get resource :kind) kind)
            (user-error "Selection contains a non-commentable diff line"))
          (push resource resources))
        (setq position (line-beginning-position 2))))
    (nreverse resources)))

(defun gh-commit--review-selection ()
  "Return a GitHub review location for point or the active region."
  (let ((resources (gh-commit--diff-selection-resources 'review-line)))
    (let ((first (car resources))
          (last (car (last resources))))
      (unless (and first
                   (seq-every-p
                    (lambda (resource)
                      (and (equal (plist-get resource :path)
                                  (plist-get first :path))
                           (equal (plist-get resource :side)
                                  (plist-get first :side))
                           (equal (plist-get resource :hunk)
                                  (plist-get first :hunk))))
                    resources))
        (user-error "Review ranges must stay in one file, hunk, and diff side"))
      (append
       (list :path (plist-get first :path)
             :line (plist-get last :line)
             :side (plist-get last :side)
             :subject-type "LINE")
       (when (> (length resources) 1)
         (list :start-line (plist-get first :line)
               :start-side (plist-get first :side)))))))

(defun gh-commit--commit-selection ()
  "Return the commit diff location selected at point or by the region.
GitHub commit comments have one diff position, so a multi-line selection is
anchored to its final selected line."
  (let* ((resources (gh-commit--diff-selection-resources 'commit-line))
         (first (car resources))
         (last (car (last resources))))
    (unless (and first
                 (seq-every-p
                  (lambda (resource)
                    (and (equal (plist-get resource :path)
                                (plist-get first :path))
                         (equal (plist-get resource :hunk)
                                (plist-get first :hunk))))
                  resources))
      (user-error "Commit comment selections must stay in one file and hunk"))
    (list :path (plist-get first :path)
          :position (plist-get last :position)
          :line (plist-get last :line))))

(defun gh-commit--add-review-draft (draft)
  "Add local review DRAFT to the current Review page."
  (unless (and gh-commit--review-number gh-commit--review-head)
    (user-error "This Commit page is not reviewing a Pull Request"))
  (let* ((body (plist-get draft :body))
         (draft (plist-put draft :id (cl-incf gh-commit--draft-sequence)))
         (drafts (gh-commit--review-drafts
                  gh-buffer-context gh-commit--review-number
                  gh-commit--review-head)))
    (when (string-empty-p (string-trim (or body "")))
      (user-error "Review comment cannot be empty"))
    (gh-commit--set-review-drafts
     gh-buffer-context gh-commit--review-number gh-commit--review-head
     (append drafts (list draft)))
    (message "Collected review comment (%d total)" (1+ (length drafts)))
    (gh-ui-refresh)))

(defun gh-commit-review-comment-add (body)
  "Collect review comment BODY for the current diff line or region."
  (interactive (list (read-string "Review comment: ")))
  (gh-commit--add-review-draft
   (plist-put (gh-commit--review-selection) :body body)))

(defun gh-commit-review-file-comment-add (path body)
  "Collect whole-file review comment BODY for PATH."
  (interactive
   (let ((resource (gh-ui-resource-at-point)))
     (list (or (plist-get resource :path) (read-string "Path: "))
           (read-string "File review comment: "))))
  (gh-commit--add-review-draft
   (list :path path :subject-type "FILE" :body body)))

(defun gh-commit-review-draft-edit (body)
  "Replace the local review draft at point with BODY."
  (interactive
   (let* ((resource (gh-ui-resource-at-point))
          (draft (plist-get resource :data)))
     (unless (eq (plist-get resource :kind) 'review-draft)
       (user-error "No local review draft at point"))
     (when (plist-get resource :stale)
       (user-error "Stale drafts are read-only"))
     (list (read-string "Review comment: " (plist-get draft :body)))))
  (when (string-empty-p (string-trim body))
    (user-error "Review comment cannot be empty"))
  (let* ((resource (gh-ui-resource-at-point))
         (key (plist-get resource :draft-key))
         (id (plist-get resource :draft-id))
         (drafts (gethash key gh-commit--review-drafts)))
    (puthash key
             (mapcar (lambda (draft)
                       (if (equal (plist-get draft :id) id)
                           (plist-put (copy-sequence draft) :body body)
                         draft))
                     drafts)
             gh-commit--review-drafts)
    (gh-ui-refresh)))

(defun gh-commit-review-draft-delete ()
  "Delete the local review draft at point, including a stale draft."
  (interactive)
  (let* ((resource (gh-ui-resource-at-point))
         (key (plist-get resource :draft-key))
         (id (plist-get resource :draft-id)))
    (unless (eq (plist-get resource :kind) 'review-draft)
      (user-error "No local review draft at point"))
    (let ((remaining
           (seq-remove (lambda (draft) (equal (plist-get draft :id) id))
                       (gethash key gh-commit--review-drafts))))
      (if remaining
          (puthash key remaining gh-commit--review-drafts)
        (remhash key gh-commit--review-drafts)))
    (gh-ui-refresh)))

(defun gh-commit-review-reply (body)
  "Immediately publish BODY as a reply to the review thread at point."
  (interactive (list (read-string "Reply: ")))
  (let ((resource (gh-ui-resource-at-point)))
    (unless (eq (plist-get resource :kind) 'review-thread)
      (user-error "No review thread at point"))
    (unless (plist-get resource :can-reply)
      (user-error "GitHub does not allow you to reply to this thread"))
    (when (string-empty-p (string-trim body))
      (user-error "Reply cannot be empty"))
    (gh-api--pr-review-reply
     gh-buffer-context gh-commit--review-number
     (plist-get resource :root-id) body
     (lambda (_) (message "Review reply published") (gh-ui-refresh t))
     #'gh-core--user-error)))

(defun gh-commit-review-toggle-resolved ()
  "Resolve or unresolve the review thread at point."
  (interactive)
  (let* ((resource (gh-ui-resource-at-point))
         (resolved (plist-get resource :resolved))
         (allowed (plist-get resource
                             (if resolved :can-unresolve :can-resolve))))
    (unless (eq (plist-get resource :kind) 'review-thread)
      (user-error "No review thread at point"))
    (unless (plist-get resource :thread-id)
      (user-error "Thread resolution is unavailable on this GitHub host"))
    (unless allowed
      (user-error "GitHub does not allow you to change this thread"))
    (gh-api--pr-review-thread-resolved
     gh-buffer-context gh-commit--review-number
     (plist-get resource :thread-id) (not resolved)
     (lambda (_)
       (message "Review thread %s" (if resolved "reopened" "resolved"))
       (gh-ui-refresh t))
     #'gh-core--user-error)))

(defun gh-commit-review-submit (&optional event body)
  "Submit current local drafts as a Pull Request review EVENT with BODY."
  (interactive)
  (unless (and gh-commit--review-number gh-commit--review-head)
    (user-error "This Commit page is not reviewing a Pull Request"))
  (let* ((event-name
          (or (and event (upcase (symbol-name event)))
              (completing-read
               "Review event: " '("COMMENT" "APPROVE" "REQUEST_CHANGES")
               nil t nil nil "COMMENT")))
         (event (or event (intern (downcase event-name))))
         (body (or body (read-string "Review summary: ")))
         (drafts (gh-commit--review-drafts
                  gh-buffer-context gh-commit--review-number
                  gh-commit--review-head)))
    (when (and (memq event '(comment request_changes))
               (string-empty-p (string-trim body)))
      (user-error "%s reviews require a summary" event-name))
    (gh-api--pr-review
     gh-buffer-context gh-commit--review-number event body drafts
     (lambda (_)
       (gh-commit--set-review-drafts
        gh-buffer-context gh-commit--review-number gh-commit--review-head nil)
       (message "Review submitted")
       (gh-ui-refresh t))
     #'gh-core--user-error gh-commit--review-head)))

(transient-define-prefix gh-commit-review-dispatch ()
  "Pull Request review actions in the Commit page."
  [["Review"
    ("c" "Add line/range draft" gh-commit-review-comment-add)
    ("C" "Add file draft" gh-commit-review-file-comment-add)
    ("V" "Submit review" gh-commit-review-submit)]
   ["At point"
    ("e" "Edit draft" gh-commit-review-draft-edit)
    ("k" "Delete draft" gh-commit-review-draft-delete)
    ("r" "Reply to thread" gh-commit-review-reply)
    ("x" "Resolve / unresolve" gh-commit-review-toggle-resolved)]
   ["Inspect"
    ("g" "Refresh" gh-ui-refresh)
    ("b" "Browse tree" gh-commit-browse-tree)
    ("o" "Browse web" gh-ui-browse)]])

;;;###autoload
(defun gh-commit-review (number &optional context)
  "Review the latest head of Pull Request NUMBER using the Commit page."
  (interactive (list (read-number "Pull Request number: ")))
  (setq context (gh-commit--context context))
  (gh-ui--open-page
   (format "*gh: %s · PR #%s Review*"
           (gh-context-repository context) number)
   context 'commit-review number
   (lambda (success error force)
     (gh-commit--fetch-review context number success error force))
   (lambda (data) (gh-commit--render-review context number data))
   :setup
   (lambda ()
     (setq gh-commit--review-number number
           gh-buffer-dispatch-function #'gh-commit-review-dispatch)
     (local-set-key (kbd "c") #'gh-commit-review-comment-add)
     (local-set-key (kbd "C") #'gh-commit-review-file-comment-add)
     (local-set-key (kbd "e") #'gh-commit-review-draft-edit)
     (local-set-key (kbd "k") #'gh-commit-review-draft-delete)
     (local-set-key (kbd "r") #'gh-commit-review-reply)
     (local-set-key (kbd "x") #'gh-commit-review-toggle-resolved)
     (local-set-key (kbd "V") #'gh-commit-review-submit))))

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
   :preview preview
   :setup (lambda ()
            (setq gh-commit--sha sha
                  gh-buffer-dispatch-function #'gh-commit-dispatch)
            (local-set-key (kbd "b") #'gh-commit-browse-tree)
            (local-set-key (kbd "c") #'gh-commit-inline-comment)
            (local-set-key (kbd "C") #'gh-commit-comment))))

(defun gh-commit-comment (body &optional context sha path position)
  "Add BODY on Commit SHA, optionally at PATH and diff POSITION."
  (interactive (list (read-string "Commit comment: ")))
  (setq context (gh-commit--context context)
        sha (or sha gh-commit--sha))
  (when (string-empty-p (string-trim (or body "")))
    (user-error "Commit comment cannot be empty"))
  (gh-api--commit-comment
   context sha body path position
   (lambda (_) (message "Commit comment added")
     (when (derived-mode-p 'gh-section-mode) (gh-ui-refresh t)))
   #'gh-core--user-error))

(defun gh-commit-inline-comment (body &optional context sha)
  "Add BODY as an inline comment at the selected Commit diff position."
  (interactive (list (read-string "Inline commit comment: ")))
  (let ((location (gh-commit--commit-selection)))
    (gh-commit-comment body context sha
                       (plist-get location :path)
                       (plist-get location :position))))

(transient-define-prefix gh-commit-dispatch ()
  "Commit actions."
  [["Inspect"
   ("g" "Refresh" gh-ui-refresh)
    ("b" "Browse tree" gh-commit-browse-tree)
    ("o" "Browse web" gh-ui-browse)]
   ["Comment"
    ("c" "Inline comment" gh-commit-inline-comment)
    ("C" "General comment" gh-commit-comment)]])

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

(gh-candidate-register
 'commit-review
 :open (lambda (resource)
         (gh-commit-review (plist-get resource :number)
                           (plist-get resource :context))))

(gh-candidate-register 'commit-more :open (lambda (_resource)
                                            (gh-commit-load-more)))

(provide 'gh-commit)
;;; gh-commit.el ends here
