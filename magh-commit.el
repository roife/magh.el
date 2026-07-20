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
(require 'magh-ui)

(defvar-local magh-commit--params nil)
(defvar-local magh-commit--sha nil)
(defvar-local magh-commit--review-number nil)
(defvar-local magh-commit--review-head nil)

(defvar magh-commit--review-drafts (make-hash-table :test #'equal)
  "Local Pull Request review drafts keyed by host, repository, PR, and head.")

(defvar magh-commit--draft-sequence 0
  "Sequence used to give local review drafts stable identifiers.")

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

(defun magh-commit--diff-heading (text faces)
  "Return TEXT as a full-line diff heading carrying FACES."
  (let ((heading (if (string-suffix-p "\n" text) text (concat text "\n"))))
    (dolist (face (ensure-list faces))
      (font-lock-append-text-property
       0 (length heading) 'font-lock-face face heading))
    heading))

(defun magh-commit--diff-file-heading (text)
  "Return file heading TEXT with Magit's diff file background faces."
  (magh-commit--diff-heading
   text '(magit-diff-file-heading magit-diff-file-heading-highlight)))

(defun magh-commit--diff-hunk-heading (text)
  "Return hunk heading TEXT with Magit's diff hunk background face."
  (magh-commit--diff-heading text 'magit-diff-hunk-heading))

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
  (let ((items (if (magh-page-p data) (magh-page-items data) data))
        (next (and (magh-page-p data) (magh-page-next data))))
    (magh-ui--insert-header "Repository" (magh-context-repository context)
                          'magh-repository
                          (magh-resource-create 'repository context))
    (magh-ui--insert-header
     "History" (or (plist-get params :path) (plist-get params :ref) "HEAD"))
    (insert "\n")
    (if items
        (dolist (commit items) (magh-commit--insert-row context commit))
      (insert (propertize "No commits found.\n" 'font-lock-face 'shadow)))
    (if next
        (magh-ui--section (more 'more (magh-resource-create 'commit-more context) t)
          (format "Load next page (%d loaded)" (length items))
          (insert "Press RET to append more commits.\n"))
      (insert (propertize (format "End of history (%d commits).\n"
                                  (length items))
                          'font-lock-face 'shadow)))))

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
  (magh-ui--row
   (concat
    (magh-ui--styled (if inline "Inline comment" "Comment")
                   'magh-conversation-kind)
    " by")
   (magh-ui--styled (magh-core--name (alist-get 'user comment)) 'magh-author)
   (magh-ui--styled (magh-core--date (alist-get 'created_at comment)) 'magh-date)))

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
      (magh-commit--insert-patch-records
       (magh-commit--parse-patch-lines
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
         (magh-commit--diff-file-heading (plist-get file :heading))
         (unless (string-empty-p (plist-get file :preamble))
           (magh-ui--insert-diff (plist-get file :preamble)))
         (cl-loop
          for hunk in (plist-get file :hunks)
          for hunk-index from 1
          do
          (magh-ui--section (diff-hunk (cons file-index hunk-index) nil nil)
            (magh-commit--diff-hunk-heading (plist-get hunk :heading))
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
            (magh-commit--diff-file-heading
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

;;; Pull Request review

(defun magh-commit--review-key (context number head)
  "Return the local review key for NUMBER at HEAD in CONTEXT."
  (list (magh-context-host context) (magh-context-repository context) number head))

(defun magh-commit--review-drafts (context number head)
  "Return local drafts for Pull Request NUMBER at HEAD in CONTEXT."
  (gethash (magh-commit--review-key context number head)
           magh-commit--review-drafts))

(defun magh-commit--set-review-drafts (context number head drafts)
  "Store DRAFTS for Pull Request NUMBER at HEAD in CONTEXT."
  (let ((key (magh-commit--review-key context number head)))
    (if drafts
        (puthash key drafts magh-commit--review-drafts)
      (remhash key magh-commit--review-drafts))))

(defun magh-commit--stale-review-drafts (context number head)
  "Return stale draft entries for NUMBER excluding current HEAD."
  (let ((prefix (list (magh-context-host context)
                      (magh-context-repository context) number))
        entries)
    (maphash
     (lambda (key drafts)
       (when (and (equal prefix (seq-take key 3))
                  (not (equal head (nth 3 key))))
         (push (cons key drafts) entries)))
     magh-commit--review-drafts)
    (nreverse entries)))

(defun magh-commit--fetch-review (context number success error force)
  "Fetch the current review surface for Pull Request NUMBER."
  (magh-api--pr-get
   context number
   (lambda (pr)
     (let ((head (alist-get 'headRefOid pr))
           (base (alist-get 'baseRefOid pr)))
       (if (or (string-empty-p (or head ""))
               (string-empty-p (or base "")))
           (funcall error
                    (magh-core--error
                     'magh-api-error
                     "Pull Request has no base or head commit"))
         (magh-core--collect-async
          (list
           (cons 'commit
                 (lambda (ok fail)
                   (magh-api--commit-get context head ok fail force)))
           (cons 'comparison
                 (lambda (ok fail)
                   (magh-api--compare context base head ok fail force)))
           (cons 'reviews
                 (lambda (ok fail)
                   (magh-api--pr-reviews context number ok fail force)))
           (cons 'threads
                 (lambda (ok fail)
                   (magh-api--pr-review-threads
                    context number ok fail force))))
          (lambda (result)
            (funcall success (cons (cons 'pr pr) result)))
          error))))
   error force))

(defun magh-commit--parse-patch-lines
    (patch context number head path &optional resource-kind)
  "Parse PATCH into display records with GitHub review coordinates."
  (let ((old-line 0)
        (new-line 0)
        (hunk 0)
        diff-position
        records)
    (dolist (text (split-string (magh-ui--normalize-newlines (or patch ""))
                                "\n" nil))
      (let (line side location hunk-heading)
        (cond
         ((string-match
           "^@@ -\\([0-9]+\\)\\(?:,[0-9]+\\)? +\\+\\([0-9]+\\)\\(?:,[0-9]+\\)? @@"
           text)
          (if diff-position
              (cl-incf diff-position)
            (setq diff-position 0))
          (setq old-line (string-to-number (match-string 1 text))
                new-line (string-to-number (match-string 2 text)))
          (cl-incf hunk)
          (setq hunk-heading t))
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
          (list :text text :hunk hunk :hunk-heading hunk-heading)
          (when location
            (list :resource
                  (magh-resource-create
                   (or resource-kind 'review-line)
                   context :number number :sha head :path path
                   :line line :side side :hunk hunk
                   :position diff-position))))
         records)))
    (nreverse records)))

(defun magh-commit--partition-patch-records (records)
  "Partition parsed patch RECORDS into a preamble and hunk records."
  (let (preamble hunks current)
    (dolist (record records)
      (if (plist-get record :hunk-heading)
          (progn
            (when current
              (push (list :heading (car current)
                          :records (nreverse (cdr current)))
                    hunks))
            (setq current (list record)))
        (if current
            (setcdr current (cons record (cdr current)))
          (push record preamble))))
    (when current
      (push (list :heading (car current)
                  :records (nreverse (cdr current)))
            hunks))
    (list :preamble (nreverse preamble) :hunks (nreverse hunks))))

(defun magh-commit--insert-patch-records (records insert-record)
  "Insert parsed patch RECORDS as foldable hunks.
INSERT-RECORD inserts one non-heading record, including any anchored comments."
  (let ((parts (magh-commit--partition-patch-records records)))
    (dolist (record (plist-get parts :preamble))
      (funcall insert-record record))
    (dolist (hunk (plist-get parts :hunks))
      (let ((heading (plist-get hunk :heading)))
        (magh-ui--section (diff-hunk (plist-get heading :hunk) nil nil)
          (magh-commit--diff-hunk-heading (plist-get heading :text))
          (dolist (record (plist-get hunk :records))
            (funcall insert-record record)))))))

(defun magh-commit--review-thread-resource (context number head thread)
  "Create a structured review resource from THREAD."
  (let ((first (car (alist-get 'comments thread))))
    (magh-resource-create
     'review-thread context
     :number number :sha head :path (alist-get 'path thread)
     :line (alist-get 'line thread) :side (alist-get 'side thread)
     :thread-id (alist-get 'id thread) :root-id (alist-get 'root_id thread)
     :review-id (alist-get 'pull_request_review_id first)
     :resolved (alist-get 'is_resolved thread)
     :outdated (alist-get 'is_outdated thread)
     :can-reply (alist-get 'viewer_can_reply thread)
     :can-resolve (alist-get 'viewer_can_resolve thread)
     :can-unresolve (alist-get 'viewer_can_unresolve thread)
     :data thread)))

(defun magh-commit--draft-resource (context number head key draft &optional stale)
  "Create a review draft resource for DRAFT stored below KEY."
  (magh-resource-create
   'review-draft context :number number :sha head :path (plist-get draft :path)
   :line (plist-get draft :line) :side (plist-get draft :side)
   :draft-id (plist-get draft :id) :draft-key key :stale stale :data draft))

(defun magh-commit--review-comment-heading (comment &optional reply)
  "Return a heading for review COMMENT, marking it as REPLY when non-nil."
  (let ((author (magh-core--name (alist-get 'user comment)))
        (date (or (alist-get 'created_at comment)
                  (alist-get 'updated_at comment))))
    (magh-ui--row
     (concat
      (magh-ui--styled (if reply "Reply" "Comment") 'magh-conversation-kind)
      " by")
     (magh-ui--styled author 'magh-author)
     (magh-ui--styled (magh-core--date date) 'magh-date))))

(defun magh-commit--insert-review-comment (context comment &optional reply)
  "Insert review COMMENT, marking it as REPLY when non-nil."
  (insert (magh-commit--review-comment-heading comment reply) "\n")
  (magh-ui--insert-markdown (alist-get 'body comment) context))

(defun magh-commit--insert-review-thread (context number head thread)
  "Insert normalized review THREAD for NUMBER at HEAD."
  (let* ((resource (magh-commit--review-thread-resource
                    context number head thread))
         (comments (alist-get 'comments thread))
         (state (cond ((alist-get 'is_outdated thread) "OUTDATED")
                      ((alist-get 'is_resolved thread) "RESOLVED")
                      (t "OPEN"))))
    (magh-ui--section (inline-comment
                     (or (alist-get 'id thread)
                         (alist-get 'root_id thread))
                     resource nil)
      (magh-ui--row
       (magh-ui--styled "Review thread" 'magh-conversation-kind)
       (magh-ui--styled state (magh-core--state-face state)))
      (when-let* ((first (car comments)))
        (magh-commit--insert-review-comment context first))
      (dolist (reply (cdr comments))
        (magh-ui--section (comment (alist-get 'id reply) resource nil)
          (magh-commit--review-comment-heading reply t)
          (magh-ui--insert-markdown (alist-get 'body reply) context))))))

(defun magh-commit--insert-review-draft
    (context number head key draft &optional stale)
  "Insert local review DRAFT, optionally marked STALE."
  (let ((resource (magh-commit--draft-resource
                   context number head key draft stale)))
    (magh-ui--section (inline-comment (plist-get draft :id) resource nil)
      (magh-ui--row
       (magh-ui--styled (if stale "Stale draft" "Draft review comment")
                      'magh-conversation-kind)
       (when stale
         (magh-ui--styled (format "%.10s" (nth 3 key)) 'magh-hash)))
      (magh-ui--insert-markdown (plist-get draft :body) context))))

(defun magh-commit--thread-at-location-p (thread path line side)
  "Return non-nil when THREAD is anchored at PATH, LINE, and SIDE."
  (and (not (alist-get 'is_outdated thread))
       (equal (alist-get 'path thread) path)
       (equal (alist-get 'line thread) line)
       (equal (upcase (or (alist-get 'side thread) "RIGHT")) side)))

(defun magh-commit--draft-at-location-p (draft path line side)
  "Return non-nil when DRAFT is anchored at PATH, LINE, and SIDE."
  (and (equal (plist-get draft :subject-type) "LINE")
       (equal (plist-get draft :path) path)
       (equal (plist-get draft :line) line)
       (equal (upcase (or (plist-get draft :side) "RIGHT")) side)))

(defun magh-commit--review-head-context (context pr head)
  "Return the repository context containing PR HEAD."
  (if-let* ((repository
             (alist-get 'nameWithOwner (alist-get 'headRepository pr))))
      (magh-context-copy
       (magh-context-from-repository repository (magh-context-host context))
       :ref head)
    (magh-context-copy context :ref head)))

(defun magh-commit--insert-review-file
    (context head-context number head file threads drafts draft-key)
  "Insert review FILE with THREADS and local DRAFTS."
  (let* ((path (alist-get 'filename file))
         (patch (alist-get 'patch file))
         (file-resource
          (magh-resource-create
           'file (magh-context-copy head-context :path path)
           :number number :sha head :path path :ref head :data file))
         (file-threads
          (seq-filter (lambda (thread)
                        (equal (alist-get 'path thread) path))
                      threads))
         (file-drafts
          (seq-filter (lambda (draft)
                        (equal (plist-get draft :path) path))
                      drafts))
         (comment-count
          (seq-reduce
           (lambda (count thread)
             (+ count (length (alist-get 'comments thread))))
           file-threads 0))
         inserted-threads inserted-drafts)
    (magh-ui--section (file path file-resource nil)
      (magh-commit--diff-file-heading
       (magh-ui--row
        (magh-ui--styled path 'magh-file)
        (magh-ui--styled (or (alist-get 'status file) "modified") 'magh-permission)
        (magh-ui--styled (format "+%s" (or (alist-get 'additions file) 0))
                       'magh-added)
        (magh-ui--styled (format "-%s" (or (alist-get 'deletions file) 0))
                       'magh-removed)
        (when (> comment-count 0)
          (magh-ui--styled
           (format "%d comment%s"
                   comment-count (if (= comment-count 1) "" "s"))
           'magh-permission))))
      (dolist (thread file-threads)
        (when (equal (upcase (or (alist-get 'subject_type thread) "LINE"))
                     "FILE")
          (push thread inserted-threads)
          (magh-commit--insert-review-thread context number head thread)))
      (dolist (draft file-drafts)
        (when (equal (plist-get draft :subject-type) "FILE")
          (push draft inserted-drafts)
          (magh-commit--insert-review-draft
           context number head draft-key draft)))
      (if (string-empty-p (or patch ""))
          (insert (propertize "Binary or oversized diff unavailable.\n"
                              'font-lock-face 'shadow))
        (magh-commit--insert-patch-records
         (magh-commit--parse-patch-lines patch context number head path)
         (lambda (record)
           (let ((start (point))
                 (resource (plist-get record :resource)))
             (magh-ui--insert-diff (concat (plist-get record :text) "\n"))
             (when resource
               (add-text-properties start (point) (list 'magh-resource resource))
               (let ((line (plist-get resource :line))
                     (side (plist-get resource :side)))
                 (dolist (thread file-threads)
                   (when (and (not (memq thread inserted-threads))
                              (magh-commit--thread-at-location-p
                               thread path line side))
                     (push thread inserted-threads)
                     (magh-commit--insert-review-thread
                      context number head thread)))
                 (dolist (draft file-drafts)
                   (when (and (not (memq draft inserted-drafts))
                              (magh-commit--draft-at-location-p
                               draft path line side))
                     (push draft inserted-drafts)
                     (magh-commit--insert-review-draft
                      context number head draft-key draft)))))))))
      (let ((unmapped-threads
             (seq-remove (lambda (thread) (memq thread inserted-threads))
                         file-threads))
            (unmapped-drafts
             (seq-remove (lambda (draft) (memq draft inserted-drafts))
                         file-drafts)))
        (when (or unmapped-threads unmapped-drafts)
          (magh-ui--section (outdated-review 'outdated-review nil nil)
            "Outdated or unmapped review comments"
            (dolist (thread unmapped-threads)
              (magh-commit--insert-review-thread context number head thread))
            (dolist (draft unmapped-drafts)
              (magh-commit--insert-review-draft
               context number head draft-key draft))))))))

(defun magh-commit--review-targets (review threads)
  "Return inline comment targets in THREADS belonging to REVIEW."
  (let ((review-id (alist-get 'id review)) targets)
    (dolist (thread threads)
      (let ((first (car (alist-get 'comments thread))))
        (dolist (comment (alist-get 'comments thread))
          (when (equal (alist-get 'pull_request_review_id comment) review-id)
            (push (list :path (alist-get 'path thread)
                        :line (alist-get 'line thread)
                        :thread-id (alist-get 'id thread)
                        :root-id (alist-get 'root_id thread)
                        :comment-id (alist-get 'id comment)
                        :author (magh-core--name (alist-get 'user comment))
                        :reply (not (eq comment first)))
                  targets)))))
    (nreverse targets)))

(defun magh-commit--insert-review-target-link
    (context number head review-id target)
  "Insert one linked review TARGET below REVIEW-ID."
  (let* ((path (plist-get target :path))
         (line (plist-get target :line))
         (comment-id (plist-get target :comment-id))
         (resource
          (magh-resource-create
           'review-summary context :number number :sha head
           :review-id review-id :targets (list target))))
    (magh-ui--section (review-link (cons review-id comment-id) resource nil)
      (magh-ui--row
       (concat "  "
               (magh-ui--styled
                (if line (format "%s:%s" path line) path) 'magh-file))
       (magh-ui--styled (plist-get target :author) 'magh-author)
       (magh-ui--styled (format "#%s" comment-id) 'magh-resource-number)))))

(defun magh-commit--section-resource (section)
  "Return the structured resource stored on SECTION, if any."
  (let ((value (oref section value)))
    (and (magh-section-value-p value)
         (magh-section-value-resource value))))

(defun magh-commit--review-target-section-p (section target)
  "Return non-nil when SECTION represents review TARGET."
  (let* ((value (oref section value))
         (key (and (magh-section-value-p value)
                   (magh-section-value-key value)))
         (resource (magh-commit--section-resource section)))
    (if (plist-get target :reply)
        (and (eq (oref section type) 'comment)
             (equal key (plist-get target :comment-id)))
      (and (eq (oref section type) 'inline-comment)
           (eq (plist-get resource :kind) 'review-thread)
           (equal (plist-get resource :root-id)
                  (plist-get target :root-id))))))

(defun magh-commit--materialize-review-target (section target)
  "Expand SECTION as needed and return the section for review TARGET."
  (magit-section-show section)
  (or (and (magh-commit--review-target-section-p section target) section)
      (cl-loop for child in (oref section children)
               thereis (magh-commit--materialize-review-target child target))))

(defun magh-commit--goto-review-target (resource)
  "Jump from linked review RESOURCE to its inline comment."
  (unless (and (eq magh-buffer-resource-kind 'commit-review)
               (equal magh-buffer-resource-id (plist-get resource :number)))
    (user-error "Review links can only be followed from their Review page"))
  (let* ((target (car (plist-get resource :targets)))
         (changed-files
          (seq-find (lambda (section)
                      (eq (oref section type) 'changed-files))
                    (oref magit-root-section children))))
    (unless (and target changed-files)
      (user-error "Review has no linked inline comment"))
    (magit-section-show changed-files)
    (let* ((file
            (seq-find
             (lambda (section)
               (equal (plist-get (magh-commit--section-resource section) :path)
                      (plist-get target :path)))
             (oref changed-files children)))
           (target-section
            (and file (magh-commit--materialize-review-target file target))))
      (unless target-section
        (user-error "Linked review comment is not present in this diff"))
      (goto-char (oref target-section start))
      (when-let* ((window (get-buffer-window (current-buffer) t)))
        (set-window-point window (point))
        (with-selected-window window (recenter)))
      (when (> (length (plist-get resource :targets)) 1)
        (message "Showing first of %d linked comments"
                 (length (plist-get resource :targets)))))))

(defun magh-commit--insert-review-summaries
    (context number head reviews threads)
  "Insert submitted REVIEWS and links to their inline comments in THREADS."
  (magh-ui--section (reviews 'reviews nil nil)
    (format "Reviews (%d)" (length reviews))
    (dolist (review reviews)
      (let* ((id (or (alist-get 'id review)
                     (format "%s:%s"
                             (magh-core--name
                              (or (alist-get 'author review)
                                  (alist-get 'user review)))
                             (or (alist-get 'submittedAt review)
                                 (alist-get 'submitted_at review)))))
             (author (or (alist-get 'author review)
                         (alist-get 'user review)))
             (submitted (or (alist-get 'submittedAt review)
                            (alist-get 'submitted_at review)))
             (targets (magh-commit--review-targets review threads))
             (body (or (alist-get 'body review) ""))
             (has-body (not (string-empty-p (string-trim body)))))
        (magh-ui--section (review id nil (or has-body targets))
          (magh-ui--row
           (magh-ui--styled
            (or (alist-get 'state review) "COMMENTED")
            (magh-core--state-face (alist-get 'state review)))
           "by"
           (magh-ui--styled (magh-core--name author) 'magh-author)
           (magh-ui--styled (magh-core--date submitted) 'magh-date)
           (when targets
             (magh-ui--styled
              (format "(%d linked comment%s)"
                      (length targets) (if (= (length targets) 1) "" "s"))
              'magh-permission)))
          (when has-body
            (magh-ui--insert-markdown body context)
            (when targets (insert "\n")))
          (dolist (target targets)
            (magh-commit--insert-review-target-link
             context number head id target)))))))

(defun magh-commit--render-review (context number result)
  "Render Pull Request NUMBER review RESULT using the Commit page."
  (let* ((pr (alist-get 'pr result))
         (commit (alist-get 'commit result))
         (comparison (alist-get 'comparison result))
         (reviews (or (alist-get 'reviews result) (alist-get 'reviews pr)))
         (threads (alist-get 'threads result))
         (head (alist-get 'headRefOid pr))
         (base (alist-get 'baseRefOid pr))
         (files (alist-get 'files comparison))
         (draft-key (magh-commit--review-key context number head))
         (drafts (magh-commit--review-drafts context number head))
         (stale (magh-commit--stale-review-drafts context number head))
         (head-context (magh-commit--review-head-context context pr head))
         (pr-resource
          (magh-resource-create 'pr context :number number
                              :title (alist-get 'title pr)
                              :url (alist-get 'url pr))))
    (setq magh-commit--review-number number
          magh-commit--review-head head
          magh-commit--sha head)
    (let ((start (point)))
      (insert (propertize (format "PR #%s  " number)
                          'font-lock-face 'magh-resource-number)
              (propertize (alist-get 'title pr)
                          'font-lock-face 'magh-resource-title) "\n")
      (add-text-properties start (point) (list 'magh-resource pr-resource)))
    (magh-ui--insert-header "Repository" (magh-context-repository context)
                          'magh-repository)
    (magh-ui--insert-header "Review"
                          (or (alist-get 'reviewDecision pr) "REVIEW_REQUIRED")
                          (magh-core--state-face (alist-get 'reviewDecision pr)))
    (magh-ui--insert-header "Base" (format "%s · %.10s"
                                          (alist-get 'baseRefName pr) base)
                          'magh-branch)
    (magh-ui--insert-header "Head" (format "%s · %.10s"
                                          (alist-get 'headRefName pr) head)
                          'magh-branch)
    (when commit
      (magh-ui--insert-header
       "Commit"
       (car (string-lines
             (alist-get 'message (alist-get 'commit commit))))
       'magh-resource-title))
    (when stale
      (insert (propertize
               (format "Warning: %d draft set(s) belong to an older PR head.\n"
                       (length stale))
               'font-lock-face 'magh-error)))
    (insert "\n")
    (magh-commit--insert-review-summaries
     context number head reviews threads)
    (magh-ui--section (changed-files 'changed-files nil nil)
      (format "Changed files (%d) · Local drafts (%d)"
              (length files) (length drafts))
      (dolist (file files)
        (magh-commit--insert-review-file
         context head-context number head file threads drafts draft-key)))
    (when stale
      (magh-ui--section (stale-drafts 'stale-drafts nil nil)
        "Stale drafts (cannot be submitted)"
        (dolist (entry stale)
          (dolist (draft (cdr entry))
            (magh-commit--insert-review-draft
             context number head (car entry) draft t)))))))

(defun magh-commit--diff-selection-resources (kind)
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
        (let ((resource (magh-ui-resource-at-point)))
          (unless (eq (plist-get resource :kind) kind)
            (user-error "Selection contains a non-commentable diff line"))
          (push resource resources))
        (setq position (line-beginning-position 2))))
    (nreverse resources)))

(defun magh-commit--review-selection ()
  "Return a GitHub review location for point or the active region."
  (let* ((resources (magh-commit--diff-selection-resources 'review-line))
         (first (car resources))
         (last (car (last resources))))
    (unless (seq-every-p
             (lambda (resource)
               (and (equal (plist-get resource :path) (plist-get first :path))
                    (equal (plist-get resource :side) (plist-get first :side))
                    (equal (plist-get resource :hunk) (plist-get first :hunk))))
             resources)
      (user-error "Review ranges must stay in one file, hunk, and diff side"))
    (append
     (list :path (plist-get first :path)
           :line (plist-get last :line)
           :side (plist-get last :side)
           :subject-type "LINE")
     (when (> (length resources) 1)
       (list :start-line (plist-get first :line)
             :start-side (plist-get first :side))))))

(defun magh-commit--commit-selection ()
  "Return the commit diff location selected at point or by the region.
GitHub commit comments have one diff position, so a multi-line selection is
anchored to its final selected line."
  (let* ((resources (magh-commit--diff-selection-resources 'commit-line))
         (first (car resources))
         (last (car (last resources))))
    (unless (seq-every-p
             (lambda (resource)
               (and (equal (plist-get resource :path) (plist-get first :path))
                    (equal (plist-get resource :hunk) (plist-get first :hunk))))
             resources)
      (user-error "Commit comment selections must stay in one file and hunk"))
    (list :path (plist-get first :path)
          :position (plist-get last :position)
          :line (plist-get last :line))))

(defun magh-commit--add-review-draft (draft)
  "Add local review DRAFT to the current Review page."
  (unless (and magh-commit--review-number magh-commit--review-head)
    (user-error "This Commit page is not reviewing a Pull Request"))
  (let* ((body (plist-get draft :body))
         (draft (plist-put draft :id (cl-incf magh-commit--draft-sequence)))
         (drafts (magh-commit--review-drafts
                  magh-buffer-context magh-commit--review-number
                  magh-commit--review-head)))
    (when (string-empty-p (string-trim (or body "")))
      (user-error "Review comment cannot be empty"))
    (magh-commit--set-review-drafts
     magh-buffer-context magh-commit--review-number magh-commit--review-head
     (append drafts (list draft)))
    (message "Collected review comment (%d total)" (1+ (length drafts)))
    (magh-ui-refresh)))

(defun magh-commit-review-comment-add (body)
  "Collect review comment BODY for the current diff line or region."
  (interactive (list (read-string "Review comment: ")))
  (magh-commit--add-review-draft
   (plist-put (magh-commit--review-selection) :body body)))

(defun magh-commit-review-file-comment-add (path body)
  "Collect whole-file review comment BODY for PATH."
  (interactive
   (let ((resource (magh-ui-resource-at-point)))
     (list (or (plist-get resource :path) (read-string "Path: "))
           (read-string "File review comment: "))))
  (magh-commit--add-review-draft
   (list :path path :subject-type "FILE" :body body)))

(defun magh-commit-review-draft-edit (body)
  "Replace the local review draft at point with BODY."
  (interactive
   (let* ((resource (magh-ui-resource-at-point))
          (draft (plist-get resource :data)))
     (unless (eq (plist-get resource :kind) 'review-draft)
       (user-error "No local review draft at point"))
     (when (plist-get resource :stale)
       (user-error "Stale drafts are read-only"))
     (list (read-string "Review comment: " (plist-get draft :body)))))
  (when (string-empty-p (string-trim body))
    (user-error "Review comment cannot be empty"))
  (let* ((resource (magh-ui-resource-at-point))
         (key (plist-get resource :draft-key))
         (id (plist-get resource :draft-id))
         (drafts (gethash key magh-commit--review-drafts)))
    (puthash key
             (mapcar (lambda (draft)
                       (if (equal (plist-get draft :id) id)
                           (plist-put (copy-sequence draft) :body body)
                         draft))
                     drafts)
             magh-commit--review-drafts)
    (magh-ui-refresh)))

(defun magh-commit-review-draft-delete ()
  "Delete the local review draft at point, including a stale draft."
  (interactive)
  (let* ((resource (magh-ui-resource-at-point))
         (key (plist-get resource :draft-key))
         (id (plist-get resource :draft-id)))
    (unless (eq (plist-get resource :kind) 'review-draft)
      (user-error "No local review draft at point"))
    (let ((remaining
           (seq-remove (lambda (draft) (equal (plist-get draft :id) id))
                       (gethash key magh-commit--review-drafts))))
      (if remaining
          (puthash key remaining magh-commit--review-drafts)
        (remhash key magh-commit--review-drafts)))
    (magh-ui-refresh)))

(defun magh-commit-review-reply (body)
  "Immediately publish BODY as a reply to the review thread at point."
  (interactive (list (read-string "Reply: ")))
  (let ((resource (magh-ui-resource-at-point)))
    (unless (eq (plist-get resource :kind) 'review-thread)
      (user-error "No review thread at point"))
    (unless (plist-get resource :can-reply)
      (user-error "GitHub does not allow you to reply to this thread"))
    (when (string-empty-p (string-trim body))
      (user-error "Reply cannot be empty"))
    (magh-api--pr-review-reply
     magh-buffer-context magh-commit--review-number
     (plist-get resource :root-id) body
     (lambda (_) (message "Review reply published") (magh-ui-refresh t))
     #'magh-core--user-error)))

(defun magh-commit-review-toggle-resolved ()
  "Resolve or unresolve the review thread at point."
  (interactive)
  (let* ((resource (magh-ui-resource-at-point))
         (resolved (plist-get resource :resolved))
         (allowed (plist-get resource
                             (if resolved :can-unresolve :can-resolve))))
    (unless (eq (plist-get resource :kind) 'review-thread)
      (user-error "No review thread at point"))
    (unless (plist-get resource :thread-id)
      (user-error "Thread resolution is unavailable on this GitHub host"))
    (unless allowed
      (user-error "GitHub does not allow you to change this thread"))
    (magh-api--pr-review-thread-resolved
     magh-buffer-context magh-commit--review-number
     (plist-get resource :thread-id) (not resolved)
     (lambda (_)
       (message "Review thread %s" (if resolved "reopened" "resolved"))
       (magh-ui-refresh t))
     #'magh-core--user-error)))

(defun magh-commit-review-submit (&optional event body)
  "Submit current local drafts as a Pull Request review EVENT with BODY."
  (interactive)
  (unless (and magh-commit--review-number magh-commit--review-head)
    (user-error "This Commit page is not reviewing a Pull Request"))
  (let* ((event-name
          (or (and event (upcase (symbol-name event)))
              (completing-read
               "Review event: " '("COMMENT" "APPROVE" "REQUEST_CHANGES")
               nil t nil nil "COMMENT")))
         (event (or event (intern (downcase event-name))))
         (body (or body (read-string "Review summary: ")))
         (drafts (magh-commit--review-drafts
                  magh-buffer-context magh-commit--review-number
                  magh-commit--review-head)))
    (when (and (memq event '(comment request_changes))
               (string-empty-p (string-trim body)))
      (user-error "%s reviews require a summary" event-name))
    (magh-api--pr-review
     magh-buffer-context magh-commit--review-number event body drafts
     (lambda (_)
       (magh-commit--set-review-drafts
        magh-buffer-context magh-commit--review-number magh-commit--review-head nil)
       (message "Review submitted")
       (magh-ui-refresh t))
     #'magh-core--user-error magh-commit--review-head)))

(transient-define-prefix magh-commit-review-dispatch ()
  "Pull Request review actions in the Commit page."
  [["Review"
    ("c" "Add line/range draft" magh-commit-review-comment-add)
    ("C" "Add file draft" magh-commit-review-file-comment-add)
    ("V" "Submit review" magh-commit-review-submit)]
   ["At point"
    ("e" "Edit draft" magh-commit-review-draft-edit)
    ("k" "Delete draft" magh-commit-review-draft-delete)
    ("r" "Reply to thread" magh-commit-review-reply)
    ("x" "Resolve / unresolve" magh-commit-review-toggle-resolved)]
   ["Inspect"
    ("g" "Refresh" magh-ui-refresh)
    ("b" "Browse tree" magh-commit-browse-tree)
    ("o" "Browse web" magh-ui-browse)]])

;;;###autoload
(defun magh-commit-review (number &optional context)
  "Review the latest head of Pull Request NUMBER using the Commit page."
  (interactive (list (read-number "Pull Request number: ")))
  (setq context (magh-ui--repository-context context))
  (magh-ui--open-page
   (format "*magh: %s · PR #%s Review*"
           (magh-context-repository context) number)
   context 'commit-review number
   (lambda (success error force)
     (magh-commit--fetch-review context number success error force))
   (lambda (data) (magh-commit--render-review context number data))
   :setup
   (lambda ()
     (setq magh-commit--review-number number
           magh-buffer-dispatch-function #'magh-commit-review-dispatch)
     (local-set-key (kbd "c") #'magh-commit-review-comment-add)
     (local-set-key (kbd "C") #'magh-commit-review-file-comment-add)
     (local-set-key (kbd "e") #'magh-commit-review-draft-edit)
     (local-set-key (kbd "k") #'magh-commit-review-draft-delete)
     (local-set-key (kbd "r") #'magh-commit-review-reply)
     (local-set-key (kbd "x") #'magh-commit-review-toggle-resolved)
     (local-set-key (kbd "V") #'magh-commit-review-submit))))

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
   (lambda (_) (message "Commit comment added")
     (magh-ui--refresh-if-page))
   #'magh-core--user-error))

(defun magh-commit-inline-comment (body &optional context sha)
  "Add BODY as an inline comment at the selected Commit diff position."
  (interactive (list (read-string "Inline commit comment: ")))
  (let ((location (magh-commit--commit-selection)))
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

(magh-candidate-register
 'commit-review
 :open (lambda (resource)
         (magh-commit-review (plist-get resource :number)
                           (plist-get resource :context))))

(magh-candidate-register
 'review-summary
 :open #'magh-commit--goto-review-target)

(magh-candidate-register 'commit-more :open (lambda (_resource)
                                            (magh-commit-load-more)))

(provide 'magh-commit)
;;; magh-commit.el ends here
