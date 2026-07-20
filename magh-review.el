;;; magh-review.el --- Pull Request diff review workflow -*- lexical-binding: t; -*-

;; Copyright (C) 2026
;; SPDX-License-Identifier: GPL-2.0-only

;; Author: magh.el contributors
;; Keywords: tools, vc, github

;;; Commentary:

;; Pull Request review pages, diff targets, inline threads, local drafts, and
;; review submission.  Commit history and commit detail pages remain in
;; `magh-commit'.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'transient)
(require 'magh-api)
(require 'magh-browse)
(require 'magh-candidate)
(require 'magh-diff)
(require 'magh-ui)

(defvar-local magh-review--review-number nil)
(defvar-local magh-review--review-head nil)

(defvar magh-review--review-drafts (make-hash-table :test #'equal)
  "Local Pull Request review drafts keyed by host, repository, PR, and head.")

(defvar magh-review--draft-sequence 0
  "Sequence used to give local review drafts stable identifiers.")

;;; Pull Request review

(defun magh-review--review-key (context number head)
  "Return the local review key for NUMBER at HEAD in CONTEXT."
  (list (magh-context-host context) (magh-context-repository context) number head))

(defun magh-review--review-drafts (context number head)
  "Return local drafts for Pull Request NUMBER at HEAD in CONTEXT."
  (gethash (magh-review--review-key context number head)
           magh-review--review-drafts))

(defun magh-review--set-review-drafts (context number head drafts)
  "Store DRAFTS for Pull Request NUMBER at HEAD in CONTEXT."
  (let ((key (magh-review--review-key context number head)))
    (if drafts
        (puthash key drafts magh-review--review-drafts)
      (remhash key magh-review--review-drafts))))

(defun magh-review--stale-review-drafts (context number head)
  "Return stale draft entries for NUMBER excluding current HEAD."
  (let ((prefix (list (magh-context-host context)
                      (magh-context-repository context) number))
        entries)
    (maphash
     (lambda (key drafts)
       (when (and (equal prefix (seq-take key 3))
                  (not (equal head (nth 3 key))))
         (push (cons key drafts) entries)))
     magh-review--review-drafts)
    (nreverse entries)))

(defun magh-review--fetch-review (context number success error force)
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

(defun magh-review--review-thread-resource (context number head thread)
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

(defun magh-review--draft-resource (context number head key draft &optional stale)
  "Create a review draft resource for DRAFT stored below KEY."
  (magh-resource-create
   'review-draft context :number number :sha head :path (plist-get draft :path)
   :line (plist-get draft :line) :side (plist-get draft :side)
   :draft-id (plist-get draft :id) :draft-key key :stale stale :data draft))

(defun magh-review--review-comment-heading (comment &optional reply)
  "Return a heading for review COMMENT, marking it as REPLY when non-nil."
  (let ((author (magh-core--name (alist-get 'user comment)))
        (date (or (alist-get 'created_at comment)
                  (alist-get 'updated_at comment))))
    (magh-ui--conversation-heading
     (if reply "Reply" "Comment") author (magh-core--date date))))

(defun magh-review--insert-review-comment (context comment &optional reply)
  "Insert review COMMENT, marking it as REPLY when non-nil."
  (insert (magh-review--review-comment-heading comment reply) "\n")
  (magh-ui--insert-markdown (alist-get 'body comment) context))

(defun magh-review--insert-review-thread (context number head thread)
  "Insert normalized review THREAD for NUMBER at HEAD."
  (let* ((resource (magh-review--review-thread-resource
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
        (magh-review--insert-review-comment context first))
      (dolist (reply (cdr comments))
        (magh-ui--section (comment (alist-get 'id reply) resource nil)
          (magh-review--review-comment-heading reply t)
          (magh-ui--insert-markdown (alist-get 'body reply) context))))))

(defun magh-review--insert-review-draft
    (context number head key draft &optional stale)
  "Insert local review DRAFT, optionally marked STALE."
  (let ((resource (magh-review--draft-resource
                   context number head key draft stale)))
    (magh-ui--section (inline-comment (plist-get draft :id) resource nil)
      (magh-ui--row
       (magh-ui--styled (if stale "Stale draft" "Draft review comment")
                      'magh-conversation-kind)
       (when stale
         (magh-ui--styled (format "%.10s" (nth 3 key)) 'magh-hash)))
      (magh-ui--insert-markdown (plist-get draft :body) context))))

(defun magh-review--thread-at-location-p (thread path line side)
  "Return non-nil when THREAD is anchored at PATH, LINE, and SIDE."
  (and (not (alist-get 'is_outdated thread))
       (equal (alist-get 'path thread) path)
       (equal (alist-get 'line thread) line)
       (equal (upcase (or (alist-get 'side thread) "RIGHT")) side)))

(defun magh-review--draft-at-location-p (draft path line side)
  "Return non-nil when DRAFT is anchored at PATH, LINE, and SIDE."
  (and (equal (plist-get draft :subject-type) "LINE")
       (equal (plist-get draft :path) path)
       (equal (plist-get draft :line) line)
       (equal (upcase (or (plist-get draft :side) "RIGHT")) side)))

(defun magh-review--review-head-context (context pr head)
  "Return the repository context containing PR HEAD."
  (if-let* ((repository
             (alist-get 'nameWithOwner (alist-get 'headRepository pr))))
      (magh-context-copy
       (magh-context-from-repository repository (magh-context-host context))
       :ref head)
    (magh-context-copy context :ref head)))

(defun magh-review--insert-review-file
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
      (magh-ui--diff-file-heading
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
          (magh-review--insert-review-thread context number head thread)))
      (dolist (draft file-drafts)
        (when (equal (plist-get draft :subject-type) "FILE")
          (push draft inserted-drafts)
          (magh-review--insert-review-draft
           context number head draft-key draft)))
      (if (string-empty-p (or patch ""))
          (insert (propertize "Binary or oversized diff unavailable.\n"
                              'font-lock-face 'shadow))
        (magh-diff--insert-patch-records
         (magh-diff--parse-patch-lines patch context number head path)
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
                              (magh-review--thread-at-location-p
                               thread path line side))
                     (push thread inserted-threads)
                     (magh-review--insert-review-thread
                      context number head thread)))
                 (dolist (draft file-drafts)
                   (when (and (not (memq draft inserted-drafts))
                              (magh-review--draft-at-location-p
                               draft path line side))
                     (push draft inserted-drafts)
                     (magh-review--insert-review-draft
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
              (magh-review--insert-review-thread context number head thread))
            (dolist (draft unmapped-drafts)
              (magh-review--insert-review-draft
               context number head draft-key draft))))))))

(defun magh-review--review-targets (review threads)
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

(defun magh-review--insert-review-target-link
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

(defun magh-review--section-resource (section)
  "Return the structured resource stored on SECTION, if any."
  (let ((value (oref section value)))
    (and (magh-section-value-p value)
         (magh-section-value-resource value))))

(defun magh-review--review-target-section-p (section target)
  "Return non-nil when SECTION represents review TARGET."
  (let* ((value (oref section value))
         (key (and (magh-section-value-p value)
                   (magh-section-value-key value)))
         (resource (magh-review--section-resource section)))
    (if (plist-get target :reply)
        (and (eq (oref section type) 'comment)
             (equal key (plist-get target :comment-id)))
      (and (eq (oref section type) 'inline-comment)
           (eq (plist-get resource :kind) 'review-thread)
           (equal (plist-get resource :root-id)
                  (plist-get target :root-id))))))

(defun magh-review--materialize-review-target (section target)
  "Expand SECTION as needed and return the section for review TARGET."
  (magit-section-show section)
  (or (and (magh-review--review-target-section-p section target) section)
      (cl-loop for child in (oref section children)
               thereis (magh-review--materialize-review-target child target))))

(defun magh-review--goto-review-target (resource)
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
               (equal (plist-get (magh-review--section-resource section) :path)
                      (plist-get target :path)))
             (oref changed-files children)))
           (target-section
            (and file (magh-review--materialize-review-target file target))))
      (unless target-section
        (user-error "Linked review comment is not present in this diff"))
      (goto-char (oref target-section start))
      (when-let* ((window (get-buffer-window (current-buffer) t)))
        (set-window-point window (point))
        (with-selected-window window (recenter)))
      (when (length> (plist-get resource :targets) 1)
        (message "Showing first of %d linked comments"
                 (length (plist-get resource :targets)))))))

(defun magh-review--insert-review-summaries
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
             (targets (magh-review--review-targets review threads))
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
                      (length targets) (if (length= targets 1) "" "s"))
              'magh-permission)))
          (when has-body
            (magh-ui--insert-markdown body context)
            (when targets (insert "\n")))
          (dolist (target targets)
            (magh-review--insert-review-target-link
             context number head id target)))))))

(defun magh-review--render-review (context number result)
  "Render Pull Request NUMBER review RESULT using the Review page."
  (let* ((pr (alist-get 'pr result))
         (commit (alist-get 'commit result))
         (comparison (alist-get 'comparison result))
         (reviews (or (alist-get 'reviews result) (alist-get 'reviews pr)))
         (threads (alist-get 'threads result))
         (head (alist-get 'headRefOid pr))
         (base (alist-get 'baseRefOid pr))
         (files (alist-get 'files comparison))
         (draft-key (magh-review--review-key context number head))
         (drafts (magh-review--review-drafts context number head))
         (stale (magh-review--stale-review-drafts context number head))
         (head-context (magh-review--review-head-context context pr head))
         (pr-resource
          (magh-resource-create 'pr context :number number
                              :title (alist-get 'title pr)
                              :url (alist-get 'url pr))))
    (setq magh-review--review-number number
          magh-review--review-head head)
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
    (magh-review--insert-review-summaries
     context number head reviews threads)
    (magh-ui--section (changed-files 'changed-files nil nil)
      (format "Changed files (%d) · Local drafts (%d)"
              (length files) (length drafts))
      (dolist (file files)
        (magh-review--insert-review-file
         context head-context number head file threads drafts draft-key)))
    (when stale
      (magh-ui--section (stale-drafts 'stale-drafts nil nil)
        "Stale drafts (cannot be submitted)"
        (dolist (entry stale)
          (dolist (draft (cdr entry))
            (magh-review--insert-review-draft
             context number head (car entry) draft t)))))))

(defun magh-review--review-selection ()
  "Return a GitHub review location for point or the active region."
  (let* ((resources (magh-diff--selection-resources 'review-line))
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
     (when (length> resources 1)
       (list :start-line (plist-get first :line)
             :start-side (plist-get first :side))))))

(defun magh-review--add-review-draft (draft)
  "Add local review DRAFT to the current Review page."
  (unless (and magh-review--review-number magh-review--review-head)
    (user-error "This page is not reviewing a Pull Request"))
  (let* ((body (plist-get draft :body))
         (draft (plist-put draft :id (cl-incf magh-review--draft-sequence)))
         (drafts (magh-review--review-drafts
                  magh-buffer-context magh-review--review-number
                  magh-review--review-head)))
    (when (string-empty-p (string-trim (or body "")))
      (user-error "Review comment cannot be empty"))
    (magh-review--set-review-drafts
     magh-buffer-context magh-review--review-number magh-review--review-head
     (append drafts (list draft)))
    (message "Collected review comment (%d total)" (1+ (length drafts)))
    (magh-ui-refresh)))

(defun magh-review-comment-add (body)
  "Collect review comment BODY for the current diff line or region."
  (interactive (list (read-string "Review comment: ")))
  (magh-review--add-review-draft
   (plist-put (magh-review--review-selection) :body body)))

(defun magh-review-file-comment-add (path body)
  "Collect whole-file review comment BODY for PATH."
  (interactive
   (let ((resource (magh-ui-resource-at-point)))
     (list (or (plist-get resource :path) (read-string "Path: "))
           (read-string "File review comment: "))))
  (magh-review--add-review-draft
   (list :path path :subject-type "FILE" :body body)))

(defun magh-review-draft-edit (body)
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
         (drafts (gethash key magh-review--review-drafts)))
    (puthash key
             (mapcar (lambda (draft)
                       (if (equal (plist-get draft :id) id)
                           (plist-put (copy-sequence draft) :body body)
                         draft))
                     drafts)
             magh-review--review-drafts)
    (magh-ui-refresh)))

(defun magh-review-draft-delete ()
  "Delete the local review draft at point, including a stale draft."
  (interactive)
  (let* ((resource (magh-ui-resource-at-point))
         (key (plist-get resource :draft-key))
         (id (plist-get resource :draft-id)))
    (unless (eq (plist-get resource :kind) 'review-draft)
      (user-error "No local review draft at point"))
    (let ((remaining
           (seq-remove (lambda (draft) (equal (plist-get draft :id) id))
                       (gethash key magh-review--review-drafts))))
      (if remaining
          (puthash key remaining magh-review--review-drafts)
        (remhash key magh-review--review-drafts)))
    (magh-ui-refresh)))

(defun magh-review-reply (body)
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
     magh-buffer-context magh-review--review-number
     (plist-get resource :root-id) body
     (lambda (_) (message "Review reply published") (magh-ui-refresh t))
     #'magh-core--user-error)))

(defun magh-review-toggle-resolved ()
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
     magh-buffer-context magh-review--review-number
     (plist-get resource :thread-id) (not resolved)
     (lambda (_)
       (message "Review thread %s" (if resolved "reopened" "resolved"))
       (magh-ui-refresh t))
     #'magh-core--user-error)))

(defun magh-review-submit (&optional event body)
  "Submit current local drafts as a Pull Request review EVENT with BODY."
  (interactive)
  (unless (and magh-review--review-number magh-review--review-head)
    (user-error "This page is not reviewing a Pull Request"))
  (let* ((event-name
          (or (and event (upcase (symbol-name event)))
              (completing-read
               "Review event: " '("COMMENT" "APPROVE" "REQUEST_CHANGES")
               nil t nil nil "COMMENT")))
         (event (or event (intern (downcase event-name))))
         (body (or body (read-string "Review summary: ")))
         (drafts (magh-review--review-drafts
                  magh-buffer-context magh-review--review-number
                  magh-review--review-head)))
    (when (and (memq event '(comment request_changes))
               (string-empty-p (string-trim body)))
      (user-error "%s reviews require a summary" event-name))
    (magh-api--pr-review
     magh-buffer-context magh-review--review-number event body drafts
     (lambda (_)
       (magh-review--set-review-drafts
        magh-buffer-context magh-review--review-number magh-review--review-head nil)
       (message "Review submitted")
       (magh-ui-refresh t))
     #'magh-core--user-error magh-review--review-head)))

(defun magh-review-browse-tree (&optional context head)
  "Browse the repository tree at the current Review HEAD in CONTEXT."
  (interactive)
  (setq context (magh-ui--repository-context context)
        head (or head magh-review--review-head))
  (unless head
    (user-error "No Pull Request head is available in this buffer"))
  (magh-browse-repository context head ""))

(transient-define-prefix magh-review-dispatch ()
  "Pull Request review actions in the Review page."
  [["Review"
    ("c" "Add line/range draft" magh-review-comment-add)
    ("C" "Add file draft" magh-review-file-comment-add)
    ("V" "Submit review" magh-review-submit)]
   ["At point"
    ("e" "Edit draft" magh-review-draft-edit)
    ("k" "Delete draft" magh-review-draft-delete)
    ("r" "Reply to thread" magh-review-reply)
    ("x" "Resolve / unresolve" magh-review-toggle-resolved)]
   ["Inspect"
    ("g" "Refresh" magh-ui-refresh)
    ("b" "Browse tree" magh-review-browse-tree)
    ("o" "Browse web" magh-ui-browse)]])

;;;###autoload
(defun magh-review (number &optional context)
  "Review the latest head of Pull Request NUMBER using the Review page."
  (interactive (list (read-number "Pull Request number: ")))
  (setq context (magh-ui--repository-context context))
  (magh-ui--open-page
   (format "*magh: %s · PR #%s Review*"
           (magh-context-repository context) number)
   context 'commit-review number
   (lambda (success error force)
     (magh-review--fetch-review context number success error force))
   (lambda (data) (magh-review--render-review context number data))
   :setup
   (lambda ()
     (setq magh-review--review-number number
           magh-buffer-dispatch-function #'magh-review-dispatch)
     (local-set-key (kbd "c") #'magh-review-comment-add)
     (local-set-key (kbd "C") #'magh-review-file-comment-add)
     (local-set-key (kbd "e") #'magh-review-draft-edit)
     (local-set-key (kbd "k") #'magh-review-draft-delete)
     (local-set-key (kbd "r") #'magh-review-reply)
     (local-set-key (kbd "x") #'magh-review-toggle-resolved)
     (local-set-key (kbd "V") #'magh-review-submit))))


(magh-candidate-register
 'commit-review
 :open (lambda (resource)
         (magh-review (plist-get resource :number)
                      (plist-get resource :context))))

(magh-candidate-register
 'review-summary
 :open #'magh-review--goto-review-target)

(provide 'magh-review)
;;; magh-review.el ends here
