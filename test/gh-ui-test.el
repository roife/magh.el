;;; gh-ui-test.el --- Native page and Magit integration tests -*- lexical-binding: t; -*-

(require 'gh-test-helper)
(require 'gh)
(require 'gh-magit)

(ert-deftest gh-ui-faces-inherit-loaded-magit-faces ()
  (should (featurep 'magit))
  (should (featurep 'magit-diff))
  (should (featurep 'magit-log))
  (should (featurep 'magit-process))
  (dolist (mapping '((gh-section-heading . magit-section-heading)
                     (gh-resource-number . magit-refname-pullreq)
                     (gh-resource-title . magit-section-secondary-heading)
                     (gh-conversation-kind . magit-section-secondary-heading)
                     (gh-repository . magit-branch-remote)
                     (gh-branch . magit-branch-local)
                     (gh-author . magit-log-author)
                     (gh-date . magit-log-date)
                     (gh-tag . magit-tag)
                     (gh-hash . magit-hash)
                     (gh-workflow . magit-refname)
                     (gh-file . magit-filename)
                     (gh-label . magit-keyword)
                     (gh-permission . magit-dimmed)
                     (gh-added . magit-diffstat-added)
                     (gh-removed . magit-diffstat-removed)
                     (gh-open-state . magit-process-ok)
                     (gh-pending-state . magit-branch-warning)
                     (gh-draft-state . magit-dimmed)
                     (gh-closed-state . magit-process-ng)
                     (gh-metadata-key . magit-header-line-key)
                     (gh-loading . magit-dimmed)
                     (gh-error . magit-process-ng)))
    (should (facep (car mapping)))
    (should (facep (cdr mapping)))
    (should (eq (face-attribute (car mapping) :inherit nil nil)
                (cdr mapping))))
  (should (eq (face-attribute 'gh-conversation-kind :weight nil nil) 'bold)))

(ert-deftest gh-ui-semantic-row-has-no-fixed-width-padding ()
  (let* ((gh-date-format-function #'identity)
         (data '((number . 7) (title . "Issue title") (state . "OPEN")
                 (author . ((login . "alice")))
                 (updatedAt . "2026-07-02T00:00:00Z")))
         (row (gh-ui--format-row (gh-issue--row-values data))))
    (should (equal row "OPEN #7 Issue title"))
    (dolist (expected '(("OPEN" . gh-open-state)
                        ("#7" . gh-resource-number)
                        ("Issue title" . gh-resource-title)))
      (let ((position (string-match (regexp-quote (car expected)) row)))
        (should position)
        (should (eq (get-text-property position 'font-lock-face row)
                    (cdr expected)))))))

(ert-deftest gh-ui-magit-heading-preserves-row-faces ()
  (let* ((case-fold-search nil)
         (gh-date-format-function #'identity)
         (gh-issue--limit 10)
         (data '(((number . 7) (title . "Issue title") (state . "OPEN")
                  (author . ((login . "alice")))
                  (updatedAt . "2026-07-02T00:00:00Z"))))
         (text (gh-test-render-page
                'issue-list "open"
                (lambda (items)
                  (gh-issue--render-list
                   (gh-context-from-repository "o/r") "open" items))
                data)))
    (should (string-match-p
             "OPEN #7 Issue title\n" text))
    (should (string-match-p
             "OPEN #7 Issue title\n\nLoad more" text))
    (dolist (expected '(("OPEN" . gh-open-state)
                        ("#7" . gh-resource-number)
                        ("Issue title" . gh-resource-title)))
      (let ((position (string-match (regexp-quote (car expected)) text)))
        (should position)
        (should (eq (get-text-property position 'font-lock-face text)
                    (cdr expected)))))))

(ert-deftest gh-ui-repository-status-rows-follow-ui-layout ()
  (let* ((case-fold-search nil)
         (gh-date-format-function #'identity)
         (context (gh-context-from-repository "o/r"))
         (result
          '((repository . ((nameWithOwner . "o/r") (visibility . "PUBLIC")
                           (viewerHasStarred . t)
                           (viewerSubscription . "SUBSCRIBED")
                           (stargazerCount . 12) (forkCount . 3)
                           (watchers . ((totalCount . 4)))))
            (viewer-forked . t)
            (languages . ((Emacs\ Lisp . 75) (Shell . 25)))
            (prs . (((number . 8) (title . "PR title") (state . "OPEN")
                     (author . ((login . "alice")))
                     (reviewDecision . "REVIEW_REQUIRED")
                     (updatedAt . "2026-07-02T00:00:00Z"))))
            (issues . (((number . 7) (title . "Issue title") (state . "OPEN")
                        (author . ((login . "bob")))
                        (updatedAt . "2026-07-03T00:00:00Z"))))
            (runs . (((databaseId . 42) (displayTitle . "Run title")
                      (workflowName . "CI") (conclusion . "SUCCESS")
                      (createdAt . "2026-07-04T00:00:00Z"))))
            (commits . (((sha . "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa")
                         (commit . ((message . "Recent change")
                                    (author . ((name . "Alice")
                                               (date . "2026-07-05T00:00:00Z"))))))))
            (releases . (((tagName . "v1.0") (name . "Version 1"))
                         ((tagName . "v2.0") (name . "Version 2"))
                         ((tagName . "v3.0") (name . "Version 3"))
                         ((tagName . "v4.0") (name . "Version 4"))
                         ((tagName . "v5.0") (name . "Version 5"))
                         ((tagName . "v6.0") (name . "Version 6"))))))
         (text (gh-test-render-page
                'repository "o/r"
                (lambda (data) (gh-repo--render-status context data)) result)))
    (should (string-match-p
             (regexp-quote
              "OPEN #8 PR title REVIEW_REQUIRED\n")
             text))
    (should (string-match-p
             (regexp-quote
              "SUCCESS Run title CI\n")
             text))
    (should (string-match-p
             "Stats: 12 stars, 3 forks, 4 watchers\n" text))
    (should (string-match-p
             "Viewer status: starred, watching, forked\n" text))
    (should (string-match-p "Recent commits\n" text))
    (should (string-match-p
             (regexp-quote
              "No description.\n\nStatistics\n\nRecent commits\n")
             text))
    (should (string-match-p "aaaaaaaaaa Recent change Alice" text))
    (should (string-match-p
             "2026-07-05T00:00:00Z\n\nPull requests\n" text))
    (should (string-match-p "Releases\n" text))
    (should-not (string-match-p
                 "\\(?:Pull requests\\|Issues\\|Actions\\|Releases\\) ("
                 text))
    (should-not (string-match-p "v6.0" text))
    (should-not (string-match-p "#42.*Run title" text))
    (dolist (expected '(("#8" . gh-resource-number)
                        ("PR title" . gh-resource-title)
                        ("REVIEW_REQUIRED" . gh-pending-state)
                        ("Run title" . gh-resource-title)
                        ("CI" . gh-workflow)))
      (let ((position (string-match (regexp-quote (car expected)) text)))
        (should position)
        (should (eq (get-text-property position 'font-lock-face text)
                    (cdr expected)))))))

(ert-deftest gh-ui-list-metadata-is-inside-expanded-details ()
  (let ((context (gh-context-from-repository "o/r"))
        (gh-date-format-function #'identity))
    (dolist
        (case
         `((,(lambda ()
               (gh-issue--insert-row
                context
                '((number . 7) (title . "Issue") (state . "OPEN")
                  (author . ((login . "alice")))
                  (createdAt . "created") (updatedAt . "updated"))))
            "OPEN #7 Issue\n" "Author: alice\n" "Updated: updated\n")
           (,(lambda ()
               (gh-pr--insert-row
                context
                '((number . 8) (title . "PR") (state . "OPEN")
                  (author . ((login . "bob")))
                  (createdAt . "created") (updatedAt . "updated"))))
            "OPEN #8 PR\n" "Author: bob\n" "Updated: updated\n")
           (,(lambda ()
               (gh-actions--insert-run
                context
                '((databaseId . 9) (displayTitle . "CI")
                  (status . "queued") (workflowName . "Build")
                  (createdAt . "created"))))
            "QUEUED CI Build\n" "Created: created\n")))
      (with-temp-buffer
        (gh-section-mode)
        (let ((inhibit-read-only t))
          (magit-insert-section (root)
            (funcall (car case))))
        (let ((section (car (oref magit-root-section children))))
          (should (string-match-p (nth 1 case) (buffer-string)))
          (should-not (string-match-p (nth 2 case) (buffer-string)))
          (let ((inhibit-read-only t))
            (magit-section-show section))
          (should (string-match-p (nth 2 case) (buffer-string)))
          (when-let* ((updated (nth 3 case)))
            (should (string-match-p updated (buffer-string)))))))))

(ert-deftest gh-ui-repository-permission-and-date-are-expanded-details ()
  (let ((context (gh-context-from-repository "o/r"))
        (gh-date-format-function #'identity)
        (result
         '((user . ((login . "me") (followers . 0) (following . 0)))
           (repositories . (((nameWithOwner . "o/r")
                             (visibility . "PUBLIC")
                             (viewerPermission . "ADMIN")
                             (updatedAt . "updated")))))))
    (with-temp-buffer
      (gh-section-mode)
      (setq gh-buffer-context context
            gh-buffer-resource-kind 'user-status
            gh-buffer-resource-id 'viewer)
      (gh-ui--replace
       (lambda (data) (gh-pages--render-user-status context data)) result nil)
      (let* ((repositories
              (seq-find (lambda (section)
                          (eq (oref section type) 'repositories))
                        (oref magit-root-section children)))
             (repository (car (oref repositories children))))
        (should (string-match-p "My pull requests\n" (buffer-string)))
        (should (string-match-p "Repositories\n" (buffer-string)))
        (should-not (string-match-p
                     "\\(?:My pull requests\\|Repositories\\) ("
                     (buffer-string)))
        (should (string-match-p "public o/r\n" (buffer-string)))
        (should-not (string-match-p "ADMIN\\|Updated: updated" (buffer-string)))
        (let ((inhibit-read-only t))
          (magit-section-show repository))
        (should (string-match-p "Permission: ADMIN\n" (buffer-string)))
        (should (string-match-p "Updated: updated\n" (buffer-string)))))))

(ert-deftest gh-ui-language-statistics-only-show-percentages ()
  (with-temp-buffer
    (gh-repo--insert-languages '((Emacs\ Lisp . 75) (Shell . 25)))
    (should (equal (buffer-string)
                   "Emacs Lisp: 75.0%\nShell: 25.0%\n"))
    (should-not (string-match-p "bytes\\|Size:" (buffer-string)))))

(ert-deftest gh-ui-repository-viewer-status-shows-negative-states ()
  (should
   (equal (gh-repo--viewer-status
           '((viewerHasStarred . nil) (viewerSubscription . "IGNORED")) nil)
          "not starred, not watching, not forked")))

(ert-deftest gh-ui-comment-sections-have-a-blank-line-between-siblings ()
  (with-temp-buffer
    (magit-section-mode)
    (let ((inhibit-read-only t))
      (magit-insert-section (root)
        (gh-ui--section (conversation 'conversation nil nil)
          "Conversation"
          (gh-ui--section (comment 1 nil nil) "First comment")
          (gh-ui--section (comment 2 nil nil) "Second comment"))))
    (should (string-match-p
             "First comment\n\nSecond comment\n" (buffer-string)))))

(ert-deftest gh-ui-conversation-kind-labels-use-bold-highlight-face ()
  (let* ((gh-date-format-function #'identity)
         (context (gh-context-from-repository "o/r"))
         (items
          '((comment . ((id . "c1") (author . ((login . "alice")))
                        (createdAt . "2026-07-01") (body . "Comment body")))
            (review . ((id . "r1") (author . ((login . "bob")))
                       (submittedAt . "2026-07-02") (state . "APPROVED")
                       (body . "Review body")))
            (inline . ((id . 3) (user . ((login . "carol")))
                       (created_at . "2026-07-03") (path . "a.el")
                       (line . 7) (body . "Inline body")))))
         (text
          (gh-test-render-page
           'conversation 1
           (lambda (_)
             (gh-pr--render-conversation context items "main"))
           nil)))
    (dolist (label '("Comment" "Review" "Inline comment"))
      (let ((position (string-match (regexp-quote label) text)))
        (should position)
        (should (eq (get-text-property position 'font-lock-face text)
                    'gh-conversation-kind))))))

(ert-deftest gh-ui-prose-sections-enable-visual-line-mode ()
  (with-temp-buffer
    (gh-section-mode)
    (should-not visual-line-mode)
    (let ((inhibit-read-only t))
      (magit-insert-section (root)
        (gh-ui--section (description 'description nil nil)
          "One-line description")))
    (should visual-line-mode))
  (with-temp-buffer
    (should-not visual-line-mode)
    (gh-ui--insert-markdown "Comment body")
    (should visual-line-mode)))

(ert-deftest gh-ui-actions-log-groups-repeated-job-and-step-columns ()
  (let ((text
         (gh-actions--simplify-log
          (concat "build\tcheckout\t2026-07-15T12:34:56.1234567Z first\n"
                  "build\tcheckout\t2026-07-15T12:34:57Z second\n"
                  "build\ttest\t2026-07-15T12:35:01Z test\n"
                  "lint\tsetup\tplain line\n"))))
    (should
     (equal (substring-no-properties text)
            (concat "build\n  checkout\n12:34:56 first\n12:34:57 second\n"
                    "  test\n12:35:01 test\n\nlint\n  setup\nplain line\n")))
    (should (eq (get-text-property 0 'font-lock-face text) 'gh-workflow))))

(ert-deftest gh-ui-message-parts-promotes-summary-without-repeating-it ()
  (should (equal (gh-ui--message-parts "Summary\n\nBody text")
                 '("Summary" . "\nBody text")))
  (should (equal (gh-ui--message-parts "Summary\r\n\r\nBody text")
                 '("Summary" . "\nBody text")))
  (should (equal (gh-ui--message-parts "" "No description.")
                 '("No description."))))

(ert-deftest gh-ui-description-renders-like-a-revision-message ()
  (let* ((context (gh-context-from-repository "o/r"))
         (text
          (gh-test-render-page
           'issue 1
           (lambda (data) (gh-issue--render-view context data))
           '((number . 1) (title . "Issue") (state . "OPEN")
             (body . "Summary\n\nBody text")))))
    (should (string-match-p "#1 Issue\n" text))
    (should (string-match-p "Summary\n\nBody text\n" text))
    (should-not (string-match-p "Description" text))
    (let ((position 0) (count 0))
      (while (string-match "Summary" text position)
        (setq count (1+ count) position (match-end 0)))
      (should (= count 1)))))

(ert-deftest gh-ui-diff-preserves-added-and-removed-line-faces ()
  (with-temp-buffer
    (gh-ui--insert-diff
     "diff --git a/a.el b/a.el\n--- a/a.el\n+++ b/a.el\n@@ -1 +1 @@\n-old\n+new\n")
    (dolist (expected '(("-old" . diff-removed) ("+new" . diff-added)))
      (goto-char (point-min))
      (search-forward (car expected))
      (let ((face (get-text-property (1- (point)) 'font-lock-face)))
        (should (if (listp face)
                    (memq (cdr expected) face)
                  (eq face (cdr expected))))))))

(defun gh-test-render-page (kind id renderer data)
  "Render DATA with RENDERER as a KIND page and return its text."
  (with-temp-buffer
    (gh-section-mode)
    (setq gh-buffer-context (gh-context-from-repository "o/r")
          gh-buffer-resource-kind kind
          gh-buffer-resource-id id)
    (gh-ui--replace renderer data nil)
    (should magit-root-section)
    ;; Reproduce the just-in-time refontification that happens after an
    ;; asynchronous renderer yields back to interactive Emacs.
    (font-lock-flush (point-min) (point-max))
    (font-lock-ensure (point-min) (point-max))
    (buffer-string)))

(ert-deftest gh-ui-late-generation-cannot-overwrite-newer-page ()
  (let ((gh-display-buffer-function (lambda (buffer) buffer))
        callbacks
        (context (gh-context-from-repository "o/r")))
    (let ((buffer
           (gh-ui--open-page
            " *gh generation test*" context 'repository "o/r"
            (lambda (success error _force)
              (push (cons success error) callbacks))
            (lambda (data) (insert (format "value=%s" data))))))
      (unwind-protect
          (with-current-buffer buffer
            (gh-ui-refresh t)
            (should (= (length callbacks) 2))
            (let ((new (car callbacks)) (old (cadr callbacks)))
              (funcall (car new) "new")
              (funcall (car old) "old"))
            (should (string-match-p "value=new" (buffer-string)))
            (should-not (string-match-p "value=old" (buffer-string))))
        (when (buffer-live-p buffer) (kill-buffer buffer))))))

(ert-deftest gh-ui-visibility-survives-page-recreation-via-magit-cache ()
  (let ((gh-display-buffer-function #'identity)
        (gh-section-cache-visibility t)
        (gh-ui--visibility-cache (make-hash-table :test #'equal))
        (context (gh-context-from-repository "o/r"))
        callback)
    (cl-labels
        ((open-page
          ()
          (gh-ui--open-page
           " *gh visibility test*" context 'issue-list 'open
           (lambda (success _error _force) (setq callback success))
           (lambda (_data)
             (gh-ui--section (items 'items nil t)
               "Items"
               (insert "Body\n"))))))
      (let ((buffer (open-page)))
        (with-current-buffer buffer
          (funcall callback nil)
          (magit-section-show (car (oref magit-root-section children))))
        (kill-buffer buffer))
      (setq callback nil)
      (let ((buffer (open-page)))
        (unwind-protect
            (with-current-buffer buffer
              (funcall callback nil)
              (should-not
               (oref (car (oref magit-root-section children)) hidden)))
          (kill-buffer buffer))))))

(ert-deftest gh-ui-markdown-creates-native-reference-buttons ()
  (let ((context (gh-context-from-repository "o/r"))
        (gh-view-inline-images nil))
    (with-temp-buffer
      (gh-ui--insert-markdown
       "Fixes #12 by @octocat in deadbee and https://github.com/o/r/pull/9"
       context)
      (goto-char (point-min))
      (search-forward "#12")
      (should (eq (plist-get (button-get (button-at (1- (point))) 'gh-resource)
                             :kind)
                  'issue))
      (search-forward "@octocat")
      (should (eq (plist-get (button-get (button-at (1- (point))) 'gh-resource)
                             :kind)
                  'user))
      (search-forward "deadbee")
      (should (eq (plist-get (button-get (button-at (1- (point))) 'gh-resource)
                             :kind)
                  'commit)))))

(ert-deftest gh-ui-markdown-normalizes-carriage-return-line-endings ()
  (let ((gh-view-inline-images nil))
    (with-temp-buffer
      (gh-ui--insert-markdown "First\r\nSecond\rThird")
      (should (equal (buffer-substring-no-properties (point-min) (point-max))
                     "First\nSecond\nThird\n"))
      (should-not (string-match-p "\r" (buffer-string))))))

(ert-deftest gh-ui-resource-renderers-accept-representative-data ()
  (let* ((context (gh-context-from-repository "o/r"))
         (issue
          '((number . 7) (title . "Issue title") (state . "OPEN")
            (author . ((login . "alice")))
            (assignees . (((login . "bob"))))
            (labels . (((name . "bug"))))
            (milestone . ((title . "v1"))) (body . "Fixes #3")
            (createdAt . "2026-07-01T00:00:00Z")
            (updatedAt . "2026-07-02T00:00:00Z")
            (comments . (((id . "c1") (author . ((login . "bob")))
                          (createdAt . "2026-07-02T00:00:00Z")
                          (body . "A comment"))))))
         (pr
          '((pr . ((number . 8) (title . "PR title") (state . "OPEN")
                   (author . ((login . "alice"))) (headRefName . "topic")
                   (headRefOid . "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa")
                   (baseRefName . "main") (body . "PR body")
                   (statusCheckRollup
                    . (((name . "build") (status . "COMPLETED")
                        (conclusion . "SUCCESS")
                        (detailsUrl . "https://github.com/o/r/actions/runs/42"))))
                   (comments . nil) (reviews . nil)))
            (commits . nil)
            (files . (((filename . "src/a.el") (additions . 2)
                       (deletions . 1))))
            (review-comments . nil)))
         (run
          '((databaseId . 42) (displayTitle . "CI") (name . "CI")
            (workflowName . "CI") (workflowDatabaseId . 2)
            (status . "completed") (conclusion . "success")
            (headBranch . "main")
            (headSha . "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa")
            (event . "push")
            (jobs . (((databaseId . 11) (name . "test")
                      (status . "completed") (conclusion . "success")
                      (steps . (((number . 1) (name . "checkout")
                                 (status . "completed")
                                 (conclusion . "success")))))))))
         (release
          '((tagName . "v1.0") (name . "Version 1")
            (targetCommitish . "main") (author . ((login . "alice")))
            (publishedAt . "2026-07-02T00:00:00Z") (body . "Notes")
            (assets . (((id . 4) (name . "asset.zip") (size . 12)
                        (downloadCount . 3))))))
         (commit
          '((commit . ((sha . "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa")
                       (commit . ((message . "Commit subject")
                                  (author . ((name . "Alice")
                                             (email . "a@example.com")
                                             (date . "2026-07-01T00:00:00Z")))
                                  (committer . ((name . "Alice")
                                                (email . "a@example.com")
                                                (date . "2026-07-01T00:00:00Z")))))
                       (stats . ((additions . 2) (deletions . 1)))
                       (parents . (((sha . "bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb"))))
                       (files . (((filename . "src/a.el") (status . "modified")
                                  (additions . 2) (deletions . 1)
                                  (patch . "@@ -1 +1 @@\n-old\n+new"))))))
            (comments . (((id . 1) (user . ((login . "bob")))
                          (created_at . "2026-07-02T00:00:00Z")
                          (body . "Looks good"))))
            (diff . "diff --git a/src/a.el b/src/a.el\n-old\n+new"))))
    (should (string-match-p "Issue title"
                            (gh-test-render-page
                             'issue 7
                             (lambda (data) (gh-issue--render-view context data))
                             issue)))
    (should (string-match-p "Checks (1)"
                            (gh-test-render-page
                             'pr 8
                             (lambda (data) (gh-pr--render-view context data))
                             pr)))
    (should (string-match-p "checkout"
                            (gh-test-render-page
                             'run 42
                             (lambda (data) (gh-actions--render-run context data))
                             run)))
    (should (string-match-p "asset.zip"
                            (gh-test-render-page
                             'release "v1.0"
                             (lambda (data) (gh-release--render-view context data))
                             release)))
    (should (string-match-p "Changed files (1)"
                            (gh-test-render-page
                             'commit "aaaaaaaa"
                             (lambda (data) (gh-commit--render-view context data))
                             commit)))))

(ert-deftest gh-magit-status-hook-only-starts-async-work-and-renders-loading ()
  (let ((context (gh-context-copy (gh-context-from-repository "o/r")
                                  :branch "main"))
        (gh-magit-status-sections '(pr issue run))
        (gh-hide-forge-duplicates nil)
        (gh-magit-summary-scope 'repository)
        (gh-magit--cache (make-hash-table :test #'equal))
        calls)
    (cl-letf (((symbol-function 'gh-magit--context) (lambda () context))
              ((symbol-function 'gh-api--pr-list)
               (lambda (&rest _) (push 'pr calls) 'request))
              ((symbol-function 'gh-api--issue-list)
               (lambda (&rest _) (push 'issue calls) 'request))
              ((symbol-function 'gh-api--run-list)
               (lambda (&rest _) (push 'run calls) 'request)))
      (with-temp-buffer
        (let ((inhibit-read-only t))
          (magit-insert-section (status)
            (magit-insert-section (recent)
              (magit-insert-heading "Recent commits"))
            (gh-magit-insert-github)))
        (should (string-match-p "loading…" (buffer-string)))
        (should (string-match-p
                 "Recent commits\n\nGitHub\n  loading…" (buffer-string)))
        (should (equal (sort calls
                             (lambda (a b)
                               (string< (symbol-name a) (symbol-name b))))
                       '(issue pr run)))))))

(ert-deftest gh-magit-forge-duplicate-policy-keeps-actions ()
  (let ((gh-hide-forge-duplicates t)
        (gh-magit-status-sections '(pr issue run)))
    (cl-letf (((symbol-function 'featurep)
               (lambda (feature) (eq feature 'forge))))
      (should (equal (gh-magit--effective-sections) '(run))))))

(provide 'gh-ui-test)
;;; gh-ui-test.el ends here
