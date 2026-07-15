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
                (cdr mapping)))))

(ert-deftest gh-ui-semantic-row-has-no-fixed-width-padding ()
  (let* ((gh-date-format-function #'identity)
         (data '((number . 7) (title . "Issue title") (state . "OPEN")
                 (author . ((login . "alice")))
                 (updatedAt . "2026-07-02T00:00:00Z")))
         (row (gh-ui--format-row (gh-issue--row-values data))))
    (should (equal row
                   "OPEN  #7  Issue title  alice  2026-07-02T00:00:00Z"))
    (dolist (expected '(("OPEN" . gh-open-state)
                        ("#7" . gh-resource-number)
                        ("Issue title" . gh-resource-title)
                        ("alice" . gh-author)
                        ("2026-07-02T00:00:00Z" . gh-date)))
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
             "OPEN  #7  Issue title  alice  2026-07-02T00:00:00Z\n" text))
    (dolist (expected '(("OPEN" . gh-open-state)
                        ("#7" . gh-resource-number)
                        ("Issue title" . gh-resource-title)
                        ("alice" . gh-author)))
      (let ((position (string-match (regexp-quote (car expected)) text)))
        (should position)
        (should (eq (get-text-property position 'font-lock-face text)
                    (cdr expected)))))))

(ert-deftest gh-ui-repository-status-rows-follow-ui-layout ()
  (let* ((case-fold-search nil)
         (gh-date-format-function #'identity)
         (context (gh-context-from-repository "o/r"))
         (result
          '((repository . ((nameWithOwner . "o/r") (visibility . "PUBLIC")))
            (languages)
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
            (releases . (((tagName . "v1.0") (name . "Version 1"))))))
         (text (gh-test-render-page
                'repository "o/r"
                (lambda (data) (gh-repo--render-status context data)) result)))
    (should (string-match-p
             (regexp-quote
              "OPEN  #8  PR title  alice  REVIEW_REQUIRED  2026-07-02T00:00:00Z\n")
             text))
    (should (string-match-p
             (regexp-quote
              "SUCCESS  Run title  CI  2026-07-04T00:00:00Z\n")
             text))
    (should-not (string-match-p "#42.*Run title" text))
    (dolist (expected '(("#8" . gh-resource-number)
                        ("PR title" . gh-resource-title)
                        ("alice" . gh-author)
                        ("REVIEW_REQUIRED" . gh-pending-state)
                        ("Run title" . gh-resource-title)
                        ("CI" . gh-workflow)))
      (let ((position (string-match (regexp-quote (car expected)) text)))
        (should position)
        (should (eq (get-text-property position 'font-lock-face text)
                    (cdr expected)))))))

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
            (gh-magit-insert-github)))
        (should (string-match-p "loading…" (buffer-string)))
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
