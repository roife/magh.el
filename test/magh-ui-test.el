;;; magh-ui-test.el --- Native page and Magit integration tests -*- lexical-binding: t; -*-

(require 'magh-test-helper)
(require 'magh)
(require 'magh-magit)

(ert-deftest magh-pr-list-places-review-decision-after-state ()
  (let* ((magh-pr--limit 10)
         (context (magh-context-from-repository "o/r"))
         (data '(((number . 8) (title . "PR title") (state . "OPEN")
                  (reviewDecision . "CHANGES_REQUESTED"))
                 ((number . 9) (title . "No decision") (state . "OPEN"))))
         (text (magh-test-render-page
                'pr-list "open"
                (lambda (items) (magh-pr--render-list context "open" items))
                data)))
    (should (string-match-p
             "OPEN CHANGES_REQUESTED #8 PR title\n" text))
    (should (string-match-p "OPEN #9 No decision\n" text))))

(ert-deftest magh-search-results-load-only-after-confirmation ()
  (let* ((context (magh-context-from-repository "o/r"))
         (resource
          (magh-search--resource
           context 'code
           '((path . "src/a.el")
             (repository . ((nameWithOwner . "o/r")))
             (sha . "blob-sha")
             (url . "https://github.com/o/r/blob/deadbeef/src/a.el"))))
         (candidate (magh-candidate-string "o/r src/a.el match" resource
                                         'magh-search 0))
         opened)
    (dolist (kind '(repos issues prs code commits))
      (setq opened nil)
      (cl-letf (((symbol-function 'consult--async-pipeline)
                 (lambda (&rest _) 'async-collection))
                ((symbol-function 'consult--read)
                 (lambda (_collection &rest options)
                   (should-not (plist-member options :state))
                   (should-not (plist-member options :annotate))
                   (funcall (plist-get options :lookup)
                            (substring-no-properties candidate)
                            (list candidate))))
                ((symbol-function 'magh-resource-open)
                 (lambda (value) (setq opened value))))
        (magh-search--consult context kind nil nil))
      (should (eq opened resource)))
    (should (equal (plist-get opened :ref) "deadbeef"))))














(ert-deftest magh-repository-clone-allows-an-existing-empty-directory ()
  (let ((directory (make-temp-file "magh-clone-test-" t)) captured)
    (unwind-protect
        (cl-letf (((symbol-function 'magh-api--repo-clone)
                   (lambda (_context target _success _error &optional _arguments)
                     (setq captured target))))
          (magh-repository-clone "o/r" directory)
          (should (equal captured directory)))
      (delete-directory directory t))))

(ert-deftest magh-temporary-clone-keeps-its-reserved-directory ()
  (let ((root (make-temp-file "magh-temporary-clone-test-" t)) captured)
    (unwind-protect
        (let ((magh-temporary-clone-directory root))
          (cl-letf (((symbol-function 'magh-api--repo-clone)
                     (lambda (_context target _success _error &optional _arguments)
                       (setq captured target))))
            (magh-repo-clone-temporary
             (magh-context-from-repository "o/r")))
          (should (file-directory-p captured)))
      (delete-directory root t))))





(ert-deftest magh-pr-conversation-review-items-open-the-pr-review ()
  (let* ((magh-date-format-function #'identity)
         (context (magh-context-from-repository "base/repo"))
         (items
          (magh-pr--conversation-items
           '((comments . nil))
           '(((id . 101) (user . ((login . "alice")))
              (submitted_at . "2026-07-01") (state . "APPROVED") (body . ""))
             ((id . 102) (user . ((login . "carol")))
              (submitted_at . "2026-07-02") (state . "COMMENTED") (body . ""))
             ((id . 103) (user . ((login . "dave")))
              (submitted_at . "2026-07-03")
              (state . "CHANGES_REQUESTED") (body . "")))
           '(((id . 2) (pull_request_review_id . 101)
              (user . ((login . "bob"))) (created_at . "2026-06-30")
              (path . "src/a.el") (line . 7) (body . "Inline body")))))
         text)
    (should (= (length items) 2))
    (should (= (length (alist-get 'inlineComments (cdar items))) 1))
    (with-temp-buffer
      (magh-section-mode)
      (setq magh-buffer-context context
            magh-buffer-resource-kind 'conversation
            magh-buffer-resource-id 1)
      (magh-ui--replace
       (lambda (_data) (magh-pr--render-conversation context 7 items)) nil nil)
      (setq text (buffer-string))
      (let* ((conversation (car (oref magit-root-section children)))
             (review (car (oref conversation children))))
        (should (oref review hidden))
        (magit-section-show review)
        (should-not (oref review hidden))
        (should (equal (mapcar (lambda (section) (oref section type))
                               (oref review children))
                       '(inline-comment)))
        (setq text (buffer-string))))
    (should (string-match-p
             "Review by alice APPROVED · 2026-07-01 · 1 inline comment" text))
    (should-not (string-match-p "Review by carol" text))
    (should (string-match-p
             "Review by dave CHANGES_REQUESTED · 2026-07-03" text))
    (dolist (spec '(("Review by alice" :review-id 101)
                    ("Inline comment by bob" :comment-id 2)))
      (let* ((position (string-match (car spec) text))
             (resource (and position
                            (get-text-property position 'magh-resource text))))
        (should resource)
        (should (eq (plist-get resource :kind) 'commit-review))
        (should (= (plist-get resource :number) 7))
        (should (equal (plist-get resource (nth 1 spec)) (nth 2 spec)))))
    (let* ((position (string-match "src/a.el:7" text))
           (resource (and position
                          (get-text-property position 'magh-resource text))))
      (should (equal (plist-get resource :path) "src/a.el"))
      (should (= (plist-get resource :line) 7)))))



(ert-deftest magh-ui-actions-log-groups-repeated-job-and-step-columns ()
  (let ((text
         (magh-actions--simplify-log
          (concat "build\tcheckout\t2026-07-15T12:34:56.1234567Z first\n"
                  "build\tcheckout\t2026-07-15T12:34:57Z second\n"
                  "build\ttest\t2026-07-15T12:35:01Z test\n"
                  "lint\tsetup\tplain line\n"))))
    (should
     (equal (substring-no-properties text)
            (concat "build\n  checkout\n12:34:56 first\n12:34:57 second\n"
                    "  test\n12:35:01 test\n\nlint\n  setup\nplain line\n")))
    (should (eq (get-text-property 0 'font-lock-face text) 'magh-workflow))))



(defun magh-test-render-page (kind id renderer data)
  "Render DATA with RENDERER as a KIND page and return its text."
  (with-temp-buffer
    (magh-section-mode)
    (setq magh-buffer-context (magh-context-from-repository "o/r")
          magh-buffer-resource-kind kind
          magh-buffer-resource-id id)
    (magh-ui--replace renderer data nil)
    (should magit-root-section)
    ;; Reproduce the just-in-time refontification that happens after an
    ;; asynchronous renderer yields back to interactive Emacs.
    (font-lock-flush (point-min) (point-max))
    (font-lock-ensure (point-min) (point-max))
    (buffer-string)))

(ert-deftest magh-status-pages-render-successful-sections-beside-failures ()
  (let* ((context (magh-context-from-repository "o/r"))
         (user-error (magh-core--error 'magh-api-error
                                      "Assigned issues unavailable"))
         (user-result
          (magh-batch-result-create
           :values
           (list
            (cons 'user '((login . "alice") (name . "Alice")
                          (followers . 2) (following . 3)))
            (cons 'notifications nil) (cons 'review-requests nil)
            (cons 'assigned-prs nil) (cons 'my-prs nil)
            (cons 'repositories nil))
           :errors (list (cons 'assigned-issues user-error))))
         (user-text
          (magh-test-render-page
           'user-status 'viewer
           (lambda (data) (magh-pages--render-user-status context data))
           user-result)))
    (should (string-match-p "User: Alice (@alice)" user-text))
    (should (string-match-p "Unavailable: Assigned issues unavailable"
                            user-text))
    (should (string-match-p "Repositories" user-text)))
  (let* ((context (magh-context-from-repository "o/r"))
         (branches-error
          (magh-core--error 'magh-api-error "Branches unavailable"))
         (repo-result
          (magh-batch-result-create
           :values
           (list
            (cons 'repository
                  '((nameWithOwner . "o/r") (visibility . "PUBLIC")
                    (stargazerCount . 1) (forkCount . 2)
                    (watchers . ((totalCount . 3)))))
            (cons 'viewer-forked nil) (cons 'languages nil)
            (cons 'issues nil) (cons 'prs nil) (cons 'runs nil)
            (cons 'commits nil) (cons 'releases nil))
           :errors (list (cons 'branches branches-error))))
         (repo-text
          (magh-test-render-page
           'repository "o/r"
           (lambda (data) (magh-repo--render-status context data))
           repo-result)))
    (should (string-match-p "Repository: o/r" repo-text))
    (should (string-match-p "Unavailable: Branches unavailable" repo-text))
    (should (string-match-p "Releases" repo-text))))

(ert-deftest magh-ui-load-next-page-appends-without-refetching-earlier-items ()
  (with-temp-buffer
    (setq magh-ui--data (magh-page-create :items '(1 2) :next "cursor")
          magh-ui--generation 3)
    (let (received)
      (cl-letf (((symbol-function 'magh-ui--update-data)
                 (lambda (data) (setq magh-ui--data data))))
        (magh-ui--load-next-page
         (lambda (cursor success _error)
           (setq received cursor)
           (funcall success (magh-page-create :items '(3) :next nil)))
         "items"))
      (should (equal received "cursor"))
      (should (equal (magh-page-items magh-ui--data) '(1 2 3)))
      (should-not (magh-page-next magh-ui--data))
      (should-not magh-ui--page-loading))))

(ert-deftest magh-repo-switch-remote-opens-the-selected-context ()
  (let* ((base (magh-context-copy
                (magh-context-from-repository "upstream/widgets")
                :root "/tmp/widgets/" :remote "origin"))
         (fork (magh-context-copy
                (magh-context-from-repository "alice/widgets")
                :root "/tmp/widgets/" :remote "fork"))
         opened)
    (with-temp-buffer
      (setq magh-buffer-context base)
      (cl-letf (((symbol-function 'magh-context-local-remotes)
                 (lambda (_context) `(("origin" . ,base) ("fork" . ,fork))))
                ((symbol-function 'magh-repo-status)
                 (lambda (context) (setq opened context))))
        (magh-repo-switch-remote "fork")))
    (should (eq opened fork))))

(ert-deftest magh-ui-late-generation-cannot-overwrite-newer-page ()
  (let ((magh-display-buffer-function (lambda (buffer) buffer))
        callbacks
        (context (magh-context-from-repository "o/r")))
    (let ((buffer
           (magh-ui--open-page
            " *magh generation test*" context 'repository "o/r"
            (lambda (success error _force)
              (push (cons success error) callbacks))
            (lambda (data) (insert (format "value=%s" data))))))
      (unwind-protect
          (with-current-buffer buffer
            (magh-ui-refresh t)
            (should (= (length callbacks) 2))
            (let ((new (car callbacks)) (old (cadr callbacks)))
              (funcall (car new) "new")
              (funcall (car old) "old"))
            (should (string-match-p "value=new" (buffer-string)))
            (should-not (string-match-p "value=old" (buffer-string))))
        (when (buffer-live-p buffer) (kill-buffer buffer))))))






(ert-deftest magh-ui-visibility-survives-page-recreation-via-magit-cache ()
  (let ((magh-display-buffer-function #'identity)
        (magh-section-cache-visibility t)
        (magh-ui--visibility-cache (make-hash-table :test #'equal))
        (context (magh-context-from-repository "o/r"))
        callback)
    (cl-labels
        ((open-page
          ()
          (magh-ui--open-page
           " *magh visibility test*" context 'issue-list 'open
           (lambda (success _error _force) (setq callback success))
           (lambda (_data)
             (magh-ui--section (items 'items nil t)
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

(ert-deftest magh-ui-markdown-creates-native-reference-buttons ()
  (let ((context (magh-context-from-repository "o/r"))
        (magh-view-inline-images nil))
    (with-temp-buffer
      (magh-ui--insert-markdown
       "Fixes #12 by @octocat in deadbee and https://github.com/o/r/pull/9"
       context)
      (goto-char (point-min))
      (search-forward "#12")
      (should (eq (plist-get (button-get (button-at (1- (point))) 'magh-resource)
                             :kind)
                  'issue))
      (search-forward "@octocat")
      (should (eq (plist-get (button-get (button-at (1- (point))) 'magh-resource)
                             :kind)
                  'user))
      (search-forward "deadbee")
      (should (eq (plist-get (button-get (button-at (1- (point))) 'magh-resource)
                             :kind)
                  'commit)))))
(ert-deftest magh-ui-image-response-skips-header-separator-newline ()
  (let ((target (generate-new-buffer " *magh-image-target*"))
        (response (generate-new-buffer " *magh-image-response*"))
        overlay image-bytes)
    (unwind-protect
        (progn
          (with-current-buffer target
            (insert "placeholder")
            (setq overlay (make-overlay (point-min) (point-max))))
          (with-current-buffer response
            (set-buffer-multibyte nil)
            (insert "HTTP/1.1 200 OK\nContent-Type: image/png\n\n")
            (setq-local url-http-end-of-headers (copy-marker (1- (point))))
            (insert (unibyte-string #x89 ?P ?N ?G))
            (cl-letf (((symbol-function 'create-image)
                       (lambda (data &rest _)
                         (setq image-bytes data)
                         '(image :type png))))
              (magh-ui--image-finished nil target overlay
                                       "https://example.test/image.png")))
          (should (equal image-bytes (unibyte-string #x89 ?P ?N ?G)))
          (should (equal (overlay-get overlay 'display) '(image :type png))))
      (when (buffer-live-p response) (kill-buffer response))
      (when (buffer-live-p target) (kill-buffer target)))))

(ert-deftest magh-ui-resource-renderers-accept-representative-data ()
  (let* ((context (magh-context-from-repository "o/r"))
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
    (let ((text (magh-test-render-page
                 'issue 7
                 (lambda (data) (magh-issue--render-view context data))
                 issue)))
      (should (string-match-p "Issue title" text))
      (should (string-match-p "Description\nFixes #3\n" text))
      (should (string-match-p "Comment by bob" text)))
    (let ((text (magh-test-render-page
                 'pr 8
                 (lambda (data) (magh-pr--render-view context data))
                 pr)))
      (should (string-match-p "Checks (1)" text))
      (should (string-match-p "Description\nPR body\n" text)))
    (should (string-match-p "checkout"
                            (magh-test-render-page
                             'run 42
                             (lambda (data) (magh-actions--render-run context data))
                             run)))
    (let ((text (magh-test-render-page
                 'release "v1.0"
                 (lambda (data) (magh-release--render-view context data))
                 release)))
      (should (string-match-p "asset.zip" text))
      (should (string-match-p "Release notes\nNotes\n" text)))
    (let ((text (magh-test-render-page
                 'commit "aaaaaaaa"
                 (lambda (data) (magh-commit--render-view context data))
                 commit)))
      (should (string-match-p "Changed files (1)" text))
      (should (string-match-p "Commit subject\n" text))
      (should-not (string-match-p "Message\n" text))
      (should (string-match-p "Comment by bob" text)))))

(ert-deftest magh-magit-status-hook-only-starts-async-work-and-renders-loading ()
  (let ((context (magh-context-copy (magh-context-from-repository "o/r")
                                  :branch "main"))
        (magh-magit-status-sections '(pr issue run))
        (magh-hide-forge-duplicates nil)
        (magh-magit-summary-scope 'repository)
        (magh-magit--cache (make-hash-table :test #'equal))
        calls)
    (cl-letf (((symbol-function 'magh-magit--context) (lambda () context))
              ((symbol-function 'magh-api--pr-list)
               (lambda (&rest _) (push 'pr calls) 'request))
              ((symbol-function 'magh-api--issue-list)
               (lambda (&rest _) (push 'issue calls) 'request))
              ((symbol-function 'magh-api--run-list)
               (lambda (&rest _) (push 'run calls) 'request)))
      (with-temp-buffer
        (let ((inhibit-read-only t))
          (magit-insert-section (status)
            (magit-insert-section (recent)
              (magit-insert-heading "Recent commits"))
            (magh-magit-insert-github)))
        (should (string-match-p "loading…" (buffer-string)))
        (should (string-match-p
                 "Recent commits\n\nGitHub\n  loading…" (buffer-string)))
        (should (equal (sort calls
                             (lambda (a b)
                               (string< (symbol-name a) (symbol-name b))))
                       '(issue pr run)))))))




(ert-deftest magh-commit-review-patch-parser-maps-lines-and-sides ()
  (let* ((context (magh-context-from-repository "o/r"))
         (records
          (magh-commit--parse-patch-lines
           (concat "@@ -2,3 +2,3 @@\n"
                   " context\n-old\n+new\n"
                   "@@ -10 +10,2 @@\n+next\n context2")
           context 7 "HEAD" "src/a.el"))
         (resources (delq nil (mapcar (lambda (record)
                                        (plist-get record :resource))
                                      records))))
    (should (equal (mapcar (lambda (resource)
                             (list (plist-get resource :line)
                                   (plist-get resource :side)
                                   (plist-get resource :hunk)
                                   (plist-get resource :position)))
                           resources)
                   '((2 "RIGHT" 1 1)
                     (3 "LEFT" 1 2)
                     (3 "RIGHT" 1 3)
                     (10 "RIGHT" 2 5)
                     (11 "RIGHT" 2 6))))))

(ert-deftest magh-commit-review-selection-builds-multiline-location ()
  (let ((context (magh-context-from-repository "o/r"))
        (transient-mark-mode t))
    (with-temp-buffer
      (dolist (line '(4 5))
        (let ((start (point))
              (resource (magh-resource-create
                         'review-line context :path "src/a.el" :line line
                         :side "RIGHT" :hunk 1)))
          (insert "+code\n")
          (add-text-properties start (point) (list 'magh-resource resource))))
      (goto-char (point-min))
      (set-mark (point-max))
      (setq mark-active t)
      (should (equal (magh-commit--review-selection)
                     '(:path "src/a.el" :line 5 :side "RIGHT"
                       :subject-type "LINE" :start-line 4
                       :start-side "RIGHT"))))))

(ert-deftest magh-commit-review-selection-rejects-mixed-sides ()
  (let ((context (magh-context-from-repository "o/r"))
        (transient-mark-mode t))
    (with-temp-buffer
      (dolist (spec '((4 "LEFT") (4 "RIGHT")))
        (let ((start (point))
              (resource (magh-resource-create
                         'review-line context :path "src/a.el"
                         :line (car spec) :side (cadr spec) :hunk 1)))
          (insert "diff\n")
          (add-text-properties start (point) (list 'magh-resource resource))))
      (goto-char (point-min))
      (set-mark (point-max))
      (setq mark-active t)
      (should-error (magh-commit--review-selection) :type 'user-error))))



(ert-deftest magh-commit-full-diff-groups-files-and-hunks ()
  (let* ((diff (concat
                "diff --git a/a.el b/a.el\n"
                "index 1111111..2222222 100644\n"
                "--- a/a.el\n+++ b/a.el\n"
                "@@ -1 +1 @@\n-old\n+new\n"
                "@@ -10,0 +11 @@\n+later\n"
                "diff --git a/b.el b/b.el\n"
                "new file mode 100644\n"
                "--- /dev/null\n+++ b/b.el\n"
                "@@ -0,0 +1 @@\n+hello\n"))
         (files (magh-commit--split-full-diff diff)))
    (should (= (length files) 2))
    (should (equal (mapcar (lambda (file) (plist-get file :heading)) files)
                   '("diff --git a/a.el b/a.el"
                     "diff --git a/b.el b/b.el")))
    (should (string-match-p "index 1111111" (plist-get (car files) :preamble)))
    (should (= (length (plist-get (car files) :hunks)) 2))
    (should (= (length (plist-get (cadr files) :hunks)) 1))
    (should (string-match-p "+later"
                            (plist-get (cadr (plist-get (car files) :hunks))
                                       :body)))))




(ert-deftest magh-commit-review-submit-failure-preserves-local-drafts ()
  (let* ((context (magh-context-from-repository "o/r"))
         (magh-commit--review-drafts (make-hash-table :test #'equal))
         (key (magh-commit--review-key context 7 "HEAD"))
         (draft '(:id 1 :path "src/a.el" :line 1 :side "RIGHT"
                  :subject-type "LINE" :body "Keep me"))
         reported)
    (puthash key (list draft) magh-commit--review-drafts)
    (cl-letf (((symbol-function 'magh-api--pr-review)
               (lambda (_context _number _event _body _comments
                        _callback errback &optional _head)
                 (funcall errback
                          (magh-core--error 'magh-api-error "Review failed"))))
              ((symbol-function 'message)
               (lambda (format-string &rest arguments)
                 (setq reported (apply #'format format-string arguments)))))
      (let ((magh-buffer-context context)
            (magh-commit--review-number 7)
            (magh-commit--review-head "HEAD"))
        (magh-commit-review-submit 'approve "")))
    (should (equal reported "magh: Review failed"))
    (should (equal (gethash key magh-commit--review-drafts) (list draft)))))


(provide 'magh-ui-test)
;;; magh-ui-test.el ends here
