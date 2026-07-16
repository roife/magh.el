;;; magh-ui-test.el --- Native page and Magit integration tests -*- lexical-binding: t; -*-

(require 'magh-test-helper)
(require 'magh)
(require 'magh-magit)

(defun magh-test-font-lock-face-p (text position face)
  "Return non-nil when TEXT at POSITION carries FACE."
  (let ((value (get-text-property position 'font-lock-face text)))
    (if (listp value) (memq face value) (eq value face))))

(ert-deftest magh-ui-faces-inherit-loaded-magit-faces ()
  (should (featurep 'magit))
  (should (featurep 'magit-diff))
  (should (featurep 'magit-log))
  (should (featurep 'magit-process))
  (dolist (mapping '((magh-section-heading . magit-section-heading)
                     (magh-resource-number . magit-refname-pullreq)
                     (magh-resource-title . magit-section-secondary-heading)
                     (magh-conversation-kind . magit-section-secondary-heading)
                     (magh-repository . magit-branch-remote)
                     (magh-branch . magit-branch-local)
                     (magh-author . magit-log-author)
                     (magh-date . magit-log-date)
                     (magh-tag . magit-tag)
                     (magh-hash . magit-hash)
                     (magh-workflow . magit-refname)
                     (magh-file . magit-filename)
                     (magh-label . magit-keyword)
                     (magh-permission . magit-dimmed)
                     (magh-added . magit-diffstat-added)
                     (magh-removed . magit-diffstat-removed)
                     (magh-open-state . magit-process-ok)
                     (magh-pending-state . magit-branch-warning)
                     (magh-draft-state . magit-dimmed)
                     (magh-closed-state . magit-process-ng)
                     (magh-metadata-key . magit-header-line-key)
                     (magh-loading . magit-dimmed)
                     (magh-error . magit-process-ng)))
    (should (facep (car mapping)))
    (should (facep (cdr mapping)))
    (should (eq (face-attribute (car mapping) :inherit nil nil)
                (cdr mapping))))
  (should (eq (face-attribute 'magh-conversation-kind :weight nil nil) 'bold))
  (should (facep 'magh-inline-comment))
  (should (eq (face-attribute 'magh-inline-comment :extend nil nil) t)))

(ert-deftest magh-ui-semantic-row-has-no-fixed-width-padding ()
  (let* ((magh-date-format-function #'identity)
         (data '((number . 7) (title . "Issue title") (state . "OPEN")
                 (author . ((login . "alice")))
                 (updatedAt . "2026-07-02T00:00:00Z")))
         (row (magh-ui--format-row (magh-issue--row-values data))))
    (should (equal row "OPEN #7 Issue title"))
    (dolist (expected '(("OPEN" . magh-open-state)
                        ("#7" . magh-resource-number)
                        ("Issue title" . magh-resource-title)))
      (let ((position (string-match (regexp-quote (car expected)) row)))
        (should position)
        (should (eq (get-text-property position 'font-lock-face row)
                    (cdr expected)))))))

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

(ert-deftest magh-search-marginalia-owns-search-annotations ()
  (let* ((context (magh-context-from-repository "o/r"))
         (data '((fullName . "o/r") (visibility . "PUBLIC")
                 (stargazersCount . 12) (description . "Repository summary")))
         (resource (magh-search--resource context 'repos data))
         (display (magh-search--format resource))
         (candidate (magh-candidate-string display resource 'magh-search 0))
         (annotation (magh-search--marginalia-annotate candidate))
         (annotation-text (substring-no-properties annotation)))
    (should (equal (substring-no-properties display) "o/r"))
    (should (eq (get-text-property 0 'face display) 'magh-repository))
    (should-not (string-match-p "12\\|Repository summary" display))
    (should (string-match-p "★12" annotation-text))
    (should (string-match-p "Repository summary" annotation-text))
    (should-not (string-match-p "public" annotation-text))
    (dolist (expected '(("★12" . marginalia-number)
                        ("Repository summary" . marginalia-documentation)))
      (let ((position (string-match (regexp-quote (car expected)) annotation)))
        (should position)
        (should (eq (get-text-property position 'face annotation)
                    (cdr expected)))))
    (should (equal (car (alist-get 'magh-search marginalia-annotators))
                   'magh-search--marginalia-annotate))))

(ert-deftest magh-search-marginalia-columns-use-semantic-colors ()
  (let* ((context (magh-context-from-repository "o/r"))
         (issue
          (magh-search--resource
           context 'issues
           '((number . 7) (title . "Issue") (state . "OPEN")
             (author . ((login . "alice")))
             (repository . ((nameWithOwner . "o/r"))))))
         (issue-candidate
          (magh-candidate-string (magh-search--format issue) issue 'magh-search 0))
         (issue-annotation (magh-search--marginalia-annotate issue-candidate))
         (file
          (magh-search--resource
           context 'code
           '((path . "src/a.el")
             (repository . ((nameWithOwner . "o/r")))
             (sha . "blob-sha")
             (textMatches . (((fragment . "matched code"))))
             (url . "https://github.com/o/r/blob/deadbeef/src/a.el"))))
         (file-candidate
          (magh-candidate-string (magh-search--format file) file 'magh-search 0))
         (file-display (magh-search--format file))
         (file-annotation (magh-search--marginalia-annotate file-candidate)))
    (should (eq (get-text-property 0 'face file-display) 'magh-file))
    (dolist (case `((,issue-annotation "o/r" magh-repository)
                    (,issue-annotation "OPEN" magh-open-state)
                    (,issue-annotation "alice" magh-author)
                    (,file-annotation "o/r" magh-repository)
                    (,file-annotation "matched code" font-lock-string-face)))
      (pcase-let ((`(,annotation ,text ,face) case))
      (let ((position (string-match (regexp-quote text) annotation)))
          (should position)
          (should (eq (get-text-property position 'face annotation) face)))))))

(ert-deftest magh-search-repository-list-resources-have-native-actions ()
  (let* ((context (magh-context-from-repository "o/r"))
         (run (magh-search--resource
               context 'actions
               '((databaseId . 42) (displayTitle . "Build")
                 (conclusion . "SUCCESS") (workflowName . "CI")
                 (headBranch . "main") (event . "push"))))
         (release (magh-search--resource
                   context 'releases
                   '((tagName . "v1.0") (name . "Version 1"))))
         (branch (magh-search--resource
                  context 'branches
                  '((name . "topic")
                    (commit . ((sha . "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa")))))))
    (should (eq (plist-get run :kind) 'run))
    (should (equal (substring-no-properties (magh-search--format run))
                   "SUCCESS Build CI main"))
    (should (eq (plist-get release :kind) 'release))
    (should (equal (substring-no-properties (magh-search--format release))
                   "PUBLISHED v1.0 Version 1"))
    (should (eq (plist-get branch :kind) 'branch))
    (should (equal (magh-context-ref (plist-get branch :context)) "topic"))
    (should (equal (substring-no-properties (magh-search--format branch))
                   "topic"))))

(ert-deftest magh-repository-consult-search-routes-server-and-list-kinds ()
  (let ((context (magh-context-from-repository "o/r")) calls)
    (cl-letf (((symbol-function 'magh-consult-search)
               (lambda (kind actual-context initial options)
                 (push (list 'server kind actual-context initial options) calls)))
              ((symbol-function 'magh-search--consult-repository-list)
               (lambda (actual-context kind initial)
                 (push (list 'list kind actual-context initial) calls))))
      (dolist (kind '(issues prs code commits actions releases branches))
        (magh-repository-consult-search kind context "seed")))
    (dolist (kind '(issues prs code commits))
      (let ((call (seq-find (lambda (item)
                              (and (eq (car item) 'server)
                                   (eq (cadr item) kind)))
                            calls)))
        (should call)
        (should (equal (nth 3 call) "seed"))
        (should (equal (nth 4 call) '(:repo "o/r")))))
    (dolist (kind '(actions releases branches))
      (should (seq-find (lambda (item)
                          (and (eq (car item) 'list)
                               (eq (cadr item) kind)
                               (equal (nth 3 item) "seed")))
                        calls)))))

(ert-deftest magh-repository-consult-list-shows-async-indicator-and-opens ()
  (let ((context (magh-context-from-repository "o/r")) opened sessions)
    (cl-letf (((symbol-function 'magh-api--run-list)
               (lambda (_context params success _error &optional _force)
                 (should (equal params (list :limit magh-list-limit)))
                 (magh-core--call-later
                  success '(((databaseId . 42) (displayTitle . "Build"))))
                 'run-request))
              ((symbol-function 'magh-api--release-list)
               (lambda (_context success _error &optional _force)
                 (magh-core--call-later success '(((tagName . "v1.0"))))
                 'release-request))
              ((symbol-function 'magh-api--repo-branches)
               (lambda (_context success _error &optional _force)
                 (magh-core--call-later success '(((name . "topic"))))
                 'branch-request))
              ((symbol-function 'consult--read)
               (lambda (collection &rest options)
                 (let (events)
                   (let ((backend
                          (funcall collection
                                   (lambda (event) (push event events)))))
                     (funcall backend 'setup)
                     (funcall backend "seed"))
                   (should (member '[indicator running] events))
                   (magh-test-wait
                    (lambda () (member '[indicator finished] events)))
                   (should (member '[indicator finished] events))
                   (should (memq 'refresh events))
                   (let ((candidates
                          (seq-find (lambda (event)
                                      (and (consp event)
                                           (stringp (car event))))
                                    events)))
                     (push (list options events) sessions)
                     (car candidates)))))
              ((symbol-function 'magh-resource-open)
               (lambda (resource) (push resource opened))))
      (dolist (kind '(actions releases branches))
        (magh-search--consult-repository-list context kind "seed")))
    (should (equal (mapcar (lambda (resource) (plist-get resource :kind))
                           (nreverse opened))
                   '(run release branch)))
    (dolist (session sessions)
      (let ((options (car session)))
        (should (equal (plist-get options :category) 'magh-search))
        (should (equal (plist-get options :initial) "seed"))
        (should (eq (plist-get options :lookup) #'consult--lookup-member))))))

(ert-deftest magh-ui-magit-heading-preserves-row-faces ()
  (let* ((case-fold-search nil)
         (magh-date-format-function #'identity)
         (magh-issue--limit 10)
         (data '(((number . 7) (title . "Issue title") (state . "OPEN")
                  (author . ((login . "alice")))
                  (updatedAt . "2026-07-02T00:00:00Z"))))
         (text (magh-test-render-page
                'issue-list "open"
                (lambda (items)
                  (magh-issue--render-list
                   (magh-context-from-repository "o/r") "open" items))
                data)))
    (should (string-match-p
             "OPEN #7 Issue title\n" text))
    (should (string-match-p
             "OPEN #7 Issue title\n\nLoad more" text))
    (dolist (expected '(("OPEN" . magh-open-state)
                        ("#7" . magh-resource-number)
                        ("Issue title" . magh-resource-title)))
      (let ((position (string-match (regexp-quote (car expected)) text)))
        (should position)
        (should (eq (get-text-property position 'font-lock-face text)
                    (cdr expected)))))))

(ert-deftest magh-ui-repository-status-rows-follow-ui-layout ()
  (let* ((case-fold-search nil)
         (magh-date-format-function #'identity)
         (context (magh-context-from-repository "o/r"))
         (result
          '((repository . ((nameWithOwner . "o/r") (visibility . "PUBLIC")
                           (defaultBranchRef . ((name . "main")))
                           (viewerHasStarred . t)
                           (viewerSubscription . "SUBSCRIBED")
                           (stargazerCount . 12) (forkCount . 3)
                           (watchers . ((totalCount . 4)))))
            (viewer-forked . t)
            (languages . ((Emacs\ Lisp . 75) (Shell . 25)))
            (branches . (((name . "main")
                          (protected . t)
                          (commit . ((sha . "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"))))
                         ((name . "topic")
                          (protected . :json-false)
                          (commit . ((sha . "bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb"))))))
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
         (text (magh-test-render-page
                'repository "o/r"
                (lambda (data) (magh-repo--render-status context data)) result)))
    (should (string-match-p
             (regexp-quote
              "OPEN #8 PR title REVIEW_REQUIRED\n")
             text))
    (should (string-match-p
             (regexp-quote
              "SUCCESS Run title CI\n")
             text))
    (should (string-match-p
             (regexp-quote
              "Stats: 12 stars (starred), 3 forks (forked), 4 watchers (watching)\n")
             text))
    (should-not (string-match-p "Viewer status:" text))
    (should (string-match-p "Branch: main\n" text))
    (should (string-match-p
             (regexp-quote "Branches\n* main\ntopic\n") text))
    (let ((position (string-match "topic" text)))
      (should position)
      (should (eq (lookup-key (get-text-property position 'keymap text)
                              (kbd "<mouse-1>"))
                  'magh-repo-branch-click)))
    (should (string-match-p "Recent commits\n" text))
    (should (string-match-p
             (regexp-quote
              "Description\nNo description.\n\nStatistics\n\nBranches\n")
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
    (dolist (expected '(("#8" . magh-resource-number)
                        ("PR title" . magh-resource-title)
                        ("REVIEW_REQUIRED" . magh-pending-state)
                        ("Run title" . magh-resource-title)
                        ("CI" . magh-workflow)))
      (let ((position (string-match (regexp-quote (car expected)) text)))
        (should position)
        (should (eq (get-text-property position 'font-lock-face text)
                    (cdr expected)))))))

(ert-deftest magh-ui-repository-branch-resource-switches-status-ref ()
  (let* ((context (magh-context-from-repository "o/r"))
         (resource (magh-repo--branch-resource
                    context '((name . "topic"))))
         opened)
    (should (eq (plist-get resource :kind) 'branch))
    (should (equal (magh-context-ref (plist-get resource :context)) "topic"))
    (should-not (magh-context-branch (plist-get resource :context)))
    (cl-letf (((symbol-function 'magh-repo-status)
               (lambda (branch-context) (setq opened branch-context))))
      (magh-resource-open resource))
    (should (equal (magh-context-ref opened) "topic"))))

(ert-deftest magh-ui-list-metadata-is-inside-expanded-details ()
  (let ((context (magh-context-from-repository "o/r"))
        (magh-date-format-function #'identity))
    (dolist
        (case
         `((,(lambda ()
               (magh-issue--insert-row
                context
                '((number . 7) (title . "Issue") (state . "OPEN")
                  (author . ((login . "alice")))
                  (createdAt . "created") (updatedAt . "updated"))))
            "OPEN #7 Issue\n" "Author: alice\n" "Updated: updated\n")
           (,(lambda ()
               (magh-pr--insert-row
                context
                '((number . 8) (title . "PR") (state . "OPEN")
                  (author . ((login . "bob")))
                  (createdAt . "created") (updatedAt . "updated"))))
            "OPEN #8 PR\n" "Author: bob\n" "Updated: updated\n")
           (,(lambda ()
               (magh-actions--insert-run
                context
                '((databaseId . 9) (displayTitle . "CI")
                  (status . "queued") (workflowName . "Build")
                  (createdAt . "created"))))
            "QUEUED CI Build\n" "Created: created\n")))
      (with-temp-buffer
        (magh-section-mode)
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

(ert-deftest magh-ui-repository-permission-and-date-are-expanded-details ()
  (let ((context (magh-context-from-repository "o/r"))
        (magh-date-format-function #'identity)
        (result
         '((user . ((login . "me") (followers . 0) (following . 0)))
           (repositories . (((nameWithOwner . "o/r")
                             (visibility . "PUBLIC")
                             (viewerPermission . "ADMIN")
                             (updatedAt . "updated")))))))
    (with-temp-buffer
      (magh-section-mode)
      (setq magh-buffer-context context
            magh-buffer-resource-kind 'user-status
            magh-buffer-resource-id 'viewer)
      (magh-ui--replace
       (lambda (data) (magh-pages--render-user-status context data)) result nil)
      (let* ((repositories
              (seq-find (lambda (section)
                          (eq (oref section type) 'repositories))
                        (oref magit-root-section children)))
             (repository (car (oref repositories children))))
        (dolist (heading '("Notifications" "Review requests" "Assigned issues"
                           "Assigned pull requests" "My pull requests"
                           "Repositories"))
          (should (string-match-p (concat (regexp-quote heading) "\n")
                                  (buffer-string)))
          (should-not (string-match-p (concat (regexp-quote heading) " (")
                                      (buffer-string))))
        (should (string-match-p "public o/r\n" (buffer-string)))
        (should-not (string-match-p "ADMIN\\|Updated: updated" (buffer-string)))
        (let ((inhibit-read-only t))
          (magit-section-show repository))
        (should (string-match-p "Permission: ADMIN\n" (buffer-string)))
        (should (string-match-p "Updated: updated\n" (buffer-string)))))))

(ert-deftest magh-ui-language-statistics-only-show-percentages ()
  (with-temp-buffer
    (magh-repo--insert-languages '((Emacs\ Lisp . 75) (Shell . 25)))
    (should (equal (buffer-string)
                   "Emacs Lisp: 75.0%\nShell: 25.0%\n"))
    (should-not (string-match-p "bytes\\|Size:" (buffer-string)))))

(ert-deftest magh-ui-repository-stats-omit-negative-viewer-states ()
  (should
   (equal (magh-repo--stats
           '((stargazerCount . 12) (forkCount . 3)
             (watchers . ((totalCount . 4)))
             (viewerHasStarred . nil) (viewerSubscription . "IGNORED"))
           nil)
          "12 stars, 3 forks, 4 watchers")))

(ert-deftest magh-ui-comment-sections-have-a-blank-line-between-siblings ()
  (with-temp-buffer
    (magit-section-mode)
    (let ((inhibit-read-only t))
      (magit-insert-section (root)
        (magh-ui--section (conversation 'conversation nil nil)
          "Conversation"
          (magh-ui--section (comment 1 nil nil) "First comment")
          (magh-ui--section (comment 2 nil nil) "Second comment"))))
    (should (string-match-p
             "First comment\n\nSecond comment\n" (buffer-string)))))

(ert-deftest magh-ui-conversation-kind-labels-use-bold-highlight-face ()
  (let* ((magh-date-format-function #'identity)
         (context (magh-context-from-repository "o/r"))
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
          (magh-test-render-page
           'conversation 1
           (lambda (_)
             (magh-pr--render-conversation context 1 items))
           nil)))
    (dolist (label '("Comment" "Review" "Inline comment"))
      (let ((position (string-match (regexp-quote label) text)))
        (should position)
        (should (magh-test-font-lock-face-p
                 text position 'magh-conversation-kind))))))

(ert-deftest magh-ui-comment-kind-labels-use-conversation-face ()
  (let* ((magh-date-format-function #'identity)
         (context (magh-context-from-repository "o/r"))
         (comment '((id . 1) (user . ((login . "alice")))
                    (created_at . "2026-07-01") (body . "Comment body")))
         (issue-comment
          '((id . "I1") (author . ((login . "alice")))
            (createdAt . "2026-07-01") (body . "Issue comment")))
         (files-result
          '((pr . ((headRefOid . "HEAD")))
            (files . (((filename . "a.el") (additions . 1) (deletions . 1)
                       (patch . "@@ -1 +1 @@\n-old\n+new"))))
            (review-comments
             . (((id . 3) (path . "a.el") (line . 1)
                 (user . ((login . "alice"))) (body . "Inline body"))))))
         (entries
          `(("Comment"
             . ,(magh-test-render-page
                 'issue 1
                 (lambda (_) (magh-issue--render-comment context issue-comment))
                 nil))
            ("Comment"
             . ,(magh-test-render-page
                 'commit "aaaaaaaa"
                 (lambda (_)
                   (magh-commit--insert-commit-comment
                    context "aaaaaaaa" comment))
                 nil))
            ("Inline comment"
             . ,(magh-test-render-page
                 'commit "aaaaaaaa"
                 (lambda (_)
                   (magh-commit--insert-commit-comment
                    context "aaaaaaaa" comment t))
                 nil))
            ("Comment"
             . ,(magh-test-render-page
                 'commit-review 1
                 (lambda (_)
                   (magh-commit--insert-review-comment context comment))
                 nil))
            ("Reply"
             . ,(magh-test-render-page
                 'commit-review 1
                 (lambda (_)
                   (magh-commit--insert-review-comment context comment t))
                 nil))
            ("Inline comment"
             . ,(magh-test-render-page
                 'pr 1
                 (lambda (data) (magh-pr--render-files context 1 data))
                 files-result)))))
    (dolist (entry entries)
      (let* ((label (car entry))
             (text (cdr entry))
             (position (string-match (concat label " by alice") text)))
        (should position)
        (should (magh-test-font-lock-face-p
                 text position 'magh-conversation-kind))
        (should-not (magh-test-font-lock-face-p
                     text (+ position (length label))
                     'magh-conversation-kind))))))

(ert-deftest magh-ui-inline-comment-blocks-use-background-face ()
  (let* ((magh-date-format-function #'identity)
         (context (magh-context-from-repository "o/r"))
         (comment '((id . 1) (path . "a.el") (line . 7) (position . 2)
                    (user . ((login . "alice")))
                    (created_at . "2026-07-01") (body . "Commit inline body")))
         (commit-inline
          (magh-test-render-page
           'commit "HEAD"
           (lambda (_)
             (magh-commit--insert-commit-comment context "HEAD" comment t))
           nil))
         (commit-general
          (magh-test-render-page
           'commit "HEAD"
           (lambda (_)
             (magh-commit--insert-commit-comment context "HEAD" comment))
           nil))
         (pr-inline
          (magh-test-render-page
           'pr 1
           (lambda (_)
             (magh-pr--render-conversation
              context 1
              '((inline . ((id . 2) (path . "a.el") (line . 7)
                           (user . ((login . "bob")))
                           (created_at . "2026-07-01")
                           (body . "PR inline body"))))))
           nil)))
    (cl-labels ((has-inline-face-p
                 (needle text)
                 (let* ((position (string-match (regexp-quote needle) text))
                        (face (and position
                                   (get-text-property
                                    position 'font-lock-face text))))
                   (should position)
                   (if (listp face)
                       (memq 'magh-inline-comment face)
                     (eq face 'magh-inline-comment)))))
      (dolist (spec `((,commit-inline "Inline comment" "Location" "Commit inline body")
                      (,pr-inline "Inline comment" "Location" "PR inline body")))
        (dolist (needle (cdr spec))
          (should (has-inline-face-p needle (car spec)))))
      (should-not (has-inline-face-p "Comment" commit-general))
      (should-not (has-inline-face-p "Commit inline body" commit-general)))))

(ert-deftest magh-pr-conversation-review-items-open-the-pr-review ()
  (let* ((magh-date-format-function #'identity)
         (context (magh-context-from-repository "base/repo"))
         (head-oid "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa")
         (items
          `((review . ((id . "r1") (author . ((login . "alice")))
                       (submittedAt . "submitted") (state . "APPROVED")
                       (commit . ((oid . ,head-oid))) (body . "")))
            (inline . ((id . 2) (user . ((login . "bob")))
                       (created_at . "created") (path . "src/a.el")
                       (line . 7) (body . "Inline body")))))
         (text
          (magh-test-render-page
           'conversation 1
           (lambda (_)
             (magh-pr--render-conversation
              context 7 items))
           nil)))
    (should-not (string-match-p "Author: alice\n" text))
    (should-not (string-match-p "Commit: " text))
    (dolist (spec '(("Review by alice" :review-id "r1")
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

(ert-deftest magh-pr-files-open-the-pr-review ()
  (let* ((context (magh-context-from-repository "o/r"))
         (result
          '((pr . ((number . 7) (title . "Review me") (state . "OPEN")
                   (headRefName . "topic") (baseRefName . "main")
                   (comments . nil) (reviews . nil)))
            (commits . nil)
            (files . (((filename . "src/a.el")
                       (additions . 2) (deletions . 1))))
            (review-comments . nil)))
         (text
          (magh-test-render-page
           'pr 7
           (lambda (data) (magh-pr--render-view context data))
           result)))
    (dolist (spec '(("Files (1)" nil)
                    ("src/a.el" "src/a.el")))
      (let* ((position (string-match (car spec) text))
             (resource (and position
                            (get-text-property position 'magh-resource text))))
        (should resource)
        (should (eq (plist-get resource :kind) 'commit-review))
        (should (= (plist-get resource :number) 7))
        (should (equal (plist-get resource :path) (cadr spec)))))))

(ert-deftest magh-ui-prose-sections-enable-visual-line-mode ()
  (with-temp-buffer
    (magh-section-mode)
    (should-not visual-line-mode)
    (let ((inhibit-read-only t))
      (magit-insert-section (root)
        (magh-ui--section (description 'description nil nil)
          "One-line description")))
    (should visual-line-mode))
  (with-temp-buffer
    (should-not visual-line-mode)
    (magh-ui--insert-markdown "Comment body")
    (should visual-line-mode)))

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

(ert-deftest magh-ui-description-has-an-explicit-heading ()
  (let* ((context (magh-context-from-repository "o/r"))
         (text
          (magh-test-render-page
           'issue 1
           (lambda (data) (magh-issue--render-view context data))
           '((number . 1) (title . "Issue") (state . "OPEN")
             (body . "Summary\n\nBody text")))))
    (should (string-match-p "#1 Issue\n" text))
    (should (string-match-p "Description\n" text))
    (should (string-match-p "Summary\n\nBody text\n" text))
    (let ((position 0) (count 0))
      (while (string-match "Summary" text position)
        (setq count (1+ count) position (match-end 0)))
      (should (= count 1)))))

(ert-deftest magh-ui-diff-preserves-added-and-removed-line-faces ()
  (with-temp-buffer
    (magh-ui--insert-diff
     "diff --git a/a.el b/a.el\n--- a/a.el\n+++ b/a.el\n@@ -1 +1 @@\n-old\n+new\n")
    (dolist (expected '(("-old" . diff-removed) ("+new" . diff-added)))
      (goto-char (point-min))
      (search-forward (car expected))
      (let ((face (get-text-property (1- (point)) 'font-lock-face)))
        (should (if (listp face)
                    (memq (cdr expected) face)
                  (eq face (cdr expected))))))))

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

(ert-deftest magh-ui-markdown-normalizes-carriage-return-line-endings ()
  (let ((magh-view-inline-images nil))
    (with-temp-buffer
      (magh-ui--insert-markdown "First\r\nSecond\rThird")
      (should (equal (buffer-substring-no-properties (point-min) (point-max))
                     "First\nSecond\nThird\n"))
      (should-not (string-match-p "\r" (buffer-string))))))

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

(ert-deftest magh-magit-forge-duplicate-policy-keeps-actions ()
  (let ((magh-hide-forge-duplicates t)
        (magh-magit-status-sections '(pr issue run)))
    (cl-letf (((symbol-function 'featurep)
               (lambda (feature) (eq feature 'forge))))
      (should (equal (magh-magit--effective-sections) '(run))))))

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

(ert-deftest magh-commit-inline-selection-anchors-region-to-final-position ()
  (let ((context (magh-context-from-repository "o/r"))
        (transient-mark-mode t))
    (with-temp-buffer
      (dolist (spec '((3 "LEFT" 2) (3 "RIGHT" 3)))
        (let ((start (point))
              (resource
               (magh-resource-create
                'commit-line context :path "src/a.el" :line (nth 0 spec)
                :side (nth 1 spec) :position (nth 2 spec) :hunk 1)))
          (insert "diff\n")
          (add-text-properties start (point) (list 'magh-resource resource))))
      (goto-char (point-min))
      (set-mark (point-max))
      (setq mark-active t)
      (should (equal (magh-commit--commit-selection)
                     '(:path "src/a.el" :position 3 :line 3))))))

(ert-deftest magh-commit-renderer-places-positioned-comments-inline ()
  (let* ((context (magh-context-from-repository "o/r"))
         (data
          '((commit . ((sha . "HEAD")
                       (commit . ((message . "Commit subject")
                                  (author . ((name . "Alice")
                                             (email . "a@example.com")))
                                  (committer . ((name . "Alice")
                                                (email . "a@example.com")))))
                       (stats . ((additions . 1) (deletions . 1)))
                       (parents)
                       (files . (((filename . "src/a.el")
                                  (status . "modified")
                                  (additions . 1) (deletions . 1)
                                  (patch . "@@ -1 +1 @@\n-old\n+new"))))))
            (comments . (((id . 9) (path . "src/a.el") (position . 2)
                          (line . 1) (user . ((login . "bob")))
                          (body . "Inline body"))))
            (diff . "diff --git a/src/a.el b/src/a.el\n-old\n+new")))
         (text
          (magh-test-render-page
           'commit "HEAD"
           (lambda (result) (magh-commit--render-view context result))
           data)))
    (should (string-match-p "Inline comment by bob" text))
    (should (string-match-p "Location: src/a.el:1" text))
    (should (string-match-p "Inline body" text))))

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

(ert-deftest magh-commit-detail-sections-and-full-diff-fold-independently ()
  (let* ((context (magh-context-from-repository "o/r"))
         (diff (concat
                "diff --git a/a.el b/a.el\n"
                "--- a/a.el\n+++ b/a.el\n"
                "@@ -1 +1 @@\n-old\n+new\n"
                "@@ -10,0 +11 @@\n+later\n"
                "diff --git a/b.el b/b.el\n"
                "--- a/b.el\n+++ b/b.el\n"
                "@@ -1 +1 @@\n-old-b\n+new-b\n"))
         (data
          `((commit . ((sha . "HEAD")
                       (commit . ((message . "Commit subject")
                                  (author . ((name . "Alice") (email . "a@x")))
                                  (committer . ((name . "Alice") (email . "a@x")))))
                       (stats . ((additions . 3) (deletions . 2)))
                       (parents)
                       (files . (((filename . "a.el") (status . "modified")
                                  (additions . 2) (deletions . 1)
                                  (patch . ,(concat
                                             "@@ -1 +1 @@\n-old\n+new\n"
                                             "@@ -10,0 +11 @@\n+later")))
                                 ((filename . "b.el") (status . "modified")
                                  (additions . 1) (deletions . 1)
                                  (patch . "@@ -1 +1 @@\n-old-b\n+new-b"))))))
            (comments . (((id . 1) (user . ((login . "bob")))
                          (body . "General comment"))))
            (diff . ,diff))))
    (with-temp-buffer
      (magh-section-mode)
      (setq magh-buffer-context context
            magh-buffer-resource-kind 'commit
            magh-buffer-resource-id "HEAD")
      (magh-ui--replace
       (lambda (result) (magh-commit--render-view context result)) data nil)
      (let* ((top-level (oref magit-root-section children))
             (types (mapcar (lambda (section) (oref section type)) top-level))
             (changed-files-section
              (seq-find (lambda (section)
                          (eq (oref section type) 'changed-files))
                        top-level))
             (diff-section (seq-find (lambda (section)
                                       (eq (oref section type) 'diff))
                                     top-level)))
        (should (equal types '(message changed-files diff comments)))
        (let* ((text (buffer-string))
               (file-position
                (string-match (regexp-quote "a.el modified") text))
               (file-newline (and file-position
                                  (string-match "\n" text file-position)))
               (hunk-position (and file-newline
                                   (string-match
                                    (regexp-quote "@@ -1 +1 @@")
                                    text file-newline)))
               (hunk-newline (and hunk-position
                                  (string-match "\n" text hunk-position))))
          (dolist (position (list file-position file-newline))
            (should position)
            (should (magh-test-font-lock-face-p
                     text position 'magit-diff-file-heading))
            (should (magh-test-font-lock-face-p
                     text position 'magit-diff-file-heading-highlight)))
          (dolist (position (list hunk-position hunk-newline))
            (should position)
            (should (magh-test-font-lock-face-p
                     text position 'magit-diff-hunk-heading))))
        (let* ((files (oref changed-files-section children))
               (first-file-hunks (oref (car files) children)))
          (should (equal (mapcar (lambda (section) (oref section type)) files)
                         '(file file)))
          (should (equal (mapcar (lambda (section) (oref section type))
                                 first-file-hunks)
                         '(diff-hunk diff-hunk)))
          (let ((hunk (car first-file-hunks)))
            (magit-section-hide hunk)
            (should (oref hunk hidden))
            (magit-section-show hunk)
            (should-not (oref hunk hidden))))
        (should (oref diff-section hidden))
        (magit-section-show diff-section)
        (let ((file-sections (oref diff-section children)))
          (should (equal (mapcar (lambda (section) (oref section type))
                                 file-sections)
                         '(diff-file diff-file)))
          (should (equal (mapcar (lambda (section) (oref section type))
                                 (oref (car file-sections) children))
                         '(diff-hunk diff-hunk)))
          (should (equal (mapcar (lambda (section) (oref section type))
                                 (oref (cadr file-sections) children))
                         '(diff-hunk)))
          (let ((hunk (car (oref (car file-sections) children))))
            (magit-section-hide hunk)
            (should (oref hunk hidden))
            (magit-section-show hunk)
            (should-not (oref hunk hidden))))))))

(ert-deftest magh-commit-review-renderer-shows-threads-drafts-and-stale-state ()
  (let* ((context (magh-context-from-repository "o/r"))
         (magh-commit--review-drafts (make-hash-table :test #'equal))
         (current-key (magh-commit--review-key context 7 "HEAD"))
         (stale-key (magh-commit--review-key context 7 "OLD"))
         (result
          `((pr . ((number . 7)
                   (title . "Review me")
                   (baseRefName . "main")
                   (baseRefOid . "BASE")
                   (headRefName . "topic")
                   (headRefOid . "HEAD")
                   (reviewDecision . "REVIEW_REQUIRED")
                   (reviews . (((id . "R1")
                                (state . "APPROVED")
                                (author . ((login . "alice")))
                                (submittedAt . "2026-07-01T00:00:00Z")
                                (body . "Summary"))))))
            (commit . ((sha . "HEAD")
                       (commit . ((message . "Head commit")))))
            (comparison
             . ((files . (((filename . "src/a.el")
                            (status . "modified")
                            (additions . 2)
                            (deletions . 1)
                            (patch . ,(concat
                                       "@@ -1 +1 @@\n-old\n+new\n"
                                       "@@ -10,0 +11 @@\n+later")))))))
            (threads
             . (((id . "T1")
                 (root_id . 10)
                 (path . "src/a.el")
                 (line . 1)
                 (side . "RIGHT")
                 (subject_type . "LINE")
                 (viewer_can_reply . t)
                 (comments . (((id . 10)
                               (user . ((login . "bob")))
                               (created_at . "2026-07-01T00:00:00Z")
                               (body . "Please change this")))))))))
         text)
    (puthash current-key
             '((:id 1 :path "src/a.el" :line 1 :side "RIGHT"
                :subject-type "LINE" :body "Local draft"))
             magh-commit--review-drafts)
    (puthash stale-key
             '((:id 2 :path "src/old.el" :subject-type "FILE"
                :body "Old draft"))
             magh-commit--review-drafts)
    (with-temp-buffer
      (magh-section-mode)
      (setq magh-buffer-context context
            magh-buffer-resource-kind 'commit-review
            magh-buffer-resource-id 7)
      (magh-ui--replace
       (lambda (data) (magh-commit--render-review context 7 data)) result nil)
      (setq text (buffer-string))
      (let* ((changed-files
              (seq-find (lambda (section)
                          (eq (oref section type) 'changed-files))
                        (oref magit-root-section children)))
             (file (car (oref changed-files children)))
             (hunks (oref file children)))
        (let* ((rendered (buffer-string))
               (file-position
                (string-match (regexp-quote "src/a.el modified") rendered))
               (hunk-position
                (string-match (regexp-quote "@@ -1 +1 @@") rendered)))
          (should (magh-test-font-lock-face-p
                   rendered file-position 'magit-diff-file-heading-highlight))
          (should (magh-test-font-lock-face-p
                   rendered hunk-position 'magit-diff-hunk-heading)))
        (should (equal (mapcar (lambda (section) (oref section type)) hunks)
                       '(diff-hunk diff-hunk)))
        (should (equal (mapcar (lambda (section) (oref section type))
                               (oref (car hunks) children))
                       '(inline-comment inline-comment)))
        (magit-section-hide (car hunks))
        (should (oref (car hunks) hidden))
        (magit-section-show (car hunks))
        (should-not (oref (car hunks) hidden))))
    (should (string-match-p "PR #7  Review me" text))
    (should (string-match-p "APPROVED by alice" text))
    (should (string-match-p "Please change this" text))
    (should (string-match-p "Local draft" text))
    (should (string-match-p "Stale drafts (cannot be submitted)" text))
    (should (string-match-p "Old draft" text))))

(ert-deftest magh-pr-review-opens-structured-commit-review-resource ()
  (let ((context (magh-context-from-repository "o/r")) captured)
    (cl-letf (((symbol-function 'magh-resource-open)
               (lambda (resource) (setq captured resource))))
      (let ((magh-buffer-context context)
            (magh-pr--view-number 7)
            (magh-pr--dispatch-resource nil))
        (magh-pr-review)))
    (should (eq (plist-get captured :kind) 'commit-review))
    (should (= (plist-get captured :number) 7))
    (should (eq (plist-get captured :context) context))))

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

(ert-deftest magh-topic-editors-remove-cleared-milestones ()
  (let ((context (magh-context-from-repository "o/r")) submit changes)
    (cl-letf (((symbol-function 'magh-edit-open)
               (lambda (_name _fields _values _body submit-function &rest _)
                 (setq submit submit-function)))
              ((symbol-function 'magh-api--issue-edit)
               (lambda (_context _number values _success _error)
                 (setq changes values))))
      (magh-issue--open-edit-editor
       context 7
       '((title . "Issue") (body . "Body")
         (milestone . ((title . "Version 1")))))
      (funcall submit
               '(:title "Issue" :assignees nil :labels nil
                 :milestone nil :projects nil)
               "Body" #'ignore #'ignore))
    (should (plist-get changes :remove-milestone))
    (should-not (plist-member changes :milestone)))
  (let ((context (magh-context-from-repository "o/r")) submit changes)
    (cl-letf (((symbol-function 'magh-edit-open)
               (lambda (_name _fields _values _body submit-function &rest _)
                 (setq submit submit-function)))
              ((symbol-function 'magh-api--pr-edit)
               (lambda (_context _number values _success _error)
                 (setq changes values))))
      (magh-pr--open-edit-editor
       context 8
       '((title . "PR") (body . "Body") (baseRefName . "main")
         (milestone . ((title . "Version 1")))))
      (funcall submit
               '(:title "PR" :base "main" :reviewers nil :assignees nil
                 :labels nil :milestone nil :projects nil)
               "Body" #'ignore #'ignore))
    (should (plist-get changes :remove-milestone))
    (should-not (plist-member changes :milestone))))

(provide 'magh-ui-test)
;;; magh-ui-test.el ends here
