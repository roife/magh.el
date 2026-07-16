;;; magh-api-test.el --- API contract tests for magh.el -*- lexical-binding: t; -*-

(require 'magh-test-helper)
(require 'magh-api)
(require 'magh-command)

(ert-deftest magh-command-interactive-input-uses-shell-argument-syntax ()
  (let ((magh-display-buffer-function #'identity)
        captured)
    (cl-letf (((symbol-function 'read-shell-command)
               (lambda (&rest _) "issue create --title 'two words' --body '$HOME'"))
              ((symbol-function 'magh-context-resolve)
               (lambda (&rest _) (magh-context-create)))
              ((symbol-function 'magh-client--start-pty)
               (lambda (argv _name _context) (setq captured argv) 'buffer)))
      (call-interactively #'magh-command))
    (should (equal captured
                   '("issue" "create" "--title" "two words"
                     "--body" "$HOME")))))

(ert-deftest magh-api-issue-list-keeps-each-cli-argument-separate ()
  (let ((context (magh-context-from-repository "acme/widgets" "github.com"))
        captured)
    (cl-letf (((symbol-function 'magh-client--json-async)
               (lambda (argv _success _error &rest keys)
                 (setq captured (cons argv keys)) 'request)))
      (magh-api--issue-list
       context '(:state "open" :labels ("help wanted" "good first issue")
                 :assignee "octo cat" :limit 17)
       #'ignore #'ignore))
    (let ((argv (car captured)))
      (should (member "help wanted" argv))
      (should (member "good first issue" argv))
      (should (member "octo cat" argv))
      (should (equal (cadr (member "--repo" argv)) "acme/widgets"))
      (should (equal (cadr (member "--limit" argv)) "17")))))

(ert-deftest magh-api-topic-edits-can-remove-milestones ()
  (let ((context (magh-context-from-repository "acme/widgets")) calls)
    (cl-letf (((symbol-function 'magh-client--mutate-text)
               (lambda (argv _success _error &rest _keys)
                 (push argv calls))))
      (magh-api--issue-edit context 7 '(:remove-milestone t)
                            #'ignore #'ignore)
      (magh-api--pr-edit context 8 '(:remove-milestone t)
                         #'ignore #'ignore))
    (dolist (argv calls)
      (should (member "--remove-milestone" argv))
      (should-not (member "--milestone" argv)))))

(ert-deftest magh-api-topic-edit-arguments-are-shared ()
  (should
   (equal
    (magh-api--topic-edit-args
     '(:milestone "v1" :add-reviewers ("alice")
       :remove-assignees ("bob") :add-labels ("bug")
       :remove-projects ("Old") :body "Body"))
    '("--milestone" "v1" "--add-reviewer" "alice"
      "--remove-assignee" "bob" "--add-label" "bug"
      "--remove-project" "Old" "--body-file" "-"))))

(ert-deftest magh-api-rest-pagination-and-fields-are-typed ()
  (should
   (equal
    (magh-api--rest-argv "repos/o/r/items" "GET"
                       '((page . 2) (enabled . t) (name . "two words")) t)
    '("api" "repos/o/r/items" "--method" "GET" "--paginate" "--slurp"
      "-F" "page=2" "-F" "enabled=true" "-f" "name=two words"))))

(ert-deftest magh-api-repo-viewer-fork-uses-owner-affiliation ()
  (let ((context (magh-context-from-repository "acme/widgets"))
        captured result)
    (cl-letf (((symbol-function 'magh-client--json-async)
               (lambda (argv success _error &rest keys)
                 (setq captured (cons argv keys))
                 (funcall success
                          '((data . ((repository .
                                     ((forks . ((totalCount . 1)))))))))
                 'request)))
      (magh-api--repo-viewer-forked-p
       context (lambda (value) (setq result value)) #'ignore t))
    (should result)
    (should (member "owner=acme" (car captured)))
    (should (member "name=widgets" (car captured)))
    (should (string-match-p
             (regexp-quote "forks(first:1,affiliations:[OWNER])")
             (cadr (member "-f" (car captured)))))
    (should (plist-get (cdr captured) :force))))

(ert-deftest magh-api-paginated-content-contracts-are-normalized-once ()
  (should
   (equal (magh-api--flatten-pages
           '((((id . 1)) ((id . 2))) (((id . 3)))))
          '(((id . 1)) ((id . 2)) ((id . 3)))))
  (should
   (equal (magh-api--decode-content
           '((encoding . "base64") (content . "aGVs\r\nbG8=")))
          "hello"))
  (should
   (equal (magh-api--decode-content
           '((encoding . "utf-8") (content . "plain text")))
          "plain text")))

(ert-deftest magh-api-search-arguments-share-one-builder ()
  (should
   (equal
    (magh-api--search-argv
     'repos "two words"
     '(:owner "octo cat" :state "open"))
    (append
     '("search" "repos" "two words" "--limit")
     (list (number-to-string magh-list-limit)
           "--json" (alist-get 'repos magh-api--search-fields)
           "--owner" "octo cat" "--state" "open")))))

(ert-deftest magh-api-dashboard-searches-only-active-pull-requests ()
  (let ((context (magh-context-create)) calls)
    (cl-letf (((symbol-function 'magh-api--search)
               (lambda (_context kind query _callback _errback &optional force options)
                 (push (list kind query force options) calls))))
      (magh-api--review-requests context #'ignore #'ignore)
      (magh-api--my-prs context #'ignore #'ignore))
    (should
     (equal
      (nreverse calls)
      '((prs "" nil (:review-requested "@me" :state "open"))
        (prs "" nil (:author "@me" :state "open")))))))

(ert-deftest magh-api-generic-graphql-mutations-bypass-read-cache ()
  (let ((context (magh-context-from-repository "acme/widgets" "github.com"))
        query-call mutation-call)
    (cl-letf (((symbol-function 'magh-api--read-json)
               (lambda (&rest arguments) (setq query-call arguments)))
              ((symbol-function 'magh-client--request-async)
               (lambda (argv _success _error &rest keys)
                 (setq mutation-call (cons argv keys)))))
      (magh-api--graphql context "query { viewer { login } }" nil
                         #'ignore #'ignore)
      (magh-api--graphql context "# update\n mutation { doThing }" nil
                         #'ignore #'ignore))
    (should query-call)
    (should (equal (cadr (member :preserve-false query-call)) t))
    (should mutation-call)
    (should-not (plist-get (cdr mutation-call) :cache))
    (should-not (plist-get (cdr mutation-call) :dedupe))
    (should (eq (plist-get (cdr mutation-call) :json-false-object)
                :json-false))))

(ert-deftest magh-api-release-body-uses-stdin-not-a-shell-or-temporary-file ()
  (let ((context (magh-context-from-repository "o/r")) captured)
    (cl-letf (((symbol-function 'magh-client--mutate-text)
               (lambda (argv _success _error &rest keys)
                 (setq captured (cons argv keys)) 'request)))
      (magh-api--release-create
       context '(:tag "v1.0" :title "A release" :body "line 1\n$HOME `x`")
       #'ignore #'ignore))
    (should (member "--notes-file" (car captured)))
    (should (equal (plist-get (cdr captured) :stdin)
                   "line 1\n$HOME `x`"))))

(ert-deftest magh-api-valid-message-field-is-not-mistaken-for-an-error ()
  (should-not (magh-api--api-errors '((message . "ordinary commit message")
                                    (sha . "abc"))))
  (should (magh-api--api-errors '((message . "Not Found")
                                (status . "404")))))

(ert-deftest magh-api-json-mutation-surfaces-graphql-errors ()
  (let (success failure)
    (cl-letf (((symbol-function 'magh-client--mutate-json)
               (lambda (_argv callback _errback &rest _keys)
                 (funcall callback
                          '((errors . (((message . "Mutation rejected")))))))))
      (magh-api--mutate-json
       (magh-context-create) '("api" "graphql") nil
       (lambda (_) (setq success t)) (lambda (error) (setq failure error))))
    (should-not success)
    (should (eq (car failure) 'magh-api-error))))

(ert-deftest magh-api-rest-release-normalizes-public-contract ()
  (let ((release
         (magh-api--normalize-rest-release
          '((id . 7) (tag_name . "v1") (target_commitish . "main")
            (draft . nil) (prerelease . t) (html_url . "https://example/r")
            (assets . (((id . 8) (name . "a.zip") (download_count . 3)
                        (browser_download_url . "https://example/a"))))))))
    (should (equal (alist-get 'tagName release) "v1"))
    (should (alist-get 'isPrerelease release))
    (should (= (alist-get
                'downloadCount (car (alist-get 'assets release))) 3))))

(ert-deftest magh-api-project-completion-uses-owner-and-json-output ()
  (let ((context (magh-context-from-repository "acme/widgets" "github.com"))
        captured)
    (cl-letf (((symbol-function 'magh-client--json-async)
               (lambda (argv _success _error &rest keys)
                 (setq captured (cons argv keys)) 'request)))
      (magh-api--project-list context #'ignore #'ignore))
    (should (equal (seq-take (car captured) 4)
                   '("project" "list" "--owner" "acme")))
    (should (member "--format" (car captured)))
    (should (equal (cadr (member "--format" (car captured))) "json"))))

(ert-deftest magh-api-cli-json-field-contracts-distinguish-list-and-view ()
  (should (memq 'startedAt magh-api--run-fields))
  (should-not (memq 'startedTime magh-api--run-fields))
  (should (memq 'isLatest magh-api--release-list-fields))
  (should-not (memq 'assets magh-api--release-list-fields))
  (should (memq 'assets magh-api--release-view-fields))
  (should-not (memq 'isLatest magh-api--release-view-fields)))

(ert-deftest magh-api-explicit-json-false-never-enables-cli-switches ()
  (let ((context (magh-context-from-repository "acme/widgets")) captured)
    (cl-letf (((symbol-function 'magh-client--mutate-text)
               (lambda (argv _success _error &rest keys)
                 (setq captured (cons argv keys)) 'request)))
      (magh-api--repo-create
       context '(:name "new" :public t :push :json-false :clone :json-false)
       #'ignore #'ignore)
      (should (member "--public" (car captured)))
      (should-not (member "--push" (car captured)))
      (should-not (member "--clone" (car captured)))
      (magh-api--pr-create
       context '(:title "PR" :base "main" :head "topic"
                 :draft :json-false :body "")
       #'ignore #'ignore)
      (should-not (member "--draft" (car captured)))
      (magh-api--release-edit
       context "v1" '(:draft :json-false :prerelease :json-false)
       #'ignore #'ignore)
      (should (member "--draft=false" (car captured)))
      (should (member "--prerelease=false" (car captured)))
      (magh-api--repo-edit
       context '(:issues nil :wiki t) #'ignore #'ignore)
      (should (member "--enable-issues=false" (car captured)))
      (should (member "--enable-wiki=true" (car captured))))))

(ert-deftest magh-api-generic-output-preserves-json-false ()
  (let ((context (magh-context-create :host "github.com")) captured rendered)
    (cl-letf (((symbol-function 'magh-client--json-async)
               (lambda (_argv success _error &rest keys)
                 (setq captured keys)
                 (funcall success '((enabled . :json-false) (missing)))
                 'request)))
      (magh-api--generic-request
       context "example" "GET" nil nil
       (lambda (data) (setq rendered (magh-command--json-string data))) #'ignore))
    (should (eq (plist-get captured :json-false-object) :json-false))
    (should (string-match-p "\\\"enabled\\\"[[:space:]]*:[[:space:]]*false"
                            rendered))
    (should (string-match-p "\\\"missing\\\"[[:space:]]*:[[:space:]]*null"
                            rendered))))

(ert-deftest magh-api-review-uses-add-thread-for-file-comments ()
  (let ((context (magh-context-from-repository "acme/widgets"))
        reads mutations finished failure)
    (cl-letf (((symbol-function 'magh-api--pr-get)
               (lambda (_context _number callback _errback &optional _force)
                 (funcall callback '((id . "PR_node"))) 'pr-request))
              ((symbol-function 'magh-client--json-async)
               (lambda (argv success _error &rest keys)
                 (push (cons argv keys) reads)
                 (funcall success
                          '((data . ((node . ((reviews . ((nodes)))))))))
                 'read-request))
              ((symbol-function 'magh-client--mutate-json)
               (lambda (argv success _error &rest keys)
                 (let* ((payload (json-parse-string
                                  (plist-get keys :stdin)
                                  :object-type 'alist :array-type 'list))
                        (query (alist-get 'query payload)))
                   (push (cons argv keys) mutations)
                   (funcall
                    success
                    (cond
                     ((string-match-p "addPullRequestReview(input" query)
                      '((data . ((addPullRequestReview
                                  . ((pullRequestReview . ((id . "REVIEW_node")))))))))
                     ((string-match-p "addPullRequestReviewThread" query)
                      '((data . ((addPullRequestReviewThread
                                  . ((thread . ((id . "THREAD_node")))))))))
                     (t '((data . ((submitPullRequestReview
                                   . ((pullRequestReview . ((id . "REVIEW_node")))))))))))
                 'mutation-request))))
      (magh-api--pr-review
       context 12 'approve "Looks good"
       '((:path "src/a.el" :subject-type "FILE" :body "Whole file"))
       (lambda (_) (setq finished t)) (lambda (error) (setq failure error))))
    (should finished)
    (should-not failure)
    (should (= (length reads) 1))
    (should (= (length mutations) 3))
    (setq mutations (nreverse mutations))
    (let* ((thread-call (nth 1 mutations))
           (payload (json-parse-string
                     (plist-get (cdr thread-call) :stdin)
                     :object-type 'alist :array-type 'list))
           (query (alist-get 'query payload))
           (variables (alist-get 'variables payload))
           (input (alist-get 'input variables)))
      (should (string-match-p "AddPullRequestReviewThreadInput" query))
      (should (equal (alist-get 'pullRequestReviewId input)
                     "REVIEW_node"))
      (should (equal (alist-get 'subjectType input) "FILE"))
      (should-not (alist-get 'line input)))
    (let* ((submit-call (nth 2 mutations))
           (payload (json-parse-string
                     (plist-get (cdr submit-call) :stdin)
                     :object-type 'alist :array-type 'list))
           (input (magh-api--json-at payload 'variables 'input)))
      (should (equal (alist-get 'event input) "APPROVE")))))

(ert-deftest magh-api-review-rolls-back-a-new-pending-review-on-thread-error ()
  (let ((context (magh-context-from-repository "acme/widgets"))
        (thread-error (magh-core--error 'magh-api-error "Thread rejected"))
        queries failure finished)
    (cl-letf (((symbol-function 'magh-api--pr-get)
               (lambda (_context _number callback _errback &optional _force)
                 (funcall callback '((id . "PR_node"))) 'pr-request))
              ((symbol-function 'magh-client--json-async)
               (lambda (_argv success _error &rest _keys)
                 (funcall success
                          '((data . ((node . ((reviews . ((nodes)))))))))
                 'read-request))
              ((symbol-function 'magh-client--mutate-json)
               (lambda (_argv success error &rest keys)
                 (let* ((payload (json-parse-string
                                  (plist-get keys :stdin)
                                  :object-type 'alist :array-type 'list))
                        (query (alist-get 'query payload)))
                   (push query queries)
                   (cond
                    ((string-match-p "addPullRequestReview(input" query)
                     (funcall
                      success
                      '((data . ((addPullRequestReview
                                  . ((pullRequestReview . ((id . "REVIEW_node"))))))))))
                    ((string-match-p "addPullRequestReviewThread" query)
                     (funcall error thread-error))
                    ((string-match-p "deletePullRequestReview" query)
                     (funcall success '((data . ((deletePullRequestReview))))))
                    (t (ert-fail (format "Unexpected mutation: %s" query))))
                 'mutation-request))))
      (magh-api--pr-review
       context 12 'comment "Summary"
       '((:path "src/a.el" :line 3 :side "RIGHT"
          :subject-type "LINE" :body "Needs work"))
       (lambda (_) (setq finished t)) (lambda (error) (setq failure error))))
    (should-not finished)
    (should (equal failure thread-error))
    (setq queries (nreverse queries))
    (should (= (length queries) 3))
    (should (string-match-p "addPullRequestReview(input" (nth 0 queries)))
    (should (string-match-p "addPullRequestReviewThread" (nth 1 queries)))
    (should (string-match-p "deletePullRequestReview" (nth 2 queries)))
    (should-not (seq-some (lambda (query)
                            (string-match-p "submitPullRequestReview" query))
                          queries))))

(ert-deftest magh-api-review-thread-pages-flatten-slurped-data ()
  (let* ((data
          (json-parse-string
           (concat
            "[{\"data\":{\"repository\":{\"pullRequest\":{\"reviewThreads\":"
            "{\"nodes\":[{\"id\":\"A\"}]}}}}},"
            "{\"data\":{\"repository\":{\"pullRequest\":{\"reviewThreads\":"
            "{\"nodes\":[{\"id\":\"B\"}]}}}}}]")
           :object-type 'alist :array-type 'list))
         (nodes (magh-api--pr-review-thread-pages data)))
    (should (equal (mapcar (lambda (node) (alist-get 'id node)) nodes)
                   '("A" "B")))))

(ert-deftest magh-api-review-threads-group-replies-and-metadata ()
  (let* ((root '((id . 10) (path . "src/a.el") (line . 4)
                 (side . "RIGHT") (body . "Root")))
         (reply-1 '((id . 11) (in_reply_to_id . 10) (body . "First")))
         (reply-2 '((id . 12) (in_reply_to_id . 10) (body . "Second")))
         (metadata
          '(((id . "THREAD") (path . "src/a.el") (line . 4)
             (diffSide . "RIGHT") (subjectType . "LINE")
             (isResolved . t) (isOutdated . :json-false)
             (viewerCanReply . t) (viewerCanResolve . :json-false)
             (viewerCanUnresolve . t)
             (comments . ((nodes . (((databaseId . 10)))))))))
         (threads (magh-api--pr-review-normalize-threads
                   (list root reply-1 reply-2) metadata))
         (thread (car threads)))
    (should (= (length threads) 1))
    (should (equal (alist-get 'id thread) "THREAD"))
    (should (alist-get 'is_resolved thread))
    (should-not (alist-get 'is_outdated thread))
    (should (alist-get 'viewer_can_unresolve thread))
    (should-not (alist-get 'viewer_can_resolve thread))
    (should (equal (mapcar (lambda (item) (alist-get 'id item))
                           (alist-get 'comments thread))
                   '(10 11 12)))))

(ert-deftest magh-api-review-threads-rest-fallback-remains-replyable ()
  (let* ((thread
          (car (magh-api--pr-review-normalize-threads
                '(((id . 10) (path . "src/a.el") (line . 4)
                   (side . "RIGHT") (body . "Root")))
                nil))))
    (should-not (alist-get 'id thread))
    (should (alist-get 'viewer_can_reply thread))
    (should-not (alist-get 'viewer_can_resolve thread))))

(ert-deftest magh-api-review-reply-uses-top-level-comment-endpoint ()
  (let ((context (magh-context-from-repository "acme/widgets")) captured)
    (cl-letf (((symbol-function 'magh-client--mutate-json)
               (lambda (argv success _error &rest keys)
                 (setq captured (cons argv keys))
                 (funcall success '((id . 12)))
                 'request)))
      (magh-api--pr-review-reply context 7 10 "Reply body" #'ignore #'ignore))
    (should (member "repos/acme/widgets/pulls/7/comments/10/replies"
                    (car captured)))
    (should (member "body=Reply body" (car captured)))))

(ert-deftest magh-api-review-thread-resolution-carries-thread-id ()
  (let ((context (magh-context-from-repository "acme/widgets")) payload)
    (cl-letf (((symbol-function 'magh-client--mutate-json)
               (lambda (_argv success _error &rest keys)
                 (setq payload
                       (json-parse-string
                        (plist-get keys :stdin)
                        :object-type 'alist :array-type 'list))
                 (funcall success '((data . ((resolveReviewThread)))))
                 'request)))
      (magh-api--pr-review-thread-resolved
       context 7 "THREAD" t #'ignore #'ignore))
    (should (string-match-p "resolveReviewThread"
                            (alist-get 'query payload)))
    (should (equal (magh-api--json-at payload 'variables 'input 'threadId)
                   "THREAD"))))

(ert-deftest magh-api-review-binds-new-pending-review-to-head ()
  (let ((context (magh-context-from-repository "acme/widgets")) create-input done)
    (cl-letf (((symbol-function 'magh-api--pr-get)
               (lambda (_context _number callback _errback &optional _force)
                 (funcall callback '((id . "PR"))) 'request))
              ((symbol-function 'magh-client--json-async)
               (lambda (_argv success _error &rest _keys)
                 (funcall success
                          '((data . ((node . ((reviews . ((nodes)))))))))
                 'request))
              ((symbol-function 'magh-client--mutate-json)
               (lambda (_argv success _error &rest keys)
                 (let* ((payload (json-parse-string
                                  (plist-get keys :stdin)
                                  :object-type 'alist :array-type 'list))
                        (query (alist-get 'query payload))
                        (input (magh-api--json-at payload 'variables 'input)))
                   (if (string-match-p "addPullRequestReview(input" query)
                       (progn
                         (setq create-input input)
                         (funcall
                          success
                          '((data . ((addPullRequestReview
                                      . ((pullRequestReview
                                          . ((id . "REVIEW"))))))))))
                     (if (string-match-p "submitPullRequestReview" query)
                         (funcall
                          success '((data . ((submitPullRequestReview)))))
                       (ert-fail (format "Unexpected mutation: %s" query)))))
                 'request)))
      (magh-api--pr-review context 7 'approve "" nil
                         (lambda (_) (setq done t)) #'ignore "HEAD"))
    (should done)
    (should (equal (alist-get 'pullRequestId create-input) "PR"))
    (should (equal (alist-get 'commitOID create-input) "HEAD"))))

(ert-deftest magh-api-commit-inline-comment-uses-diff-position ()
  (let ((context (magh-context-from-repository "acme/widgets")) captured)
    (cl-letf (((symbol-function 'magh-client--mutate-json)
               (lambda (argv success _error &rest keys)
                 (setq captured (cons argv keys))
                 (funcall success '((id . 1)))
                 'request)))
      (magh-api--commit-comment
       context "HEAD" "Inline body" "src/a.el" 4 #'ignore #'ignore))
    (should (member "path=src/a.el" (car captured)))
    (should (member "position=4" (car captured)))
    (should-not (seq-some (lambda (arg) (string-prefix-p "line=" arg))
                          (car captured)))))

(provide 'magh-api-test)
;;; magh-api-test.el ends here
