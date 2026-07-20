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

(ert-deftest magh-api-topic-pages-use-cursors-and-normalize-connections ()
  (let ((context (magh-context-from-repository "acme/widgets"))
        captured result)
    (cl-letf (((symbol-function 'magh-client--json-async)
               (lambda (argv success _error &rest keys)
                 (setq captured (cons argv keys))
                 (funcall
                  success
                  '((data
                     . ((search
                         . ((nodes
                             . (((__typename . "Issue")
                                 (number . 7) (title . "Paged")
                                 (state . "OPEN")
                                 (author . ((login . "alice")))
                                 (assignees
                                  . ((nodes . (((login . "bob"))))))
                                 (labels . ((nodes . (((name . "bug"))))))
                                 (comments . ((totalCount . 2))))))
                            (pageInfo . ((hasNextPage . t)
                                         (endCursor . "cursor-2")))))))))
                 'request)))
      (magh-api--issue-page
       context
       '(:state "open" :limit 25 :assignee "octo cat"
         :labels ("help wanted"))
       "cursor-1" (lambda (page) (setq result page)) #'ignore))
    (should (equal (car captured) '("api" "graphql" "--input" "-")))
    (let* ((payload (json-parse-string
                     (plist-get (cdr captured) :stdin)
                     :object-type 'alist :array-type 'list))
           (variables (alist-get 'variables payload))
           (query-string (alist-get 'queryString variables)))
      (should (= (alist-get 'first variables) 25))
      (should (equal (alist-get 'after variables) "cursor-1"))
      (should (string-match-p "repo:acme/widgets is:issue is:open"
                              query-string))
      (should (string-match-p "assignee:\"octo cat\"" query-string))
      (should (string-match-p "label:\"help wanted\"" query-string)))
    (should (magh-page-p result))
    (should (equal (magh-page-next result) "cursor-2"))
    (let ((item (car (magh-page-items result))))
      (should-not (assq '__typename item))
      (should (equal (alist-get 'assignees item)
                     '(((login . "bob")))))
      (should (equal (alist-get 'labels item)
                     '(((name . "bug"))))))))

(ert-deftest magh-api-pr-page-query-preserves-state-and-list-filters ()
  (let ((query (magh-api--topic-search-query
                (magh-context-from-repository "acme/widgets") 'pr
                '(:state "closed" :base "main" :head "topic"
                  :draft t :labels ("needs review")))))
    (should (string-match-p "is:pr is:closed is:unmerged" query))
    (should (string-match-p "base:main" query))
    (should (string-match-p "head:topic" query))
    (should (string-match-p "draft:true" query))
    (should (string-match-p "label:\"needs review\"" query))))

(ert-deftest magh-api-topic-first-page-serializes-a-null-cursor ()
  (let ((context (magh-context-from-repository "acme/widgets")) captured)
    (cl-letf (((symbol-function 'magh-client--json-async)
               (lambda (_argv _success _error &rest keys)
                 (setq captured keys)
                 'request)))
      (magh-api--pr-page context '(:state "open") nil #'ignore #'ignore))
    (let* ((payload (json-parse-string
                     (plist-get captured :stdin)
                     :object-type 'alist :array-type 'list
                     :null-object :null))
           (variables (alist-get 'variables payload)))
      (should (eq (alist-get 'after variables) :null)))))

(ert-deftest magh-api-commit-page-sends-bounded-page-parameters ()
  (let ((context (magh-context-from-repository "acme/widgets"))
        captured result)
    (cl-letf (((symbol-function 'magh-client--json-async)
               (lambda (argv success _error &rest _keys)
                 (setq captured argv)
                 (funcall success '(((sha . "a")) ((sha . "b"))))
                 'request)))
      (magh-api--commit-page
       context '(:limit 2 :ref "main") 3
       (lambda (page) (setq result page)) #'ignore))
    (should (member "per_page=2" captured))
    (should (member "page=3" captured))
    (should (member "sha=main" captured))
    (should (equal (mapcar (lambda (item) (alist-get 'sha item))
                           (magh-page-items result))
                   '("a" "b")))
    (should (= (magh-page-next result) 4))
    (should (= (magh-api--page-size '(:limit 1000)) 100))))

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

(ert-deftest magh-api-artifact-download-and-delete-preserve-contracts ()
  (let ((context (magh-context-from-repository
                  "acme/widgets" "github.example"))
        mutations invalidated)
    (cl-letf (((symbol-function 'magh-client--mutate-text)
               (lambda (argv success _error &rest _keys)
                 (push argv mutations)
                 (funcall success "")
                 'request))
              ((symbol-function 'magh-client-invalidate)
               (lambda (domain) (push domain invalidated))))
      (magh-api--artifact-download
       context 71 '("linux build" "macOS") "/tmp/magh artifacts"
       #'ignore #'ignore)
      (magh-api--artifact-delete context 71 902 #'ignore #'ignore))
    (let ((download (cadr mutations)) (delete (car mutations)))
      (should
       (equal download
              '("run" "download" "71" "--dir" "/tmp/magh artifacts"
                "--name" "linux build" "--name" "macOS"
                "--repo" "acme/widgets")))
      (should (equal delete
                     '("api" "repos/acme/widgets/actions/artifacts/902"
                       "--method" "DELETE"))))
    (should (member '(:host "github.example" :repository "acme/widgets"
                      :resource artifact :id 71)
                    invalidated))
    (should (member '(:host "github.example" :repository "acme/widgets"
                      :resource run :id 71)
                    invalidated))))

(ert-deftest magh-api-project-field-types-map-to-one-cli-value-flag ()
  (dolist (contract
           '(("TEXT" "hello" "--text")
             ("NUMBER" 12.5 "--number")
             ("DATE" "2026-07-20" "--date")
             ("SINGLE_SELECT" "OPT" "--single-select-option-id")
             ("ITERATION" "ITER" "--iteration-id")))
    (let ((argv (magh-api--project-field-args
                 "PROJECT" "ITEM" "FIELD"
                 (nth 0 contract) (nth 1 contract) nil)))
      (should (equal (cadr (member "--id" argv)) "ITEM"))
      (should (equal (cadr (member "--project-id" argv)) "PROJECT"))
      (should (equal (cadr (member "--field-id" argv)) "FIELD"))
      (should (equal (cadr (member (nth 2 contract) argv))
                     (format "%s" (nth 1 contract))))
      (should (= 1 (cl-count-if
                    (lambda (arg)
                      (member arg '("--text" "--number" "--date"
                                    "--single-select-option-id"
                                    "--iteration-id" "--clear")))
                    argv)))))
  (let ((argv (magh-api--project-field-args
               "PROJECT" "ITEM" "FIELD" "TEXT" nil t)))
    (should (member "--clear" argv))
    (should-not (member "--text" argv))))

(ert-deftest magh-api-discussion-page-normalizes-cursor-in-json-payload ()
  (let ((context (magh-context-from-repository "acme/widgets")) payloads)
    (cl-letf (((symbol-function 'magh-client--json-async)
               (lambda (_argv _success _error &rest keys)
                 (push (json-parse-string
                        (plist-get keys :stdin)
                        :object-type 'alist :array-type 'list
                        :null-object :null)
                       payloads)
                 'request)))
      (magh-api--discussion-page context '(:limit 25) nil #'ignore #'ignore)
      (magh-api--discussion-page
       context '(:limit 25 :category-id "CAT") "CURSOR" #'ignore #'ignore))
    (let ((first (cadr payloads)) (next (car payloads)))
      (should (eq (magh-api--json-at first 'variables 'after) :null))
      (should (eq (magh-api--json-at first 'variables 'categoryId) :null))
      (should (= (magh-api--json-at first 'variables 'first) 25))
      (should (equal (magh-api--json-at next 'variables 'after) "CURSOR"))
      (should (equal (magh-api--json-at next 'variables 'categoryId) "CAT")))))

(ert-deftest magh-api-discussion-mutations-send-typed-graphql-json-inputs ()
  (let ((context (magh-context-from-repository "acme/widgets")) calls)
    (cl-letf (((symbol-function 'magh-client--mutate-json)
               (lambda (argv _success _error &rest keys)
                 (push (cons argv
                             (json-parse-string
                              (plist-get keys :stdin)
                              :object-type 'alist :array-type 'list))
                       calls)
                 'request)))
      (magh-api--discussion-create
       context '(:repository-id "REPO" :category-id "CAT"
                 :title "Title" :body "line 1\nline 2")
       #'ignore #'ignore)
      (magh-api--discussion-comment
       context 7 "DISC" "reply\nbody" #'ignore #'ignore "PARENT")
      (magh-api--discussion-close
       context 7 "DISC" "duplicate" #'ignore #'ignore)
      (magh-api--discussion-mark-answer
       context 7 "COMMENT" t #'ignore #'ignore))
    (dolist (call calls)
      (should (equal (car call) '("api" "graphql" "--input" "-"))))
    (let* ((create (nth 3 calls))
           (reply (nth 2 calls))
           (close (nth 1 calls))
           (answer (nth 0 calls)))
      (should
       (equal (magh-api--json-at (cdr create) 'variables 'input)
              '((repositoryId . "REPO") (categoryId . "CAT")
                (title . "Title") (body . "line 1\nline 2"))))
      (should
       (equal (magh-api--json-at (cdr reply) 'variables 'input 'replyToId)
              "PARENT"))
      (should
       (equal (magh-api--json-at (cdr close) 'variables 'input 'reason)
              "DUPLICATE"))
      (should
       (equal (magh-api--json-at (cdr answer) 'variables 'input)
              '((id . "COMMENT")))))))

(ert-deftest magh-api-gist-json-preserves-special-filenames-and-multiline-content ()
  (let ((context (magh-context-create)) calls)
    (cl-letf (((symbol-function 'magh-client--mutate-json)
               (lambda (argv _success _error &rest keys)
                 (push (cons argv
                             (json-parse-string
                              (plist-get keys :stdin)
                              :object-type 'alist :array-type 'list
                              :null-object :null
                              :false-object :json-false))
                       calls)
                 'request)))
      (magh-api--gist-create
       context
       '(:description "desc\nline" :public :json-false
         :files (("a b[$].el" :content "(message \"$HOME\")\n`x`\n")))
       #'ignore #'ignore)
      (magh-api--gist-update
       context "GIST"
       '(:files (("old name.txt" :filename "new name.txt"
                  :content "new\ncontent")
                 ("remove.me")))
       #'ignore #'ignore))
    (let* ((create (cadr calls)) (update (car calls))
           (create-file
            (alist-get (intern "a b[$].el")
                       (alist-get 'files (cdr create))))
           (renamed
            (alist-get (intern "old name.txt")
                       (alist-get 'files (cdr update)))))
      (should (equal (car create)
                     '("api" "gists" "--method" "POST" "--input" "-")))
      (should (equal (car update)
                     '("api" "gists/GIST" "--method" "PATCH" "--input" "-")))
      (should (eq (alist-get 'public (cdr create)) :json-false))
      (should (equal (alist-get 'description (cdr create)) "desc\nline"))
      (should (equal (alist-get 'content create-file)
                     "(message \"$HOME\")\n`x`\n"))
      (should (equal (alist-get 'filename renamed) "new name.txt"))
      (should (equal (alist-get 'content renamed) "new\ncontent"))
      (should (eq (alist-get (intern "remove.me")
                             (alist-get 'files (cdr update)))
                  :null)))))

(ert-deftest magh-api-gist-refuses-to-remove-its-last-file ()
  (let ((context (magh-context-create)) updated failure)
    (cl-letf (((symbol-function 'magh-api--gist-get)
               (lambda (_context _id callback _errback &optional _force)
                 (funcall callback
                          '((files . ((only.txt . ((filename . "only.txt")))))))))
              ((symbol-function 'magh-api--gist-update)
               (lambda (&rest _) (setq updated t))))
      (magh-api--gist-file-remove
       context "GIST" "only.txt" #'ignore
       (lambda (error) (setq failure error))))
    (should-not updated)
    (should (eq (car failure) 'magh-invalid-input))))

(provide 'magh-api-test)
;;; magh-api-test.el ends here
