;;; gh-api-test.el --- API contract tests for gh.el -*- lexical-binding: t; -*-

(require 'gh-test-helper)
(require 'gh-api)
(require 'gh-command)

(ert-deftest gh-api-issue-list-keeps-each-cli-argument-separate ()
  (let ((context (gh-context-from-repository "acme/widgets" "github.com"))
        captured)
    (cl-letf (((symbol-function 'gh-client--json-async)
               (lambda (argv _success _error &rest keys)
                 (setq captured (cons argv keys)) 'request)))
      (gh-api--issue-list
       context '(:state "open" :labels ("help wanted" "good first issue")
                 :assignee "octo cat" :limit 17)
       #'ignore #'ignore))
    (let ((argv (car captured)))
      (should (member "help wanted" argv))
      (should (member "good first issue" argv))
      (should (member "octo cat" argv))
      (should (equal (cadr (member "--repo" argv)) "acme/widgets"))
      (should (equal (cadr (member "--limit" argv)) "17")))))

(ert-deftest gh-api-rest-pagination-and-fields-are-typed ()
  (should
   (equal
    (gh-api--rest-argv "repos/o/r/items" "GET"
                       '((page . 2) (enabled . t) (name . "two words")) t)
    '("api" "repos/o/r/items" "--method" "GET" "--paginate" "--slurp"
      "-F" "page=2" "-F" "enabled=true" "-f" "name=two words"))))

(ert-deftest gh-api-paginated-content-contracts-are-normalized-once ()
  (should
   (equal (gh-api--flatten-pages
           '((((id . 1)) ((id . 2))) (((id . 3)))))
          '(((id . 1)) ((id . 2)) ((id . 3)))))
  (should
   (equal (gh-api--decode-content
           '((encoding . "base64") (content . "aGVs\r\nbG8=")))
          "hello"))
  (should
   (equal (gh-api--decode-content
           '((encoding . "utf-8") (content . "plain text")))
          "plain text")))

(ert-deftest gh-api-search-arguments-share-one-builder ()
  (should
   (equal
    (gh-api--search-argv
     'repos "two words"
     '(:owner "octo cat" :state "open"))
    (append
     '("search" "repos" "two words" "--limit")
     (list (number-to-string gh-list-limit)
           "--json" (alist-get 'repos gh-api--search-fields)
           "--owner" "octo cat" "--state" "open")))))

(ert-deftest gh-api-release-body-uses-stdin-not-a-shell-or-temporary-file ()
  (let ((context (gh-context-from-repository "o/r")) captured)
    (cl-letf (((symbol-function 'gh-client--mutate-text)
               (lambda (argv _success _error &rest keys)
                 (setq captured (cons argv keys)) 'request)))
      (gh-api--release-create
       context '(:tag "v1.0" :title "A release" :body "line 1\n$HOME `x`")
       #'ignore #'ignore))
    (should (member "--notes-file" (car captured)))
    (should (equal (plist-get (cdr captured) :stdin)
                   "line 1\n$HOME `x`"))))

(ert-deftest gh-api-valid-message-field-is-not-mistaken-for-an-error ()
  (should-not (gh-api--api-errors '((message . "ordinary commit message")
                                    (sha . "abc"))))
  (should (gh-api--api-errors '((message . "Not Found")
                                (status . "404")))))

(ert-deftest gh-api-json-mutation-surfaces-graphql-errors ()
  (let (success failure)
    (cl-letf (((symbol-function 'gh-client--mutate-json)
               (lambda (_argv callback _errback &rest _keys)
                 (funcall callback
                          '((errors . (((message . "Mutation rejected")))))))))
      (gh-api--mutate-json
       (gh-context-create) '("api" "graphql") nil
       (lambda (_) (setq success t)) (lambda (error) (setq failure error))))
    (should-not success)
    (should (eq (car failure) 'gh-api-error))))

(ert-deftest gh-api-rest-release-normalizes-public-contract ()
  (let ((release
         (gh-api--normalize-rest-release
          '((id . 7) (tag_name . "v1") (target_commitish . "main")
            (draft . nil) (prerelease . t) (html_url . "https://example/r")
            (assets . (((id . 8) (name . "a.zip") (download_count . 3)
                        (browser_download_url . "https://example/a"))))))))
    (should (equal (alist-get 'tagName release) "v1"))
    (should (alist-get 'isPrerelease release))
    (should (= (alist-get
                'downloadCount (car (alist-get 'assets release))) 3))))

(ert-deftest gh-api-project-completion-uses-owner-and-json-output ()
  (let ((context (gh-context-from-repository "acme/widgets" "github.com"))
        captured)
    (cl-letf (((symbol-function 'gh-client--json-async)
               (lambda (argv _success _error &rest keys)
                 (setq captured (cons argv keys)) 'request)))
      (gh-api--project-list context #'ignore #'ignore))
    (should (equal (seq-take (car captured) 4)
                   '("project" "list" "--owner" "acme")))
    (should (member "--format" (car captured)))
    (should (equal (cadr (member "--format" (car captured))) "json"))))

(ert-deftest gh-api-cli-json-field-contracts-distinguish-list-and-view ()
  (should (memq 'startedAt gh-api--run-fields))
  (should-not (memq 'startedTime gh-api--run-fields))
  (should (memq 'isLatest gh-api--release-list-fields))
  (should-not (memq 'assets gh-api--release-list-fields))
  (should (memq 'assets gh-api--release-view-fields))
  (should-not (memq 'isLatest gh-api--release-view-fields)))

(ert-deftest gh-api-explicit-json-false-never-enables-cli-switches ()
  (let ((context (gh-context-from-repository "acme/widgets")) captured)
    (cl-letf (((symbol-function 'gh-client--mutate-text)
               (lambda (argv _success _error &rest keys)
                 (setq captured (cons argv keys)) 'request)))
      (gh-api--repo-create
       context '(:name "new" :public t :push :json-false :clone :json-false)
       #'ignore #'ignore)
      (should (member "--public" (car captured)))
      (should-not (member "--push" (car captured)))
      (should-not (member "--clone" (car captured)))
      (gh-api--pr-create
       context '(:title "PR" :base "main" :head "topic"
                 :draft :json-false :body "")
       #'ignore #'ignore)
      (should-not (member "--draft" (car captured)))
      (gh-api--release-edit
       context "v1" '(:draft :json-false :prerelease :json-false)
       #'ignore #'ignore)
      (should (member "--draft=false" (car captured)))
      (should (member "--prerelease=false" (car captured)))
      (gh-api--repo-edit
       context '(:issues nil :wiki t) #'ignore #'ignore)
      (should (member "--enable-issues=false" (car captured)))
      (should (member "--enable-wiki=true" (car captured))))))

(ert-deftest gh-api-generic-output-preserves-json-false ()
  (let ((context (gh-context-create :host "github.com")) captured rendered)
    (cl-letf (((symbol-function 'gh-client--json-async)
               (lambda (_argv success _error &rest keys)
                 (setq captured keys)
                 (funcall success '((enabled . :json-false) (missing)))
                 'request)))
      (gh-api--generic-request
       context "example" "GET" nil nil
       (lambda (data) (setq rendered (gh-command--json-string data))) #'ignore))
    (should (eq (plist-get captured :json-false-object) :json-false))
    (should (string-match-p "\\\"enabled\\\"[[:space:]]*:[[:space:]]*false"
                            rendered))
    (should (string-match-p "\\\"missing\\\"[[:space:]]*:[[:space:]]*null"
                            rendered))))

(ert-deftest gh-api-review-uses-add-thread-for-file-comments ()
  (let ((context (gh-context-from-repository "acme/widgets"))
        reads mutations finished failure)
    (cl-letf (((symbol-function 'gh-api--pr-get)
               (lambda (_context _number callback _errback &optional _force)
                 (funcall callback '((id . "PR_node"))) 'pr-request))
              ((symbol-function 'gh-client--json-async)
               (lambda (argv success _error &rest keys)
                 (push (cons argv keys) reads)
                 (funcall success
                          '((data . ((node . ((reviews . ((nodes)))))))))
                 'read-request))
              ((symbol-function 'gh-client--mutate-json)
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
      (gh-api--pr-review
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
           (input (gh-api--json-at payload 'variables 'input)))
      (should (equal (alist-get 'event input) "APPROVE")))))

(ert-deftest gh-api-review-rolls-back-a-new-pending-review-on-thread-error ()
  (let ((context (gh-context-from-repository "acme/widgets"))
        (thread-error (gh-core--error 'gh-api-error "Thread rejected"))
        queries failure finished)
    (cl-letf (((symbol-function 'gh-api--pr-get)
               (lambda (_context _number callback _errback &optional _force)
                 (funcall callback '((id . "PR_node"))) 'pr-request))
              ((symbol-function 'gh-client--json-async)
               (lambda (_argv success _error &rest _keys)
                 (funcall success
                          '((data . ((node . ((reviews . ((nodes)))))))))
                 'read-request))
              ((symbol-function 'gh-client--mutate-json)
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
      (gh-api--pr-review
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

(provide 'gh-api-test)
;;; gh-api-test.el ends here
