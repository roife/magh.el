;;; gh-client-test.el --- Async transport tests for gh.el -*- lexical-binding: t; -*-

(require 'gh-test-helper)
(require 'gh-client)

(ert-deftest gh-client-fast-process-is-delivered-exactly-once ()
  (gh-test-with-clean-client
    (dotimes (_ 20)
      (let ((count 0) result failure)
        (gh-client--json-async
         '("fast-json")
         (lambda (value) (setq result value) (cl-incf count))
         (lambda (error) (setq failure error) (cl-incf count))
         :cache nil :dedupe nil)
        (gh-test-wait (lambda () (= count 1)))
        (should-not failure)
        (should (alist-get 'ok result))
        (accept-process-output nil 0.01)
        (should (= count 1))))))

(ert-deftest gh-client-preserves-argv-host-and-json-false-contract ()
  (gh-test-with-clean-client
    (let (result failure)
      (gh-client--json-async
       '("echo-json" "two words" "$(never-evaluated)")
       (lambda (value) (setq result value))
       (lambda (error) (setq failure error))
       :context (gh-context-create :host "github.example.com") :cache nil)
      (gh-test-wait (lambda () (or result failure)))
      (should-not failure)
      (should (equal (alist-get 'argv result)
                     '("two words" "$(never-evaluated)")))
      (should (equal (alist-get 'host result) "github.example.com")))
    (let (result)
      (gh-client--json-async '("fast-json") (lambda (value) (setq result value))
                             #'ignore :cache nil)
      (gh-test-wait (lambda () result))
      (should-not (alist-get 'disabled result)))))

(ert-deftest gh-client-generic-json-can-distinguish-false-from-null ()
  (gh-test-with-clean-client
    (let (default preserved)
      (gh-client--json-async
       '("fast-json") (lambda (value) (setq default value)) #'ignore)
      (gh-test-wait (lambda () default))
      (gh-client--json-async
       '("fast-json") (lambda (value) (setq preserved value)) #'ignore
       :json-false-object :json-false)
      (gh-test-wait (lambda () preserved))
      (should-not (alist-get 'disabled default))
      (should (eq (alist-get 'disabled preserved) :json-false))
      (should (= (gh-client-cache-size) 2)))))

(ert-deftest gh-client-cache-and-inflight-deduplicate-asynchronously ()
  (gh-test-with-clean-client
    (let (first second first-result second-result)
      (setq first
            (gh-client--json-async
             '("delay-json" "0.12") (lambda (value) (setq first-result value))
             #'ignore))
      (setq second
            (gh-client--json-async
             '("delay-json" "0.12") (lambda (value) (setq second-result value))
             #'ignore))
      (should (eq first second))
      (should (= (gh-client-inflight-size) 1))
      (gh-test-wait (lambda () (and first-result second-result)))
      (should (= (gh-client-cache-size) 1))
      (let (cached callback-ran)
        (setq cached
              (gh-client--json-async
               '("delay-json" "0.12")
               (lambda (_value) (setq callback-ran t)) #'ignore))
        (should-not cached)
        (should-not callback-ran)
        (gh-test-wait (lambda () callback-ran))))))

(ert-deftest gh-client-cancellation-is-typed ()
  (gh-test-with-clean-client
    (let (failure success)
      (let ((request
             (gh-client--json-async
              '("delay-json" "1") (lambda (_) (setq success t))
              (lambda (error) (setq failure error)) :cache nil :dedupe nil)))
        (should (gh-client-cancel request)))
      (gh-test-wait (lambda () failure))
      (should-not success)
      (should (eq (car failure) 'gh-cancelled)))))

(ert-deftest gh-client-errors-redact-token-shaped-values ()
  (gh-test-with-clean-client
    (let (failure)
      (gh-client--text-async '("fail" "1") #'ignore
                             (lambda (error) (setq failure error)) :cache nil)
      (gh-test-wait (lambda () failure))
      (should (eq (car failure) 'gh-command-error))
      (should (string-match-p "<redacted>" (gh-error-message failure)))
      (should-not (string-match-p "SECRET" (gh-error-message failure))))))

(ert-deftest gh-client-invalid-json-is-typed ()
  (gh-test-with-clean-client
    (let (failure)
      (gh-client--json-async '("invalid-json") #'ignore
                             (lambda (error) (setq failure error)) :cache nil)
      (gh-test-wait (lambda () failure))
      (should (eq (car failure) 'gh-json-error)))))

(ert-deftest gh-client-missing-executable-cleans-transport-buffers ()
  (gh-test-with-clean-client
    (let ((gh-executable "/missing/gh") failure)
      (gh-client--text-async
       '("version") #'ignore (lambda (error) (setq failure error))
       :cache nil :dedupe nil)
      (gh-test-wait (lambda () failure))
      (should (eq (car failure) 'gh-missing-executable))
      (should-not
       (cl-find-if
        (lambda (buffer)
          (string-match-p "\\` \\*gh \\(?:stdout\\|stderr\\)\\*"
                          (buffer-name buffer)))
        (buffer-list))))))

(ert-deftest gh-client-stream-is-cancellable-and-delivers-complete-text ()
  (gh-test-with-clean-client
    (let (chunks result)
      (gh-client--stream
       '("stream")
       (lambda (chunk _request) (push chunk chunks))
       (lambda (text) (setq result text)) #'ignore)
      (gh-test-wait (lambda () result))
      (should (equal result "alpha-beta-gamma"))
      (should (equal (apply #'concat (nreverse chunks)) result)))))

(ert-deftest gh-client-does-not-call-back-into-killed-source-buffer ()
  (gh-test-with-clean-client
    (let ((source (generate-new-buffer " *gh dead source*")) called)
      (gh-client--json-async
       '("delay-json" "0.08") (lambda (_) (setq called t))
       (lambda (_) (setq called t)) :source-buffer source :cache nil)
      (kill-buffer source)
      (gh-test-wait (lambda () (zerop (gh-client-inflight-size))))
      (should-not called))))

(provide 'gh-client-test)
;;; gh-client-test.el ends here
