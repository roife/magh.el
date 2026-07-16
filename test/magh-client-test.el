;;; magh-client-test.el --- Async transport tests for magh.el -*- lexical-binding: t; -*-

(require 'magh-test-helper)
(require 'magh-client)

(ert-deftest magh-client-fast-process-is-delivered-exactly-once ()
  (magh-test-with-clean-client
    (dotimes (_ 20)
      (let ((count 0) result failure)
        (magh-client--json-async
         '("fast-json")
         (lambda (value) (setq result value) (cl-incf count))
         (lambda (error) (setq failure error) (cl-incf count))
         :cache nil :dedupe nil)
        (magh-test-wait (lambda () (= count 1)))
        (should-not failure)
        (should (alist-get 'ok result))
        (accept-process-output nil 0.01)
        (should (= count 1))))))

(ert-deftest magh-client-preserves-argv-host-and-json-false-contract ()
  (magh-test-with-clean-client
    (let (result failure)
      (magh-client--json-async
       '("echo-json" "two words" "$(never-evaluated)")
       (lambda (value) (setq result value))
       (lambda (error) (setq failure error))
       :context (magh-context-create :host "github.example.com") :cache nil)
      (magh-test-wait (lambda () (or result failure)))
      (should-not failure)
      (should (equal (alist-get 'argv result)
                     '("two words" "$(never-evaluated)")))
      (should (equal (alist-get 'host result) "github.example.com")))
    (let (result)
      (magh-client--json-async '("fast-json") (lambda (value) (setq result value))
                             #'ignore :cache nil)
      (magh-test-wait (lambda () result))
      (should-not (alist-get 'disabled result)))))

(ert-deftest magh-client-generic-json-can-distinguish-false-from-null ()
  (magh-test-with-clean-client
    (let (default preserved)
      (magh-client--json-async
       '("fast-json") (lambda (value) (setq default value)) #'ignore)
      (magh-test-wait (lambda () default))
      (magh-client--json-async
       '("fast-json") (lambda (value) (setq preserved value)) #'ignore
       :json-false-object :json-false)
      (magh-test-wait (lambda () preserved))
      (should-not (alist-get 'disabled default))
      (should (eq (alist-get 'disabled preserved) :json-false))
      (should (= (magh-client-cache-size) 2)))))

(ert-deftest magh-client-cache-and-inflight-deduplicate-asynchronously ()
  (magh-test-with-clean-client
    (let (first second first-result second-result)
      (setq first
            (magh-client--json-async
             '("delay-json" "0.12") (lambda (value) (setq first-result value))
             #'ignore))
      (setq second
            (magh-client--json-async
             '("delay-json" "0.12") (lambda (value) (setq second-result value))
             #'ignore))
      (should (eq first second))
      (should (= (magh-client-inflight-size) 1))
      (magh-test-wait (lambda () (and first-result second-result)))
      (should (= (magh-client-cache-size) 1))
      (let (cached callback-ran)
        (setq cached
              (magh-client--json-async
               '("delay-json" "0.12")
               (lambda (_value) (setq callback-ran t)) #'ignore))
        (should-not cached)
        (should-not callback-ran)
        (magh-test-wait (lambda () callback-ran))))))

(ert-deftest magh-client-cancellation-is-typed ()
  (magh-test-with-clean-client
    (let (failure success)
      (let ((request
             (magh-client--json-async
              '("delay-json" "1") (lambda (_) (setq success t))
              (lambda (error) (setq failure error)) :cache nil :dedupe nil)))
        (should (magh-client-cancel request)))
      (magh-test-wait (lambda () failure))
      (should-not success)
      (should (eq (car failure) 'magh-cancelled)))))

(ert-deftest magh-client-errors-redact-token-shaped-values ()
  (magh-test-with-clean-client
    (let (failure)
      (magh-client--text-async '("fail" "1") #'ignore
                             (lambda (error) (setq failure error)) :cache nil)
      (magh-test-wait (lambda () failure))
      (should (eq (car failure) 'magh-command-error))
      (should (string-match-p "<redacted>" (magh-error-message failure)))
      (should-not (string-match-p "SECRET" (magh-error-message failure))))))

(ert-deftest magh-client-invalid-json-is-typed ()
  (magh-test-with-clean-client
    (let (failure)
      (magh-client--json-async '("invalid-json") #'ignore
                             (lambda (error) (setq failure error)) :cache nil)
      (magh-test-wait (lambda () failure))
      (should (eq (car failure) 'magh-json-error)))))

(ert-deftest magh-client-missing-executable-cleans-transport-buffers ()
  (magh-test-with-clean-client
    (let ((magh-executable "/missing/gh") failure)
      (magh-client--text-async
       '("version") #'ignore (lambda (error) (setq failure error))
       :cache nil :dedupe nil)
      (magh-test-wait (lambda () failure))
      (should (eq (car failure) 'magh-missing-executable))
      (should-not
       (cl-find-if
        (lambda (buffer)
          (string-match-p "\\` \\*gh \\(?:stdout\\|stderr\\)\\*"
                          (buffer-name buffer)))
        (buffer-list))))))

(ert-deftest magh-client-stream-is-cancellable-and-delivers-complete-text ()
  (magh-test-with-clean-client
    (let (chunks result)
      (magh-client--stream
       '("stream")
       (lambda (chunk _request) (push chunk chunks))
       (lambda (text) (setq result text)) #'ignore)
      (magh-test-wait (lambda () result))
      (should (equal result "alpha-beta-gamma"))
      (should (equal (apply #'concat (nreverse chunks)) result)))))

(ert-deftest magh-client-does-not-call-back-into-killed-source-buffer ()
  (magh-test-with-clean-client
    (let ((source (generate-new-buffer " *gh dead source*")) called)
      (magh-client--json-async
       '("delay-json" "0.08") (lambda (_) (setq called t))
       (lambda (_) (setq called t)) :source-buffer source :cache nil)
      (kill-buffer source)
      (magh-test-wait (lambda () (zerop (magh-client-inflight-size))))
      (should-not called))))

(provide 'magh-client-test)
;;; magh-client-test.el ends here
