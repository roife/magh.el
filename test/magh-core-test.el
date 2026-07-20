;;; magh-core-test.el --- Core tests for magh.el -*- lexical-binding: t; -*-

(require 'magh-test-helper)
(require 'magh-core)
(require 'magh-candidate)

(ert-deftest magh-core-context-parses-supported-remotes ()
  (dolist (case '(("git@github.example.com:owner/repo.git"
                   "github.example.com" "owner/repo")
                  ("ssh://git@github.com/owner/repo.git"
                   "github.com" "owner/repo")
                  ("https://github.com/owner/repo.git"
                   "github.com" "owner/repo")))
    (let ((context (magh-context-from-repository (car case))))
      (should (equal (magh-context-host context) (nth 1 case)))
      (should (equal (magh-context-repository context) (nth 2 case))))))

(ert-deftest magh-core-url-path-encodes-components-not-slashes ()
  (should (equal (magh-core--url-path "dir/a b#.el") "dir/a%20b%23.el")))

(ert-deftest magh-core-error-messages-hide-condition-internals ()
  (let ((error (magh-core--error 'magh-command-error "Request failed"
                                 '(:exit-code 1)))
        reported)
    (should (equal (magh-error-message error) "Request failed"))
    (cl-letf (((symbol-function 'message)
               (lambda (format-string &rest arguments)
                 (setq reported (apply #'format format-string arguments)))))
      (should (equal (magh-core--user-error error) "magh: Request failed")))
    (should (equal reported "magh: Request failed"))))

(ert-deftest magh-core-read-repository-prompts-only-when-needed ()
  (let ((local (magh-context-from-repository "local/repo")) prompted)
    (cl-letf (((symbol-function 'magh-context-resolve)
               (lambda (&rest _) local))
              ((symbol-function 'completing-read)
               (lambda (&rest _) (setq prompted t) "ignored/repo")))
      (should (eq (magh-context-read-repository) local))
      (should-not prompted)))
  (let ((magh-known-repositories '("known/repo")) collection)
    (cl-letf (((symbol-function 'magh-context-resolve)
               (lambda (&rest _) (magh-context-create :host "example.com")))
              ((symbol-function 'completing-read)
               (lambda (_prompt values &rest _)
                 (setq collection values)
                 "chosen/repo")))
      (let ((context (magh-context-read-repository)))
        (should (equal collection '("known/repo")))
        (should (equal (magh-context-host context) "example.com"))
        (should (equal (magh-context-repository context) "chosen/repo"))))))

(ert-deftest magh-core-comment-count-accepts-cli-and-graphql-shapes ()
  (should (= (magh-core--comments-count
              '((commentsCount . 4) (comments . (((id . 1))))))
             4))
  (should (= (magh-core--comments-count
              '((comments . ((totalCount . 3)))))
             3))
  (should (= (magh-core--comments-count
              '((comments . (((id . 1)) ((id . 2))))))
             2)))

(ert-deftest magh-core-settled-batch-keeps-successes-next-to-errors ()
  (let ((failure (magh-core--error 'magh-api-error "Issues unavailable"))
        result)
    (magh-core--collect-async-settled
     (list
      (cons 'repository
            (lambda (success _error)
              (funcall success '((nameWithOwner . "o/r")))))
      (cons 'issues
            (lambda (_success error) (funcall error failure))))
     (lambda (value) (setq result value)))
    (should (magh-batch-result-p result))
    (should (equal (magh-batch-value result 'repository)
                   '((nameWithOwner . "o/r"))))
    (should (equal (magh-batch-error result 'issues) failure))))

(ert-deftest magh-core-pages-append-items-and-adopt-the-continuation ()
  (let ((page (magh-page-append
               (magh-page-create :items '(1 2) :next "old")
               (magh-page-create :items '(3 4) :next "new"))))
    (should (equal (magh-page-items page) '(1 2 3 4)))
    (should (equal (magh-page-next page) "new"))))

(ert-deftest magh-core-local-context-follows-and-can-switch-git-remotes ()
  (skip-unless (executable-find "git"))
  (let ((root (make-temp-file "magh-remotes-" t)))
    (unwind-protect
        (cl-labels
            ((git (&rest args)
               (let ((default-directory root))
                 (should (zerop (apply #'process-file "git" nil nil nil args))))))
          (git "init" "--quiet")
          (git "symbolic-ref" "HEAD" "refs/heads/main")
          (git "remote" "add" "origin"
               "git@github.com:upstream/widgets.git")
          (git "remote" "add" "fork"
               "git@github.com:alice/widgets.git")
          (git "symbolic-ref" "refs/remotes/origin/HEAD"
               "refs/remotes/origin/main")
          (git "symbolic-ref" "refs/remotes/fork/HEAD"
               "refs/remotes/fork/trunk")
          (git "config" "branch.main.remote" "fork")
          (let ((inferred (magh-core--local-context root))
                (origin (magh-context-from-local-remote root "origin")))
            (should (equal (magh-context-remote inferred) "fork"))
            (should (equal (magh-context-repository inferred) "alice/widgets"))
            (should (equal (magh-context-default-branch inferred) "trunk"))
            (should (equal (magh-context-remote origin) "origin"))
            (should (equal (magh-context-repository origin)
                           "upstream/widgets"))
            (should (equal (magh-context-default-branch origin) "main"))
            (should (equal (mapcar #'car (magh-context-local-remotes inferred))
                           '("fork" "origin")))))
      (delete-directory root t))))

(ert-deftest magh-candidate-url-produces-structured-native-resources ()
  (let ((issue (magh-resource-from-url
                "https://github.com/acme/widgets/issues/42"))
        (run (magh-resource-from-url
              "https://github.com/acme/widgets/actions/runs/99"))
        (workflow (magh-resource-from-url
                   "https://github.com/acme/widgets/actions/workflows/ci.yml")))
    (should (eq (plist-get issue :kind) 'issue))
    (should (= (plist-get issue :number) 42))
    (should (equal (plist-get issue :repository) "acme/widgets"))
    (should (eq (plist-get run :kind) 'run))
    (should (= (plist-get run :id) 99))
    (should (eq (plist-get workflow :kind) 'workflow))
    (should (equal (plist-get workflow :id) "ci.yml"))))

(ert-deftest magh-candidate-display-and-action-data-stay-separated ()
  (let* ((resource (magh-resource-create
                    'issue (magh-context-from-repository "o/r")
                    :number 7 :title "same"))
         (candidate (magh-candidate-string "same" resource 'magh-issue 3)))
    (should (equal (substring-no-properties candidate) (concat "same\0" "3")))
    (should (eq (get-text-property 0 'magh-resource candidate) resource))
    (should (eq (get-text-property 0 'category candidate) 'magh-issue))))

(provide 'magh-core-test)
;;; magh-core-test.el ends here
