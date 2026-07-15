;;; gh-core-test.el --- Core tests for gh.el -*- lexical-binding: t; -*-

(require 'gh-test-helper)
(require 'gh-core)
(require 'gh-candidate)

(ert-deftest gh-core-context-parses-supported-remotes ()
  (dolist (case '(("git@github.example.com:owner/repo.git"
                   "github.example.com" "owner/repo")
                  ("ssh://git@github.com/owner/repo.git"
                   "github.com" "owner/repo")
                  ("https://github.com/owner/repo.git"
                   "github.com" "owner/repo")))
    (let ((context (gh-context-from-repository (car case))))
      (should (equal (gh-context-host context) (nth 1 case)))
      (should (equal (gh-context-repository context) (nth 2 case))))))

(ert-deftest gh-core-context-copy-does-not-mutate-source ()
  (let* ((source (gh-context-from-repository "owner/repo" "github.com"))
         (copy (gh-context-copy source :ref "feature/topic" :path "/lisp/a.el")))
    (should-not (gh-context-ref source))
    (should (equal (gh-context-ref copy) "feature/topic"))
    (should (equal (gh-context-path copy) "lisp/a.el"))))

(ert-deftest gh-core-url-path-encodes-components-not-slashes ()
  (should (equal (gh-core--url-path "dir/a b#.el") "dir/a%20b%23.el")))

(ert-deftest gh-core-empty-git-output-is-absent ()
  (cl-letf (((symbol-function 'executable-find) (lambda (_) "/usr/bin/git"))
            ((symbol-function 'process-file) (lambda (&rest _) 0)))
    (should-not (gh-core--git-output default-directory
                                     "branch" "--show-current"))))

(ert-deftest gh-core-comment-count-accepts-cli-and-graphql-shapes ()
  (should (= (gh-core--comments-count
              '((commentsCount . 4) (comments . (((id . 1))))))
             4))
  (should (= (gh-core--comments-count
              '((comments . ((totalCount . 3)))))
             3))
  (should (= (gh-core--comments-count
              '((comments . (((id . 1)) ((id . 2))))))
             2)))

(ert-deftest gh-candidate-url-produces-structured-native-resources ()
  (let ((issue (gh-resource-from-url
                "https://github.com/acme/widgets/issues/42"))
        (run (gh-resource-from-url
              "https://github.com/acme/widgets/actions/runs/99"))
        (workflow (gh-resource-from-url
                   "https://github.com/acme/widgets/actions/workflows/ci.yml")))
    (should (eq (plist-get issue :kind) 'issue))
    (should (= (plist-get issue :number) 42))
    (should (equal (plist-get issue :repository) "acme/widgets"))
    (should (eq (plist-get run :kind) 'run))
    (should (= (plist-get run :id) 99))
    (should (eq (plist-get workflow :kind) 'workflow))
    (should (equal (plist-get workflow :id) "ci.yml"))))

(ert-deftest gh-candidate-display-and-action-data-stay-separated ()
  (let* ((resource (gh-resource-create
                    'issue (gh-context-from-repository "o/r")
                    :number 7 :title "same"))
         (candidate (gh-candidate-string "same" resource 'gh-issue 3)))
    (should (equal (substring-no-properties candidate) (concat "same\0" "3")))
    (should (eq (get-text-property 0 'gh-resource candidate) resource))
    (should (eq (get-text-property 0 'category candidate) 'gh-issue))))

(provide 'gh-core-test)
;;; gh-core-test.el ends here
