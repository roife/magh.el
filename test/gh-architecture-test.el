;;; gh-architecture-test.el --- Layering invariants for gh.el -*- lexical-binding: t; -*-

(require 'gh-test-helper)

(defconst gh-test-resource-files
  '("gh-repo.el" "gh-issue.el" "gh-pr.el" "gh-actions.el"
    "gh-release.el" "gh-commit.el" "gh-browse.el" "gh-notify.el"
    "gh-search.el" "gh-pages.el"))

(defun gh-test-file-string (name)
  "Read workspace file NAME as a string."
  (with-temp-buffer
    (insert-file-contents (expand-file-name name gh-test-root))
    (buffer-string)))

(ert-deftest gh-architecture-only-client-starts-gh-processes ()
  (dolist (file gh-test-resource-files)
    (let ((text (gh-test-file-string file)))
      (should-not (string-match-p
                   "\\_<\\(?:make-process\\|start-process\\|call-process\\|shell-command\\)\\_>"
                   text)))))

(ert-deftest gh-architecture-resource-modules-do-not-call-client-directly ()
  (dolist (file (append gh-test-resource-files '("gh-magit.el")))
    (should-not (string-match-p "gh-client-"
                                (gh-test-file-string file)))))

(ert-deftest gh-architecture-resource-modules-do-not-use-sync-helper ()
  (dolist (file gh-test-resource-files)
    (should-not (string-match-p "gh-client--request-sync"
                                (gh-test-file-string file)))))

(ert-deftest gh-ui-modules-do-not-format-fixed-width-string-columns ()
  (dolist (file (append gh-test-resource-files '("gh-magit.el" "gh-ui.el")))
    (should-not
     (string-match-p
      "(format[ \t\n]+\"[^\"]*%[-]?[0-9]+s"
      (gh-test-file-string file)))))

(ert-deftest gh-architecture-core-does-not-load-optional-integrations ()
  (dolist (file '("gh.el" "gh-core.el" "gh-client.el" "gh-api.el"
                  "gh-ui.el" "gh-candidate.el"))
    (should-not
     (string-match-p
      "(require[ \t\n]+['\"]\\(?:forge\\|embark\\|pr-review\\|nerd-icons\\)"
      (gh-test-file-string file)))))

(ert-deftest gh-documentation-backtick-symbols-resolve ()
  (require 'gh)
  (require 'gh-embark)
  (require 'gh-forge)
  (require 'gh-magit)
  (require 'gh-pr-review)
  (let (missing)
    (dolist (file '("README.md" "doc/ARCH.md" "doc/FUNCTIONALITY.md"
                    "doc/UI.md"))
      (let ((text (gh-test-file-string file))
            (position 0))
        (while (string-match "`\\(gh-[[:alnum:]-]+\\)`" text position)
          (let ((symbol (intern (match-string 1 text))))
            (unless (or (fboundp symbol) (boundp symbol)
                        (facep symbol) (featurep symbol)
                        (fboundp (intern (format "%s-p" symbol))))
              (push (cons file symbol) missing)))
          (setq position (match-end 0)))))
    (should-not (delete-dups missing))))

(provide 'gh-architecture-test)
;;; gh-architecture-test.el ends here
