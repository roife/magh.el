;;; magh-architecture-test.el --- Layering invariants for magh.el -*- lexical-binding: t; -*-

(require 'magh-test-helper)

(defconst magh-test-resource-files
  '("magh-repo.el" "magh-issue.el" "magh-pr.el" "magh-actions.el"
    "magh-release.el" "magh-commit.el" "magh-browse.el" "magh-notify.el"
    "magh-search.el" "magh-pages.el"))

(defun magh-test-file-string (name)
  "Read workspace file NAME as a string."
  (with-temp-buffer
    (insert-file-contents (expand-file-name name magh-test-root))
    (buffer-string)))

(ert-deftest magh-architecture-only-client-starts-cli-processes ()
  (dolist (file magh-test-resource-files)
    (let ((text (magh-test-file-string file)))
      (should-not (string-match-p
                   "\\_<\\(?:make-process\\|start-process\\|call-process\\|shell-command\\)\\_>"
                   text)))))

(ert-deftest magh-architecture-resource-modules-do-not-call-client-directly ()
  (dolist (file (append magh-test-resource-files '("magh-magit.el")))
    (should-not (string-match-p "magh-client-"
                                (magh-test-file-string file)))))

(ert-deftest magh-architecture-resource-modules-do-not-use-sync-helper ()
  (dolist (file magh-test-resource-files)
    (should-not (string-match-p "magh-client--request-sync"
                                (magh-test-file-string file)))))

(ert-deftest magh-ui-modules-do-not-format-fixed-width-string-columns ()
  (dolist (file (append magh-test-resource-files '("magh-magit.el" "magh-ui.el")))
    (should-not
     (string-match-p
      "(format[ \t\n]+\"[^\"]*%[-]?[0-9]+s"
      (magh-test-file-string file)))))

(ert-deftest magh-architecture-core-does-not-load-optional-integrations ()
  (dolist (file '("magh.el" "magh-core.el" "magh-client.el" "magh-api.el"
                  "magh-ui.el" "magh-candidate.el"))
    (should-not
     (string-match-p
      "(require[ \t\n]+['\"]\\(?:forge\\|embark\\|pr-review\\|nerd-icons\\)"
      (magh-test-file-string file)))))

(ert-deftest magh-public-dispatches-have-autoload-cookies ()
  (dolist (entry '(("magh-dispatch.el" . magh-dispatch)
                   ("magh-search.el" . magh-search-dispatch)
                   ("magh-notify.el" . magh-notifications-dispatch)))
    (should
     (string-match-p
      (format ";;;###autoload[ \t\n]+(transient-define-prefix[ \t\n]+%s\\_>"
              (regexp-quote (symbol-name (cdr entry))))
      (magh-test-file-string (car entry))))))

(ert-deftest magh-documentation-backtick-symbols-resolve ()
  (require 'magh)
  (require 'magh-embark)
  (require 'magh-forge)
  (require 'magh-magit)
  (require 'magh-pr-review)
  (let (missing)
    (dolist (file '("README.md"))
      (let ((text (magh-test-file-string file))
            (position 0))
        (while (string-match "`\\(magh-[[:alnum:]-]+\\)`" text position)
          (let ((symbol (intern (match-string 1 text))))
            (unless (or (fboundp symbol) (boundp symbol)
                        (facep symbol) (featurep symbol)
                        (fboundp (intern (format "%s-p" symbol))))
              (push (cons file symbol) missing)))
          (setq position (match-end 0)))))
    (should-not (delete-dups missing))))

(provide 'magh-architecture-test)
;;; magh-architecture-test.el ends here
