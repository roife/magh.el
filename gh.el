;;; gh.el --- Magit-like GitHub frontend powered by gh -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; Author: gh.el contributors
;; Version: 0.1.0
;; URL: https://github.com/roife/gh.el
;; Keywords: tools, vc, github
;; Package-Requires: ((emacs "31.1") (magit "4.0.0") (consult "2.0")
;;                    (marginalia "1.0") (transient "0.7.0")
;;                    (markdown-mode "2.6"))

;;; Commentary:

;; gh.el brings GitHub resources into native, collapsible Emacs pages while
;; delegating authentication, hosts, and network transport to GitHub CLI.
;; `M-x gh' opens User Status; `M-x gh-dispatch' opens all entry points.

;;; Code:

(require 'gh-repo)
(require 'gh-issue)
(require 'gh-pr)
(require 'gh-actions)
(require 'gh-release)
(require 'gh-commit)
(require 'gh-browse)
(require 'gh-notify)
(require 'gh-search)
(require 'gh-pages)

(require 'gh-command)
(require 'gh-dispatch)

;;;###autoload
(defun gh (&optional dispatch)
  "Open GitHub User Status.
With prefix argument DISPATCH, open the top-level gh.el menu instead."
  (interactive "P")
  (if dispatch (call-interactively #'gh-dispatch) (gh-user-status)))

(provide 'gh)
;;; gh.el ends here
