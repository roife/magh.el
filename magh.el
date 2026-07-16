;;; magh.el --- Magit-like GitHub frontend powered by gh -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; Author: magh.el contributors
;; Version: 0.1.0
;; URL: https://github.com/roife/magh.el
;; Keywords: tools, vc, github
;; Package-Requires: ((emacs "31.1") (magit "4.0.0") (consult "2.0")
;;                    (marginalia "1.0") (transient "0.7.0")
;;                    (markdown-mode "2.6"))

;;; Commentary:

;; magh.el brings GitHub resources into native, collapsible Emacs pages while
;; delegating authentication, hosts, and network transport to GitHub CLI.
;; `M-x magh' opens User Status; `M-x magh-dispatch' opens all entry points.

;;; Code:

(require 'magh-repo)
(require 'magh-issue)
(require 'magh-pr)
(require 'magh-actions)
(require 'magh-release)
(require 'magh-commit)
(require 'magh-browse)
(require 'magh-notify)
(require 'magh-search)
(require 'magh-pages)

(require 'magh-command)
(require 'magh-dispatch)

;;;###autoload
(defun magh (&optional dispatch)
  "Open GitHub User Status.
With prefix argument DISPATCH, open the top-level magh.el menu instead."
  (interactive "P")
  (if dispatch (call-interactively #'magh-dispatch) (magh-user-status)))

(provide 'magh)
;;; magh.el ends here
