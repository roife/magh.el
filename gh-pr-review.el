;;; gh-pr-review.el --- Optional emacs-pr-review bridge for gh.el -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; Author: gh.el contributors
;; Keywords: tools, vc, github
;; Package-Requires: ((emacs "31.1"))

;;; Commentary:

;; When explicitly enabled, Pull Request candidates open in the external
;; emacs-pr-review package using their structured web URL.  gh.el itself keeps
;; no hard dependency on that package.

;;; Code:

(require 'gh-candidate)

(declare-function gh-forge-mode "gh-forge" (&optional arg))
(declare-function pr-review "pr-review" (url))
(defvar gh-forge-mode nil)

(defgroup gh-pr-review nil
  "Optional emacs-pr-review interoperability for gh.el."
  :group 'gh)

(defvar gh-pr-review--saved-action nil
  "Pull Request action present before `gh-pr-review-mode' was enabled.")

(defun gh-pr-review-open-resource (resource)
  "Open Pull Request RESOURCE with emacs-pr-review."
  (let ((url (gh-resource-url resource)))
    (unless (and (eq (plist-get resource :kind) 'pr) url)
      (user-error "emacs-pr-review requires a Pull Request URL"))
    (pr-review url)))

;;;###autoload
(define-minor-mode gh-pr-review-mode
  "Use emacs-pr-review as gh.el's default Pull Request viewer."
  :global t
  :group 'gh-pr-review
  (if gh-pr-review-mode
      (condition-case error
          (progn
            (unless (require 'pr-review nil t)
              (error "emacs-pr-review is not installed"))
            (when gh-forge-mode
              (gh-forge-mode -1))
            (setq gh-pr-review--saved-action
                  (copy-sequence (assq 'pr gh-resource-actions)))
            (setf (alist-get 'pr gh-resource-actions)
                  #'gh-pr-review-open-resource))
        (error
         (setq gh-pr-review-mode nil)
         (user-error "%s" (error-message-string error))))
    (if gh-pr-review--saved-action
        (setf (alist-get 'pr gh-resource-actions)
              (cdr gh-pr-review--saved-action))
      (setq gh-resource-actions (assq-delete-all 'pr gh-resource-actions)))
    (setq gh-pr-review--saved-action nil)))

(provide 'gh-pr-review)
;;; gh-pr-review.el ends here
