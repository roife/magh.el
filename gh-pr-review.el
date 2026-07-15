;;; gh-pr-review.el --- Optional emacs-pr-review bridge for gh.el -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; Author: gh.el contributors
;; Keywords: tools, vc, github
;; Package-Requires: ((emacs "29.1"))

;;; Commentary:

;; When explicitly enabled, Pull Request candidates open in the external
;; emacs-pr-review package using their structured web URL.  gh.el itself keeps
;; no hard dependency on that package.

;;; Code:

(require 'gh-candidate)
(require 'gh-core)

(declare-function gh-forge-mode "gh-forge" (&optional arg))
(declare-function pr-review "pr-review" (url))

(defgroup gh-pr-review nil
  "Optional emacs-pr-review interoperability for gh.el."
  :group 'gh)

(defvar gh-pr-review--saved-action nil
  "Resource action present before `gh-pr-review-mode' was enabled.")

(defvar gh-pr-review--saved-action-present-p nil)

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
            (when (and (boundp 'gh-forge-mode) gh-forge-mode
                       (fboundp 'gh-forge-mode))
              (gh-forge-mode -1))
            (let ((cell (assq 'pr gh-resource-actions)))
              (setq gh-pr-review--saved-action-present-p (and cell t)
                    gh-pr-review--saved-action (cdr cell)))
            (setf (alist-get 'pr gh-resource-actions)
                  #'gh-pr-review-open-resource))
        (error
         (setq gh-pr-review-mode nil)
         (user-error "%s" (error-message-string error))))
    (if gh-pr-review--saved-action-present-p
        (setf (alist-get 'pr gh-resource-actions)
              gh-pr-review--saved-action)
      (setq gh-resource-actions (assq-delete-all 'pr gh-resource-actions)))
    (setq gh-pr-review--saved-action nil
          gh-pr-review--saved-action-present-p nil)))

(provide 'gh-pr-review)
;;; gh-pr-review.el ends here
