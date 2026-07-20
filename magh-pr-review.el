;;; magh-pr-review.el --- Optional emacs-pr-review bridge for magh.el -*- lexical-binding: t; -*-

;; Copyright (C) 2026
;; SPDX-License-Identifier: GPL-2.0-only

;; Author: magh.el contributors
;; Keywords: tools, vc, github

;;; Commentary:

;; When explicitly enabled, Pull Request candidates open in the external
;; emacs-pr-review package using their structured web URL.  magh.el itself keeps
;; no hard dependency on that package.

;;; Code:

(require 'magh-candidate)

(declare-function magh-forge-mode "magh-forge" (&optional arg))
(declare-function pr-review "pr-review" (url))
(defvar magh-forge-mode nil)

(defgroup magh-pr-review nil
  "Optional emacs-pr-review interoperability for magh.el."
  :group 'magh)

(defvar magh-pr-review--saved-action nil
  "Pull Request action present before `magh-pr-review-mode' was enabled.")

(defun magh-pr-review-open-resource (resource)
  "Open Pull Request RESOURCE with emacs-pr-review."
  (let ((url (magh-resource-url resource)))
    (unless (and (eq (plist-get resource :kind) 'pr) url)
      (user-error "emacs-pr-review requires a Pull Request URL"))
    (pr-review url)))

;;;###autoload
(define-minor-mode magh-pr-review-mode
  "Use emacs-pr-review as magh.el's default Pull Request viewer."
  :global t
  :group 'magh-pr-review
  (if magh-pr-review-mode
      (condition-case error
          (progn
            (unless (require 'pr-review nil t)
              (error "emacs-pr-review is not installed"))
            (when magh-forge-mode
              (magh-forge-mode -1))
            (setq magh-pr-review--saved-action
                  (copy-sequence (assq 'pr magh-resource-actions)))
            (setf (alist-get 'pr magh-resource-actions)
                  #'magh-pr-review-open-resource))
        (error
         (setq magh-pr-review-mode nil)
         (user-error "%s" (error-message-string error))))
    (if magh-pr-review--saved-action
        (setf (alist-get 'pr magh-resource-actions)
              (cdr magh-pr-review--saved-action))
      (setq magh-resource-actions (assq-delete-all 'pr magh-resource-actions)))
    (setq magh-pr-review--saved-action nil)))

(provide 'magh-pr-review)
;;; magh-pr-review.el ends here
