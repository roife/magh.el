;;; magh-diff.el --- Shared GitHub diff primitives -*- lexical-binding: t; -*-

;; Copyright (C) 2026
;; SPDX-License-Identifier: GPL-2.0-only

;; Author: magh.el contributors
;; Keywords: tools, vc, github

;;; Commentary:

;; Patch parsing, foldable hunk insertion, and resource-aware diff selections
;; shared by Commit comments and Pull Request reviews.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'magh-candidate)
(require 'magh-ui)

(defun magh-diff--parse-patch-lines
    (patch context number head path &optional resource-kind)
  "Parse PATCH into display records with GitHub review coordinates."
  (let ((old-line 0)
        (new-line 0)
        (hunk 0)
        diff-position
        records)
    (dolist (text (split-string (magh-ui--normalize-newlines (or patch ""))
                                "\n" nil))
      (let (line side location hunk-heading)
        (cond
         ((string-match
           "^@@ -\\([0-9]+\\)\\(?:,[0-9]+\\)? +\\+\\([0-9]+\\)\\(?:,[0-9]+\\)? @@"
           text)
          (if diff-position
              (cl-incf diff-position)
            (setq diff-position 0))
          (setq old-line (string-to-number (match-string 1 text))
                new-line (string-to-number (match-string 2 text)))
          (cl-incf hunk)
          (setq hunk-heading t))
         ((and (> hunk 0) (string-prefix-p "-" text)
               (not (string-prefix-p "---" text)))
          (cl-incf diff-position)
          (setq line old-line side "LEFT" location t)
          (cl-incf old-line))
         ((and (> hunk 0) (string-prefix-p "+" text)
               (not (string-prefix-p "+++" text)))
          (cl-incf diff-position)
          (setq line new-line side "RIGHT" location t)
          (cl-incf new-line))
         ((and (> hunk 0) (string-prefix-p " " text))
          (cl-incf diff-position)
          (setq line new-line side "RIGHT" location t)
          (cl-incf old-line)
          (cl-incf new-line))
         ((> hunk 0)
          (cl-incf diff-position)))
        (push
         (append
          (list :text text :hunk hunk :hunk-heading hunk-heading)
          (when location
            (list :resource
                  (magh-resource-create
                   (or resource-kind 'review-line)
                   context :number number :sha head :path path
                   :line line :side side :hunk hunk
                   :position diff-position))))
         records)))
    (nreverse records)))

(defun magh-diff--partition-patch-records (records)
  "Partition parsed patch RECORDS into a preamble and hunk records."
  (let ((groups (seq-group-by (lambda (record) (plist-get record :hunk))
                              records)))
    (list :preamble (alist-get 0 groups)
          :hunks
          (mapcar (lambda (group)
                    (list :heading (cadr group) :records (cddr group)))
                  (assq-delete-all 0 groups)))))

(defun magh-diff--insert-patch-records (records insert-record)
  "Insert parsed patch RECORDS as foldable hunks.
INSERT-RECORD inserts one non-heading record, including any anchored comments."
  (let ((parts (magh-diff--partition-patch-records records)))
    (dolist (record (plist-get parts :preamble))
      (funcall insert-record record))
    (dolist (hunk (plist-get parts :hunks))
      (let ((heading (plist-get hunk :heading)))
        (magh-ui--section (diff-hunk (plist-get heading :hunk) nil nil)
          (magh-ui--diff-hunk-heading (plist-get heading :text))
          (dolist (record (plist-get hunk :records))
            (funcall insert-record record)))))))

(defun magh-diff--selection-resources (kind)
  "Return ordered diff-line resources of KIND at point or in the region."
  (let* ((beg (if (use-region-p) (region-beginning) (line-beginning-position)))
         (end (if (use-region-p)
                  (max beg (1- (region-end)))
                (line-end-position)))
         position
         resources)
    (save-excursion
      (goto-char beg)
      (setq position (line-beginning-position))
      (while (<= position end)
        (goto-char position)
        (let ((resource (magh-ui-resource-at-point)))
          (unless (eq (plist-get resource :kind) kind)
            (user-error "Selection contains a non-commentable diff line"))
          (push resource resources))
        (setq position (line-beginning-position 2))))
    (nreverse resources)))

(defun magh-diff--commit-selection ()
  "Return the commit diff location selected at point or by the region.
GitHub commit comments have one diff position, so a multi-line selection is
anchored to its final selected line."
  (let* ((resources (magh-diff--selection-resources 'commit-line))
         (first (car resources))
         (last (car (last resources))))
    (unless (seq-every-p
             (lambda (resource)
               (and (equal (plist-get resource :path) (plist-get first :path))
                    (equal (plist-get resource :hunk) (plist-get first :hunk))))
             resources)
      (user-error "Commit comment selections must stay in one file and hunk"))
    (list :path (plist-get first :path)
          :position (plist-get last :position)
          :line (plist-get last :line))))


(provide 'magh-diff)
;;; magh-diff.el ends here

