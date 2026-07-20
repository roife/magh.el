;;; magh-topic.el --- Shared Issue and Pull Request presentation -*- lexical-binding: t; -*-

;; Copyright (C) 2026
;; SPDX-License-Identifier: GPL-2.0-only

;; Author: magh.el contributors
;; Keywords: tools, vc, github

;;; Commentary:

;; Shared resource, row, metadata, and structured-edit helpers for GitHub
;; Issues and Pull Requests.  Resource-specific pages and actions remain in
;; `magh-issue' and `magh-pr'.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'magh-api)
(require 'magh-candidate)
(require 'magh-edit)
(require 'magh-ui)

(defun magh-topic--resource (kind context data)
  "Create a KIND topic resource from DATA in CONTEXT."
  (magh-resource-create
   kind context :number (alist-get 'number data)
   :title (alist-get 'title data) :url (alist-get 'url data) :data data))

(defun magh-topic--row-values (kind data)
  "Return semantic row values for topic KIND and DATA."
  (let ((state (if (and (eq kind 'pr) (alist-get 'isDraft data))
                   "DRAFT"
                 (alist-get 'state data)))
        (review (and (eq kind 'pr) (alist-get 'reviewDecision data))))
    (list :state (magh-ui--styled (upcase state) (magh-core--state-face state))
          :review (magh-ui--styled review (magh-core--state-face review))
          :identifier (magh-ui--styled
                       (format "#%s" (alist-get 'number data))
                       'magh-resource-number)
          :title (magh-ui--styled (alist-get 'title data)
                                  'magh-resource-title))))

(defun magh-topic--insert-pr-branches (data)
  "Insert Pull Request branch metadata from DATA when available."
  (when (and (alist-get 'headRefName data) (alist-get 'baseRefName data))
    (magh-ui--insert-header
     "Branches" (format "%s → %s"
                        (alist-get 'headRefName data)
                        (alist-get 'baseRefName data))
     'magh-branch)))

(cl-defun magh-topic--insert-metadata
    (kind data &key details created review-placeholder branches-first)
  "Insert common metadata for topic KIND from DATA.
DETAILS includes Pull Request branches and review state.  CREATED includes the
creation date in addition to the update date.  BRANCHES-FIRST places Pull
Request branches before the author instead of after it."
  (when (and details branches-first (eq kind 'pr))
    (magh-topic--insert-pr-branches data))
  (magh-ui--insert-header "Author"
                          (magh-core--name (alist-get 'author data))
                          'magh-author)
  (when (and details (not branches-first) (eq kind 'pr))
    (magh-topic--insert-pr-branches data))
  (when (and details (eq kind 'pr))
    (let ((review (alist-get 'reviewDecision data)))
      (when (or review review-placeholder)
        (magh-ui--insert-header "Review" (or review review-placeholder)
                                (magh-core--state-face review)))))
  (magh-ui--insert-header "Labels"
                          (magh-core--names (alist-get 'labels data))
                          'magh-label)
  (when (eq kind 'issue)
    (magh-ui--insert-header "Assigned"
                            (magh-core--names (alist-get 'assignees data))
                            'magh-author))
  (magh-ui--insert-header "Comments" (magh-core--comments-count data))
  (when created
    (magh-ui--insert-header "Created"
                            (magh-core--date (alist-get 'createdAt data))
                            'magh-date))
  (magh-ui--insert-header "Updated"
                          (magh-core--date (alist-get 'updatedAt data))
                          'magh-date))

(defun magh-topic--editor-fields (context &optional reviewers)
  "Return common topic editor fields for CONTEXT.
Include the Pull Request reviewers field when REVIEWERS is non-nil."
  (let ((users (magh-edit--completion-fetcher
                #'magh-api--repo-collaborators context 'login))
        (labels (magh-edit--completion-fetcher
                 #'magh-api--repo-labels context 'name))
        (milestones (magh-edit--completion-fetcher
                     #'magh-api--repo-milestones context 'title))
        (projects (magh-edit--completion-fetcher
                   #'magh-api--project-list context 'title)))
    (append
     (when reviewers
       `((:name reviewers :multiple t :completion-fetch ,users)))
     `((:name assignees :multiple t :completion-fetch ,users)
       (:name labels :multiple t :completion-fetch ,labels)
       (:name milestone :completion-fetch ,milestones)
       (:name projects :multiple t :completion-fetch ,projects)))))

(defun magh-topic--edit-values (data &optional reviewers)
  "Return common structured editor values from topic DATA.
Include review requests when REVIEWERS is non-nil."
  (append
   (when reviewers
     (list :reviewers
           (mapcar #'magh-core--name (alist-get 'reviewRequests data))))
   (list :assignees (mapcar #'magh-core--name (alist-get 'assignees data))
         :labels (mapcar #'magh-core--name (alist-get 'labels data))
         :milestone (magh-core--name (alist-get 'milestone data))
         :projects (mapcar #'magh-core--name (alist-get 'projectItems data)))))

(defun magh-topic--edit-changes (original values changes)
  "Add topic field differences between ORIGINAL and VALUES to CHANGES."
  (let ((old-milestone (plist-get original :milestone))
        (new-milestone (plist-get values :milestone)))
    (unless (equal old-milestone new-milestone)
      (setq changes
            (if new-milestone
                (plist-put changes :milestone new-milestone)
              (plist-put changes :remove-milestone t)))))
  (dolist (spec '((:reviewers :add-reviewers :remove-reviewers)
                  (:assignees :add-assignees :remove-assignees)
                  (:labels :add-labels :remove-labels)
                  (:projects :add-projects :remove-projects)))
    (when (or (plist-member original (car spec))
              (plist-member values (car spec)))
      (let ((old (plist-get original (car spec)))
            (new (plist-get values (car spec))))
        (setq changes
              (plist-put changes (nth 1 spec)
                         (seq-difference new old #'string=)))
        (setq changes
              (plist-put changes (nth 2 spec)
                         (seq-difference old new #'string=))))))
  changes)

(provide 'magh-topic)
;;; magh-topic.el ends here
