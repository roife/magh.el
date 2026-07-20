;;; magh-dispatch.el --- Top-level Transient menus for magh.el -*- lexical-binding: t; -*-

;; Copyright (C) 2026
;; SPDX-License-Identifier: GPL-2.0-only

;; Author: magh.el contributors
;; Keywords: tools, vc, github

;;; Commentary:

;; Cross-resource entry menus only.  Contextual resource actions stay in their
;; owning modules.

;;; Code:

(require 'transient)
(require 'magh-core)

(declare-function magh-user-status "magh-pages")
(declare-function magh-repo-status "magh-repo")
(declare-function magh-repo-status-other "magh-repo")
(declare-function magh-repository-list "magh-repo")
(declare-function magh-starred-repositories "magh-repo")
(declare-function magh-favorite-repositories "magh-repo")
(declare-function magh-repository-create "magh-repo")
(declare-function magh-repository-clone "magh-repo")
(declare-function magh-repository-fork "magh-repo")
(declare-function magh-issue-list "magh-issue")
(declare-function magh-issue-create "magh-issue")
(declare-function magh-pr-list "magh-pr")
(declare-function magh-pr-create "magh-pr")
(declare-function magh-review-requests "magh-pr")
(declare-function magh-run-list "magh-actions")
(declare-function magh-workflow-list "magh-actions")
(declare-function magh-project-list "magh-project")
(declare-function magh-project-create "magh-project")
(declare-function magh-discussion-list "magh-discussion")
(declare-function magh-discussion-create "magh-discussion")
(declare-function magh-release-list "magh-release")
(declare-function magh-release-create "magh-release")
(declare-function magh-commit-list "magh-commit")
(declare-function magh-browse-repository "magh-browse")
(declare-function magh-clean-temporary-clones "magh-browse")
(declare-function magh-search-dispatch "magh-search")
(declare-function magh-notifications-dispatch "magh-notify")
(declare-function magh-gist-list "magh-pages")
(declare-function magh-gist-create "magh-pages")
(declare-function magh-command "magh-command")
(declare-function magh-api-request "magh-command")
(declare-function magh-auth-switch "magh-command")
(declare-function magh-client-clear-cache "magh-client")

;;;###autoload
(transient-define-prefix magh-dispatch ()
  "GitHub workspace."
  [["Status"
    ("u" "User status" magh-user-status)
    ("s" "Repository status" magh-repo-status)
    ("S" "Other repository" magh-repo-status-other)]
   ["Work"
    ("i" "Issues" magh-issue-list)
    ("p" "Pull requests" magh-pr-list)
    ("R" "Review requests" magh-review-requests)
    ("a" "Actions" magh-run-list)
    ("P" "Projects" magh-project-list)
    ("d" "Discussions" magh-discussion-list)]
   ["Explore"
    ("w" "Workflows" magh-workflow-list)
    ("r" "Releases" magh-release-list)
    ("h" "Commit history" magh-commit-list)
    ("t" "Repository tree" magh-browse-repository)]
   ["Discover"
    ("/" "Search" magh-search-dispatch)
    ("n" "Notifications" magh-notifications-dispatch)
    ("g" "Gists" magh-gist-list)]]
  [["General"
    ("!" "Arbitrary gh command" magh-command)
    (":" "GitHub API" magh-api-request)]
   ["Configure"
    ("M" "Management" magh-management-dispatch)
    ("," "Settings" magh-settings-dispatch)]])

(transient-define-prefix magh-management-dispatch ()
  "GitHub repository and account management."
  [["Repositories"
    ("l" "List" magh-repository-list)
    ("s" "Starred" magh-starred-repositories)
    ("f" "Favorites" magh-favorite-repositories)
    ("c" "Create" magh-repository-create)
    ("C" "Clone" magh-repository-clone)
    ("F" "Fork" magh-repository-fork)]
   ["Create resources"
    ("i" "Issue" magh-issue-create)
    ("p" "Pull request" magh-pr-create)
    ("r" "Release" magh-release-create)
    ("P" "Project" magh-project-create)
    ("d" "Discussion" magh-discussion-create)
    ("g" "Gist" magh-gist-create)]
   ["Maintenance"
    ("x" "Clean temporary clones" magh-clean-temporary-clones)
    ("A" "Switch account" magh-auth-switch)]])

(transient-define-prefix magh-settings-dispatch ()
  "magh.el settings and cache."
  [["Settings"
    ("c" "Customize magh.el"
     (lambda () (interactive) (customize-group 'magh)))
    ("x" "Clear query cache" magh-client-clear-cache)]
   ["Fallbacks"
    ("!" "Arbitrary gh command" magh-command)
    (":" "GitHub API" magh-api-request)]])

(provide 'magh-dispatch)
;;; magh-dispatch.el ends here
