;;; gh-dispatch.el --- Top-level Transient menus for gh.el -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; Author: gh.el contributors
;; Keywords: tools, vc, github
;; Package-Requires: ((emacs "31.1") (transient "0.7.0"))

;;; Commentary:

;; Cross-resource entry menus only.  Contextual resource actions stay in their
;; owning modules.

;;; Code:

(require 'transient)
(require 'gh-core)

(declare-function gh-user-status "gh-pages")
(declare-function gh-repo-status "gh-repo")
(declare-function gh-repo-status-other "gh-repo")
(declare-function gh-repository-list "gh-repo")
(declare-function gh-starred-repositories "gh-repo")
(declare-function gh-favorite-repositories "gh-repo")
(declare-function gh-repository-create "gh-repo")
(declare-function gh-repository-clone "gh-repo")
(declare-function gh-repository-fork "gh-repo")
(declare-function gh-issue-list "gh-issue")
(declare-function gh-issue-create "gh-issue")
(declare-function gh-pr-list "gh-pr")
(declare-function gh-pr-create "gh-pr")
(declare-function gh-review-requests "gh-pr")
(declare-function gh-run-list "gh-actions")
(declare-function gh-workflow-list "gh-actions")
(declare-function gh-release-list "gh-release")
(declare-function gh-release-create "gh-release")
(declare-function gh-commit-list "gh-commit")
(declare-function gh-browse-repository "gh-browse")
(declare-function gh-clean-temporary-clones "gh-browse")
(declare-function gh-search-dispatch "gh-search")
(declare-function gh-notifications-dispatch "gh-notify")
(declare-function gh-gist-list "gh-pages")
(declare-function gh-command "gh-command")
(declare-function gh-api-request "gh-command")
(declare-function gh-auth-switch "gh-command")
(declare-function gh-client-clear-cache "gh-client")

(transient-define-prefix gh-dispatch ()
  "GitHub workspace."
  [["Status"
    ("u" "User status" gh-user-status)
    ("s" "Repository status" gh-repo-status)
    ("S" "Other repository" gh-repo-status-other)]
   ["Work"
    ("i" "Issues" gh-issue-list)
    ("p" "Pull requests" gh-pr-list)
    ("R" "Review requests" gh-review-requests)
    ("a" "Actions" gh-run-list)]
   ["Explore"
    ("w" "Workflows" gh-workflow-list)
    ("r" "Releases" gh-release-list)
    ("h" "Commit history" gh-commit-list)
    ("t" "Repository tree" gh-browse-repository)]
   ["Discover"
    ("/" "Search" gh-search-dispatch)
    ("n" "Notifications" gh-notifications-dispatch)
    ("g" "Gists" gh-gist-list)]]
  [["General"
    ("!" "Arbitrary gh command" gh-command)
    (":" "GitHub API" gh-api-request)]
   ["Configure"
    ("M" "Management" gh-management-dispatch)
    ("," "Settings" gh-settings-dispatch)]])

(transient-define-prefix gh-management-dispatch ()
  "GitHub repository and account management."
  [["Repositories"
    ("l" "List" gh-repository-list)
    ("s" "Starred" gh-starred-repositories)
    ("f" "Favorites" gh-favorite-repositories)
    ("c" "Create" gh-repository-create)
    ("C" "Clone" gh-repository-clone)
    ("F" "Fork" gh-repository-fork)]
   ["Create resources"
    ("i" "Issue" gh-issue-create)
    ("p" "Pull request" gh-pr-create)
    ("r" "Release" gh-release-create)]
   ["Maintenance"
    ("x" "Clean temporary clones" gh-clean-temporary-clones)
    ("A" "Switch account" gh-auth-switch)]])

(transient-define-prefix gh-settings-dispatch ()
  "gh.el settings and cache."
  [["Settings"
    ("c" "Customize gh.el"
     (lambda () (interactive) (customize-group 'gh)))
    ("x" "Clear query cache" gh-client-clear-cache)]
   ["Fallbacks"
    ("!" "Arbitrary gh command" gh-command)
    (":" "GitHub API" gh-api-request)]])

(provide 'gh-dispatch)
;;; gh-dispatch.el ends here
