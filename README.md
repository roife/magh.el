# magh.el

`magh.el` is a Magit-style GitHub client for Emacs, powered by the
[GitHub CLI](https://cli.github.com/). It presents repositories, issues, pull
requests, reviews, Actions, releases, notifications, and source trees as native
Emacs buffers built with `magit-section`.

Authentication, network access, GitHub Enterprise hosts, and API transport are
delegated to `gh`, so `magh.el` uses the accounts and credentials you already have
configured. When a workflow does not yet have a dedicated Emacs interface, you
can still run any GitHub CLI command or make arbitrary REST and GraphQL requests
without leaving Emacs.

## Features

- User and repository status pages with collapsible Magit sections
- Native issue and pull request lists, detail pages, editing, and comments
- Pull request review drafts, inline comments, threads, and review submission
- GitHub Actions workflows, runs, jobs, steps, logs, reruns, and dispatch
- Release creation, editing, generated notes, publishing, and asset downloads
- Cancellable Consult search for repositories, issues, pull requests, code, and
  commits
- Notifications with native previews, grouping, and read/subscription actions
- Clone-free browsing of remote repository trees and files
- Repository, branch, commit, Gist, and statistics views
- Optional Magit, Embark, and Forge integration
- A PTY for arbitrary `gh` commands and a generic REST/GraphQL API viewer

## Requirements

- Emacs 31.1 or later
- [GitHub CLI](https://cli.github.com/) configured with `gh auth login`
- Magit 4.0 or later
- Consult 2.0 or later
- Marginalia 1.0 or later
- Transient 0.7 or later
- Markdown Mode 2.6 or later

Embark and Forge are optional and are loaded only when their integrations are
enabled.

## Installation

The package is currently installed from source. With `package-vc`:

```elisp
(package-vc-install "https://github.com/roife/magh.el")
```

Or clone the repository and add it to your `load-path`:

```elisp
(add-to-list 'load-path "/path/to/magh.el")
(require 'gh)
```

Before opening `magh.el`, make sure the CLI can access your account:

```sh
gh auth status
```

## Getting started

Run `M-x gh` to open your GitHub user status page. From there you can visit
review requests, assigned work, notifications, and repositories.

Run `C-u M-x gh` or `M-x gh-dispatch` to open the top-level Transient menu. A
convenient global binding is:

```elisp
(global-set-key (kbd "C-c g") #'gh)
```

Useful direct entry points include:

| Command | Description |
| --- | --- |
| `gh-user-status` | Show account activity and assigned work |
| `gh-repo-status` | Open the current repository workspace |
| `gh-repo-status-other` | Select and open any repository |
| `gh-issue-list` | Browse issues |
| `gh-pr-list` | Browse pull requests |
| `gh-review-requests` | Show pull requests awaiting your review |
| `gh-run-list` | Browse Actions runs, jobs, steps, and logs |
| `gh-workflow-list` | Browse and dispatch workflows |
| `gh-release-list` | Browse releases with preview |
| `gh-search-dispatch` | Search GitHub with Consult |
| `gh-notifications-dispatch` | Browse and manage notifications |
| `gh-browse-repository` | Browse a remote tree without cloning |
| `gh-command` | Run any `gh` command in an Emacs PTY |
| `gh-api-request` | Call an arbitrary REST or GraphQL endpoint |

Inside native resource pages, the usual Magit navigation applies: `TAB`
expands or collapses a section, `RET` visits the resource at point, `n` and `p`
move between sections, `g` refreshes, and `q` quits. Use `C-h m` in any page to
see its resource-specific commands.

Issue, pull request, and release editors use `C-c C-c` to submit and `C-c C-k`
to cancel.

## Configuration

All options are available through `M-x customize-group RET gh`. For example:

```elisp
;; Use a GitHub Enterprise host. Nil lets gh choose its authenticated default.
(setq gh-host "github.example.com")

;; Control list sizes and default filters.
(setq gh-list-limit 50
      gh-default-issue-state "open"
      gh-default-pr-state "open")

;; Disable asynchronous thumbnails for remote images in GitHub Markdown.
(setq gh-view-inline-images nil)

;; Organizations shown by `gh-favorite-repositories`.
(setq gh-favorite-organizations '("emacs-mirror" "github"))
```

By default, a host inferred from the current repository's Git remote takes
precedence over `gh-host`. Credentials remain managed by the GitHub CLI; this
package does not store access tokens.

## Optional integrations

### Magit

`gh-magit-mode` adds asynchronous pull request, issue, and Actions summaries to
Magit status buffers. The status hook displays cached data or a loading row and
does not wait for network requests.

```elisp
(require 'gh-magit)
(setq gh-magit-status-sections '(pr issue run)
      gh-magit-list-limit 10
      gh-magit-cache-ttl 30)
(gh-magit-mode 1)
```

### Embark

```elisp
(require 'gh-embark)
(gh-embark-mode 1)
```

This adds resource-aware actions for repositories, files, issues, pull
requests, releases, workflows, runs, branches, commits, and notifications.

### Forge

```elisp
(require 'gh-forge)
(gh-forge-mode 1)
```

The Forge bridge lets issue and pull request candidates open as Forge topics.
It is opt-in and keeps track of repositories it adds so that cleanup does not
remove entries from an existing Forge setup.
