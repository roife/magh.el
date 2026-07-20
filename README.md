# magh.el

`magh.el` is a Magit-style GitHub client for Emacs, powered by the
[GitHub CLI](https://cli.github.com/). It presents repositories, issues, pull
requests, reviews, Actions, Projects, Discussions, Gists, releases,
notifications, and source trees as native Emacs buffers built with
`magit-section`.

> [!WARNING]
> **Work in progress:** `magh.el` is under active development. Public commands,
> keybindings, customization options, and buffer layouts may change before the
> first stable release. Please report bugs with reproduction steps and the
> output of `M-x emacs-version` and `gh --version`.

Authentication, network access, GitHub Enterprise hosts, and API transport are
delegated to `gh`, so `magh.el` uses the accounts and credentials you already have
configured. When a workflow does not yet have a dedicated Emacs interface, you
can still run any GitHub CLI command or make arbitrary REST and GraphQL requests
without leaving Emacs.

## Features

- User and repository status pages whose independent sections survive partial
  request failures
- Cursor-paginated issue and pull request lists, plus paginated commit history
- Native issue and pull request detail pages, editing, and comments
- Pull request review drafts, inline comments, threads, and review submission
- GitHub Actions workflows, runs, jobs, steps, logs, artifacts, reruns, and
  dispatch
- Owner-level Projects with README, field metadata, Issue/PR and draft items,
  archiving, removal, and supported field-value updates
- Repository Discussions with categories, comments, replies, Q&A answers, and
  close/reopen workflows
- Release creation, editing, generated notes, publishing, and asset downloads
- Cancellable Consult search for repositories, issues, pull requests, code, and
  commits
- Notifications with native previews, grouping, and read/subscription actions
- Clone-free browsing of remote repository trees and files
- Repository, branch, commit, and statistics views, plus Gist creation and
  per-file editing
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

Embark, Forge, and emacs-pr-review are optional and are loaded only when their
integrations are enabled.

## Installation

The package is currently installed from source. With `package-vc`:

```elisp
(package-vc-install "https://github.com/roife/magh.el")
```

Or clone the repository and add it to your `load-path`:

```elisp
(add-to-list 'load-path "/path/to/magh.el")
(require 'magh)
```

Before opening `magh.el`, make sure the CLI can access your account:

```sh
gh auth status
```

## Getting started

Run `M-x magh` to open your GitHub user status page. From there you can visit
review requests, assigned work, notifications, and repositories.

Run `C-u M-x magh` or `M-x magh-dispatch` to open the top-level Transient menu. A
convenient global binding is:

```elisp
(global-set-key (kbd "C-c g") #'magh)
```

Useful direct entry points include:

| Command | Description |
| --- | --- |
| `magh-user-status` | Show account activity and assigned work |
| `magh-repo-status` | Open the current repository workspace |
| `magh-repo-status-other` | Select and open any repository |
| `magh-repo-switch-remote` | Switch a local status page to another remote |
| `magh-issue-list` | Browse issues |
| `magh-pr-list` | Browse pull requests |
| `magh-review-requests` | Show pull requests awaiting your review |
| `magh-run-list` | Browse Actions runs, jobs, steps, and logs |
| `magh-run-artifact-download` | Extract selected or all available Run artifacts |
| `magh-workflow-list` | Browse and dispatch workflows |
| `magh-project-list` | Browse Projects for a repository owner or `@me` |
| `magh-project-create` | Create an owner-level Project |
| `magh-discussion-list` | Browse repository Discussions |
| `magh-discussion-create` | Create a repository Discussion |
| `magh-gist-list` | Browse your Gists |
| `magh-gist-create` | Create a secret or public single-file Gist |
| `magh-gist-edit-metadata` | Edit a Gist description |
| `magh-gist-file-edit` | Edit a Gist file's full content |
| `magh-release-list` | Browse releases with preview |
| `magh-search-dispatch` | Search GitHub with Consult |
| `magh-notifications-dispatch` | Browse and manage notifications |
| `magh-browse-repository` | Browse a remote tree without cloning |
| `magh-command` | Run any `gh` command in an Emacs PTY |
| `magh-api-request` | Call an arbitrary REST or GraphQL endpoint |

Repository-scoped commands honor the current branch's push remote, Git's
`remote.pushDefault`, and its tracking remote before falling back to `origin`.
On a local repository status page, press `O` to switch to another GitHub
remote. Commands otherwise prompt for `OWNER/NAME`; values in
`magh-known-repositories` appear as completions in that prompt.

Inside native resource pages, the usual Magit navigation applies: `TAB`
expands or collapses a section, `RET` visits the resource at point, `n` and `p`
move between sections, `g` refreshes, and `q` quits. Press `.` for the
contextual action menu or `?` for the top-level menu. Use `b` or `o` to open the
resource at point on GitHub, and `w` to copy its URL. Use `C-h m` in any page to
see its resource-specific commands.

Issue, pull request, Project, Discussion, Release, and Gist editors use
`C-c C-c` to submit and `C-c C-k` to cancel.

Actions artifact downloads are extracted into `magh-download-directory` (or a
directory selected at download time). Expired artifacts cannot be downloaded;
artifact deletion always uses the configured destructive-action confirmation.

Projects write commands require the token used by `gh` to have the `project`
scope. Gist creation and editing require Gist write access. `magh.el` never
widens token scopes automatically; use `gh auth refresh -s project` or the
equivalent host-specific authentication workflow when GitHub reports a missing
scope. GitHub Enterprise hosts are used as configured, and unsupported or
disabled features report their API/CLI error without falling back to
`github.com`.

## Configuration

All options are available through `M-x customize-group RET magh`. For example:

```elisp
;; Use a GitHub Enterprise host. Nil lets gh choose its authenticated default.
(setq magh-host "github.example.com")

;; Control list page sizes and default filters.
(setq magh-list-limit 50
      magh-default-issue-state "open"
      magh-default-pr-state "open")

;; Opt in to remote thumbnails in GitHub Markdown. This contacts image hosts.
(setq magh-view-inline-images t)

;; Organizations shown by `magh-favorite-repositories`.
(setq magh-favorite-organizations '("emacs-mirror" "github"))
```

By default, a host inferred from the current repository's Git remote takes
precedence over `magh-host`. Credentials remain managed by the GitHub CLI; this
package does not store access tokens.

## Optional integrations

### Magit

`magh-magit-mode` adds asynchronous pull request, issue, and Actions summaries to
Magit status buffers. The status hook displays cached data or a loading row and
does not wait for network requests.

```elisp
(require 'magh-magit)
(setq magh-magit-status-sections '(pr issue run)
      magh-magit-list-limit 10
      magh-magit-cache-ttl 30)
(magh-magit-mode 1)
```

### Embark

```elisp
(require 'magh-embark)
(magh-embark-mode 1)
```

This adds resource-aware actions for repositories, files, issues, pull
requests, releases, workflows, runs, artifacts, Projects, Discussions, Gists,
branches, commits, and notifications.

### Forge

```elisp
(require 'magh-forge)
(magh-forge-mode 1)
```

The Forge bridge lets issue and pull request candidates open as Forge topics.
It is opt-in and keeps track of repositories it adds so that cleanup does not
remove entries from an existing Forge setup.

### emacs-pr-review

With the separately installed `emacs-pr-review` package, pull request
candidates can open in its review interface:

```elisp
(require 'magh-pr-review)
(magh-pr-review-mode 1)
```

The Forge and emacs-pr-review bridges are mutually exclusive. Enabling
`magh-forge-mode` disables `magh-pr-review-mode`, and enabling
`magh-pr-review-mode` disables `magh-forge-mode`.

## License

`magh.el` is distributed under the GNU General Public License, version 2 only.
See [LICENSE](LICENSE).
