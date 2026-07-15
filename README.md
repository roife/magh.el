# gh.el

`gh.el` 是一个由 [GitHub CLI](https://cli.github.com/) 驱动的 Emacs GitHub
前端。所有状态页和详情页直接建立在真正的 `magit-section` 上，遵循
`magit-status` 的信息层级、折叠、移动和上下文动作习惯；所有网络请求、认证和
GitHub Enterprise host 选择都交给 `gh`。

项目采用两层覆盖：

- 原生 Emacs 页面覆盖 User Status、Repo Status、Issue、PR、Review、Actions、Search、Gist、
  History、Statistics 和在线代码浏览。
- `M-x gh-command` 在 PTY 中运行任意 `gh` 命令，包含扩展命令和未来新增命令；
  `M-x gh-api-request` 覆盖 GitHub REST/GraphQL API。因而尚未做成专用页面的能力
  仍然可以完整使用。

## 要求与安装

- Emacs 31.1+
- `gh`，并已执行 `gh auth login`
- Magit 4.0+（UI 直接使用其 `magit-section`）
- `consult` 2.0+
- `marginalia` 1.0+（Search annotation）
- `transient` 0.7+
- `markdown-mode` 2.6+（Description 和评论的 GFM 渲染）

PR 和 Issue 评论中的远程 GFM 图片默认会异步加载为缩略图；按 `RET` 或鼠标点击图片
可打开原图。若不希望自动加载远程图片，可设置：

```elisp
(setq gh-view-inline-images nil)
```

可选集成按需加载：Embark、Forge 和 `emacs-pr-review`。没有安装这些包时，
核心功能不会加载对应模块，也不会增加硬依赖。

从源码使用：

```elisp
(add-to-list 'load-path "/path/to/gh.el")
(require 'gh)
(global-set-key (kbd "C-c g") #'gh)
```

也可以使用 `package-vc-install` 安装 Git 仓库。

## 开始使用

执行 `M-x gh` 打开当前账户的 User Status；`C-u M-x gh` 或 `M-x gh-dispatch`
打开总菜单。也可以直接调用：

| 命令 | 用途 |
|---|---|
| `gh-user-status` | Review、分配的 Issue、自己的 PR、通知和仓库 |
| `gh-repo-status` | 类似 `magit-status` 的当前仓库工作台 |
| `gh-repo-status-other` | 选择任意在线仓库的 Status |
| `gh-issue-list` / `gh-pr-list` | Issue / PR 列表与详情 |
| `gh-review-requests` | 等待自己 Review 的 PR |
| `gh-run-list` | Actions workflow runs、jobs、steps 和逐 job 日志 |
| `gh-release-view` | Release description、正文和附件 |
| `gh-search-dispatch` | 选择类型后用 Consult 实时搜索 |
| `gh-consult-search` | 异步搜索仓库、Issue、PR、代码或提交 |
| `gh-gist-list` | Gist 列表、文件内容 |
| `gh-commit-list` | 仓库或路径的提交历史 |
| `gh-statistics` | Stars、forks、watchers、语言占比等 |
| `gh-browse-repository` | 无需 clone，像 Dired 一样浏览在线目录 |
| `gh-repo-clone-temporary` | 浅克隆到临时目录并打开 Dired |
| `gh-command` | 在 Emacs PTY 中运行任意 `gh` 命令 |
| `gh-api-request` | 调用任意 GitHub REST/GraphQL API |

`M-x gh-management-dispatch` 是管理入口，`M-x gh-settings-dispatch` 调整列表 limit、
Issue/PR 默认状态、按键和可选集成；两者也分别位于主菜单的 `M` 和 `,`。

## 仓库、分支与 Commit 管理

| 命令 | 用途 |
|---|---|
| `gh-repository-list` | 列出任意用户或组织的仓库 |
| `gh-starred-repositories` | 当前账户 Starred 仓库 |
| `gh-favorite-repositories` | 聚合 `gh-favorite-organizations` 中的组织仓库 |
| `gh-repository-create` | 从零、模板或现有本地 Git 仓库创建远程仓库 |
| `gh-repository-clone` / `gh-repository-fork` | 带确认和 post hook 的 clone / fork |
| `gh-repository-settings-edit` | 结构化编辑仓库设置并提供 CAPF |
| `gh-repository-rename` / `gh-repository-delete` | 重命名或删除仓库 |
| `gh-branch-create` / `gh-branch-delete` | 创建或删除远程分支 |
| `gh-commit-list` / `gh-commit-view` | 按 ref 浏览历史和 Commit 详情 |

Repo Status 会列出远程 branches；在分支行按 `RET` 可切换当前 ref，Recent commits
和 Repository Tree 会随之更新。

Commit 详情页展示 parent、changed files、patch/diff 和评论；按 `b` 可在该 Commit 的
SHA 上打开在线文件树，按 `c` 添加 Commit 评论。

`gh-repository-search-dispatch` 把搜索固定到一个仓库，使用 Consult 覆盖 Issue、PR、
Action、Release、branch、Commit 和代码内容。Issue、PR、Commit 与代码使用可取消的
GitHub 动态搜索；Action、Release 和 branch 拉取本仓库数据后由 Consult 本地筛选。
异步请求期间，minibuffer prompt 使用 Consult 与 `consult-ripgrep` 相同的状态标志：
`*` 表示运行中、`:` 表示完成、`!` 表示失败。每种候选的默认动作由
`gh-resource-actions` 配置。

## 结构化 Issue 与 Pull Request 工作流

`gh-issue-create`、`gh-issue-edit`、`gh-pr-edit` 已使用 `gh-edit-mode` 结构化编辑缓冲区；
`C-c C-c` 提交，`C-c C-k` 取消。编辑字段上的 CAPF 会补全 assignee、reviewer、label、
milestone、Project 和 branch。

- 创建 Issue 会读取 `.github/ISSUE_TEMPLATE`；支持 close reason 和评论、带评论 reopen、
  pin/unpin、lock/unlock，以及 `gh-issue-develop` 的关联分支流程。
- `gh-pr-template-read` 读取单文件或目录形式的 PR Template。
- `gh-pr-view-commits`、`gh-pr-view-files` 分别查看 PR Commit 和 changed files。
- `gh-pr-review-comment-add` 收集单行或多行 review comment，
  `gh-pr-file-comment-add` 收集整文件评论，`gh-pr-review-submit-collected` 一次提交为
  COMMENT、APPROVE 或 REQUEST_CHANGES。
- PR 还支持 lock/unlock、Draft/Ready 切换、auto-merge 开关，以及 close 时附加评论并
  删除分支、reopen 时附加评论。
- `gh-link-issue-pr` 通过 `Closes #N` 把 Issue 与 PR 建立可追踪的关闭关系。

## Release、Workflow 与通知

`gh-release-list` 提供 Consult 预览；`gh-release-create` 和 `gh-release-edit` 使用结构化
编辑器，支持生成 release notes、把生成结果作为可编辑模板、draft、prerelease、tag、
target、Latest、发布、删除和附件下载。

`gh-workflow-list` 提供 Workflow 预览，原生详情页展示状态、YAML 和近期运行。详情页可
切换 branch/tag ref、enable/disable 或手动 dispatch，并填写任意 `key=value` inputs。
`gh-run-rerun-job` 可从一个 run 的 jobs 中选择单个 job 重跑。

`gh-notifications-dispatch` 可切换未读/全部通知及按 repository、reason、type、state、date
分组。通知有原生预览，打开后自动标记已读，并有显式 read、subscribe 和 unsubscribe
命令。GitHub 公共 REST API 没有“把单个线程重新标为 unread”的端点，因此
`gh-notification-mark-unread` 会打开已定位到该仓库已读通知的网页，让用户完成这一项操作，
不会调用未经文档支持的写接口。

在线目录中使用 `RET` 进入目录或查看文件，`^` 返回上级，`r` 切换 branch/tag/commit，
`H` 查看当前路径历史，`b` 在浏览器中打开，`C` 临时 clone。远程文件会根据扩展名启用对应 major mode，
保持只读，并用 `C-c C-o` 打开 GitHub 网页。

## Section 操作

所有原生页面共享以下按键：

- `TAB`：折叠/展开
- `RET`：进入资源
- `n` / `p`：下一个/上一个 section
- `^`：进入父 section
- `1`–`4`：显示相应 section 深度
- `M-1`–`M-4`：全局显示相应深度
- `S-TAB`：全局循环折叠状态
- `g`：刷新
- `q`：关闭窗口

Issue 页面支持创建、评论、编辑、关闭和重开。PR 页面支持创建、评论、编辑、diff、
checkout、review、merge、关闭和重开；详情页会按时间展示普通评论、Review 和行内评论。
Actions 详情页会把完整日志按 job 分组，另支持 watch、rerun 和 cancel。Release 在
Emacs 内展示 description、正文与附件。开启 `marginalia-mode` 后，Search 候选会显示
repository、state、stars、author 和代码匹配片段等 annotation；未开启时只显示主候选。
页面内按键可以用 `C-h m` 查看。

Repo/User Status 中，`/` 打开搜索类型菜单；选择后输入两个字符即启动异步搜索。
继续输入会取消旧的 `gh search` 进程并在短暂 debounce 后启动新请求，避免过量调用
GitHub Search API。选中结果后会进入对应的原生 Repo、Issue、PR、代码或 commit。

在 Status 的 Issue、PR 或 Actions 行按 `.` 打开上下文动作菜单，可直接评论、编辑、
关闭/重开、Review、Merge、查看日志、Watch、Rerun 或 Cancel，不必先进入详情页。

## Magit 与 Forge 集成

加载 `gh-magit` 并开启全局模式后，Magit status 会在自身 sections 之后异步插入：

- GitHub Pull Requests
- GitHub Issues
- Recent GitHub Actions

Issue 和 PR 默认各展示当前仓库最近的 10 条 open 记录，section 标题不显示数量。
可通过 `gh-magit-list-limit` 调整条数。在 `@ GitHub` transient 中使用
`m Magit scope` 可以切换到与当前用户相关的状态摘要；该模式会按 current branch、
needs review、assigned、mentioned 和 created by you 保留子分组。

首次刷新只插入 `loading…`，不会在 `magit-status-sections-hook` 中等待网络。查询完成后
通过 timer 刷新原 Magit buffer；summary cache 和通用 gh query cache 分别负责 TTL
与 inflight 去重。记录上使用 `RET` 进入原生详情、`o` 浏览网页、`w` 复制 URL。

```elisp
(require 'gh-magit)
(setq gh-magit-dispatch-key "@"
      gh-magit-status-sections '(pr issue run)
      gh-magit-summary-scope 'repository
      gh-magit-cache-ttl 30
      gh-hide-forge-duplicates t)
(gh-magit-mode 1)
```

`@ GitHub` 会出现在 `magit-dispatch` 的 `! Run` 后；Magit 为 Forge 保留的 `N` 不会
被修改。`M-x gh-magit-refresh` 刷新摘要，使用 prefix argument 会同时清理两层缓存。

默认的 `gh-magit` 仍只协调重复显示：当 `gh-hide-forge-duplicates` 非 nil 且 Forge 已加载
时，它不插入 PR/Issue summaries，只保留 Actions。若显式开启 `(gh-forge-mode 1)`，
Issue/PR 候选会被加入 Forge 数据库并在 Forge topic buffer 中打开；
`gh-forge-open-current-topic-in-gh` 可反向回到原生页面，
`gh-forge-remove-repository` 和 `gh-forge-remove-added-repositories` 只清理由 gh.el 加入的记录。

## Embark 与可选显示集成

```elisp
(require 'gh-embark)
(gh-embark-mode 1)

;; 可选：选择其中一个作为 PR/Issue 默认查看器
;; (require 'gh-forge)
;; (gh-forge-mode 1)
;; 或：
;; (require 'gh-pr-review)
;; (gh-pr-review-mode 1)
```

Embark 分别注册 Repo、File、Issue、PR、Release、Workflow、Run、Branch、Commit 和
Notification category。动作包括打开、浏览、clone/fork、创建资源、编辑状态、复制标题、
GitHub/HTTPS/SSH/Org URL、生成 straight `use-package` 片段、插入标题/URL/远程文件内容，
以及维护 known repos、favorite organizations 和 Workflow 模板仓库。对
`embark-select` 选中的候选可以复用相同动作进行批量操作。

## 配置

```elisp
;; GitHub Enterprise；nil 表示使用 gh 当前默认 host
(setq gh-host "github.example.com")

;; 收藏组织聚合入口与流程完成后的扩展点
(setq gh-favorite-organizations '("emacs-mirror" "github"))
(add-hook 'gh-auth-post-switch-hook #'my-gh-account-switched)
(add-hook 'gh-repository-post-clone-hook #'my-gh-after-clone)
(add-hook 'gh-repository-post-fork-hook #'my-gh-after-fork)

;; 列表遵循 doc/UI.md 的 Magit 式语义顺序，以两个空格分隔片段；
;; 不使用定宽列、空格补齐或按列截断。

;; 参考 Magit 的 buffer 展示/退出策略
(setq gh-display-buffer-function #'pop-to-buffer)
(setq gh-bury-buffer-function #'quit-window)
(add-hook 'gh-pre-display-buffer-hook #'my-before-gh-buffer)
(add-hook 'gh-post-display-buffer-hook #'my-after-gh-buffer)

;; Section 展示、折叠缓存和刷新后的光标恢复
(setq gh-view-truncate-lines t)
(add-hook 'gh-section-mode-hook #'hl-line-mode)
;; 默认所有 section 均折叠；可把更具体的 show 规则放在 t 之前
(setq gh-section-initial-visibility-alist
      '((workflow . show) ("^Comments" . show) (t . hide)))
(setq gh-section-cache-visibility t) ; 也可设为 nil 或 '(issue pr)
(setq gh-refresh-point-strategy 'section) ; section、line 或 start
(add-hook 'gh-pre-refresh-hook #'my-before-gh-refresh)
(add-hook 'gh-post-refresh-hook #'my-after-gh-refresh)

;; 所有日期共用一个格式函数
(setq gh-date-format-function
      (lambda (timestamp)
        (if timestamp
            (format-time-string "%Y-%m-%d %H:%M" (date-to-time timestamp))
          "")))

;; 列表每页数量
(setq gh-list-limit 50)

;; 关闭 Issue、合并 PR 等操作前是否确认
(setq gh-confirm-destructive-actions t)
```

界面颜色不写死，统一通过语义 face 继承 Magit 的外观。常用入口包括
`gh-section-heading`、`gh-resource-number`、`gh-resource-title`、`gh-repository`、
`gh-branch`、`gh-author`、`gh-date`、`gh-tag`、`gh-hash`，以及
`gh-open-state`、`gh-pending-state`、`gh-draft-state`、`gh-closed-state`。
可以通过 `M-x customize-group RET gh` 调整；切换主题后也会自然跟随对应的 Magit face。

## 功能覆盖原则

`gh` 本身支持的 `auth`、`browse`、`codespace`、`extension`、`gist`、`issue`、`label`、
`org`、`pr`、`project`、`release`、`repo`、`ruleset`、`run`、`search`、`secret`、
`ssh-key`、`status`、`variable`、`workflow` 等命令都可由 `gh-command` 使用。专用页面
不会重新实现认证或私自保存 token。

## 开发

```sh
make compile
make test
```

测试不访问网络；另外可使用 `cli/cli` 等公开仓库进行手工烟雾测试。
