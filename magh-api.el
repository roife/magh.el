;;; magh-api.el --- Resource API contracts for magh.el -*- lexical-binding: t; -*-

;; Copyright (C) 2026
;; SPDX-License-Identifier: GPL-2.0-only

;; Author: magh.el contributors
;; Keywords: tools, vc, github

;;; Commentary:

;; Resource modules access GitHub exclusively through this file.  It selects
;; suitable `gh' subcommands or REST endpoints, defines stable field sets,
;; normalizes paginated data, and invalidates cache domains after mutations.
;; Every read in this module is asynchronous.

;;; Code:

(require 'base64)
(require 'cl-lib)
(require 'json)
(require 'map)
(require 'seq)
(require 'subr-x)
(require 'magh-core)
(require 'magh-client)

(defconst magh-api--repo-fields
  '(name nameWithOwner description visibility url sshUrl homepageUrl
    defaultBranchRef owner viewerPermission viewerHasStarred viewerSubscription
    stargazerCount forkCount watchers issues diskUsage languages latestRelease
    repositoryTopics isArchived isFork isTemplate hasIssuesEnabled
    hasProjectsEnabled hasDiscussionsEnabled hasWikiEnabled mergeCommitAllowed
    squashMergeAllowed rebaseMergeAllowed deleteBranchOnMerge createdAt updatedAt))

(defconst magh-api--issue-list-fields
  '(number title state stateReason author assignees labels milestone comments
    createdAt updatedAt closedAt url isPinned))

(defconst magh-api--issue-view-fields
  '(id number title state stateReason author assignees labels milestone
    projectItems body comments createdAt updatedAt closedAt url isPinned
    closedByPullRequestsReferences reactionGroups parent subIssues blockedBy
    blocking))

(defconst magh-api--pr-list-fields
  '(number title state isDraft author assignees labels milestone headRefName
    headRefOid baseRefName baseRefOid reviewDecision reviewRequests comments
    changedFiles additions deletions createdAt updatedAt closedAt mergedAt url))

(defconst magh-api--topic-page-query
  (concat
   "query($queryString:String!,$first:Int!,$after:String){"
   "search(query:$queryString,type:ISSUE,first:$first,after:$after){"
   "nodes{__typename "
   "... on Issue{number title state stateReason author{login} "
   "assignees(first:100){nodes{login}} labels(first:100){nodes{name}} "
   "milestone{title} comments{totalCount} createdAt updatedAt closedAt url "
   "isPinned} "
   "... on PullRequest{number title state isDraft author{login} "
   "assignees(first:100){nodes{login}} labels(first:100){nodes{name}} "
   "milestone{title} headRefName headRefOid baseRefName baseRefOid "
   "reviewDecision comments{totalCount} changedFiles additions deletions "
   "createdAt updatedAt closedAt mergedAt url}}"
   "pageInfo{hasNextPage endCursor}}}")
  "GraphQL query used for cursor-paginated Issue and Pull Request lists.")

(defconst magh-api--pr-view-fields
  '(id number title state isDraft author assignees labels milestone projectItems
    body headRefName headRefOid headRepository headRepositoryOwner baseRefName
    baseRefOid reviewDecision reviewRequests latestReviews reviews comments
    commits files statusCheckRollup closingIssuesReferences changedFiles
    additions deletions mergeable mergeStateStatus autoMergeRequest createdAt
    updatedAt closedAt mergedAt url))

(defconst magh-api--run-fields
  '(attempt conclusion createdAt databaseId displayTitle event headBranch
    headSha name number startedAt status updatedAt url workflowDatabaseId
    workflowName))

(defconst magh-api--release-list-fields
  '(createdAt isDraft isImmutable isLatest isPrerelease name publishedAt
    tagName))

(defconst magh-api--release-view-fields
  '(apiUrl assets author body createdAt databaseId id isDraft isImmutable
    isPrerelease name publishedAt tagName tarballUrl targetCommitish uploadUrl
    url zipballUrl))

(defconst magh-api--project-fields-query
  (concat
   "query($id:ID!){node(id:$id){... on ProjectV2{fields(first:100){"
   "totalCount nodes{__typename "
   "... on ProjectV2Field{id name dataType} "
   "... on ProjectV2SingleSelectField{id name dataType options{id name}} "
   "... on ProjectV2IterationField{id name dataType configuration{"
   "iterations{id title startDate duration} "
   "completedIterations{id title startDate duration}}}}}}}}")
  "GraphQL query for editable Project field metadata.")

(defconst magh-api--discussion-page-query
  (concat
   "query($owner:String!,$name:String!,$first:Int!,$after:String,$categoryId:ID){"
   "repository(owner:$owner,name:$name){discussions(first:$first,after:$after,"
   "categoryId:$categoryId,orderBy:{field:UPDATED_AT,direction:DESC}){"
   "totalCount nodes{id number title body url createdAt updatedAt closed isAnswered "
   "author{login} category{id name emoji} comments{totalCount}}"
   "pageInfo{hasNextPage endCursor}}}}")
  "GraphQL query for one cursor-paginated Discussion page.")

(defconst magh-api--discussion-query
  (concat
   "query($owner:String!,$name:String!,$number:Int!){"
   "repository(owner:$owner,name:$name){discussion(number:$number){"
   "id number title body url createdAt updatedAt closed closedAt isAnswered "
   "viewerCanClose viewerCanReopen viewerCanUpdate author{login} "
   "category{id name emoji isAnswerable} answer{id url} "
   "comments(first:100){totalCount nodes{id databaseId body url createdAt updatedAt "
   "isAnswer viewerCanMarkAsAnswer viewerCanUnmarkAsAnswer author{login} "
   "replies(first:100){totalCount nodes{id databaseId body url createdAt updatedAt "
   "author{login}}}}}}}}")
  "GraphQL query for a Discussion and its bounded comment tree.")

(defconst magh-api--discussion-categories-query
  (concat
   "query($owner:String!,$name:String!){repository(owner:$owner,name:$name){"
   "id discussionCategories(first:100){nodes{id name emoji description "
   "isAnswerable}}}}")
  "GraphQL query for Discussion creation metadata.")

(defconst magh-api--discussion-create-mutation
  (concat
   "mutation($input:CreateDiscussionInput!){createDiscussion(input:$input){"
   "discussion{id number title url}}}"))

(defconst magh-api--discussion-update-mutation
  (concat
   "mutation($input:UpdateDiscussionInput!){updateDiscussion(input:$input){"
   "discussion{id number title url}}}"))

(defconst magh-api--discussion-comment-mutation
  (concat
   "mutation($input:AddDiscussionCommentInput!){"
   "addDiscussionComment(input:$input){comment{id databaseId url}}}"))

(defconst magh-api--discussion-close-mutation
  (concat
   "mutation($input:CloseDiscussionInput!){closeDiscussion(input:$input){"
   "discussion{id number closed}}}"))

(defconst magh-api--discussion-reopen-mutation
  (concat
   "mutation($input:ReopenDiscussionInput!){reopenDiscussion(input:$input){"
   "discussion{id number closed}}}"))

(defconst magh-api--discussion-mark-answer-mutation
  (concat
   "mutation($input:MarkDiscussionCommentAsAnswerInput!){"
   "markDiscussionCommentAsAnswer(input:$input){discussion{id number isAnswered}}}"))

(defconst magh-api--discussion-unmark-answer-mutation
  (concat
   "mutation($input:UnmarkDiscussionCommentAsAnswerInput!){"
   "unmarkDiscussionCommentAsAnswer(input:$input){discussion{id number isAnswered}}}"))

(defun magh-api--fields (fields)
  "Return comma-separated JSON FIELDS."
  (mapconcat #'symbol-name fields ","))

(defun magh-api--context (context &optional required)
  "Normalize CONTEXT, requiring a repository when REQUIRED is non-nil."
  (magh-context-resolve context required))

(defun magh-api--cancel (request)
  "Cancel asynchronous API REQUEST."
  (magh-client-cancel request))

(defun magh-api--repo-args (context)
  "Return GitHub CLI repository arguments for CONTEXT."
  (list "--repo" (magh-context-repository context)))

(defun magh-api--domain (context resource &optional id)
  "Build a cache domain for CONTEXT, RESOURCE, and optional ID."
  (append (list :host (magh-context-host context))
          (when (magh-context-repository context)
            (list :repository (magh-context-repository context)))
          (list :resource resource)
          (when id (list :id id))))

(defun magh-api--api-errors (data)
  "Return GraphQL/API errors from DATA, if present."
  (and (listp data)
       (or (alist-get 'errors data)
           (and (alist-get 'message data)
                ;; A successful endpoint can legitimately return a top-level
                ;; `message' field.  REST error objects additionally carry one
                ;; of these metadata fields; HTTP failures are also rejected by
                ;; the transport before this predicate runs.
                (or (alist-get 'documentation_url data)
                    (alist-get 'status data))
                (list (alist-get 'message data))))))

(defun magh-api--error (errors)
  "Return a typed API condition for ERRORS."
  (magh-core--error 'magh-api-error
                  (format "GitHub API error: %s"
                          (mapconcat #'magh-core--name errors "; "))
                  errors))

(cl-defun magh-api--read-json
    (context argv callback errback
             &key force domain transform (cache t) preserve-false stdin)
  "Asynchronously read JSON using ARGV in CONTEXT.
Call CALLBACK with data, optionally processed by TRANSFORM."
  (magh-client--json-async
   argv
   (lambda (data)
     (if-let* ((errors (magh-api--api-errors data)))
         (funcall errback (magh-api--error errors))
       (funcall callback (if transform (funcall transform data) data))))
   errback :context context :force force :domain domain :cache cache
   :json-false-object (and preserve-false :json-false) :stdin stdin))

(cl-defun magh-api--read-text
    (context argv callback errback &key force domain)
  "Asynchronously read text using ARGV in CONTEXT."
  (magh-client--text-async argv callback errback
                         :context context :force force :domain domain))

(defun magh-api--invalidate (domains)
  "Invalidate every cache domain in DOMAINS."
  (dolist (domain (if (keywordp (car-safe domains)) (list domains) domains))
    (magh-client-invalidate domain)))

(cl-defun magh-api--mutate-text
    (context argv domains callback errback &key stdin)
  "Run text mutation ARGV and invalidate DOMAINS on success."
  (magh-client--mutate-text
   argv (lambda (result)
          (magh-api--invalidate domains)
          (funcall callback result))
   errback :context context :stdin stdin))

(cl-defun magh-api--mutate-json
    (context argv domains callback errback &key stdin preserve-false)
  "Run JSON mutation ARGV and invalidate DOMAINS on success."
  (magh-client--mutate-json
   argv (lambda (result)
          (if-let* ((errors (magh-api--api-errors result)))
              (funcall errback (magh-api--error errors))
            (magh-api--invalidate domains)
            (funcall callback result)))
   errback :context context :stdin stdin
   :json-false-object (and preserve-false :json-false)))

(defun magh-api--flag (flag value)
  "Return CLI FLAG and VALUE arguments, or nil if VALUE is nil."
  (when value
    (list flag (if (eq value :json-false) "false" (format "%s" value)))))

(defun magh-api--true-p (value)
  "Return non-nil when VALUE represents an explicit true value."
  (and value (not (eq value :json-false))))

(defun magh-api--boolean-flag (flag value)
  "Return a single boolean FLAG argument for VALUE."
  (list (format "%s=%s" flag (if (magh-api--true-p value)
                                  "true" "false"))))

(defun magh-api--repeated-flags (flag values)
  "Return repeated FLAG arguments for VALUES."
  (seq-mapcat (lambda (value) (list flag value)) values))

(defun magh-api--plist-args (values specs)
  "Build CLI arguments from VALUES according to SPECS.
Each entry is (KEY FLAG KIND), where KIND is `repeat', `switch', `boolean',
`present', `raw', or nil for an optional FLAG value."
  (cl-loop for (key flag kind) in specs
           for value = (plist-get values key)
           append (pcase kind
                    ('repeat (magh-api--repeated-flags flag value))
                    ('switch (and (magh-api--true-p value) (list flag)))
                    ('boolean (and (plist-member values key)
                                   (magh-api--boolean-flag flag value)))
                    ('present (and (plist-member values key)
                                   (list flag (or value ""))))
                    ('raw (and value (list flag value)))
                    (_ (magh-api--flag flag value)))))

(defun magh-api--topic-edit-args (values)
  "Return CLI arguments shared by Issue and Pull Request edits."
  (append
   (magh-api--flag "--milestone" (plist-get values :milestone))
   (when (plist-get values :remove-milestone) '("--remove-milestone"))
   (magh-api--plist-args
    values '((:add-reviewers "--add-reviewer" repeat)
             (:remove-reviewers "--remove-reviewer" repeat)
             (:add-assignees "--add-assignee" repeat)
             (:remove-assignees "--remove-assignee" repeat)
             (:add-labels "--add-label" repeat)
             (:remove-labels "--remove-label" repeat)
             (:add-projects "--add-project" repeat)
             (:remove-projects "--remove-project" repeat)))
   (when (plist-member values :body) '("--body-file" "-"))))

(defun magh-api--flatten-pages (data)
  "Flatten `gh api --paginate --slurp' DATA."
  (seq-mapcat #'identity data))

(defun magh-api--read-rest-pages
    (context endpoint fields callback errback force domain
             &optional transform headers)
  "Read paginated REST ENDPOINT with FIELDS and flatten its result."
  (magh-api--read-json
   context (magh-api--rest-argv endpoint "GET" fields t headers) callback errback
   :force force :transform (or transform #'magh-api--flatten-pages)
   :domain domain))

(cl-defun magh-api--read-repo-rest-pages
    (context path resource callback errback
             &key force fields id transform headers)
  "Read repository REST pages at PATH in CONTEXT.
RESOURCE and optional ID identify the cache domain.  FIELDS defaults to a
100-item page size; TRANSFORM and HEADERS are forwarded to the REST reader."
  (setq context (magh-api--context context t))
  (magh-api--read-rest-pages
   context (magh-core--repo-endpoint context path)
   (or fields '((per_page . 100))) callback errback force
   (magh-api--domain context resource id) transform headers))

(defun magh-api--decode-content (data)
  "Decode GitHub Contents API DATA as UTF-8 text."
  (let ((content (alist-get 'content data)))
    (if (equal (alist-get 'encoding data) "base64")
        (decode-coding-string
         (base64-decode-string content)
         'utf-8)
      content)))

(defun magh-api--rest-argv (endpoint method &optional fields paginate headers)
  "Build safe `gh api' argv for ENDPOINT.
FIELDS is an alist of request fields.  PAGINATE adds `--paginate --slurp'.
HEADERS is a list of complete header strings."
  (append (list "api" endpoint "--method" method)
          (when paginate (list "--paginate" "--slurp"))
          (magh-api--repeated-flags "-H" headers)
          (seq-mapcat
           (lambda (field)
             (let ((key (car field)) (value (cdr field)))
               (list (if (or (numberp value)
                             (eq value t) (eq value :json-false))
                         "-F" "-f")
                     (format "%s=%s" key
                             (cond ((eq value t) "true")
                                   ((eq value :json-false) "false")
                                   (t value))))))
           fields)))

;;; User

(defun magh-api--user-get (context login callback errback &optional force)
  "Fetch LOGIN profile, or the current viewer when LOGIN is nil."
  (setq context (magh-api--context context))
  (magh-api--read-json
   context (list "api" (if login (format "users/%s" login) "user"))
   callback errback :force force
   :domain (magh-api--domain context 'user login)))

(defun magh-api--user-repositories
    (context owner callback errback &optional force limit)
  "Fetch repositories for OWNER, or the current account when OWNER is nil."
  (setq context (magh-api--context context))
  (magh-api--read-json
   context
   (append (list "repo" "list") (when owner (list owner))
           (list "--limit" (number-to-string (or limit magh-list-limit))
                 "--json" (magh-api--fields magh-api--repo-fields)))
   callback errback :force force
   :domain (magh-api--domain context 'repository-list owner)))

(defun magh-api--starred-repositories
    (context callback errback &optional force)
  "Fetch repositories starred by the current account."
  (setq context (magh-api--context context))
  (magh-api--read-rest-pages
   context "user/starred" `((per_page . ,(min 100 magh-list-limit)))
   callback errback force (magh-api--domain context 'starred) nil
   '("Accept: application/vnd.github.star+json")))

(defun magh-api--review-requests (context callback errback &optional force)
  "Fetch pull requests requesting review from the current account."
  (magh-api--search context 'prs "" callback errback force
                  '(:review-requested "@me" :state "open")))

(defun magh-api--assigned-issues (context callback errback &optional force)
  "Fetch issues assigned to the current account."
  (magh-api--search context 'issues "" callback errback force
                  '(:assignee "@me" :state "open")))

(defun magh-api--assigned-prs (context callback errback &optional force)
  "Fetch pull requests assigned to the current account."
  (magh-api--search context 'prs "" callback errback force
                  '(:assignee "@me" :state "open")))

(defun magh-api--my-prs (context callback errback &optional force)
  "Fetch open pull requests authored by the current account."
  (magh-api--search context 'prs "" callback errback force
                  '(:author "@me" :state "open")))

;;; Repository

(defun magh-api--repo-get (context callback errback &optional force)
  "Fetch repository metadata for CONTEXT."
  (setq context (magh-api--context context t))
  (magh-api--read-json
   context
   (list "repo" "view" (magh-context-repository context)
         "--json" (magh-api--fields magh-api--repo-fields))
   callback errback :force force :domain (magh-api--domain context 'repository)))

(defun magh-api--repo-viewer-forked-p
    (context callback errback &optional force)
  "Return whether the current viewer owns a fork of CONTEXT."
  (setq context (magh-api--context context t))
  (pcase-let* ((`(,owner ,name)
                (split-string (magh-context-repository context) "/" t))
               (query
                (concat
                 "query($owner:String!,$name:String!){"
                 "repository(owner:$owner,name:$name){"
                 "forks(first:1,affiliations:[OWNER]){totalCount}}}")))
    (magh-api--read-json
     context
     (list "api" "graphql" "-f" (concat "query=" query)
           "-F" (format "owner=%s" owner)
           "-F" (format "name=%s" name))
     callback errback :force force
     :transform
     (lambda (data)
       (> (or (magh-api--json-at data 'data 'repository 'forks 'totalCount) 0)
          0))
     :domain (magh-api--domain context 'viewer-fork))))

(defun magh-api--repo-languages (context callback errback &optional force)
  "Fetch repository language byte counts."
  (setq context (magh-api--context context t))
  (magh-api--read-json
   context (list "api" (magh-core--repo-endpoint context "languages"))
   callback errback :force force :domain (magh-api--domain context 'statistics)))

(defun magh-api--repo-branches (context callback errback &optional force)
  "Fetch repository branches."
  (magh-api--read-repo-rest-pages
   context "branches" 'branch callback errback :force force
   :fields `((per_page . ,(min 100 magh-list-limit)))))

(defun magh-api--repo-tags (context callback errback &optional force)
  "Fetch repository tags."
  (magh-api--read-repo-rest-pages
   context "tags" 'tag callback errback :force force
   :fields `((per_page . ,(min 100 magh-list-limit)))))

(defun magh-api--repo-labels (context callback errback &optional force)
  "Fetch labels available in CONTEXT."
  (magh-api--read-repo-rest-pages
   context "labels" 'label callback errback :force force))

(defun magh-api--repo-milestones (context callback errback &optional force)
  "Fetch open milestones available in CONTEXT."
  (magh-api--read-repo-rest-pages
   context "milestones" 'milestone callback errback :force force
   :fields '((state . "open") (per_page . 100))))

(defun magh-api--repo-collaborators (context callback errback &optional force)
  "Fetch assignable repository collaborators."
  (magh-api--read-repo-rest-pages
   context "assignees" 'collaborator callback errback :force force))

(defun magh-api--project-owner (context &optional owner)
  "Return explicit OWNER, CONTEXT owner, or the current account marker."
  (or owner (magh-context-owner context) "@me"))

(defun magh-api--project-domains (context owner &optional number)
  "Return cache domains affected by Project NUMBER for OWNER."
  (list (magh-api--domain context 'project-list owner)
        (magh-api--domain context 'project (and number (list owner number)))
        (magh-api--domain context 'project-item (and number (list owner number)))
        (magh-api--domain context 'project-field (and number (list owner number)))))

(defun magh-api--project-list
    (context callback errback &optional force owner include-closed)
  "Fetch Projects for OWNER.
OWNER defaults to the repository owner in CONTEXT, or `@me'.  When
INCLUDE-CLOSED is non-nil, include closed Projects."
  (setq context (magh-api--context context)
        owner (magh-api--project-owner context owner))
  (magh-api--read-json
   context
   (append (list "project" "list" "--owner" owner
                 "--limit" (number-to-string magh-list-limit)
                 "--format" "json")
           (when include-closed '("--closed")))
   callback errback :force force
   :transform (lambda (data) (alist-get 'projects data))
   :domain (magh-api--domain context 'project-list owner)))

(defun magh-api--project-get
    (context owner number callback errback &optional force)
  "Fetch Project NUMBER owned by OWNER."
  (setq context (magh-api--context context)
        owner (magh-api--project-owner context owner))
  (magh-api--read-json
   context
   (list "project" "view" (format "%s" number) "--owner" owner
         "--format" "json")
   callback errback :force force
   :domain (magh-api--domain context 'project (list owner number))))

(defun magh-api--project-items
    (context owner number callback errback &optional force)
  "Fetch items in Project NUMBER owned by OWNER."
  (setq context (magh-api--context context)
        owner (magh-api--project-owner context owner))
  (magh-api--read-json
   context
   (list "project" "item-list" (format "%s" number) "--owner" owner
         "--limit" (number-to-string magh-list-limit) "--format" "json")
   callback errback :force force
   :transform (lambda (data) (alist-get 'items data))
   :domain (magh-api--domain context 'project-item (list owner number))))

(defun magh-api--project-fields
    (context project-id callback errback &optional force)
  "Fetch editable metadata for fields in PROJECT-ID."
  (setq context (magh-api--context context))
  (magh-api--read-json
   context '("api" "graphql" "--input" "-") callback errback
   :force force
   :stdin (magh-api--graphql-payload
           magh-api--project-fields-query `((id . ,project-id)))
   :transform
   (lambda (data)
     (magh-api--json-at data 'data 'node 'fields 'nodes))
   :domain (magh-api--domain context 'project-field project-id)))

(defun magh-api--project-create (context owner values callback errback)
  "Create an OWNER Project using VALUES."
  (setq context (magh-api--context context)
        owner (magh-api--project-owner context owner))
  (magh-api--mutate-json
   context
   (append (list "project" "create" "--owner" owner "--format" "json")
           (magh-api--flag "--title" (plist-get values :title)))
   (magh-api--project-domains context owner) callback errback))

(defun magh-api--project-edit
    (context owner number values callback errback)
  "Edit Project NUMBER owned by OWNER using VALUES."
  (setq context (magh-api--context context)
        owner (magh-api--project-owner context owner))
  (magh-api--mutate-json
   context
   (append
    (list "project" "edit" (format "%s" number) "--owner" owner
          "--format" "json")
    (magh-api--plist-args
     values '((:title "--title" present) (:description "--description" present)
              (:readme "--readme" present)
              (:visibility "--visibility" present))))
   (magh-api--project-domains context owner number) callback errback))

(defun magh-api--project-close
    (context owner number closed callback errback)
  "Close Project NUMBER owned by OWNER, or reopen it when CLOSED is nil."
  (setq context (magh-api--context context)
        owner (magh-api--project-owner context owner))
  (magh-api--mutate-json
   context
   (append (list "project" "close" (format "%s" number)
                 "--owner" owner "--format" "json")
           (unless closed '("--undo")))
   (magh-api--project-domains context owner number) callback errback))

(defun magh-api--project-item-add
    (context owner number url callback errback)
  "Add the Issue or Pull Request at URL to Project NUMBER."
  (setq context (magh-api--context context)
        owner (magh-api--project-owner context owner))
  (magh-api--mutate-json
   context
   (list "project" "item-add" (format "%s" number) "--owner" owner
         "--url" url "--format" "json")
   (magh-api--project-domains context owner number) callback errback))

(defun magh-api--project-item-create
    (context owner number values callback errback)
  "Create a draft Issue in Project NUMBER using VALUES."
  (setq context (magh-api--context context)
        owner (magh-api--project-owner context owner))
  (magh-api--mutate-json
   context
   (append (list "project" "item-create" (format "%s" number)
                 "--owner" owner "--format" "json")
           (magh-api--flag "--title" (plist-get values :title))
           (magh-api--flag "--body" (plist-get values :body)))
   (magh-api--project-domains context owner number) callback errback))

(defun magh-api--project-draft-edit
    (context owner number item-id values callback errback)
  "Edit draft ITEM-ID in Project NUMBER using VALUES."
  (setq context (magh-api--context context)
        owner (magh-api--project-owner context owner))
  (magh-api--mutate-json
   context
   (append (list "project" "item-edit" "--id" item-id "--format" "json")
           (magh-api--plist-args
            values '((:title "--title" present) (:body "--body" present))))
   (magh-api--project-domains context owner number) callback errback))

(defun magh-api--project-field-args
    (project-id item-id field-id type value clear)
  "Build safe `gh project item-edit' arguments for one field.
PROJECT-ID, ITEM-ID, and FIELD-ID are node IDs.  TYPE is the Project field
data type.  VALUE is ignored when CLEAR is non-nil."
  (let ((flag
         (pcase (upcase (format "%s" type))
           ("TEXT" "--text")
           ("NUMBER" "--number")
           ("DATE" "--date")
           ("SINGLE_SELECT" "--single-select-option-id")
           ("ITERATION" "--iteration-id")
           (_ (user-error "Project field type %s is not editable" type)))))
    (append (list "project" "item-edit" "--id" item-id
                  "--project-id" project-id "--field-id" field-id)
            (if clear '("--clear") (list flag (format "%s" value)))
            '("--format" "json"))))

(defun magh-api--project-field-edit
    (context owner number project-id item-id field-id type value clear
             callback errback)
  "Set or CLEAR one Project field on ITEM-ID."
  (setq context (magh-api--context context)
        owner (magh-api--project-owner context owner))
  (magh-api--mutate-json
   context
   (magh-api--project-field-args
    project-id item-id field-id type value clear)
   (magh-api--project-domains context owner number) callback errback))

(defun magh-api--project-item-archive
    (context owner number item-id archived callback errback)
  "Archive Project ITEM-ID, or restore it when ARCHIVED is nil."
  (setq context (magh-api--context context)
        owner (magh-api--project-owner context owner))
  (magh-api--mutate-json
   context
   (append (list "project" "item-archive" (format "%s" number)
                 "--owner" owner "--id" item-id "--format" "json")
           (unless archived '("--undo")))
   (magh-api--project-domains context owner number) callback errback))

(defun magh-api--project-item-remove
    (context owner number item-id callback errback)
  "Remove ITEM-ID from Project NUMBER without deleting its content."
  (setq context (magh-api--context context)
        owner (magh-api--project-owner context owner))
  (magh-api--mutate-json
   context
   (list "project" "item-delete" (format "%s" number) "--owner" owner
         "--id" item-id "--format" "json")
   (magh-api--project-domains context owner number) callback errback))

(defun magh-api--repo-create (context values callback errback)
  "Create a repository from VALUES plist."
  (setq context (magh-api--context context))
  (let ((name (plist-get values :name)))
    (magh-api--mutate-text
     context
     (append (list "repo" "create"
                   (if-let* ((owner (plist-get values :owner)))
                       (format "%s/%s" owner name) name))
             (magh-api--plist-args
              values '((:description "--description") (:homepage "--homepage")
                       (:private "--private" switch) (:public "--public" switch)
                       (:internal "--internal" switch) (:template "--template")
                       (:source "--source") (:push "--push" switch)
                       (:clone "--clone" switch))))
     (magh-api--domain context 'repository-list) callback errback)))

(defun magh-api--repo-clone (context directory callback errback &optional git-args)
  "Clone CONTEXT into DIRECTORY asynchronously."
  (setq context (magh-api--context context t))
  (magh-api--mutate-text
   context (append (list "repo" "clone" (magh-context-repository context) directory)
                   (when git-args (cons "--" git-args)))
   nil callback errback))

(defun magh-api--repo-fork (context values callback errback)
  "Fork CONTEXT according to VALUES plist."
  (setq context (magh-api--context context t))
  (magh-api--mutate-text
   context
   (append (list "repo" "fork" (magh-context-repository context))
           (magh-api--plist-args
            values '((:organization "--org") (:remote-name "--remote-name")
                     (:clone "--clone" switch) (:remote "--remote" switch))))
   (list (magh-api--domain context 'repository)
         (magh-api--domain context 'viewer-fork)
         (magh-api--domain context 'repository-list))
   callback errback))

(defun magh-api--repo-rename (context name callback errback)
  "Rename CONTEXT repository to NAME."
  (setq context (magh-api--context context t))
  (magh-api--mutate-text
   context (append (list "repo" "rename" name "--yes")
                   (magh-api--repo-args context))
   (magh-api--domain context 'repository) callback errback))

(defun magh-api--repo-delete (context callback errback)
  "Delete CONTEXT repository."
  (setq context (magh-api--context context t))
  (magh-api--mutate-text
   context (list "repo" "delete" (magh-context-repository context) "--yes")
   (list (magh-api--domain context 'repository)
         (magh-api--domain context 'repository-list))
   callback errback))

(defun magh-api--repo-edit (context values callback errback)
  "Edit repository settings from VALUES plist."
  (setq context (magh-api--context context t))
  (magh-api--mutate-text
   context
   (append
    (list "repo" "edit" (magh-context-repository context))
    (magh-api--plist-args
     values
     '((:default-branch "--default-branch") (:description "--description")
       (:homepage "--homepage") (:visibility "--visibility")
       (:template "--template" boolean) (:issues "--enable-issues" boolean)
       (:projects "--enable-projects" boolean)
       (:discussions "--enable-discussions" boolean)
       (:wiki "--enable-wiki" boolean)
       (:merge-commit "--enable-merge-commit" boolean)
       (:squash-merge "--enable-squash-merge" boolean)
       (:rebase-merge "--enable-rebase-merge" boolean)
       (:delete-branch-on-merge "--delete-branch-on-merge" boolean)))
    (when (plist-get values :visibility)
      '("--accept-visibility-change-consequences"))
    (magh-api--repeated-flags "--add-topic" (plist-get values :add-topics))
    (magh-api--repeated-flags "--remove-topic" (plist-get values :remove-topics)))
   (magh-api--domain context 'repository) callback errback))

(defun magh-api--branch-create (context branch sha callback errback)
  "Create BRANCH at SHA in CONTEXT."
  (setq context (magh-api--context context t))
  (magh-api--mutate-json
   context
   (magh-api--rest-argv
    (magh-core--repo-endpoint context "git/refs") "POST"
    `((ref . ,(format "refs/heads/%s" branch))
      (sha . ,sha)))
   (magh-api--domain context 'branch) callback errback))

(defun magh-api--branch-delete (context branch callback errback)
  "Delete remote BRANCH in CONTEXT."
  (setq context (magh-api--context context t))
  (magh-api--mutate-text
   context
   (magh-api--rest-argv
    (magh-core--repo-endpoint
     context (format "git/refs/heads/%s" (magh-core--url-path branch)))
    "DELETE")
   (magh-api--domain context 'branch) callback errback))

;;; Issue

(defun magh-api--page-size (params)
  "Return a valid GitHub page size for PARAMS."
  (max 1 (min 100 (or (plist-get params :limit) magh-list-limit))))

(defun magh-api--search-qualifier (name value)
  "Return GitHub search qualifier NAME for VALUE."
  (let ((text (format "%s" value)))
    (when (string-match-p "[[:space:]\\\"\\\\]" text)
      (setq text
            (concat
             "\""
             (replace-regexp-in-string
              "[\\\"\\\\]" (lambda (match) (concat "\\\\" match)) text t t)
             "\"")))
    (format "%s:%s" name text)))

(defun magh-api--topic-search-query (context kind params)
  "Build a stable search query for KIND in CONTEXT using PARAMS."
  (let* ((state (downcase (or (plist-get params :state)
                              (if (eq kind 'issue)
                                  magh-default-issue-state
                                magh-default-pr-state))))
         (raw (string-trim (or (plist-get params :search) "")))
         ;; Build in reverse so repeated filters stay linear without changing
         ;; their stable CLI order.
         (parts (list (if (eq kind 'issue) "is:issue" "is:pr")
                      (magh-api--search-qualifier
                       "repo" (magh-context-repository context)))))
    (dolist (part (pcase (cons kind state)
                    (`(issue . "open") '("is:open"))
                    (`(issue . "closed") '("is:closed"))
                    (`(pr . "open") '("is:open"))
                    (`(pr . "closed") '("is:closed" "is:unmerged"))
                    (`(pr . "merged") '("is:merged"))))
      (push part parts))
    (dolist (entry `(("assignee" . ,(plist-get params :assignee))
                     ("author" . ,(plist-get params :author))
                     ("mentions" . ,(plist-get params :mention))
                     ("milestone" . ,(plist-get params :milestone))))
      (when-let* ((value (cdr entry)))
        (push (magh-api--search-qualifier (car entry) value) parts)))
    (dolist (label (plist-get params :labels))
      (push (magh-api--search-qualifier "label" label) parts))
    (when (eq kind 'pr)
      (dolist (entry `(("base" . ,(plist-get params :base))
                       ("head" . ,(plist-get params :head))))
        (when-let* ((value (cdr entry)))
          (push (magh-api--search-qualifier (car entry) value) parts)))
      (when (magh-api--true-p (plist-get params :draft))
        (push "draft:true" parts)))
    (unless (string-empty-p raw)
      (push raw parts))
    (unless (string-match-p "\\(?:\\`\\|[[:space:]]\\)sort:" raw)
      (push "sort:created-desc" parts))
    (string-join (nreverse parts) " ")))

(defun magh-api--normalize-topic-node (node)
  "Normalize GraphQL search NODE to the shapes used by native pages."
  (setq node (copy-tree node))
  (dolist (key '(assignees labels))
    (when-let* ((connection (alist-get key node)))
      (setf (alist-get key node) (alist-get 'nodes connection))))
  (assq-delete-all '__typename node))

(defun magh-api--topic-page-transform (data)
  "Convert GraphQL search DATA to a `magh-page'."
  (let* ((connection (magh-api--json-at data 'data 'search))
         (page-info (alist-get 'pageInfo connection)))
    (magh-page-create
     :items (seq-keep #'magh-api--normalize-topic-node
                      (alist-get 'nodes connection))
     :next (and (alist-get 'hasNextPage page-info)
                (alist-get 'endCursor page-info)))))

(defun magh-api--topic-page
    (context kind params cursor callback errback &optional force)
  "Fetch one cursor page of KIND in CONTEXT using PARAMS and CURSOR."
  (setq context (magh-api--context context t))
  (let ((variables
         `((queryString . ,(magh-api--topic-search-query context kind params))
           (first . ,(magh-api--page-size params))
           (after . ,(or cursor :null)))))
    (magh-api--read-json
     context '("api" "graphql" "--input" "-") callback errback
     :force force
     :domain (magh-api--domain context kind (list params cursor))
     :transform #'magh-api--topic-page-transform
     :stdin (magh-api--graphql-payload magh-api--topic-page-query variables))))

(defun magh-api--topic-list
    (context kind params default-state fields specs callback errback force)
  "Fetch CLI topic KIND using PARAMS, FIELDS, and argument SPECS."
  (setq context (magh-api--context context t))
  (magh-api--read-json
   context
   (append
    (list (symbol-name kind) "list"
          "--state" (or (plist-get params :state) default-state)
          "--limit" (number-to-string
                      (or (plist-get params :limit) magh-list-limit))
          "--json" (magh-api--fields fields))
    (magh-api--plist-args params specs)
    (magh-api--repo-args context))
   callback errback :force force :domain (magh-api--domain context kind)))

(defun magh-api--topic-get
    (context kind number fields callback errback force)
  "Fetch topic KIND NUMBER with FIELDS in CONTEXT."
  (setq context (magh-api--context context t))
  (magh-api--read-json
   context
   (append (list (symbol-name kind) "view" (number-to-string number)
                 "--json" (magh-api--fields fields))
           (magh-api--repo-args context))
   callback errback :force force
   :domain (magh-api--domain context kind number)))

(defun magh-api--topic-edit
    (context kind number values extra-args callback errback)
  "Edit topic KIND NUMBER using VALUES and EXTRA-ARGS."
  (setq context (magh-api--context context t))
  (let ((bodyp (plist-member values :body)))
    (magh-api--mutate-text
     context
     (append (list (symbol-name kind) "edit" (number-to-string number))
             (magh-api--flag "--title" (plist-get values :title))
             extra-args
             (magh-api--topic-edit-args values)
             (magh-api--repo-args context))
     (magh-api--domain context kind) callback errback
     :stdin (and bodyp (or (plist-get values :body) "")))))

(defun magh-api--topic-comment
    (context kind number body callback errback)
  "Comment BODY on topic KIND NUMBER."
  (setq context (magh-api--context context t))
  (magh-api--mutate-text
   context
   (append (list (symbol-name kind) "comment" (number-to-string number)
                 "--body-file" "-")
           (magh-api--repo-args context))
   (magh-api--domain context kind) callback errback :stdin body))

(defun magh-api--issue-page
    (context params cursor callback errback &optional force)
  "Fetch one cursor-paginated Issue page in CONTEXT."
  (magh-api--topic-page context 'issue params cursor callback errback force))

(defun magh-api--issue-list
    (context params callback errback &optional force)
  "Fetch an Issue list in CONTEXT using PARAMS plist."
  (magh-api--topic-list
   context 'issue params magh-default-issue-state magh-api--issue-list-fields
   '((:search "--search") (:assignee "--assignee")
     (:author "--author") (:mention "--mention")
     (:milestone "--milestone") (:labels "--label" repeat))
   callback errback force))

(defun magh-api--issue-get (context number callback errback &optional force)
  "Fetch Issue NUMBER in CONTEXT."
  (magh-api--topic-get
   context 'issue number magh-api--issue-view-fields callback errback force))

(defun magh-api--issue-create (context values callback errback)
  "Create an Issue using VALUES plist."
  (setq context (magh-api--context context t))
  (let ((body (or (plist-get values :body) "")))
    (magh-api--mutate-text
     context
     (append (list "issue" "create" "--title" (plist-get values :title)
                   "--body-file" "-")
             (magh-api--plist-args
              values '((:assignees "--assignee" repeat)
                       (:labels "--label" repeat) (:projects "--project" repeat)
                       (:milestone "--milestone")))
             (magh-api--repo-args context))
     (magh-api--domain context 'issue) callback errback :stdin body)))

(defun magh-api--issue-edit (context number values callback errback)
  "Edit Issue NUMBER using VALUES plist."
  (magh-api--topic-edit context 'issue number values nil callback errback))

(defun magh-api--issue-comment (context number body callback errback)
  "Comment BODY on Issue NUMBER."
  (magh-api--topic-comment context 'issue number body callback errback))

(defun magh-api--issue-close (context number reason comment callback errback)
  "Close Issue NUMBER with REASON and optional COMMENT."
  (setq context (magh-api--context context t))
  (magh-api--mutate-text
   context
   (append (list "issue" "close" (number-to-string number))
           (magh-api--flag "--reason" reason)
           (magh-api--flag "--comment" comment)
           (magh-api--repo-args context))
   (magh-api--domain context 'issue) callback errback))

(defun magh-api--issue-reopen (context number comment callback errback)
  "Reopen Issue NUMBER and optionally add COMMENT."
  (setq context (magh-api--context context t))
  (magh-api--mutate-text
   context
   (append (list "issue" "reopen" (number-to-string number))
           (magh-api--flag "--comment" comment)
           (magh-api--repo-args context))
   (magh-api--domain context 'issue) callback errback))

(defun magh-api--issue-pin (context number pinned callback errback)
  "Pin Issue NUMBER when PINNED, otherwise unpin it."
  (setq context (magh-api--context context t))
  (magh-api--mutate-text
   context
   (append (list "issue" (if pinned "pin" "unpin")
                 (number-to-string number))
           (magh-api--repo-args context))
   (magh-api--domain context 'issue) callback errback))

(defun magh-api--issue-lock (context number locked reason callback errback)
  "Lock Issue NUMBER when LOCKED, optionally using REASON; otherwise unlock."
  (setq context (magh-api--context context t))
  (magh-api--mutate-text
   context
   (append (list "issue" (if locked "lock" "unlock")
                 (number-to-string number))
           (and locked (magh-api--flag "--reason" reason))
           (magh-api--repo-args context))
   (magh-api--domain context 'issue) callback errback))

(defun magh-api--issue-develop (context number branch base checkout callback errback)
  "Start linked development for Issue NUMBER on BRANCH from BASE."
  (setq context (magh-api--context context t))
  (magh-api--mutate-text
   context
   (append (list "issue" "develop" (number-to-string number))
           (magh-api--flag "--name" branch)
           (magh-api--flag "--base" base)
           (when checkout (list "--checkout"))
           (magh-api--repo-args context))
   (list (magh-api--domain context 'issue)
         (magh-api--domain context 'branch))
   callback errback))

;;; Pull Request

(defun magh-api--pr-page
    (context params cursor callback errback &optional force)
  "Fetch one cursor-paginated Pull Request page in CONTEXT."
  (magh-api--topic-page context 'pr params cursor callback errback force))

(defun magh-api--pr-list (context params callback errback &optional force)
  "Fetch Pull Requests in CONTEXT using PARAMS plist."
  (magh-api--topic-list
   context 'pr params magh-default-pr-state magh-api--pr-list-fields
   '((:search "--search") (:assignee "--assignee")
     (:author "--author") (:base "--base") (:head "--head")
     (:labels "--label" repeat) (:draft "--draft" switch))
   callback errback force))

(defun magh-api--pr-get (context number callback errback &optional force)
  "Fetch Pull Request NUMBER in CONTEXT."
  (magh-api--topic-get
   context 'pr number magh-api--pr-view-fields callback errback force))

(defun magh-api--pr-commits (context number callback errback &optional force)
  "Fetch commits belonging to Pull Request NUMBER."
  (magh-api--read-repo-rest-pages
   context (format "pulls/%d/commits" number) 'pr-commit callback errback
   :force force :id number))

(defun magh-api--pr-files (context number callback errback &optional force)
  "Fetch changed files for Pull Request NUMBER."
  (magh-api--read-repo-rest-pages
   context (format "pulls/%d/files" number) 'pr-file callback errback
   :force force :id number))

(defun magh-api--pr-reviews (context number callback errback &optional force)
  "Fetch submitted reviews for Pull Request NUMBER."
  (magh-api--read-repo-rest-pages
   context (format "pulls/%d/reviews" number) 'pr callback errback
   :force force :id number))

(defun magh-api--compare (context base head callback errback &optional force)
  "Fetch the repository comparison from BASE through HEAD."
  (setq context (magh-api--context context t))
  (magh-api--read-json
   context
   (magh-api--rest-argv
    (magh-core--repo-endpoint
     context (format "compare/%s...%s"
                     (magh-core--url-path base)
                     (magh-core--url-path head)))
    "GET" '((per_page . 100)))
   callback errback :force force
   :domain (magh-api--domain context 'comparison (format "%s...%s" base head))))

(defun magh-api--pr-review-comments
    (context number callback errback &optional force)
  "Fetch inline review comments for Pull Request NUMBER."
  (magh-api--read-repo-rest-pages
   context (format "pulls/%d/comments" number) 'pr-review-comment callback errback
   :force force :id number))

(defun magh-api--pr-review-thread-pages (data)
  "Return review thread metadata nodes from paginated GraphQL DATA."
  (seq-mapcat
   (lambda (page)
     (magh-api--json-at page 'data 'repository 'pullRequest
                      'reviewThreads 'nodes))
   data))

(defun magh-api--pr-review-thread-metadata
    (context number callback errback &optional force)
  "Fetch paginated thread metadata for Pull Request NUMBER."
  (setq context (magh-api--context context t))
  (pcase-let* ((`(,owner ,name)
                (split-string (magh-context-repository context) "/" t))
               (query
                (concat
                 "query($owner:String!,$name:String!,$number:Int!,"
                 "$endCursor:String){repository(owner:$owner,name:$name){"
                 "pullRequest(number:$number){reviewThreads(first:100,"
                 "after:$endCursor){nodes{id path line startLine diffSide "
                 "startDiffSide subjectType isResolved isOutdated "
                 "viewerCanReply viewerCanResolve viewerCanUnresolve "
                 "resolvedBy{login} comments(first:1){nodes{databaseId}}}"
                 "pageInfo{hasNextPage endCursor}}}}}")))
    (magh-api--read-json
     context
     (list "api" "graphql" "--paginate" "--slurp"
           "-f" (concat "query=" query)
           "-F" (format "owner=%s" owner)
           "-F" (format "name=%s" name)
           "-F" (format "number=%s" number))
     callback errback :force force :preserve-false t
     :transform #'magh-api--pr-review-thread-pages
     :domain (magh-api--domain context 'pr-review-thread number))))

(defun magh-api--pr-review-normalize-threads (comments metadata)
  "Combine flat REST COMMENTS with GraphQL thread METADATA."
  (let ((metadata-by-root (make-hash-table :test #'equal))
        (comment-groups
         (seq-group-by (lambda (comment)
                         (alist-get 'in_reply_to_id comment))
                       comments)))
    (dolist (thread metadata)
      (when-let* ((root
                   (car (magh-api--json-at thread 'comments 'nodes)))
                  (database-id (alist-get 'databaseId root)))
        (puthash database-id thread metadata-by-root)))
    (mapcar
     (lambda (root)
       (let* ((root-id (alist-get 'id root))
              (thread (gethash root-id metadata-by-root))
              (line (or (alist-get 'line thread)
                        (alist-get 'line root)
                        (alist-get 'original_line root)))
              (start-line (or (alist-get 'startLine thread)
                              (alist-get 'start_line root)
                              (alist-get 'original_start_line root)))
              (side (or (alist-get 'diffSide thread)
                        (alist-get 'side root)))
              (start-side (or (alist-get 'startDiffSide thread)
                              (alist-get 'start_side root))))
         `((id . ,(alist-get 'id thread))
           (root_id . ,root-id)
           (path . ,(or (alist-get 'path thread) (alist-get 'path root)))
           (line . ,line)
           (start_line . ,start-line)
           (side . ,side)
           (start_side . ,start-side)
           (subject_type . ,(or (alist-get 'subjectType thread)
                                (if line "LINE" "FILE")))
           (is_resolved . ,(magh-api--true-p (alist-get 'isResolved thread)))
           (is_outdated . ,(magh-api--true-p (alist-get 'isOutdated thread)))
           (viewer_can_reply . ,(if thread
                                    (magh-api--true-p
                                     (alist-get 'viewerCanReply thread))
                                  t))
           (viewer_can_resolve . ,(magh-api--true-p
                                    (alist-get 'viewerCanResolve thread)))
           (viewer_can_unresolve . ,(magh-api--true-p
                                      (alist-get 'viewerCanUnresolve thread)))
           (resolved_by . ,(alist-get 'resolvedBy thread))
           (comments . ,(cons root
                              (cdr (assoc root-id comment-groups)))))))
     (cdr (assoc nil comment-groups)))))

(defun magh-api--pr-review-threads
    (context number callback errback &optional force)
  "Fetch normalized review threads for Pull Request NUMBER.
REST comments are authoritative for replies.  Missing GraphQL thread metadata
degrades to readable and replyable threads without resolution capabilities."
  (setq context (magh-api--context context t))
  (magh-api--pr-review-comments
   context number
   (lambda (comments)
     (magh-api--pr-review-thread-metadata
      context number
      (lambda (metadata)
        (funcall callback
                 (magh-api--pr-review-normalize-threads comments metadata)))
      (lambda (_error)
        (funcall callback
                 (magh-api--pr-review-normalize-threads comments nil)))
      force))
   errback force))

(defun magh-api--pr-review-reply
    (context number root-id body callback errback)
  "Reply with BODY to top-level review comment ROOT-ID on PR NUMBER."
  (setq context (magh-api--context context t))
  (magh-api--mutate-json
   context
   (magh-api--rest-argv
    (magh-core--repo-endpoint
     context (format "pulls/%d/comments/%s/replies" number root-id))
    "POST" `((body . ,body)))
   (list (magh-api--domain context 'pr number)
         (magh-api--domain context 'pr-review-comment number)
         (magh-api--domain context 'pr-review-thread number))
   callback errback))

(defun magh-api--pr-review-thread-resolved
    (context number thread-id resolved callback errback)
  "Set THREAD-ID resolution state to RESOLVED on Pull Request NUMBER."
  (setq context (magh-api--context context t))
  (let* ((field (if resolved "resolveReviewThread" "unresolveReviewThread"))
         (input-type (if resolved
                         "ResolveReviewThreadInput!"
                       "UnresolveReviewThreadInput!"))
         (query
          (format
           "mutation($input:%s){%s(input:$input){thread{id isResolved}}}"
           input-type field)))
    (magh-api--mutate-json
     context '("api" "graphql" "--input" "-")
     (list (magh-api--domain context 'pr number)
           (magh-api--domain context 'pr-review-thread number))
     callback errback
     :stdin (magh-api--graphql-payload
             query `((input . ((threadId . ,thread-id))))))))

(defun magh-api--pr-diff (context number callback errback &optional force)
  "Fetch the complete diff for Pull Request NUMBER."
  (setq context (magh-api--context context t))
  (magh-api--read-text
   context
   (append (list "pr" "diff" (number-to-string number))
           (magh-api--repo-args context))
   callback errback :force force :domain (magh-api--domain context 'pr-diff number)))

(defun magh-api--pr-create (context values callback errback)
  "Create a Pull Request from VALUES plist."
  (setq context (magh-api--context context t))
  (magh-api--mutate-text
   context
   (append (list "pr" "create" "--title" (plist-get values :title)
                 "--body-file" "-")
           (magh-api--plist-args
            values '((:base "--base") (:head "--head")
                     (:reviewers "--reviewer" repeat)
                     (:assignees "--assignee" repeat) (:labels "--label" repeat)
                     (:projects "--project" repeat) (:milestone "--milestone")
                     (:draft "--draft" switch)))
           (magh-api--repo-args context))
   (magh-api--domain context 'pr) callback errback
   :stdin (or (plist-get values :body) "")))

(defun magh-api--pr-edit (context number values callback errback)
  "Edit Pull Request NUMBER using VALUES plist."
  (magh-api--topic-edit
   context 'pr number values
   (magh-api--flag "--base" (plist-get values :base)) callback errback))

(defun magh-api--pr-comment (context number body callback errback)
  "Add BODY as a Pull Request NUMBER conversation comment."
  (magh-api--topic-comment context 'pr number body callback errback))

(defun magh-api--pr-close (context number comment delete-branch callback errback)
  "Close Pull Request NUMBER with optional COMMENT and DELETE-BRANCH."
  (setq context (magh-api--context context t))
  (magh-api--mutate-text
   context
   (append (list "pr" "close" (number-to-string number))
           (magh-api--flag "--comment" comment)
           (when delete-branch (list "--delete-branch"))
           (magh-api--repo-args context))
   (magh-api--domain context 'pr) callback errback))

(defun magh-api--pr-reopen (context number comment callback errback)
  "Reopen Pull Request NUMBER with optional COMMENT."
  (setq context (magh-api--context context t))
  (magh-api--mutate-text
   context
   (append (list "pr" "reopen" (number-to-string number))
           (magh-api--flag "--comment" comment)
           (magh-api--repo-args context))
   (magh-api--domain context 'pr) callback errback))

(defun magh-api--pr-checkout (context number callback errback)
  "Checkout Pull Request NUMBER in CONTEXT local worktree."
  (setq context (magh-api--context context t))
  (magh-api--mutate-text
   context
   (append (list "pr" "checkout" (number-to-string number))
           (magh-api--repo-args context))
   nil callback errback))

(defun magh-api--pr-merge (context number method options callback errback)
  "Merge Pull Request NUMBER using METHOD and OPTIONS plist."
  (setq context (magh-api--context context t))
  (magh-api--mutate-text
   context
   (append (list "pr" "merge" (number-to-string number)
                 (pcase method ('rebase "--rebase") ('squash "--squash")
                        (_ "--merge")))
           (magh-api--plist-args
            options '((:admin "--admin" switch) (:auto "--auto" switch)
                      (:delete-branch "--delete-branch" switch)
                      (:subject "--subject") (:body "--body")
                      (:match-head-commit "--match-head-commit")))
           (magh-api--repo-args context))
   (magh-api--domain context 'pr) callback errback))

(defun magh-api--pr-auto-merge (context number enabled method callback errback)
  "Set auto-merge for Pull Request NUMBER to ENABLED using METHOD."
  (setq context (magh-api--context context t))
  (magh-api--mutate-text
   context
   (append (list "pr" "merge" (number-to-string number))
           (if enabled
               (list "--auto" (pcase method ('rebase "--rebase")
                                      ('merge "--merge") (_ "--squash")))
             (list "--disable-auto"))
           (magh-api--repo-args context))
   (magh-api--domain context 'pr) callback errback))

(defun magh-api--pr-ready (context number ready callback errback)
  "Mark Pull Request NUMBER READY or draft."
  (setq context (magh-api--context context t))
  (magh-api--mutate-text
   context
   (append (list "pr" "ready" (number-to-string number))
           (unless ready (list "--undo"))
           (magh-api--repo-args context))
   (magh-api--domain context 'pr) callback errback))

(defun magh-api--pr-lock (context number locked reason callback errback)
  "Lock Pull Request NUMBER when LOCKED, otherwise unlock it."
  (setq context (magh-api--context context t))
  (magh-api--mutate-text
   context
   (append (list "issue" (if locked "lock" "unlock")
                 (number-to-string number))
           (and locked (magh-api--flag "--reason" reason))
           (magh-api--repo-args context))
   (magh-api--domain context 'pr) callback errback))

(defun magh-api--json-at (data &rest keys)
  "Return the value below KEYS in nested JSON alist DATA."
  (map-nested-elt data keys))

(defun magh-api--graphql-payload (query variables)
  "Serialize GraphQL QUERY and VARIABLES for `gh api --input -'."
  (json-serialize `((query . ,query) (variables . ,variables))))

(defun magh-api--pr-review-thread-input (review-id comment)
  "Build an AddPullRequestReviewThreadInput for REVIEW-ID and COMMENT."
  (let* ((subject-type
          (upcase (or (plist-get comment :subject-type)
                      (if (plist-member comment :line) "LINE" "FILE"))))
         (input `((pullRequestReviewId . ,review-id)
                  (path . ,(plist-get comment :path))
                  (body . ,(plist-get comment :body))
                  (subjectType . ,subject-type))))
    ;; File-level threads must not contain synthetic line coordinates.
    (when (equal subject-type "LINE")
      (dolist (pair '((:line . line) (:side . side)
                      (:start-line . startLine) (:start-side . startSide)))
        (when (plist-member comment (car pair))
          (push (cons (cdr pair) (plist-get comment (car pair))) input))))
    input))

(defun magh-api--pr-review
    (context number event body comments callback errback &optional head-sha)
  "Submit review EVENT with BODY and inline COMMENTS for Pull Request NUMBER.
COMMENTS is a list of plists with :path and optional :line,
:start-line, :side, :start-side, :subject-type, and :body.  GitHub's draft
review input only supports line threads, so this function creates or reuses a
pending review, adds every LINE or FILE thread asynchronously, and submits it.
When HEAD-SHA is non-nil, bind a newly created pending review to that commit."
  (setq context (magh-api--context context t))
  (let* ((domains (list (magh-api--domain context 'pr number)
                        (magh-api--domain context 'pr-review-comment number)
                        (magh-api--domain context 'pr-review-thread number)))
         (event-name (upcase (symbol-name event)))
         (graphql-argv '("api" "graphql" "--input" "-")))
    (dolist (comment comments)
      (when (or (string-empty-p (plist-get comment :path))
                (string-empty-p (plist-get comment :body)))
        (signal 'magh-invalid-input
                (list "Every review comment requires a non-empty path and body"))))
    (cl-labels
        ((mutate
          (query input domains success failure)
          (magh-api--mutate-json
           context graphql-argv domains success failure
           :stdin (magh-api--graphql-payload query `((input . ,input)))))
         (rollback
          (review-id original-error owned)
          (if owned
              (mutate
               (concat
                "mutation($input:DeletePullRequestReviewInput!){"
                "deletePullRequestReview(input:$input){clientMutationId}}")
               `((pullRequestReviewId . ,review-id)) nil
               (lambda (_) (funcall errback original-error))
               (lambda (_) (funcall errback original-error)))
            (funcall errback original-error)))
         (submit
          (review-id owned)
          (let ((input
                 (append `((pullRequestReviewId . ,review-id)
                           (event . ,event-name))
                         (unless (string-empty-p body) `((body . ,body))))))
            (mutate
             (concat
              "mutation($input:SubmitPullRequestReviewInput!){"
              "submitPullRequestReview(input:$input){"
              "pullRequestReview{id state}}}")
             input domains callback
             (lambda (error) (rollback review-id error owned)))))
         (add-threads
          (review-id remaining owned)
          (if (null remaining)
              (submit review-id owned)
            (mutate
             (concat
              "mutation($input:AddPullRequestReviewThreadInput!){"
              "addPullRequestReviewThread(input:$input){thread{id}}}")
             (magh-api--pr-review-thread-input review-id (car remaining)) nil
             (lambda (_) (add-threads review-id (cdr remaining) owned))
             (lambda (error) (rollback review-id error owned)))))
         (create-pending
          (pull-request-id)
          (mutate
           (concat
            "mutation($input:AddPullRequestReviewInput!){"
            "addPullRequestReview(input:$input){pullRequestReview{id state}}}")
           (append `((pullRequestId . ,pull-request-id))
                   (when head-sha `((commitOID . ,head-sha)))) nil
           (lambda (result)
             (let ((review-id
                    (magh-api--json-at
                     result 'data 'addPullRequestReview
                     'pullRequestReview 'id)))
               (if review-id
                   (add-threads review-id comments t)
                 (funcall
                  errback
                  (magh-core--error
                   'magh-api-error
                   "GitHub did not return the pending review ID")))))
           errback))
         (find-pending
          (pull-request-id)
          (let ((query
                 (concat
                  "query($id:ID!){node(id:$id){... on PullRequest{"
                  "reviews(last:100,states:[PENDING]){"
                  "nodes{id viewerDidAuthor commit{oid}}}}}}}")))
            (magh-api--read-json
             context graphql-argv
             (lambda (result)
               (let* ((nodes
                       (magh-api--json-at
                        result 'data 'node 'reviews 'nodes))
                      (pending
                       (seq-find
                        (lambda (review)
                          (magh-api--true-p
                           (alist-get 'viewerDidAuthor review)))
                        nodes)))
                 (cond
                  ((and pending head-sha
                        (alist-get 'commit pending)
                        (not (equal head-sha
                                    (alist-get 'oid
                                               (alist-get 'commit pending)))))
                   (funcall
                    errback
                    (magh-core--error
                     'magh-api-error
                     "Pending review belongs to an older Pull Request head")))
                  ((alist-get 'id pending)
                   (add-threads (alist-get 'id pending) comments nil))
                  (t (create-pending pull-request-id)))))
             errback :cache nil
             :stdin (magh-api--graphql-payload
                     query `((id . ,pull-request-id))))))
         (begin
          (pr)
          (if-let* ((pull-request-id (alist-get 'id pr)))
              (find-pending pull-request-id)
            (funcall
             errback
             (magh-core--error
              'magh-api-error
              "Pull Request response has no GraphQL node ID")))))
      (magh-api--pr-get context number #'begin errback))))

;;; Actions

(defun magh-api--run-list (context params callback errback &optional force)
  "Fetch workflow runs in CONTEXT using PARAMS plist."
  (setq context (magh-api--context context t))
  (magh-api--read-json
   context
   (append (list "run" "list"
                 "--limit" (number-to-string
                             (or (plist-get params :limit) magh-list-limit))
                 "--json" (magh-api--fields magh-api--run-fields))
           (magh-api--plist-args
            params '((:workflow "--workflow") (:branch "--branch")
                     (:event "--event") (:status "--status") (:user "--user")
                     (:commit "--commit") (:all "--all" switch)))
           (magh-api--repo-args context))
   callback errback :force force :domain (magh-api--domain context 'run)))

(defun magh-api--run-get (context id callback errback &optional force)
  "Fetch workflow Run ID including jobs and steps."
  (setq context (magh-api--context context t))
  (magh-api--read-json
   context
   (append (list "run" "view" (format "%s" id)
                 "--json" (magh-api--fields
                            (append magh-api--run-fields '(jobs))))
           (magh-api--repo-args context))
   callback errback :force force :domain (magh-api--domain context 'run id)))

(defun magh-api--artifact-pages (pages)
  "Flatten artifact arrays from paginated REST PAGES."
  (seq-mapcat (lambda (page) (alist-get 'artifacts page)) pages))

(defun magh-api--artifact-list
    (context run-id callback errback &optional force)
  "Fetch artifacts produced by workflow RUN-ID."
  (setq context (magh-api--context context t))
  (magh-api--read-rest-pages
   context (magh-core--repo-endpoint
            context (format "actions/runs/%s/artifacts" run-id))
   '((per_page . 100)) callback errback force
   (magh-api--domain context 'artifact run-id) #'magh-api--artifact-pages))

(defun magh-api--artifact-download
    (context run-id names directory callback errback)
  "Download and extract NAMES from RUN-ID into DIRECTORY.
When NAMES is nil, download every artifact in the run."
  (setq context (magh-api--context context t))
  (magh-api--mutate-text
   context
   (append (list "run" "download" (format "%s" run-id)
                 "--dir" (expand-file-name directory))
           (magh-api--repeated-flags "--name" names)
           (magh-api--repo-args context))
   nil callback errback))

(defun magh-api--artifact-delete
    (context run-id artifact-id callback errback)
  "Delete ARTIFACT-ID from workflow RUN-ID."
  (setq context (magh-api--context context t))
  (magh-api--mutate-text
   context
   (magh-api--rest-argv
    (magh-core--repo-endpoint
     context (format "actions/artifacts/%s" artifact-id))
    "DELETE")
   (list (magh-api--domain context 'artifact run-id)
         (magh-api--domain context 'run run-id))
   callback errback))

(defun magh-api--run-log (context id job-id callback errback &optional force)
  "Fetch text log for Run ID, optionally restricted to JOB-ID."
  (setq context (magh-api--context context t))
  (magh-api--read-text
   context
   (append (list "run" "view" (format "%s" id) "--log")
           (magh-api--flag "--job" job-id)
           (magh-api--repo-args context))
   callback errback :force force :domain (magh-api--domain context 'run-log id)))

(defun magh-api--run-watch (context id callback errback chunk)
  "Watch Run ID asynchronously, forwarding output to CHUNK."
  (setq context (magh-api--context context t))
  (magh-client--stream
   (append (list "run" "watch" (format "%s" id) "--exit-status")
           (magh-api--repo-args context))
   chunk callback errback :context context))

(defun magh-api--run-rerun (context id failed-only callback errback)
  "Rerun Run ID, restricted to failed jobs when FAILED-ONLY."
  (setq context (magh-api--context context t))
  (magh-api--mutate-text
   context
   (append (list "run" "rerun" (format "%s" id))
           (when failed-only (list "--failed"))
           (magh-api--repo-args context))
   (magh-api--domain context 'run) callback errback))

(defun magh-api--run-rerun-job (context job-id callback errback)
  "Rerun JOB-ID."
  (setq context (magh-api--context context t))
  (magh-api--mutate-text
   context
   (append (list "run" "rerun" "--job" (format "%s" job-id))
           (magh-api--repo-args context))
   (magh-api--domain context 'run) callback errback))

(defun magh-api--run-cancel (context id callback errback)
  "Cancel Run ID."
  (setq context (magh-api--context context t))
  (magh-api--mutate-text
   context
   (append (list "run" "cancel" (format "%s" id))
           (magh-api--repo-args context))
   (magh-api--domain context 'run) callback errback))

(defun magh-api--workflow-list (context callback errback &optional force)
  "Fetch workflows in CONTEXT."
  (setq context (magh-api--context context t))
  (magh-api--read-json
   context
   (append (list "workflow" "list" "--all"
                 "--limit" (number-to-string magh-list-limit)
                 "--json" "id,name,state,path")
           (magh-api--repo-args context))
   callback errback :force force :domain (magh-api--domain context 'workflow)))

(defun magh-api--workflow-get (context workflow callback errback &optional force)
  "Fetch WORKFLOW metadata."
  (setq context (magh-api--context context t))
  (magh-api--read-json
   context
   (list "api" (magh-core--repo-endpoint
                context (format "actions/workflows/%s" workflow)))
   callback errback :force force
   :domain (magh-api--domain context 'workflow workflow)))

(defun magh-api--workflow-dispatch
    (context workflow ref inputs callback errback)
  "Dispatch WORKFLOW at REF with INPUTS alist."
  (setq context (magh-api--context context t))
  (magh-api--mutate-text
   context
   (append (list "workflow" "run" (format "%s" workflow))
           (magh-api--flag "--ref" ref)
           (seq-mapcat
            (lambda (entry)
              (list "--field" (format "%s=%s" (car entry) (cdr entry))))
            inputs)
           (magh-api--repo-args context))
   (magh-api--domain context 'run) callback errback))

(defun magh-api--workflow-enable (context workflow enabled callback errback)
  "Enable WORKFLOW when ENABLED, otherwise disable it."
  (setq context (magh-api--context context t))
  (magh-api--mutate-text
   context
   (append (list "workflow" (if enabled "enable" "disable")
                 (format "%s" workflow))
           (magh-api--repo-args context))
   (magh-api--domain context 'workflow) callback errback))

;;; Release

(defun magh-api--release-list (context callback errback &optional force)
  "Fetch releases in CONTEXT."
  (setq context (magh-api--context context t))
  (magh-api--read-json
   context
   (append (list "release" "list" "--limit" (number-to-string magh-list-limit)
                 "--json" (magh-api--fields magh-api--release-list-fields))
           (magh-api--repo-args context))
   callback errback :force force :domain (magh-api--domain context 'release)))

(defun magh-api--release-get (context tag callback errback &optional force)
  "Fetch Release TAG in CONTEXT."
  (setq context (magh-api--context context t))
  (magh-api--read-json
   context
   (append (list "release" "view" tag "--json"
                 (magh-api--fields magh-api--release-view-fields))
           (magh-api--repo-args context))
   callback errback :force force :domain (magh-api--domain context 'release tag)))

(defun magh-api--normalize-rest-release (data)
  "Normalize REST Release DATA to the public magh.el Release field names."
  (let ((assets
         (mapcar
          (lambda (asset)
            (append
             `((downloadCount . ,(alist-get 'download_count asset))
               (url . ,(or (alist-get 'browser_download_url asset)
                           (alist-get 'url asset))))
             asset))
          (alist-get 'assets data))))
    (append
     `((apiUrl . ,(alist-get 'url data))
       (assets . ,assets)
       (createdAt . ,(alist-get 'created_at data))
       (databaseId . ,(alist-get 'id data))
       (isDraft . ,(alist-get 'draft data))
       (isPrerelease . ,(alist-get 'prerelease data))
       (name . ,(alist-get 'name data))
       (publishedAt . ,(alist-get 'published_at data))
       (tagName . ,(alist-get 'tag_name data))
       (targetCommitish . ,(alist-get 'target_commitish data))
       (url . ,(alist-get 'html_url data)))
     data)))

(defun magh-api--release-get-id (context id callback errback &optional force)
  "Fetch Release numeric ID in CONTEXT and normalize its REST representation."
  (setq context (magh-api--context context t))
  (magh-api--read-json
   context
   (list "api" (magh-core--repo-endpoint context (format "releases/%s" id)))
   callback errback :force force
   :transform #'magh-api--normalize-rest-release
   :domain (magh-api--domain context 'release id)))

(defun magh-api--release-create (context values callback errback)
  "Create a Release using VALUES plist."
  (setq context (magh-api--context context t))
  (let* ((generate (magh-api--true-p (plist-get values :generate-notes)))
         (body (or (plist-get values :body) ""))
         (send-body (or (not generate) (not (string-empty-p body)))))
    (magh-api--mutate-text
     context
     (append (list "release" "create" (plist-get values :tag))
             (when send-body (list "--notes-file" "-"))
             (magh-api--plist-args
              values '((:title "--title") (:target "--target")
                       (:notes-start-tag "--notes-start-tag")
                       (:draft "--draft" switch) (:prerelease "--prerelease" switch)
                       (:latest "--latest" switch)))
             (when generate (list "--generate-notes"))
             (magh-api--repo-args context))
     (magh-api--domain context 'release) callback errback
     :stdin (and send-body body))))

(defun magh-api--release-generate-notes
    (context tag target previous-tag callback errback)
  "Generate release notes for TAG at TARGET after PREVIOUS-TAG without publishing."
  (setq context (magh-api--context context t))
  (magh-api--read-json
   context
   (magh-api--rest-argv
    (magh-core--repo-endpoint context "releases/generate-notes") "POST"
    (append `((tag_name . ,tag))
            (when target `((target_commitish . ,target)))
            (when previous-tag `((previous_tag_name . ,previous-tag)))))
   callback errback :cache nil))

(defun magh-api--release-edit (context tag values callback errback)
  "Edit Release TAG using VALUES plist."
  (setq context (magh-api--context context t))
  (let ((bodyp (plist-member values :body)))
    (magh-api--mutate-text
     context
     (append (list "release" "edit" tag)
             (magh-api--plist-args
              values '((:tag "--tag") (:title "--title") (:target "--target")
                       (:draft "--draft" boolean)
                       (:prerelease "--prerelease" boolean)))
             (when (magh-api--true-p (plist-get values :latest))
               '("--latest"))
             (when bodyp '("--notes-file" "-"))
             (magh-api--repo-args context))
     (magh-api--domain context 'release) callback errback
     :stdin (and bodyp (or (plist-get values :body) "")))))

(defun magh-api--release-delete (context tag callback errback)
  "Delete Release TAG."
  (setq context (magh-api--context context t))
  (magh-api--mutate-text
   context
   (append (list "release" "delete" tag "--yes")
           (magh-api--repo-args context))
   (magh-api--domain context 'release) callback errback))

(defun magh-api--release-download
    (context tag patterns directory callback errback)
  "Download Release TAG assets matching PATTERNS into DIRECTORY."
  (setq context (magh-api--context context t))
  (magh-api--mutate-text
   context
   (append (list "release" "download" tag "--dir" directory)
           (magh-api--repeated-flags "--pattern" patterns)
           (magh-api--repo-args context))
   nil callback errback))

(defun magh-api--release-upload
    (context tag files clobber callback errback)
  "Upload FILES to Release TAG, replacing existing assets when CLOBBER."
  (setq context (magh-api--context context t))
  (magh-api--mutate-text
   context
   (append (list "release" "upload" tag)
           files (when clobber (list "--clobber"))
           (magh-api--repo-args context))
   (magh-api--domain context 'release) callback errback))

;;; Commit and remote content

(defun magh-api--commit-page
    (context params page callback errback &optional force)
  "Fetch one PAGE of commit history in CONTEXT using PARAMS plist."
  (setq context (magh-api--context context t))
  (let* ((page-size (magh-api--page-size params))
         (page (if (and (integerp page) (> page 0)) page 1)))
    (magh-api--read-json
     context
     (magh-api--rest-argv
      (magh-core--repo-endpoint context "commits") "GET"
      (append `((per_page . ,page-size) (page . ,page))
              (when-let* ((ref (plist-get params :ref))) `((sha . ,ref)))
              (when-let* ((path (plist-get params :path))) `((path . ,path)))))
     callback errback :force force
     :transform
     (lambda (items)
       (magh-page-create :items items
                         :next (and (length= items page-size) (1+ page))))
     :domain (magh-api--domain context 'commit (list params page)))))

(defun magh-api--commit-list (context params callback errback &optional force)
  "Fetch commit history in CONTEXT using PARAMS plist."
  (magh-api--commit-page
   context params nil
   (lambda (page) (funcall callback (magh-page-items page)))
   errback force))

(defun magh-api--commit-get (context sha callback errback &optional force)
  "Fetch Commit SHA details."
  (setq context (magh-api--context context t))
  (magh-api--read-json
   context
   (list "api" (magh-core--repo-endpoint
                context (format "commits/%s" (magh-core--url-path sha))))
   callback errback :force force :domain (magh-api--domain context 'commit sha)))

(defun magh-api--commit-diff (context sha callback errback &optional force)
  "Fetch Commit SHA as a diff."
  (setq context (magh-api--context context t))
  (magh-api--read-text
   context
   (magh-api--rest-argv
    (magh-core--repo-endpoint context (format "commits/%s" sha))
    "GET" nil nil '("Accept: application/vnd.github.diff"))
   callback errback :force force :domain (magh-api--domain context 'commit-diff sha)))

(defun magh-api--commit-comments (context sha callback errback &optional force)
  "Fetch comments for Commit SHA."
  (setq context (magh-api--context context t))
  (magh-api--read-rest-pages
   context (magh-core--repo-endpoint context (format "commits/%s/comments" sha))
   '((per_page . 100)) callback errback force
   (magh-api--domain context 'commit-comment sha)))

(defun magh-api--commit-comment (context sha body path position callback errback)
  "Comment BODY on Commit SHA, optionally at PATH and diff POSITION."
  (setq context (magh-api--context context t))
  (magh-api--mutate-json
   context
   (magh-api--rest-argv
    (magh-core--repo-endpoint context (format "commits/%s/comments" sha))
    "POST" (append `((body . ,body))
                   (when path `((path . ,path)))
                   (when position `((position . ,position)))))
   (magh-api--domain context 'commit-comment sha) callback errback))

(defun magh-api--content-get
    (context path ref callback errback &optional force)
  "Fetch repository contents at PATH and REF."
  (setq context (magh-api--context context t))
  (magh-api--read-json
   context
   (magh-api--rest-argv
    (magh-core--repo-endpoint
     context (concat "contents" (unless (string-empty-p (or path ""))
                                  (concat "/" (magh-core--url-path path)))))
    "GET" (when ref `((ref . ,ref))))
   callback errback :force force
   :domain (magh-api--domain context 'content (format "%s:%s" ref path))))

(defun magh-api--content-raw
    (context path ref callback errback &optional force)
  "Fetch raw remote file text at PATH and REF."
  (setq context (magh-api--context context t))
  (magh-api--read-text
   context
   (magh-api--rest-argv
    (magh-core--repo-endpoint context
                            (concat "contents/" (magh-core--url-path path)))
    "GET" (when ref `((ref . ,ref))) nil
    '("Accept: application/vnd.github.raw+json"))
   callback errback :force force
   :domain (magh-api--domain context 'content-raw (format "%s:%s" ref path))))

;;; Notification

(defun magh-api--notification-list
    (context unread-only callback errback &optional force)
  "Fetch notifications, restricting to unread when UNREAD-ONLY."
  (setq context (magh-api--context context))
  (magh-api--read-rest-pages
   context "notifications"
   `((all . ,(if unread-only :json-false t))
     (participating . :json-false) (per_page . 100))
   callback errback force (magh-api--domain context 'notification)))

(defun magh-api--notification-read (context id callback errback)
  "Mark notification thread ID as read."
  (setq context (magh-api--context context))
  (magh-api--mutate-text
   context (magh-api--rest-argv (format "notifications/threads/%s" id) "PATCH")
   (magh-api--domain context 'notification) callback errback))

(defun magh-api--notification-subscription
    (context id subscribed callback errback)
  "Set notification thread ID subscription to SUBSCRIBED."
  (setq context (magh-api--context context))
  (if (null subscribed)
      (magh-api--mutate-text
       context
       (magh-api--rest-argv
        (format "notifications/threads/%s/subscription" id) "DELETE")
       (magh-api--domain context 'notification) callback errback)
    (magh-api--mutate-json
     context
     (magh-api--rest-argv
      (format "notifications/threads/%s/subscription" id) "PUT"
      '((subscribed . t)
        (ignored . :json-false)))
     (magh-api--domain context 'notification) callback errback)))

;;; Search

(defconst magh-api--search-fields
  '((repos . "fullName,name,owner,description,visibility,stargazersCount,forksCount,updatedAt,url")
    (issues . "number,title,state,author,assignees,labels,commentsCount,body,repository,updatedAt,url,isPullRequest")
    (prs . "number,title,state,author,assignees,labels,commentsCount,body,repository,updatedAt,url")
    (code . "path,repository,sha,textMatches,url")
    (commits . "sha,commit,author,committer,parents,repository,url")))

(defun magh-api--search-argv (kind query options)
  "Build a GitHub CLI search argument list from KIND, QUERY, and OPTIONS."
  (append
   (list "search" (symbol-name kind) query
         "--limit" (number-to-string magh-list-limit)
         "--json" (alist-get kind magh-api--search-fields))
   (magh-api--plist-args
    options '((:repo "--repo" raw) (:owner "--owner" raw)
              (:state "--state" raw) (:author "--author" raw)
              (:assignee "--assignee" raw)
              (:review-requested "--review-requested" raw)
              (:language "--language" raw) (:filename "--filename" raw)
              (:extension "--extension" raw) (:sort "--sort" raw)
              (:order "--order" raw)))))

(defun magh-api--search (context kind query callback errback &optional force options)
  "Search KIND for QUERY using OPTIONS plist."
  (setq context (magh-api--context context))
  (let ((argv (magh-api--search-argv kind query options)))
    (magh-api--read-json
     context argv callback errback :force force
     :domain (magh-api--domain context 'search (list kind query options)))))

(defun magh-api--search-stream (context kind query callback errback &optional options)
  "Start a cancellable asynchronous KIND search for QUERY using OPTIONS."
  (setq context (magh-api--context context))
  (let ((argv (magh-api--search-argv kind query options)))
    (magh-client--json-async argv callback errback
                           :context context :cache nil :dedupe nil)))

;;; Discussions

(defun magh-api--discussion-cursor (cursor)
  "Normalize optional Discussion CURSOR for JSON serialization."
  (if (and cursor (not (equal cursor ""))) cursor :null))

(defun magh-api--discussion-page-data (data)
  "Normalize a Discussion connection in GraphQL DATA to `magh-page'."
  (let* ((connection
          (magh-api--json-at data 'data 'repository 'discussions))
         (page-info (alist-get 'pageInfo connection)))
    (magh-page-create
     :items (alist-get 'nodes connection)
     :next (and (magh-api--true-p (alist-get 'hasNextPage page-info))
                (alist-get 'endCursor page-info)))))

(defun magh-api--discussion-page
    (context params cursor callback errback &optional force)
  "Fetch one Discussion page in CONTEXT according to PARAMS and CURSOR."
  (setq context (magh-api--context context t))
  (let ((variables
         `((owner . ,(magh-context-owner context))
           (name . ,(magh-context-name context))
           (first . ,(magh-api--page-size params))
           (after . ,(magh-api--discussion-cursor cursor))
           (categoryId . ,(or (plist-get params :category-id) :null)))))
    (magh-api--read-json
     context '("api" "graphql" "--input" "-") callback errback
     :force force :transform #'magh-api--discussion-page-data
     :stdin (magh-api--graphql-payload
             magh-api--discussion-page-query variables)
     :domain (magh-api--domain
              context 'discussion-list
              (list (plist-get params :category-id) cursor)))))

(defun magh-api--discussion-get
    (context number callback errback &optional force)
  "Fetch Discussion NUMBER with up to 100 comments and replies."
  (setq context (magh-api--context context t))
  (magh-api--read-json
   context '("api" "graphql" "--input" "-") callback errback
   :force force
   :transform (lambda (data)
                (magh-api--json-at data 'data 'repository 'discussion))
   :stdin
   (magh-api--graphql-payload
    magh-api--discussion-query
    `((owner . ,(magh-context-owner context))
      (name . ,(magh-context-name context))
      (number . ,number)))
   :domain (magh-api--domain context 'discussion number)))

(defun magh-api--discussion-categories
    (context callback errback &optional force)
  "Fetch repository node ID and Discussion categories in CONTEXT."
  (setq context (magh-api--context context t))
  (magh-api--read-json
   context '("api" "graphql" "--input" "-") callback errback
   :force force
   :transform (lambda (data)
                (magh-api--json-at data 'data 'repository))
   :stdin
   (magh-api--graphql-payload
    magh-api--discussion-categories-query
    `((owner . ,(magh-context-owner context))
      (name . ,(magh-context-name context))))
   :domain (magh-api--domain context 'discussion-category)))

(defun magh-api--discussion-domains (context &optional number)
  "Return Discussion cache domains affected by NUMBER."
  (list (magh-api--domain context 'discussion-list)
        (magh-api--domain context 'discussion number)
        (magh-api--domain context 'notification)))

(defun magh-api--discussion-mutate
    (context query input domains callback errback)
  "Run Discussion mutation QUERY with INPUT and invalidate DOMAINS."
  (magh-api--mutate-json
   context '("api" "graphql" "--input" "-") domains callback errback
   :stdin (magh-api--graphql-payload query `((input . ,input)))
   :preserve-false t))

(defun magh-api--discussion-create (context values callback errback)
  "Create a Discussion using VALUES."
  (setq context (magh-api--context context t))
  (magh-api--discussion-mutate
   context magh-api--discussion-create-mutation
   `((repositoryId . ,(plist-get values :repository-id))
     (categoryId . ,(plist-get values :category-id))
     (title . ,(plist-get values :title))
     (body . ,(or (plist-get values :body) "")))
   (magh-api--discussion-domains context) callback errback))

(defun magh-api--discussion-update
    (context number discussion-id values callback errback)
  "Update Discussion NUMBER identified by DISCUSSION-ID using VALUES."
  (setq context (magh-api--context context t))
  (let ((input `((discussionId . ,discussion-id))))
    (dolist (entry '((:title . title) (:body . body)
                     (:category-id . categoryId)))
      (when (plist-member values (car entry))
        (push (cons (cdr entry) (or (plist-get values (car entry)) "")) input)))
    (magh-api--discussion-mutate
     context magh-api--discussion-update-mutation (nreverse input)
     (magh-api--discussion-domains context number) callback errback)))

(defun magh-api--discussion-comment
    (context number discussion-id body callback errback &optional reply-to-id)
  "Add BODY to Discussion NUMBER, optionally replying to REPLY-TO-ID."
  (setq context (magh-api--context context t))
  (magh-api--discussion-mutate
   context magh-api--discussion-comment-mutation
   (append `((discussionId . ,discussion-id) (body . ,body))
           (when reply-to-id `((replyToId . ,reply-to-id))))
   (magh-api--discussion-domains context number) callback errback))

(defun magh-api--discussion-close
    (context number discussion-id reason callback errback)
  "Close Discussion NUMBER for REASON."
  (setq context (magh-api--context context t)
        reason (upcase (format "%s" reason)))
  (unless (member reason '("RESOLVED" "OUTDATED" "DUPLICATE"))
    (user-error "Unsupported Discussion close reason: %s" reason))
  (magh-api--discussion-mutate
   context magh-api--discussion-close-mutation
   `((discussionId . ,discussion-id) (reason . ,reason))
   (magh-api--discussion-domains context number) callback errback))

(defun magh-api--discussion-reopen
    (context number discussion-id callback errback)
  "Reopen Discussion NUMBER."
  (setq context (magh-api--context context t))
  (magh-api--discussion-mutate
   context magh-api--discussion-reopen-mutation
   `((discussionId . ,discussion-id))
   (magh-api--discussion-domains context number) callback errback))

(defun magh-api--discussion-mark-answer
    (context number comment-id marked callback errback)
  "Mark COMMENT-ID as the answer to Discussion NUMBER, or unmark it."
  (setq context (magh-api--context context t))
  (magh-api--discussion-mutate
   context
   (if marked magh-api--discussion-mark-answer-mutation
     magh-api--discussion-unmark-answer-mutation)
   `((id . ,comment-id))
   (magh-api--discussion-domains context number) callback errback))

;;; Gist

(defun magh-api--gist-list (context callback errback &optional force)
  "Fetch gists for the current account."
  (setq context (magh-api--context context))
  (magh-api--read-rest-pages
   context "gists" `((per_page . ,(min 100 magh-list-limit)))
   callback errback force (magh-api--domain context 'gist)))

(defun magh-api--gist-get (context id callback errback &optional force)
  "Fetch Gist ID."
  (setq context (magh-api--context context))
  (magh-api--read-json
   context (list "api" (format "gists/%s" id)) callback errback
   :force force :domain (magh-api--domain context 'gist id)))

(defun magh-api--gist-files-object (files)
  "Return a JSON object for Gist FILES.
FILES is an alist whose keys are current filenames.  Values are plists with
optional :content and :filename keys, or nil to remove the file."
  (let ((object (make-hash-table :test #'equal)))
    (dolist (entry files object)
      (let ((filename (car entry)) (spec (cdr entry)))
        (when (string-empty-p (or filename ""))
          (user-error "Gist filenames cannot be empty"))
        (when (hash-table-contains-p filename object)
          (user-error "Duplicate Gist filename: %s" filename))
        (when (and spec (plist-member spec :filename)
                   (string-empty-p (or (plist-get spec :filename) "")))
          (user-error "Gist filenames cannot be empty"))
        (puthash
         filename
         (if (null spec)
             :null
           (append
            (when (plist-member spec :filename)
              `((filename . ,(plist-get spec :filename))))
            (when (plist-member spec :content)
              `((content . ,(or (plist-get spec :content) ""))))))
         object)))))

(defun magh-api--gist-payload (values)
  "Serialize Gist VALUES as nested JSON suitable for stdin."
  (let (payload)
    (when (plist-member values :description)
      (push `(description . ,(or (plist-get values :description) "")) payload))
    (when (plist-member values :public)
      (push `(public . ,(if (magh-api--true-p (plist-get values :public))
                           t :json-false))
            payload))
    (when (plist-member values :files)
      (push `(files . ,(magh-api--gist-files-object
                        (plist-get values :files))) payload))
    (json-serialize (nreverse payload) :false-object :json-false)))

(defun magh-api--gist-create (context values callback errback)
  "Create a Gist from VALUES using a JSON stdin payload."
  (setq context (magh-api--context context))
  (magh-api--mutate-json
   context '("api" "gists" "--method" "POST" "--input" "-")
   (magh-api--domain context 'gist) callback errback
   :stdin (magh-api--gist-payload values) :preserve-false t))

(defun magh-api--gist-update (context id values callback errback)
  "Update Gist ID from VALUES using a JSON stdin payload."
  (setq context (magh-api--context context))
  (magh-api--mutate-json
   context
   (list "api" (format "gists/%s" id) "--method" "PATCH"
         "--input" "-")
   (list (magh-api--domain context 'gist)
         (magh-api--domain context 'gist id)
         (magh-api--domain context 'gist-file))
   callback errback :stdin (magh-api--gist-payload values)
   :preserve-false t))

(defun magh-api--gist-file-remove
    (context id filename callback errback)
  "Remove FILENAME from Gist ID while preserving at least one file."
  (setq context (magh-api--context context))
  (magh-api--gist-get
   context id
   (lambda (gist)
     (if (<= (length (alist-get 'files gist)) 1)
         (funcall errback
                  (magh-core--error
                   'magh-invalid-input
                   "Cannot remove the last file from a Gist"))
       (magh-api--gist-update
        context id (list :files (list (cons filename nil)))
        callback errback)))
   errback t))

(defun magh-api--gist-file-raw
    (context id filename callback errback &optional force)
  "Fetch full raw FILENAME content from Gist ID."
  (setq context (magh-api--context context))
  (magh-api--read-text
   context (list "gist" "view" id "--raw" "--filename" filename)
   callback errback :force force
   :domain (magh-api--domain context 'gist-file (list id filename))))

;;; Generic API

(defun magh-api--generic-request
    (context endpoint method fields paginate callback errback)
  "Request arbitrary REST ENDPOINT with METHOD, FIELDS, and PAGINATE."
  (setq context (magh-api--context context))
  (magh-api--read-json
   context (magh-api--rest-argv endpoint method fields paginate)
   (lambda (data)
     (unless (string= method "GET")
       (magh-client-invalidate
        (append (list :host (magh-context-host context))
                (when (magh-context-repository context)
                  (list :repository (magh-context-repository context))))))
     (funcall callback data))
   errback
   :transform (and paginate #'magh-api--flatten-pages)
   :domain (magh-api--domain context 'generic-api endpoint)
   :cache (string= method "GET") :preserve-false t))

(defun magh-api--graphql
    (context query variables callback errback)
  "Run GraphQL QUERY with VARIABLES alist."
  (setq context (magh-api--context context))
  (let* ((argv
          (append (list "api" "graphql" "-f" (concat "query=" query))
                  (seq-mapcat
                   (lambda (entry)
                     (list "-F" (format "%s=%s" (car entry) (cdr entry))))
                   variables)))
         (mutation-p
          (string-match-p
           "\\`[[:space:]]*\\(?:#[^\n]*\n[[:space:]]*\\)*mutation\\_>"
           query)))
    (if mutation-p
        (magh-api--mutate-json
         context argv
         (append (list :host (magh-context-host context))
                 (when (magh-context-repository context)
                   (list :repository (magh-context-repository context))))
         callback errback :preserve-false t)
      (magh-api--read-json
       context argv callback errback
       :domain (magh-api--domain context 'graphql (sxhash-equal query))
       :preserve-false t))))

(provide 'magh-api)
;;; magh-api.el ends here
