;;; magh-ui.el --- Magit-section UI framework for magh.el -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; Author: magh.el contributors
;; Keywords: tools, vc, github

;;; Commentary:

;; Native GitHub pages derive directly from `magit-section-mode'.  This module
;; owns page lifetime, asynchronous refresh generations, point and visibility
;; restoration, semantic rendering, and shared navigation keys.  It contains
;; no resource-specific API calls.

;;; Code:

(require 'ansi-color)
(require 'browse-url)
(require 'button)
(require 'cl-lib)
(require 'diff-mode)
(require 'eieio)
(require 'magit)
(require 'magit-diff)
(require 'magit-log)
(require 'magit-process)
(require 'magit-section)
(require 'markdown-mode)
(require 'subr-x)
(require 'url)
(require 'url-http)
(require 'magh-candidate)
(require 'magh-core)

(declare-function magh-dispatch "magh-dispatch")
(defvar url-http-end-of-headers)

;;; Faces

(defface magh-section-heading '((t :inherit magit-section-heading))
  "Face for magh.el section headings." :group 'magh)
(defface magh-resource-number '((t :inherit magit-refname-pullreq))
  "Face for Issue and Pull Request numbers." :group 'magh)
(defface magh-resource-title '((t :inherit magit-section-secondary-heading))
  "Face for resource titles." :group 'magh)
(defface magh-conversation-kind
  '((t :inherit magit-section-secondary-heading :weight bold))
  "Face for conversation type labels." :group 'magh)
(defface magh-inline-comment
  '((((class color) (background light))
     :extend t :background "#eef6ff")
    (((class color) (background dark))
     :extend t :background "#263442")
    (t :extend t))
  "Face for inline comment blocks." :group 'magh)
(defface magh-repository '((t :inherit magit-branch-remote))
  "Face for repository names." :group 'magh)
(defface magh-branch '((t :inherit magit-branch-local))
  "Face for branch and ref names." :group 'magh)
(defface magh-author '((t :inherit magit-log-author))
  "Face for GitHub users." :group 'magh)
(defface magh-date '((t :inherit magit-log-date))
  "Face for dates and relative ages." :group 'magh)
(defface magh-tag '((t :inherit magit-tag))
  "Face for release tags." :group 'magh)
(defface magh-hash '((t :inherit magit-hash))
  "Face for commit hashes." :group 'magh)
(defface magh-workflow '((t :inherit magit-refname))
  "Face for workflow names." :group 'magh)
(defface magh-file '((t :inherit magit-filename))
  "Face for repository paths and filenames." :group 'magh)
(defface magh-label '((t :inherit magit-keyword))
  "Face for Issue and Pull Request labels." :group 'magh)
(defface magh-permission '((t :inherit magit-dimmed))
  "Face for visibility and permission metadata." :group 'magh)
(defface magh-added '((t :inherit magit-diffstat-added))
  "Face for addition counts." :group 'magh)
(defface magh-removed '((t :inherit magit-diffstat-removed))
  "Face for deletion counts." :group 'magh)
(defface magh-open-state '((t :inherit magit-process-ok))
  "Face for open, successful, and active states." :group 'magh)
(defface magh-pending-state '((t :inherit magit-branch-warning :weight bold))
  "Face for pending and in-progress states." :group 'magh)
(defface magh-draft-state '((t :inherit magit-dimmed :weight bold))
  "Face for draft, skipped, and neutral states." :group 'magh)
(defface magh-closed-state '((t :inherit magit-process-ng))
  "Face for closed, failed, and cancelled states." :group 'magh)
(defface magh-metadata-key '((t :inherit magit-header-line-key))
  "Face for metadata labels." :group 'magh)
(defface magh-loading '((t :inherit magit-dimmed :slant italic))
  "Face for asynchronous loading placeholders." :group 'magh)
(defface magh-error '((t :inherit magit-process-ng))
  "Face for inline request errors." :group 'magh)

;;; Section values and buffer state

(cl-defstruct (magh-section-value
               (:constructor magh-section-value-create (&key key resource)))
  "A stable section KEY paired with a structured RESOURCE plist."
  key resource)

(cl-defmethod magit-section-ident-value ((object magh-section-value))
  "Use OBJECT's stable key to correlate section incarnations."
  (magh-section-value-key object))

(defvar-local magh-buffer-context nil
  "Repository/navigation context for the current magh.el page.")
(defvar-local magh-buffer-resource-kind nil
  "Top-level resource kind displayed by the current magh.el page.")
(defvar-local magh-buffer-resource-id nil
  "Stable top-level resource identifier displayed by this page.")
(defvar-local magh-buffer-refresh-function nil
  "Asynchronous fetch function for the current page.")
(defvar-local magh-buffer-render-function nil
  "Renderer for fetched page data.")
(defvar-local magh-buffer-dispatch-function nil
  "Contextual Transient function for the current page.")
(defvar-local magh-ui--generation 0)
(defvar-local magh-ui--data nil)

(defun magh-ui--repository-context (&optional context)
  "Resolve CONTEXT or the current page's required repository context."
  (magh-context-resolve (or context magh-buffer-context) t))

(defvar magh-ui--visibility-cache (make-hash-table :test #'equal)
  "Visibility snapshots for pages reopened during this Emacs session.")

(defvar-keymap magh-section-mode-map
  :parent magit-section-mode-map
  "RET" #'magh-ui-visit
  "g" #'magh-ui-refresh
  "q" #'magh-ui-quit
  "b" #'magh-ui-browse
  "o" #'magh-ui-browse
  "w" #'magh-ui-copy-url
  "." #'magh-ui-dispatch
  "?" #'magh-dispatch)

(defvar-keymap magh-ui-image-map
  :doc "Keymap placed on asynchronously loaded Markdown images."
  "RET" #'magh-ui-open-image-at-point
  "<mouse-1>" #'magh-ui-open-image-at-mouse)

(define-derived-mode magh-section-mode magit-section-mode "Magh"
  "Major mode for native Magit-like GitHub resource pages."
  :group 'magh
  (setq-local truncate-lines magh-view-truncate-lines)
  (setq-local magit-section-initial-visibility-alist
              magh-section-initial-visibility-alist)
  (setq-local revert-buffer-function
              (lambda (&rest _) (magh-ui-refresh t)))
  (add-hook 'kill-buffer-hook #'magh-ui--remember-visibility nil t))

(defmacro magh-ui--section (spec heading &rest body)
  "Insert a native section described by SPEC, HEADING, and BODY.
SPEC is (TYPE KEY RESOURCE &optional HIDE).  KEY is stable across refreshes and
RESOURCE is a structured resource plist.  Top-level sections and consecutive
comment sections are separated by one blank line.  Description and comment
sections enable `visual-line-mode' for their page."
  (declare (indent 2) (debug t))
  (let ((resource-var (gensym "resource")))
    (pcase-let ((`(,type ,key ,resource . ,rest) spec))
      `(let ((,resource-var ,resource))
         (when (and (memq ',type '(description comment inline-comment))
                    (not visual-line-mode))
           (visual-line-mode 1))
         (when (or (eq magit-insert-section--current magit-root-section)
                   (memq ',type '(comment inline-comment)))
           (magh-ui--ensure-section-gap))
         (magit-insert-section
             (,type
              (magh-section-value-create :key ,key :resource ,resource-var)
              ,(car rest))
           (let ((start (point)))
             (magit-insert-heading ,heading)
             (when (eq ',type 'inline-comment)
               (font-lock-append-text-property
                start (point) 'font-lock-face 'magh-inline-comment))
             (when ,resource-var
               (add-text-properties start (point)
                                    (list 'magh-resource ,resource-var))))
           (magit-insert-section-body
             (let ((start (point)))
               ,@body
               (when (eq ',type 'inline-comment)
                 (font-lock-append-text-property
                  start (point) 'font-lock-face 'magh-inline-comment)))))))))

(defun magh-ui--ensure-section-gap ()
  "Ensure one blank line before a following sibling section.
Do not add space before the first child of the current parent."
  (when (and magit-insert-section--current
             (oref magit-insert-section--current children)
             (> (point) (1+ (point-min)))
             (not (equal (buffer-substring-no-properties
                          (- (point) 2) (point))
                         "\n\n")))
    (insert "\n")))

(defun magh-ui-resource-at-point ()
  "Return the structured resource at point."
  (or (magh-candidate-at-point)
      (let* ((section (magit-current-section))
             (value (and section (oref section value))))
        (when (magh-section-value-p value)
          (magh-section-value-resource value)))))

(defun magh-ui--visibility-enabled-p ()
  "Return non-nil when visibility caching applies to this page."
  (or (eq magh-section-cache-visibility t)
      (memq magh-buffer-resource-kind magh-section-cache-visibility)))

(defun magh-ui--page-key ()
  "Return the session visibility cache key for the current page."
  (list magh-buffer-resource-kind magh-buffer-resource-id
        (magh-context-host magh-buffer-context)
        (magh-context-repository magh-buffer-context)
        (magh-context-ref magh-buffer-context)
        (magh-context-path magh-buffer-context)))

(defun magh-ui--remember-visibility ()
  "Remember current page section visibility when configured."
  (when (and (magh-ui--visibility-enabled-p) magit-root-section)
    (puthash (magh-ui--page-key) (copy-tree magit-section-visibility-cache)
             magh-ui--visibility-cache)))

(defun magh-ui--capture-state ()
  "Capture point, window start, and stable section identity."
  (let ((window (get-buffer-window (current-buffer) t))
        (section (magit-current-section)))
    (list :section (and section (magit-section-ident section))
          :line (line-number-at-pos)
          :column (current-column)
          :point (point)
          :window-start (and window (window-start window)))))

(defun magh-ui--restore-state (state)
  "Restore point and window position from STATE."
  (pcase magh-refresh-point-strategy
    ('section
     (if-let* ((ident (plist-get state :section))
               (section (magit-get-section ident)))
         (goto-char (oref section start))
       (goto-char (min (plist-get state :point) (point-max)))))
    ('line
     (goto-char (point-min))
     (forward-line (1- (plist-get state :line)))
     (move-to-column (plist-get state :column)))
    (_ (goto-char (point-min))))
  (when-let* ((window (get-buffer-window (current-buffer) t))
              (start (plist-get state :window-start)))
    (set-window-start window (min start (point-max)) t)))

(defun magh-ui--replace (renderer data state)
  "Replace page contents by calling RENDERER with DATA, then restore STATE."
  (let ((inhibit-read-only t))
    (erase-buffer)
    ;; Leave `magit-root-section' in place until the top-level macro starts.
    ;; `magit-insert-section' saves it as the predecessor and installs the new
    ;; root; pre-clearing it breaks nested section parentage on refresh.
    (magit-insert-section
        (magh-page (magh-section-value-create
                  :key (magh-ui--page-key)
                  :resource (magh-resource-create
                             magh-buffer-resource-kind magh-buffer-context
                             :id magh-buffer-resource-id)))
      (funcall renderer data))
    (goto-char (point-min))
    (when state (magh-ui--restore-state state))))

(defun magh-ui--render-loading ()
  "Render the initial asynchronous loading page."
  (magh-ui--replace
   (lambda (_)
     (magh-ui--section (loading 'loading nil)
       (propertize "Loading GitHub data…" 'font-lock-face 'magh-loading)))
   nil nil))

(defun magh-ui--render-error (error state)
  "Render typed ERROR inline and retain refresh STATE."
  (magh-ui--replace
   (lambda (_)
     (magh-ui--section (error 'request-error nil)
       (propertize "GitHub request failed" 'font-lock-face 'magh-error)
       (insert (propertize (concat "  " (magh-error-message error) "\n")
                           'font-lock-face 'magh-error))
       (insert "  Press g to retry.\n")))
   nil state))

(defun magh-ui-refresh (&optional force)
  "Asynchronously refresh the current native page.
With FORCE non-nil (interactively, a prefix argument), bypass completed cache."
  (interactive "P")
  (unless magh-buffer-refresh-function
    (user-error "This magh.el page has no refresh function"))
  (run-hooks 'magh-pre-refresh-hook)
  (cl-incf magh-ui--generation)
  (let ((generation magh-ui--generation)
        (state (magh-ui--capture-state)))
    (setq header-line-format
          (propertize " Loading GitHub data…" 'font-lock-face 'magh-loading))
    (unless magh-ui--data (magh-ui--render-loading))
    (funcall
     magh-buffer-refresh-function
     (lambda (data)
       (when (= generation magh-ui--generation)
         (setq magh-ui--data data
               header-line-format nil)
         (magh-ui--replace magh-buffer-render-function data state)
         (run-hooks 'magh-post-refresh-hook)))
     (lambda (error)
       (when (= generation magh-ui--generation)
         (setq header-line-format nil)
         (magh-ui--render-error error state)
         (run-hooks 'magh-post-refresh-hook)))
     force)))

(defun magh-ui--refresh-if-page ()
  "Refresh the current buffer when it is a native magh.el page."
  (when (derived-mode-p 'magh-section-mode)
    (magh-ui-refresh t)))

(cl-defun magh-ui--open-page
    (name context kind id fetch render &key preview setup)
  "Open a native page NAME for KIND and ID in CONTEXT.
FETCH is called with success, error, and force arguments.  RENDER inserts page
contents from fetched data.  PREVIEW makes the buffer disposable.  SETUP, when
non-nil, runs in the page buffer after mode initialization."
  (let ((buffer (get-buffer-create name)))
    (with-current-buffer buffer
      (unless (derived-mode-p 'magh-section-mode) (magh-section-mode))
      (setq magh-buffer-context context
            magh-buffer-resource-kind kind
            magh-buffer-resource-id id
            magh-buffer-preview-p preview
            magh-buffer-refresh-function fetch
            magh-buffer-render-function render)
      (unless magit-root-section
        (let ((cache-visibility (magh-ui--visibility-enabled-p)))
          (setq magit-section-cache-visibility (and cache-visibility t)
                magit-section-visibility-cache
                (and cache-visibility
                     (copy-tree (gethash (magh-ui--page-key)
                                         magh-ui--visibility-cache))))))
      (when setup (funcall setup))
      (magh-ui-refresh))
    (run-hooks 'magh-pre-display-buffer-hook)
    (if preview
        (display-buffer buffer)
      (funcall magh-display-buffer-function buffer))
    (run-hooks 'magh-post-display-buffer-hook)
    buffer))

;;; Commands

(defun magh-ui-visit ()
  "Visit the native resource at point."
  (interactive)
  (magh-resource-open (magh-ui-resource-at-point)))

(defun magh-ui-browse ()
  "Explicitly browse the resource at point on GitHub."
  (interactive)
  (magh-resource-browse (magh-ui-resource-at-point)))

(defun magh-ui-copy-url ()
  "Copy the resource URL at point."
  (interactive)
  (magh-resource-copy-url (magh-ui-resource-at-point)))

(defun magh-ui-dispatch ()
  "Open the contextual action menu for this page."
  (interactive)
  (let* ((resource (magh-ui-resource-at-point))
         (action (magh-candidate--action resource :dispatch)))
    (cond
     (action (funcall action resource))
     (magh-buffer-dispatch-function (funcall magh-buffer-dispatch-function))
     (t (user-error "No contextual actions for this page")))))

(defun magh-ui-quit ()
  "Leave the current magh.el page using `magh-bury-buffer-function'."
  (interactive)
  (magh-ui--remember-visibility)
  (if magh-buffer-preview-p
      (kill-buffer)
    (funcall magh-bury-buffer-function)))

;;; Shared renderers

(defun magh-ui--insert-header (label value &optional face resource)
  "Insert metadata LABEL and VALUE with optional FACE and RESOURCE."
  (when (and value (not (equal value "")))
    (insert (propertize (concat label ":")
                        'font-lock-face 'magh-metadata-key)
            " ")
    (let ((start (point)))
      (insert (propertize (format "%s" value)
                          'font-lock-face (or face 'default)))
      (when resource
        (add-text-properties start (point) (list 'magh-resource resource))))
    (insert "\n")))

(defun magh-ui--insert-resource-line (text resource &optional face)
  "Insert TEXT line carrying structured RESOURCE."
  (let ((start (point)))
    (insert (if face (propertize text 'font-lock-face face) text) "\n")
    (add-text-properties start (point) (list 'magh-resource resource))))

(defun magh-ui--styled (value face)
  "Return VALUE as text carrying FACE, or nil when VALUE is empty."
  (when (and value (not (equal value "")))
    (propertize (format "%s" value) 'font-lock-face face)))

(defun magh-ui--row (&rest values)
  "Join non-empty VALUES with one space, preserving text properties.
Rows intentionally have no column widths, padding, or truncation; their shape
matches ordinary Magit section headings and the layouts in doc/UI.md."
  (mapconcat
   #'identity
   (delq nil
         (mapcar (lambda (value)
                   (when value
                     (let ((text (format "%s" value)))
                       (unless (string-empty-p text) text))))
                 values))
   " "))

(defun magh-ui--format-row (values &optional fields)
  "Join semantic FIELDS from plist VALUES without fixed-width columns."
  (apply #'magh-ui--row
         (mapcar (lambda (field) (plist-get values field))
                 (or fields
                     '(:state :identifier :title :author :review :updated)))))

(defun magh-ui--adopt-font-lock-faces (start end &optional object)
  "Move persistent `face' properties from START to END to `font-lock-face'.
OBJECT is a string when operating on a string and nil for the current buffer.
Magit section buffers let Font Lock manage `face', so renderer-owned styles
must use `font-lock-face' to survive just-in-time refontification."
  (let ((position start)
        (inhibit-read-only t))
    (while (< position end)
      (let* ((next (next-single-property-change
                    position 'face object end))
             (face (get-text-property position 'face object)))
        (when face
          (font-lock-prepend-text-property
           position next 'font-lock-face face object)
          (remove-list-of-text-properties position next '(face) object))
        (setq position next))))
  object)

(defun magh-ui--fontified-string (text mode)
  "Return TEXT fontified using MODE."
  (with-temp-buffer
    (insert (or text ""))
    (delay-mode-hooks (funcall mode))
    (font-lock-ensure)
    (let ((result (buffer-substring (point-min) (point-max))))
      (magh-ui--adopt-font-lock-faces 0 (length result) result)
      result)))

(defun magh-ui--make-resource-button (start end resource)
  "Turn START to END into a native RESOURCE button."
  (when resource
    (make-text-button
     start end 'follow-link t 'magh-resource resource
     'help-echo "Open native magh.el resource"
     'action (lambda (button)
               (magh-resource-open (button-get button 'magh-resource))))))

(defun magh-ui-open-image-at-point ()
  "Open the original Markdown image represented at point."
  (interactive)
  (let ((url (get-char-property (point) 'magh-image-url)))
    (unless url (user-error "No Markdown image at point"))
    (browse-url url)))

(defun magh-ui-open-image-at-mouse (event)
  "Open original Markdown image clicked by mouse EVENT."
  (interactive "e")
  (mouse-set-point event)
  (magh-ui-open-image-at-point))

(defun magh-ui--image-finished (status target overlay url)
  "Install an image response described by STATUS into TARGET OVERLAY for URL."
  (let ((response (current-buffer)))
    (unwind-protect
        (when (and (buffer-live-p target) (overlay-buffer overlay)
                   (null (plist-get status :error)))
          (let* ((start (marker-position url-http-end-of-headers))
                 (size (- (point-max) start)))
            (when (<= size magh-view-inline-image-max-bytes)
              (let* ((bytes (buffer-substring-no-properties start (point-max)))
                     (image (ignore-errors
                              (create-image bytes nil t
                                            :max-width
                                            magh-view-inline-image-max-width))))
                (when image
                  (with-current-buffer target
                    (overlay-put overlay 'display image)
                    (overlay-put overlay 'help-echo
                                 (format "Open original image: %s" url))))))))
      (kill-buffer response))))

(defun magh-ui--load-inline-images (start end)
  "Start asynchronous image loads for Markdown image syntax in START to END."
  (when (and magh-view-inline-images (display-images-p))
    (save-excursion
      (goto-char start)
      (while (re-search-forward
              "!\\[\\([^]\n]*\\)\\](\\(https?://[^) \\t\\n]+\\))" end t)
        (let* ((alt (match-string-no-properties 1))
               (url (match-string-no-properties 2))
               (overlay (make-overlay (match-beginning 0) (match-end 0)
                                      (current-buffer) nil t)))
          (overlay-put overlay 'display
                       (propertize (format "[image: %s]" alt)
                                   'font-lock-face 'magh-loading))
          (overlay-put overlay 'magh-image-url url)
          (overlay-put overlay 'keymap magh-ui-image-map)
          (overlay-put overlay 'mouse-face 'highlight)
          (overlay-put overlay 'evaporate t)
          (condition-case _error
              (url-retrieve url #'magh-ui--image-finished
                            (list (current-buffer) overlay url) t t)
            (error
             (overlay-put overlay 'display
                          (propertize (format "[image unavailable: %s]" alt)
                                      'font-lock-face 'magh-error)))))))))

(defun magh-ui--linkify-references (start end context)
  "Make supported GitHub references between START and END native links."
  (save-excursion
    (goto-char start)
    (while (re-search-forward "https?://[^][()<> \t\n]+" end t)
      (let* ((begin (match-beginning 0))
             (finish (match-end 0))
             (url (match-string-no-properties 0))
             ;; URL parsing performs its own regexp matches, so capture every
             ;; position before calling it.
             (resource (magh-resource-from-url url context)))
        (magh-ui--make-resource-button begin finish resource)))
    (goto-char start)
    (while (re-search-forward
            "\\b\\([[:alnum:]_.-]+/[[:alnum:]_.-]+\\)#\\([0-9]+\\)\\|\\(?:^\\|[^[:alnum:]/]\\)\\(#\\([0-9]+\\)\\)"
            end t)
      (unless (button-at (or (match-beginning 1) (match-beginning 3)))
        (let* ((repository (and (match-beginning 1) (match-string 1)))
               (number (string-to-number
                        (or (and (match-beginning 2) (match-string 2))
                            (match-string 4))))
               (button-start (if repository (match-beginning 1)
                               (match-beginning 3)))
               (button-end (match-end 0))
               (resource-context
                (if repository
                    (magh-context-from-repository
                     repository (and context (magh-context-host context)))
                  context)))
          (when resource-context
            (magh-ui--make-resource-button
             button-start button-end
             (magh-resource-create 'issue resource-context :number number))))))
    (when context
      (goto-char start)
      (while (re-search-forward "\\b[[:xdigit:]]\\{7,40\\}\\b" end t)
        (unless (button-at (match-beginning 0))
          (magh-ui--make-resource-button
           (match-beginning 0) (match-end 0)
           (magh-resource-create 'commit context
                               :sha (match-string-no-properties 0)))))
      (goto-char start)
      (while (re-search-forward
              "\\(?:^\\|[^[:alnum:]_]\\)@\\([[:alnum:]-]+\\)" end t)
        (unless (button-at (match-beginning 1))
          (magh-ui--make-resource-button
           (1- (match-beginning 1)) (match-end 1)
           (magh-resource-create 'user context
                               :login (match-string-no-properties 1))))))))

(defun magh-ui--normalize-newlines (text)
  "Return TEXT with CRLF and lone CR line endings converted to LF."
  (string-replace "\r" "\n"
                  (string-replace "\r\n" "\n" (or text ""))))

(defun magh-ui--insert-markdown (text &optional context)
  "Insert Markdown TEXT with GFM fontification and native resource links."
  (unless visual-line-mode
    (visual-line-mode 1))
  (let ((start (point)))
    (insert (magh-ui--fontified-string
             (magh-ui--normalize-newlines text) 'gfm-mode))
    (unless (bolp) (insert "\n"))
    (let ((end (point)))
      (magh-ui--linkify-references start end (or context magh-buffer-context))
      (magh-ui--load-inline-images start end))))

(defun magh-ui--insert-diff (text)
  "Insert and fontify diff TEXT."
  (insert (magh-ui--fontified-string text 'diff-mode))
  (unless (bolp) (insert "\n")))

(defun magh-ui--insert-ansi (text)
  "Insert ANSI-colored TEXT safely."
  (let ((start (point)))
    (insert (or text ""))
    (ansi-color-apply-on-region start (point))
    (magh-ui--adopt-font-lock-faces start (point))
    (unless (bolp) (insert "\n"))))

(provide 'magh-ui)
;;; magh-ui.el ends here
