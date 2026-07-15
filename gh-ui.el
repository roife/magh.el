;;; gh-ui.el --- Magit-section UI framework for gh.el -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; Author: gh.el contributors
;; Keywords: tools, vc, github
;; Package-Requires: ((emacs "31.1") (magit "4.0.0")
;;                    (markdown-mode "2.6"))

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
(require 'gh-candidate)
(require 'gh-core)

(declare-function gh-dispatch "gh-dispatch")
(defvar url-http-end-of-headers)

;;; Faces

(defface gh-section-heading '((t :inherit magit-section-heading))
  "Face for gh.el section headings." :group 'gh)
(defface gh-resource-number '((t :inherit magit-refname-pullreq))
  "Face for Issue and Pull Request numbers." :group 'gh)
(defface gh-resource-title '((t :inherit magit-section-secondary-heading))
  "Face for resource titles." :group 'gh)
(defface gh-repository '((t :inherit magit-branch-remote))
  "Face for repository names." :group 'gh)
(defface gh-branch '((t :inherit magit-branch-local))
  "Face for branch and ref names." :group 'gh)
(defface gh-author '((t :inherit magit-log-author))
  "Face for GitHub users." :group 'gh)
(defface gh-date '((t :inherit magit-log-date))
  "Face for dates and relative ages." :group 'gh)
(defface gh-tag '((t :inherit magit-tag))
  "Face for release tags." :group 'gh)
(defface gh-hash '((t :inherit magit-hash))
  "Face for commit hashes." :group 'gh)
(defface gh-workflow '((t :inherit magit-refname))
  "Face for workflow names." :group 'gh)
(defface gh-file '((t :inherit magit-filename))
  "Face for repository paths and filenames." :group 'gh)
(defface gh-label '((t :inherit magit-keyword))
  "Face for Issue and Pull Request labels." :group 'gh)
(defface gh-permission '((t :inherit magit-dimmed))
  "Face for visibility and permission metadata." :group 'gh)
(defface gh-added '((t :inherit magit-diffstat-added))
  "Face for addition counts." :group 'gh)
(defface gh-removed '((t :inherit magit-diffstat-removed))
  "Face for deletion counts." :group 'gh)
(defface gh-open-state '((t :inherit magit-process-ok))
  "Face for open, successful, and active states." :group 'gh)
(defface gh-pending-state '((t :inherit magit-branch-warning :weight bold))
  "Face for pending and in-progress states." :group 'gh)
(defface gh-draft-state '((t :inherit magit-dimmed :weight bold))
  "Face for draft, skipped, and neutral states." :group 'gh)
(defface gh-closed-state '((t :inherit magit-process-ng))
  "Face for closed, failed, and cancelled states." :group 'gh)
(defface gh-metadata-key '((t :inherit magit-header-line-key))
  "Face for metadata labels." :group 'gh)
(defface gh-loading '((t :inherit magit-dimmed :slant italic))
  "Face for asynchronous loading placeholders." :group 'gh)
(defface gh-error '((t :inherit magit-process-ng))
  "Face for inline request errors." :group 'gh)

;;; Section values and buffer state

(cl-defstruct (gh-section-value
               (:constructor gh-section-value-create (&key key resource)))
  "A stable section KEY paired with a structured RESOURCE plist."
  key resource)

(cl-defmethod magit-section-ident-value ((object gh-section-value))
  "Use OBJECT's stable key to correlate section incarnations."
  (gh-section-value-key object))

(defvar-local gh-buffer-context nil
  "Repository/navigation context for the current gh.el page.")
(defvar-local gh-buffer-resource-kind nil
  "Top-level resource kind displayed by the current gh.el page.")
(defvar-local gh-buffer-resource-id nil
  "Stable top-level resource identifier displayed by this page.")
(defvar-local gh-buffer-refresh-function nil
  "Asynchronous fetch function for the current page.")
(defvar-local gh-buffer-render-function nil
  "Renderer for fetched page data.")
(defvar-local gh-buffer-dispatch-function nil
  "Contextual Transient function for the current page.")
(defvar-local gh-ui--generation 0)
(defvar-local gh-ui--data nil)

(defvar gh-ui--visibility-cache (make-hash-table :test #'equal)
  "Visibility snapshots for pages reopened during this Emacs session.")

(defvar-keymap gh-section-mode-map
  :parent magit-section-mode-map
  "RET" #'gh-ui-visit
  "g" #'gh-ui-refresh
  "q" #'gh-ui-quit
  "b" #'gh-ui-browse
  "o" #'gh-ui-browse
  "w" #'gh-ui-copy-url
  "." #'gh-ui-dispatch
  "?" #'gh-ui-main-dispatch)

(defvar-keymap gh-ui-image-map
  :doc "Keymap placed on asynchronously loaded Markdown images."
  "RET" #'gh-ui-open-image-at-point
  "<mouse-1>" #'gh-ui-open-image-at-mouse)

(define-derived-mode gh-section-mode magit-section-mode "gh"
  "Major mode for native Magit-like GitHub resource pages."
  :group 'gh
  (setq-local truncate-lines gh-view-truncate-lines)
  (setq-local magit-section-initial-visibility-alist
              gh-section-initial-visibility-alist)
  (setq-local revert-buffer-function
              (lambda (&rest _) (gh-ui-refresh t)))
  (add-hook 'kill-buffer-hook #'gh-ui--remember-visibility nil t))

(defmacro gh-ui--section (spec heading &rest body)
  "Insert a native section described by SPEC, HEADING, and BODY.
SPEC is (TYPE KEY RESOURCE &optional HIDE).  KEY is stable across refreshes and
RESOURCE is a structured resource plist."
  (declare (indent 2) (debug t))
  (let ((resource-var (gensym "resource")))
    (pcase-let ((`(,type ,key ,resource . ,rest) spec))
      `(let ((,resource-var ,resource))
         (magit-insert-section
             (,type
              (gh-section-value-create :key ,key :resource ,resource-var)
              ,(car rest))
           (let ((start (point)))
             (magit-insert-heading ,heading)
             (when ,resource-var
               (add-text-properties start (point)
                                    (list 'gh-resource ,resource-var))))
           (magit-insert-section-body
             ,@body))))))

(defun gh-ui--section-resource (&optional section)
  "Return structured resource stored in SECTION or current section."
  (let* ((section (or section (magit-current-section)))
         (value (and section (oref section value))))
    (when (gh-section-value-p value)
      (gh-section-value-resource value))))

(defun gh-ui-resource-at-point ()
  "Return the structured resource at point."
  (or (get-text-property (point) 'gh-resource)
      (get-text-property (line-beginning-position) 'gh-resource)
      (gh-ui--section-resource)))

(defun gh-ui--visibility-enabled-p ()
  "Return non-nil when visibility caching applies to this page."
  (or (eq gh-section-cache-visibility t)
      (memq gh-buffer-resource-kind gh-section-cache-visibility)))

(defun gh-ui--page-key ()
  "Return the session visibility cache key for the current page."
  (list gh-buffer-resource-kind gh-buffer-resource-id
        (gh-context-host gh-buffer-context)
        (gh-context-repository gh-buffer-context)
        (gh-context-ref gh-buffer-context)
        (gh-context-path gh-buffer-context)))

(defun gh-ui--remember-visibility ()
  "Remember current page section visibility when configured."
  (when (and (gh-ui--visibility-enabled-p) magit-root-section)
    (puthash (gh-ui--page-key) (copy-tree magit-section-visibility-cache)
             gh-ui--visibility-cache)))

(defun gh-ui--capture-state ()
  "Capture point, window start, and stable section identity."
  (let ((window (get-buffer-window (current-buffer) t))
        (section (magit-current-section)))
    (list :section (and section (magit-section-ident section))
          :line (line-number-at-pos)
          :column (current-column)
          :point (point)
          :window-start (and window (window-start window)))))

(defun gh-ui--restore-state (state)
  "Restore point and window position from STATE."
  (pcase gh-refresh-point-strategy
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

(defun gh-ui--replace (renderer data state)
  "Replace page contents by calling RENDERER with DATA, then restore STATE."
  (let ((inhibit-read-only t))
    (erase-buffer)
    ;; Leave `magit-root-section' in place until the top-level macro starts.
    ;; `magit-insert-section' saves it as the predecessor and installs the new
    ;; root; pre-clearing it breaks nested section parentage on refresh.
    (magit-insert-section
        (gh-page (gh-section-value-create
                  :key (gh-ui--page-key)
                  :resource (gh-resource-create
                             gh-buffer-resource-kind gh-buffer-context
                             :id gh-buffer-resource-id)))
      (funcall renderer data))
    (goto-char (point-min))
    (when state (gh-ui--restore-state state))))

(defun gh-ui--render-loading ()
  "Render the initial asynchronous loading page."
  (gh-ui--replace
   (lambda (_)
     (gh-ui--section (loading 'loading nil)
       (propertize "Loading GitHub data…" 'font-lock-face 'gh-loading)))
   nil nil))

(defun gh-ui--render-error (error state)
  "Render typed ERROR inline and retain refresh STATE."
  (gh-ui--replace
   (lambda (_)
     (gh-ui--section (error 'request-error nil)
       (propertize "GitHub request failed" 'font-lock-face 'gh-error)
       (insert (propertize (concat "  " (gh-error-message error) "\n")
                           'font-lock-face 'gh-error))
       (insert "  Press g to retry.\n")))
   nil state))

(defun gh-ui-refresh (&optional force)
  "Asynchronously refresh the current native page.
With FORCE non-nil (interactively, a prefix argument), bypass completed cache."
  (interactive "P")
  (unless gh-buffer-refresh-function
    (user-error "This gh.el page has no refresh function"))
  (run-hooks 'gh-pre-refresh-hook)
  (cl-incf gh-ui--generation)
  (let ((generation gh-ui--generation)
        (state (gh-ui--capture-state)))
    (setq header-line-format
          (propertize " Loading GitHub data…" 'font-lock-face 'gh-loading))
    (unless gh-ui--data (gh-ui--render-loading))
    (funcall
     gh-buffer-refresh-function
     (lambda (data)
       (when (= generation gh-ui--generation)
         (setq gh-ui--data data
               header-line-format nil)
         (gh-ui--replace gh-buffer-render-function data state)
         (run-hooks 'gh-post-refresh-hook)))
     (lambda (error)
       (when (= generation gh-ui--generation)
         (setq header-line-format nil)
         (gh-ui--render-error error state)
         (run-hooks 'gh-post-refresh-hook)))
     (and force t))))

(cl-defun gh-ui--open-page
    (name context kind id fetch render &key preview setup)
  "Open a native page NAME for KIND and ID in CONTEXT.
FETCH is called with success, error, and force arguments.  RENDER inserts page
contents from fetched data.  PREVIEW makes the buffer disposable.  SETUP, when
non-nil, runs in the page buffer after mode initialization."
  (let ((buffer (get-buffer-create name)))
    (with-current-buffer buffer
      (unless (derived-mode-p 'gh-section-mode) (gh-section-mode))
      (setq gh-buffer-context context
            gh-buffer-resource-kind kind
            gh-buffer-resource-id id
            gh-buffer-preview-p preview
            gh-buffer-refresh-function fetch
            gh-buffer-render-function render)
      (unless magit-root-section
        (let ((cache-visibility (gh-ui--visibility-enabled-p)))
          (setq magit-section-cache-visibility (and cache-visibility t)
                magit-section-visibility-cache
                (and cache-visibility
                     (copy-tree (gethash (gh-ui--page-key)
                                         gh-ui--visibility-cache))))))
      (when setup (funcall setup))
      (gh-ui-refresh))
    (run-hooks 'gh-pre-display-buffer-hook)
    (if preview
        (display-buffer buffer)
      (funcall gh-display-buffer-function buffer))
    (run-hooks 'gh-post-display-buffer-hook)
    buffer))

;;; Commands

(defun gh-ui-visit ()
  "Visit the native resource at point."
  (interactive)
  (gh-resource-open (gh-ui-resource-at-point)))

(defun gh-ui-browse ()
  "Explicitly browse the resource at point on GitHub."
  (interactive)
  (gh-resource-browse (gh-ui-resource-at-point)))

(defun gh-ui-copy-url ()
  "Copy the resource URL at point."
  (interactive)
  (gh-resource-copy-url (gh-ui-resource-at-point)))

(defun gh-ui-dispatch ()
  "Open the contextual action menu for this page."
  (interactive)
  (let* ((resource (gh-ui-resource-at-point))
         (action (and resource
                      (plist-get (gh-candidate-actions
                                  (plist-get resource :kind))
                                 :dispatch))))
    (cond
     (action (funcall action resource))
     (gh-buffer-dispatch-function (funcall gh-buffer-dispatch-function))
     (t (user-error "No contextual actions for this page")))))

(defun gh-ui-main-dispatch ()
  "Open the top-level gh.el dispatch menu."
  (interactive)
  (call-interactively #'gh-dispatch))

(defun gh-ui-quit ()
  "Leave the current gh.el page using `gh-bury-buffer-function'."
  (interactive)
  (gh-ui--remember-visibility)
  (if gh-buffer-preview-p
      (kill-buffer)
    (funcall gh-bury-buffer-function)))

;;; Shared renderers

(defun gh-ui--insert-header (label value &optional face resource)
  "Insert metadata LABEL and VALUE with optional FACE and RESOURCE."
  (when (and value (not (equal value "")))
    (insert (propertize (concat label ":")
                        'font-lock-face 'gh-metadata-key)
            " ")
    (let ((start (point)))
      (insert (propertize (format "%s" value)
                          'font-lock-face (or face 'default)))
      (when resource
        (add-text-properties start (point) (list 'gh-resource resource))))
    (insert "\n")))

(defun gh-ui--insert-resource-line (text resource &optional face)
  "Insert TEXT line carrying structured RESOURCE."
  (let ((start (point)))
    (insert (if face (propertize text 'font-lock-face face) text) "\n")
    (add-text-properties start (point) (list 'gh-resource resource))))

(defun gh-ui--styled (value face)
  "Return VALUE as text carrying FACE, or nil when VALUE is empty."
  (when (and value (not (equal value "")))
    (propertize (format "%s" value) 'font-lock-face face)))

(defun gh-ui--row (&rest values)
  "Join non-empty VALUES with two spaces, preserving text properties.
Rows intentionally have no column widths, padding, or truncation; their shape
matches ordinary Magit section headings and the layouts in doc/UI.md."
  (mapconcat
   #'identity
   (delq nil
         (mapcar (lambda (value)
                   (when value
                     (let ((text (if (stringp value) value
                                   (format "%s" value))))
                       (unless (string-empty-p text) text))))
                 values))
   "  "))

(defun gh-ui--format-row (values &optional fields)
  "Join semantic FIELDS from plist VALUES without fixed-width columns."
  (apply #'gh-ui--row
         (mapcar (lambda (field) (plist-get values field))
                 (or fields
                     '(:state :identifier :title :author :review :updated)))))

(defun gh-ui--adopt-font-lock-faces (start end &optional object)
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

(defun gh-ui--fontified-string (text mode)
  "Return TEXT fontified using MODE."
  (with-temp-buffer
    (insert (or text ""))
    (delay-mode-hooks (funcall mode))
    (font-lock-ensure)
    (let ((result (buffer-substring (point-min) (point-max))))
      (gh-ui--adopt-font-lock-faces 0 (length result) result)
      result)))

(defun gh-ui--make-resource-button (start end resource)
  "Turn START to END into a native RESOURCE button."
  (when resource
    (make-text-button
     start end 'follow-link t 'gh-resource resource
     'help-echo "Open native gh.el resource"
     'action (lambda (button)
               (gh-resource-open (button-get button 'gh-resource))))))

(defun gh-ui-open-image-at-point ()
  "Open the original Markdown image represented at point."
  (interactive)
  (let ((url (get-char-property (point) 'gh-image-url)))
    (unless url (user-error "No Markdown image at point"))
    (browse-url url)))

(defun gh-ui-open-image-at-mouse (event)
  "Open original Markdown image clicked by mouse EVENT."
  (interactive "e")
  (mouse-set-point event)
  (gh-ui-open-image-at-point))

(defun gh-ui--image-finished (status target overlay url)
  "Install an image response described by STATUS into TARGET OVERLAY for URL."
  (let ((response (current-buffer)))
    (unwind-protect
        (when (and (buffer-live-p target) (overlay-buffer overlay)
                   (null (plist-get status :error)))
          (let* ((start (if (markerp url-http-end-of-headers)
                            (marker-position url-http-end-of-headers)
                          url-http-end-of-headers))
                 (size (- (point-max) start)))
            (when (<= size gh-view-inline-image-max-bytes)
              (let* ((bytes (buffer-substring-no-properties start (point-max)))
                     (image (ignore-errors
                              (create-image bytes nil t
                                            :max-width
                                            gh-view-inline-image-max-width))))
                (when image
                  (with-current-buffer target
                    (overlay-put overlay 'display image)
                    (overlay-put overlay 'help-echo
                                 (format "Open original image: %s" url))))))))
      (kill-buffer response))))

(defun gh-ui--load-inline-images (start end)
  "Start asynchronous image loads for Markdown image syntax in START to END."
  (when (and gh-view-inline-images (display-images-p))
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
                                   'font-lock-face 'gh-loading))
          (overlay-put overlay 'gh-image-url url)
          (overlay-put overlay 'keymap gh-ui-image-map)
          (overlay-put overlay 'mouse-face 'highlight)
          (overlay-put overlay 'evaporate t)
          (condition-case _error
              (url-retrieve url #'gh-ui--image-finished
                            (list (current-buffer) overlay url) t t)
            (error
             (overlay-put overlay 'display
                          (propertize (format "[image unavailable: %s]" alt)
                                      'font-lock-face 'gh-error)))))))))

(defun gh-ui--linkify-references (start end context)
  "Make supported GitHub references between START and END native links."
  (save-excursion
    (goto-char start)
    (while (re-search-forward "https?://[^][()<> \t\n]+" end t)
      (let* ((begin (match-beginning 0))
             (finish (match-end 0))
             (url (match-string-no-properties 0))
             ;; URL parsing performs its own regexp matches, so capture every
             ;; position before calling it.
             (resource (gh-resource-from-url url context)))
        (gh-ui--make-resource-button begin finish resource)))
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
                    (gh-context-from-repository
                     repository (and context (gh-context-host context)))
                  context)))
          (when resource-context
            (gh-ui--make-resource-button
             button-start button-end
             (gh-resource-create 'issue resource-context :number number))))))
    (when context
      (goto-char start)
      (while (re-search-forward "\\b[[:xdigit:]]\\{7,40\\}\\b" end t)
        (unless (button-at (match-beginning 0))
          (gh-ui--make-resource-button
           (match-beginning 0) (match-end 0)
           (gh-resource-create 'commit context
                               :sha (match-string-no-properties 0)))))
      (goto-char start)
      (while (re-search-forward
              "\\(?:^\\|[^[:alnum:]_]\\)@\\([[:alnum:]-]+\\)" end t)
        (unless (button-at (match-beginning 1))
          (gh-ui--make-resource-button
           (1- (match-beginning 1)) (match-end 1)
           (gh-resource-create 'user context
                               :login (match-string-no-properties 1))))))))

(defun gh-ui--insert-markdown (text &optional context)
  "Insert Markdown TEXT with GFM fontification and native resource links."
  (let ((start (point)))
    (insert (gh-ui--fontified-string text 'gfm-mode))
    (unless (bolp) (insert "\n"))
    (let ((end (point)))
      (gh-ui--linkify-references start end (or context gh-buffer-context))
      (gh-ui--load-inline-images start end))))

(defun gh-ui--insert-diff (text)
  "Insert and fontify diff TEXT."
  (let ((start (point)))
    (insert (gh-ui--fontified-string text 'diff-mode))
    (unless (bolp) (insert "\n"))
    (font-lock-prepend-text-property
     start (point) 'font-lock-face 'default)))

(defun gh-ui--insert-ansi (text)
  "Insert ANSI-colored TEXT safely."
  (let ((start (point)))
    (insert (or text ""))
    (ansi-color-apply-on-region start (point))
    (gh-ui--adopt-font-lock-faces start (point))
    (unless (bolp) (insert "\n"))))

(provide 'gh-ui)
;;; gh-ui.el ends here
