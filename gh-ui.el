;;; gh-ui.el --- Magit-section UI framework for gh.el -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; Author: gh.el contributors
;; Keywords: tools, vc, github
;; Package-Requires: ((emacs "29.1") (magit "4.0.0"))

;;; Commentary:

;; Native GitHub pages derive directly from `magit-section-mode'.  This module
;; owns page lifetime, asynchronous refresh generations, point and visibility
;; restoration, semantic rendering, and shared navigation keys.  It contains
;; no resource-specific API calls.

;;; Code:

(require 'ansi-color)
(require 'button)
(require 'cl-lib)
(require 'diff-mode)
(require 'eieio)
(require 'magit)
(require 'magit-diff)
(require 'magit-log)
(require 'magit-process)
(require 'magit-section)
(require 'subr-x)
(require 'url)
(require 'gh-core)

(declare-function gh-dispatch "gh-dispatch")
(declare-function gh-resource-open "gh-candidate")
(declare-function gh-resource-browse "gh-candidate")
(declare-function gh-resource-from-url "gh-candidate")
(declare-function gh-resource-create "gh-candidate")
(declare-function gh-resource-url "gh-candidate")
(declare-function gh-candidate-actions "gh-candidate")

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
(defvar-local gh-buffer-source-buffer nil
  "Buffer from which the current gh.el page was opened.")
(defvar-local gh-buffer-preview-p nil
  "Whether the current buffer is a disposable Consult preview.")
(defvar-local gh-buffer-refresh-function nil
  "Asynchronous fetch function for the current page.")
(defvar-local gh-buffer-render-function nil
  "Renderer for fetched page data.")
(defvar-local gh-buffer-dispatch-function nil
  "Contextual Transient function for the current page.")
(defvar-local gh-ui--generation 0)
(defvar-local gh-ui--data nil)
(defvar-local gh-ui--pending-state nil)
(defvar-local gh-ui--context-signature nil)

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
  (pcase-let ((`(,type ,key ,resource . ,rest) spec))
    `(magit-insert-section
         ((eval ',type)
          (gh-section-value-create :key ,key :resource ,resource)
          ,(car rest))
       (let ((start (point)))
         (magit-insert-heading ,heading)
         (when ,resource
           (add-text-properties start (point)
                                (list 'gh-resource ,resource))))
       (magit-insert-section-body
         ,@body))))

(defun gh-ui--section-resource (&optional section)
  "Return structured resource stored in SECTION or current section."
  (let* ((section (or section (magit-current-section)))
         (value (and section (oref section value))))
    (cond
     ((gh-section-value-p value) (gh-section-value-resource value))
     ((and (listp value) (plist-get value :kind)) value)
     (t nil))))

(defun gh-ui-resource-at-point ()
  "Return the structured resource at point."
  (or (get-text-property (point) 'gh-resource)
      (get-text-property (line-beginning-position) 'gh-resource)
      (gh-ui--section-resource)))

(defun gh-ui--visibility-enabled-p ()
  "Return non-nil when visibility caching applies to this page."
  (or (eq gh-section-cache-visibility t)
      (and (listp gh-section-cache-visibility)
           (memq gh-buffer-resource-kind gh-section-cache-visibility))))

(defun gh-ui--page-key ()
  "Return the session visibility cache key for the current page."
  (list gh-buffer-resource-kind gh-buffer-resource-id
        (and gh-buffer-context (gh-context-host gh-buffer-context))
        (and gh-buffer-context (gh-context-repository gh-buffer-context))
        (and gh-buffer-context (gh-context-ref gh-buffer-context))
        (and gh-buffer-context (gh-context-path gh-buffer-context))))

(defun gh-ui--walk-sections (function &optional root)
  "Call FUNCTION for ROOT and all its descendants."
  (when-let* ((section (or root magit-root-section)))
    (funcall function section)
    (dolist (child (oref section children))
      (gh-ui--walk-sections function child))))

(defun gh-ui--visibility-snapshot ()
  "Return an alist of section identifiers and hidden states."
  (let (result)
    (gh-ui--walk-sections
     (lambda (section)
       (unless (eq section magit-root-section)
         (push (cons (magit-section-ident section) (oref section hidden)) result))))
    result))

(defun gh-ui--remember-visibility ()
  "Remember current page section visibility when configured."
  (when (and (gh-ui--visibility-enabled-p) magit-root-section)
    (puthash (gh-ui--page-key) (gh-ui--visibility-snapshot)
             gh-ui--visibility-cache)))

(defun gh-ui--restore-cached-visibility ()
  "Apply a visibility snapshot for a newly opened page."
  (when-let* ((snapshot (and (gh-ui--visibility-enabled-p)
                             (gethash (gh-ui--page-key)
                                      gh-ui--visibility-cache))))
    (gh-ui--walk-sections
     (lambda (section)
       (when-let* ((cell (assoc (magit-section-ident section) snapshot)))
         (if (cdr cell)
             (magit-section-hide section)
           (magit-section-show section)))))))

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
       (goto-char (min (or (plist-get state :point) (point-min)) (point-max)))))
    ('line
     (goto-char (point-min))
     (forward-line (1- (max 1 (or (plist-get state :line) 1))))
     (move-to-column (or (plist-get state :column) 0)))
    (_ (goto-char (point-min))))
  (when-let* ((window (get-buffer-window (current-buffer) t))
              (start (plist-get state :window-start)))
    (set-window-start window (min start (point-max)) t)))

(defun gh-ui--context-signature ()
  "Return a value identifying the current buffer request context."
  (list (and gh-buffer-context (gh-context-host gh-buffer-context))
        (and gh-buffer-context (gh-context-repository gh-buffer-context))
        (and gh-buffer-context (gh-context-ref gh-buffer-context))
        (and gh-buffer-context (gh-context-path gh-buffer-context))
        gh-buffer-resource-kind gh-buffer-resource-id))

(defun gh-ui--replace (renderer data state)
  "Replace page contents by calling RENDERER with DATA, then restore STATE."
  (let* ((old-root magit-root-section)
         (loading-only
          (and old-root
               (let ((children (oref old-root children)))
                 (and (= (length children) 1)
                      (eq (oref (car children) type) 'loading)))))
        (inhibit-read-only t))
    (erase-buffer)
    ;; Leave `magit-root-section' pointing at OLD-ROOT until the top-level
    ;; macro starts.  `magit-insert-section' atomically saves that value as its
    ;; predecessor and installs the new root; pre-clearing it breaks nested
    ;; section parentage on the second asynchronous refresh.
    (magit-insert-section
        (gh-page (gh-section-value-create
                  :key (gh-ui--page-key)
                  :resource (gh-resource-create
                             gh-buffer-resource-kind gh-buffer-context
                             :id gh-buffer-resource-id)))
      (funcall renderer data))
    (when (or (null old-root) loading-only)
      (gh-ui--restore-cached-visibility))
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
        (signature (gh-ui--context-signature))
        (state (gh-ui--capture-state)))
    (setq gh-ui--pending-state state
          gh-ui--context-signature signature)
    (setq header-line-format
          (propertize " Loading GitHub data…" 'font-lock-face 'gh-loading))
    (unless gh-ui--data (gh-ui--render-loading))
    (funcall
     gh-buffer-refresh-function
     (lambda (data)
       (when (and (= generation gh-ui--generation)
                  (equal signature (gh-ui--context-signature)))
         (setq gh-ui--data data
               header-line-format nil)
         (gh-ui--replace gh-buffer-render-function data gh-ui--pending-state)
         (run-hooks 'gh-post-refresh-hook)))
     (lambda (error)
       (when (and (= generation gh-ui--generation)
                  (equal signature (gh-ui--context-signature)))
         (setq header-line-format nil)
         (gh-ui--render-error error gh-ui--pending-state)
         (run-hooks 'gh-post-refresh-hook)))
     (and force t))))

(cl-defun gh-ui--open-page
    (name context kind id fetch render &key preview source-buffer setup)
  "Open a native page NAME for KIND and ID in CONTEXT.
FETCH is called with success, error, and force arguments.  RENDER inserts page
contents from fetched data.  PREVIEW makes the buffer disposable.  SETUP, when
non-nil, runs in the page buffer after mode initialization."
  (let* ((source (or source-buffer (current-buffer)))
         (buffer (get-buffer-create name)))
    (with-current-buffer buffer
      (unless (derived-mode-p 'gh-section-mode) (gh-section-mode))
      (setq gh-buffer-context context
            gh-buffer-resource-kind kind
            gh-buffer-resource-id id
            gh-buffer-source-buffer source
            gh-buffer-preview-p preview
            gh-buffer-refresh-function fetch
            gh-buffer-render-function render
            gh-ui--context-signature (gh-ui--context-signature))
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
  (let ((resource (gh-ui-resource-at-point)))
    (unless resource (user-error "No GitHub resource at point"))
    (if (fboundp 'gh-resource-open)
        (gh-resource-open resource)
      (user-error "GitHub resource actions are not loaded"))))

(defun gh-ui-browse ()
  "Explicitly browse the resource at point on GitHub."
  (interactive)
  (let ((resource (gh-ui-resource-at-point)))
    (unless resource (user-error "No GitHub resource at point"))
    (if (fboundp 'gh-resource-browse)
        (gh-resource-browse resource)
      (browse-url (plist-get resource :url)))))

(defun gh-ui-copy-url ()
  "Copy the resource URL at point."
  (interactive)
  (let ((resource (gh-ui-resource-at-point)))
    (unless (and resource (fboundp 'gh-resource-url))
      (user-error "No GitHub resource URL at point"))
    (let ((url (gh-resource-url resource)))
      (unless url (user-error "Resource has no GitHub URL"))
      (kill-new url)
      (message "Copied %s" url))))

(defun gh-ui-dispatch ()
  "Open the contextual action menu for this page."
  (interactive)
  (let* ((resource (gh-ui-resource-at-point))
         (action (and resource (fboundp 'gh-candidate-actions)
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
  (if (fboundp 'gh-dispatch)
      (call-interactively #'gh-dispatch)
    (user-error "gh.el dispatch is not loaded")))

(defun gh-ui-quit ()
  "Leave the current gh.el page using `gh-bury-buffer-function'."
  (interactive)
  (gh-ui--remember-visibility)
  (if gh-buffer-preview-p
      (kill-buffer (current-buffer))
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

(defun gh-ui--insert-state (state)
  "Insert semantic STATE text."
  (insert (propertize (upcase (format "%s" state))
                      'font-lock-face (gh-core--state-face state))))

(defun gh-ui--insert-resource-line (text resource &optional face)
  "Insert TEXT line carrying structured RESOURCE."
  (let ((start (point)))
    (insert (if face (propertize text 'font-lock-face face) text) "\n")
    (add-text-properties start (point) (list 'gh-resource resource))))

(defun gh-ui--styled (value face)
  "Return VALUE as text carrying FACE, or nil when VALUE is empty."
  (when value
    (let ((text (format "%s" value)))
      (unless (string-empty-p text)
        (propertize text 'font-lock-face face)))))

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

(defun gh-ui--face-sequence (face)
  "Return FACE as a sequence suitable for a face text property."
  (cond
   ((null face) nil)
   ((and (listp face) (not (keywordp (car face)))) face)
   (t (list face))))

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
          (let* ((existing (get-text-property
                            position 'font-lock-face object))
                 (faces (delete-dups
                         (append (gh-ui--face-sequence face)
                                 (gh-ui--face-sequence existing))))
                 (merged (if (cdr faces) faces (car faces))))
            (put-text-property position next 'font-lock-face merged object)
            (remove-list-of-text-properties
             position next '(face) object)))
        (setq position next))))
  object)

(defun gh-ui--fontified-string (text mode)
  "Return TEXT fontified using MODE when available."
  (with-temp-buffer
    (insert (or text ""))
    (when (fboundp mode)
      (delay-mode-hooks (funcall mode))
      (font-lock-ensure))
    (let ((result (buffer-substring (point-min) (point-max))))
      (gh-ui--adopt-font-lock-faces 0 (length result) result)
      result)))

(defun gh-ui--make-resource-button (start end resource)
  "Turn START to END into a native RESOURCE button."
  (when (and resource (fboundp 'gh-resource-open))
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

(defun gh-ui--image-response-start ()
  "Return start of the body in the current URL response buffer."
  (goto-char (point-min))
  (or (and (boundp 'url-http-end-of-headers)
           (cond
            ((markerp url-http-end-of-headers)
             (marker-position url-http-end-of-headers))
            ((integerp url-http-end-of-headers)
             url-http-end-of-headers)))
      (and (re-search-forward "\r?\n\r?\n" nil t) (point))))

(defun gh-ui--image-finished (status target overlay url)
  "Install an image response described by STATUS into TARGET OVERLAY for URL."
  (let ((response (current-buffer)))
    (unwind-protect
        (when (and (buffer-live-p target) (overlayp overlay)
                   (overlay-buffer overlay) (null (plist-get status :error)))
          (let* ((start (gh-ui--image-response-start))
                 (size (and start (- (point-max) start))))
            (when (and size (<= size gh-view-inline-image-max-bytes))
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
      (when (buffer-live-p response) (kill-buffer response)))))

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
             (resource (and (fboundp 'gh-resource-from-url)
                            (gh-resource-from-url url context))))
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
  (let ((start (point))
        (mode (and (require 'markdown-mode nil t) 'gfm-mode)))
    (insert (if mode (gh-ui--fontified-string text mode) (or text "")))
    (unless (bolp) (insert "\n"))
    (let ((end (point)))
      (gh-ui--linkify-references start end (or context gh-buffer-context))
      (gh-ui--load-inline-images start end))))

(defun gh-ui--insert-diff (text)
  "Insert and fontify diff TEXT."
  (let ((start (point)))
    (insert (gh-ui--fontified-string text 'diff-mode))
    (unless (bolp) (insert "\n"))
    (add-face-text-property start (point) 'default nil)
    (gh-ui--adopt-font-lock-faces start (point))))

(defun gh-ui--insert-ansi (text)
  "Insert ANSI-colored TEXT safely."
  (let ((start (point)))
    (insert (or text ""))
    (ansi-color-apply-on-region start (point))
    (gh-ui--adopt-font-lock-faces start (point))
    (unless (bolp) (insert "\n"))))

(provide 'gh-ui)
;;; gh-ui.el ends here
