;;; gh-edit.el --- Structured GitHub resource editor -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; Author: gh.el contributors
;; Keywords: tools, vc, github

;;; Commentary:

;; This generic editor understands `field: value' headers and a body after a
;; `---' separator.  Resource modules own field definitions, validation, and
;; API submission.  Completion data is prefetched asynchronously; CAPF never
;; waits on GitHub.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'gh-core)

(declare-function gh-ui-refresh "gh-ui")

(defface gh-edit-field '((t :inherit font-lock-keyword-face :weight bold))
  "Face for structured editor field names." :group 'gh)
(defface gh-edit-separator '((t :inherit shadow))
  "Face for the structured editor separator." :group 'gh)

(defvar-local gh-edit-source-buffer nil)
(defvar-local gh-edit-fields nil)
(defvar-local gh-edit-submit-function nil)
(defvar-local gh-edit-after-success-function nil)
(defvar-local gh-edit-submitting nil)
(defvar-local gh-edit--completion-values nil)

(defvar-keymap gh-edit-mode-map
  :parent text-mode-map
  "C-c C-c" #'gh-edit-submit
  "C-c C-k" #'gh-edit-cancel)

(define-derived-mode gh-edit-mode text-mode "gh-edit"
  "Major mode for structured GitHub resource editing."
  :group 'gh
  (setq-local font-lock-defaults
              '((
                 ("^\\([[:alnum:]-]+\\):" 1 'gh-edit-field)
                 ("^---$" . 'gh-edit-separator))))
  (setq-local completion-at-point-functions '(gh-edit-completion-at-point))
  (setq-local require-final-newline t))

(defun gh-edit--field-name (definition)
  "Return string name for field DEFINITION."
  (symbol-name (plist-get definition :name)))

(defun gh-edit--definition (name)
  "Return field definition for NAME."
  (cl-find name gh-edit-fields :key #'gh-edit--field-name :test #'string=))

(defun gh-edit--encode-value (value definition)
  "Serialize VALUE according to field DEFINITION."
  (cond
   ((null value) "")
   ((plist-get definition :multiple)
    (mapconcat #'identity value ","))
   ((eq value t) "true")
   ((eq value :json-false) "false")
   (t (format "%s" value))))

(defun gh-edit--decode-value (text definition)
  "Parse TEXT according to field DEFINITION."
  (let ((text (string-trim text)))
    (cond
     ((plist-get definition :multiple)
      (if (string-empty-p text) nil
        (split-string text "[ \t]*,[ \t]*" t)))
     ((eq (plist-get definition :type) 'boolean)
      (pcase (downcase text)
        ("true" t)
        ((or "false" "") :json-false)
        (_ (signal 'gh-invalid-input
                   (list (format "%s must be true or false"
                                 (gh-edit--field-name definition)))))))
     ((eq (plist-get definition :type) 'integer)
      (if (string-empty-p text) nil
        (if (string-match-p "\\`[0-9]+\\'" text)
            (string-to-number text)
          (signal 'gh-invalid-input
                  (list (format "%s must be an integer"
                                (gh-edit--field-name definition)))))))
     (t text))))

(defun gh-edit--insert-template (values body)
  "Insert editor template using VALUES and BODY."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (dolist (definition gh-edit-fields)
      (let* ((name (plist-get definition :name))
             (value (plist-get values (intern (format ":%s" name)))))
        (insert (symbol-name name) ": "
                (gh-edit--encode-value value definition) "\n")))
    (insert "---\n" (or body ""))
    (unless (bolp) (insert "\n"))
    (goto-char (point-min))
    (set-buffer-modified-p nil)))

(defun gh-edit--parse ()
  "Parse current editor into (VALUES BODY)."
  (save-excursion
    (goto-char (point-min))
    (let (values)
      (while (and (not (eobp)) (not (looking-at-p "^---[ \t]*$")))
        (unless (looking-at "^\\([[:alnum:]-]+\\):[ \t]*\\(.*\\)$")
          (signal 'gh-invalid-input
                  (list (format "Malformed field line %d" (line-number-at-pos)))))
        (let* ((name (match-string-no-properties 1))
               (definition (gh-edit--definition name)))
          (unless definition
            (signal 'gh-invalid-input (list (format "Unknown field: %s" name))))
          (setq values
                (plist-put values
                           (intern (format ":%s" name))
                           (gh-edit--decode-value
                            (match-string-no-properties 2) definition))))
        (forward-line 1))
      (unless (looking-at-p "^---[ \t]*$")
        (signal 'gh-invalid-input (list "Missing `---' body separator")))
      (forward-line 1)
      (list values
            (string-remove-suffix
             "\n" (buffer-substring-no-properties (point) (point-max)))))))

(defun gh-edit--validate (values)
  "Validate parsed VALUES using current field definitions."
  (dolist (definition gh-edit-fields)
    (let* ((name (plist-get definition :name))
           (value (plist-get values (intern (format ":%s" name)))))
      (when (and (plist-get definition :required)
                 (or (null value) (equal value "")))
        (signal 'gh-invalid-input
                (list (format "%s is required" (symbol-name name)))))))
  values)

(defun gh-edit--field-at-point ()
  "Return the field definition for the current line."
  (save-excursion
    (beginning-of-line)
    (when (looking-at "^\\([[:alnum:]-]+\\):")
      (gh-edit--definition (match-string-no-properties 1)))))

(defun gh-edit-completion-at-point ()
  "Complete the structured field value at point without network waits."
  (when-let* ((definition (gh-edit--field-at-point))
              (name (plist-get definition :name))
              (values (or (gethash name gh-edit--completion-values)
                          (plist-get definition :choices))))
    (save-excursion
      (let ((end (line-end-position)) start)
        (if (plist-get definition :multiple)
            (progn
              (skip-chars-backward "^," (line-beginning-position))
              (setq start (point))
              (skip-chars-forward " \t" end)
              (setq start (point)))
          (beginning-of-line)
          (search-forward ":" end)
          (skip-chars-forward " \t" end)
          (setq start (point)))
        (list start end values :exclusive 'no)))))

(defun gh-edit--prime-completions ()
  "Start asynchronous completion fetches declared by field definitions."
  (let ((buffer (current-buffer)))
    (dolist (definition gh-edit-fields)
      (let ((name (plist-get definition :name))
            (fetch (plist-get definition :completion-fetch)))
        (when fetch
          (funcall
           fetch
           (lambda (values)
             (when (buffer-live-p buffer)
               (with-current-buffer buffer
                 (puthash name values gh-edit--completion-values))))
           #'ignore))))))

(cl-defun gh-edit-open
    (name fields values body submit &key after-success)
  "Open structured editor NAME.
FIELDS defines the header, VALUES and BODY provide initial content, and SUBMIT
receives values, body, success, and error callbacks."
  (let ((source (current-buffer))
        (buffer (get-buffer-create name)))
    (with-current-buffer buffer
      (gh-edit-mode)
      (setq gh-edit-source-buffer source
            gh-edit-fields fields
            gh-edit-submit-function submit
            gh-edit-after-success-function after-success
            gh-edit-submitting nil
            gh-edit--completion-values (make-hash-table :test #'eq))
      (gh-edit--insert-template values body)
      (gh-edit--prime-completions))
    (run-hooks 'gh-pre-display-buffer-hook)
    (funcall gh-display-buffer-function buffer)
    (run-hooks 'gh-post-display-buffer-hook)
    buffer))

(defun gh-edit-submit ()
  "Validate and asynchronously submit the current structured editor."
  (interactive)
  (when gh-edit-submitting
    (user-error "A submission is already in progress"))
  (unless gh-edit-submit-function
    (user-error "This editor has no submit function"))
  (pcase-let* ((`(,values ,body) (gh-edit--parse))
               (values (gh-edit--validate values))
               (buffer (current-buffer))
               (source gh-edit-source-buffer))
    (setq gh-edit-submitting t
          header-line-format (propertize " Submitting to GitHub…" 'face 'warning))
    (funcall
     gh-edit-submit-function values body
     (lambda (result)
       (when (buffer-live-p buffer)
         (with-current-buffer buffer
           (setq gh-edit-submitting nil header-line-format nil)
           (when gh-edit-after-success-function
             (funcall gh-edit-after-success-function result))
           (kill-buffer buffer)))
       (when (buffer-live-p source)
         (with-current-buffer source
           (when (derived-mode-p 'gh-section-mode)
             (gh-ui-refresh t)))))
     (lambda (error)
       (when (buffer-live-p buffer)
         (with-current-buffer buffer
           (let ((text (gh-error-message error)))
             (setq gh-edit-submitting nil
                   header-line-format
                   (propertize (format " Submission failed: %s" text)
                               'face 'error))
             (message "gh: %s" text))))))))

(defun gh-edit-cancel ()
  "Cancel editing, preserving the source page."
  (interactive)
  (when (or (not (buffer-modified-p))
            (yes-or-no-p "Discard this GitHub edit? "))
    (let ((source gh-edit-source-buffer))
      (kill-buffer)
      (when (buffer-live-p source)
        (pop-to-buffer source)))))

(provide 'gh-edit)
;;; gh-edit.el ends here
