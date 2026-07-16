;;; magh-edit.el --- Structured GitHub resource editor -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; Author: magh.el contributors
;; Keywords: tools, vc, github

;;; Commentary:

;; This generic editor understands `field: value' headers and a body after a
;; `---' separator.  Resource modules own field definitions, validation, and
;; API submission.  Completion data is prefetched asynchronously; CAPF never
;; waits on GitHub.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'magh-core)

(declare-function magh-ui-refresh "magh-ui")

(defface magh-edit-field '((t :inherit font-lock-keyword-face :weight bold))
  "Face for structured editor field names." :group 'magh)
(defface magh-edit-separator '((t :inherit shadow))
  "Face for the structured editor separator." :group 'magh)

(defvar-local magh-edit-source-buffer nil)
(defvar-local magh-edit-fields nil)
(defvar-local magh-edit-submit-function nil)
(defvar-local magh-edit-after-success-function nil)
(defvar-local magh-edit-submitting nil)
(defvar-local magh-edit--completion-values nil)

(defvar-keymap magh-edit-mode-map
  :parent text-mode-map
  "C-c C-c" #'magh-edit-submit
  "C-c C-k" #'magh-edit-cancel)

(define-derived-mode magh-edit-mode text-mode "magh-edit"
  "Major mode for structured GitHub resource editing."
  :group 'magh
  (setq-local font-lock-defaults
              '((
                 ("^\\([[:alnum:]-]+\\):" 1 'magh-edit-field)
                 ("^---$" . 'magh-edit-separator))))
  (setq-local completion-at-point-functions '(magh-edit-completion-at-point))
  (setq-local require-final-newline t))

(defun magh-edit--field-name (definition)
  "Return string name for field DEFINITION."
  (symbol-name (plist-get definition :name)))

(defun magh-edit--definition (name)
  "Return field definition for NAME."
  (cl-find name magh-edit-fields :key #'magh-edit--field-name :test #'string=))

(defun magh-edit--encode-value (value definition)
  "Serialize VALUE according to field DEFINITION."
  (cond
   ((null value) "")
   ((plist-get definition :multiple)
    (mapconcat #'identity value ","))
   ((eq value t) "true")
   ((eq value :json-false) "false")
   (t (format "%s" value))))

(defun magh-edit--decode-value (text definition)
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
        (_ (signal 'magh-invalid-input
                   (list (format "%s must be true or false"
                                 (magh-edit--field-name definition)))))))
     ((eq (plist-get definition :type) 'integer)
      (if (string-empty-p text) nil
        (if (string-match-p "\\`[0-9]+\\'" text)
            (string-to-number text)
          (signal 'magh-invalid-input
                  (list (format "%s must be an integer"
                                (magh-edit--field-name definition)))))))
     (t (if (and (string-empty-p text)
                 (not (plist-get definition :allow-empty)))
            nil
          text)))))

(defun magh-edit--insert-template (values body)
  "Insert editor template using VALUES and BODY."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (dolist (definition magh-edit-fields)
      (let* ((name (plist-get definition :name))
             (value (plist-get values (intern (format ":%s" name)))))
        (insert (symbol-name name) ": "
                (magh-edit--encode-value value definition) "\n")))
    (insert "---\n" (or body ""))
    (unless (bolp) (insert "\n"))
    (goto-char (point-min))
    (set-buffer-modified-p nil)))

(defun magh-edit--parse ()
  "Parse current editor into (VALUES BODY)."
  (save-excursion
    (goto-char (point-min))
    (let (values)
      (while (and (not (eobp)) (not (looking-at-p "^---[ \t]*$")))
        (unless (looking-at "^\\([[:alnum:]-]+\\):[ \t]*\\(.*\\)$")
          (signal 'magh-invalid-input
                  (list (format "Malformed field line %d" (line-number-at-pos)))))
        (let* ((name (match-string-no-properties 1))
               (definition (magh-edit--definition name)))
          (unless definition
            (signal 'magh-invalid-input (list (format "Unknown field: %s" name))))
          (setq values
                (plist-put values
                           (intern (format ":%s" name))
                           (magh-edit--decode-value
                            (match-string-no-properties 2) definition))))
        (forward-line 1))
      (unless (looking-at-p "^---[ \t]*$")
        (signal 'magh-invalid-input (list "Missing `---' body separator")))
      (forward-line 1)
      (list values
            (string-remove-suffix
             "\n" (buffer-substring-no-properties (point) (point-max)))))))

(defun magh-edit--validate (values)
  "Validate parsed VALUES using current field definitions."
  (dolist (definition magh-edit-fields)
    (let* ((name (plist-get definition :name))
           (value (plist-get values (intern (format ":%s" name)))))
      (when (and (plist-get definition :required)
                 (or (null value) (equal value "")))
        (signal 'magh-invalid-input
                (list (format "%s is required" (symbol-name name)))))))
  values)

(defun magh-edit--field-at-point ()
  "Return the field definition for the current line."
  (save-excursion
    (beginning-of-line)
    (when (looking-at "^\\([[:alnum:]-]+\\):")
      (magh-edit--definition (match-string-no-properties 1)))))

(defun magh-edit-completion-at-point ()
  "Complete the structured field value at point without network waits."
  (when-let* ((definition (magh-edit--field-at-point))
              (name (plist-get definition :name))
              (values (or (gethash name magh-edit--completion-values)
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

(defun magh-edit--completion-fetcher (function context field)
  "Return a completion fetcher using FUNCTION in CONTEXT for FIELD."
  (lambda (success error)
    (funcall function context
             (lambda (items)
               (funcall success
                        (mapcar (lambda (item) (alist-get field item)) items)))
             error)))

(defun magh-edit--prime-completions ()
  "Start asynchronous completion fetches declared by field definitions."
  (let ((buffer (current-buffer)))
    (dolist (definition magh-edit-fields)
      (let ((name (plist-get definition :name))
            (fetch (plist-get definition :completion-fetch)))
        (when fetch
          (funcall
           fetch
           (lambda (values)
             (when (buffer-live-p buffer)
               (with-current-buffer buffer
                 (puthash name values magh-edit--completion-values))))
           #'ignore))))))

(cl-defun magh-edit-open
    (name fields values body submit &key after-success)
  "Open structured editor NAME.
FIELDS defines the header, VALUES and BODY provide initial content, and SUBMIT
receives values, body, success, and error callbacks."
  (let ((source (current-buffer))
        (buffer (get-buffer-create name)))
    (with-current-buffer buffer
      (magh-edit-mode)
      (setq magh-edit-source-buffer source
            magh-edit-fields fields
            magh-edit-submit-function submit
            magh-edit-after-success-function after-success
            magh-edit-submitting nil
            magh-edit--completion-values (make-hash-table :test #'eq))
      (magh-edit--insert-template values body)
      (magh-edit--prime-completions))
    (run-hooks 'magh-pre-display-buffer-hook)
    (funcall magh-display-buffer-function buffer)
    (run-hooks 'magh-post-display-buffer-hook)
    buffer))

(defun magh-edit-submit ()
  "Validate and asynchronously submit the current structured editor."
  (interactive)
  (when magh-edit-submitting
    (user-error "A submission is already in progress"))
  (unless magh-edit-submit-function
    (user-error "This editor has no submit function"))
  (pcase-let* ((`(,values ,body) (magh-edit--parse))
               (values (magh-edit--validate values))
               (buffer (current-buffer))
               (source magh-edit-source-buffer))
    (setq magh-edit-submitting t
          header-line-format (propertize " Submitting to GitHub…" 'face 'warning))
    (funcall
     magh-edit-submit-function values body
     (lambda (result)
       (when (buffer-live-p buffer)
         (with-current-buffer buffer
           (setq magh-edit-submitting nil header-line-format nil)
           (when magh-edit-after-success-function
             (funcall magh-edit-after-success-function result))
           (kill-buffer buffer)))
       (when (buffer-live-p source)
         (with-current-buffer source
           (magh-ui--refresh-if-page))))
     (lambda (error)
       (when (buffer-live-p buffer)
         (with-current-buffer buffer
           (let ((text (magh-error-message error)))
             (setq magh-edit-submitting nil
                   header-line-format
                   (propertize (format " Submission failed: %s" text)
                               'face 'error))
             (message "magh: %s" text))))))))

(defun magh-edit-cancel ()
  "Cancel editing, preserving the source page."
  (interactive)
  (when (or (not (buffer-modified-p))
            (yes-or-no-p "Discard this GitHub edit? "))
    (let ((source magh-edit-source-buffer))
      (kill-buffer)
      (when (buffer-live-p source)
        (pop-to-buffer source)))))

(provide 'magh-edit)
;;; magh-edit.el ends here
