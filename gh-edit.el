;;; gh-edit.el --- Structured GitHub resource editor -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; Author: gh.el contributors
;; Keywords: tools, vc, github
;; Package-Requires: ((emacs "29.1"))

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

(defvar-local gh-edit-context nil)
(defvar-local gh-edit-resource nil)
(defvar-local gh-edit-source-buffer nil)
(defvar-local gh-edit-fields nil)
(defvar-local gh-edit-submit-function nil)
(defvar-local gh-edit-after-success-function nil)
(defvar-local gh-edit-submitting nil)
(defvar-local gh-edit-original-values nil)
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
    (mapconcat (lambda (item) (format "%s" item)) value ","))
   ((eq value t) "true")
   ((eq value :json-false) "false")
   (t (format "%s" value))))

(defun gh-edit--decode-value (text definition)
  "Parse TEXT according to field DEFINITION."
  (let ((text (string-trim text)))
    (cond
     ((plist-get definition :multiple)
      (if (string-empty-p text) nil
        (mapcar #'string-trim (split-string text "," t))))
     ((eq (plist-get definition :type) 'boolean)
      (pcase (downcase text)
        ((or "true" "yes" "1") t)
        ((or "false" "no" "0" "") :json-false)
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

(defun gh-edit--values-get (values name)
  "Get NAME from plist or alist VALUES."
  (if (keywordp (car-safe values))
      (plist-get values (intern (format ":%s" name)))
    (or (alist-get name values)
        (alist-get (symbol-name name) values nil nil #'equal))))

(defun gh-edit--insert-template (values body)
  "Insert editor template using VALUES and BODY."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (dolist (definition gh-edit-fields)
      (let* ((name (plist-get definition :name))
             (value (gh-edit--values-get values name)))
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
    (let (values separator)
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
      (setq separator (line-end-position))
      (goto-char separator)
      (forward-line 1)
      (list values
            (string-remove-suffix
             "\n" (buffer-substring-no-properties (point) (point-max)))))))

(defun gh-edit--validate (values)
  "Validate parsed VALUES using current field definitions."
  (dolist (definition gh-edit-fields)
    (let* ((name (plist-get definition :name))
           (value (plist-get values (intern (format ":%s" name))))
           (validator (plist-get definition :validate)))
      (when (and (plist-get definition :required)
                 (or (null value) (equal value "")))
        (signal 'gh-invalid-input
                (list (format "%s is required" (symbol-name name)))))
      (when validator
        (let ((message (funcall validator value values)))
          (when (stringp message)
            (signal 'gh-invalid-input (list message)))))))
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
          (search-forward ":" end t)
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
           (lambda (_error) nil)))))))

(cl-defun gh-edit-open
    (name context resource fields values body submit
          &key source-buffer after-success)
  "Open structured editor NAME.
CONTEXT and RESOURCE are preserved for navigation.  FIELDS defines the header,
VALUES and BODY provide initial content, and SUBMIT receives values, body,
success, and error callbacks."
  (let ((source (or source-buffer (current-buffer)))
        (buffer (get-buffer-create name)))
    (with-current-buffer buffer
      (gh-edit-mode)
      (setq gh-edit-context context
            gh-edit-resource resource
            gh-edit-source-buffer source
            gh-edit-fields fields
            gh-edit-submit-function submit
            gh-edit-after-success-function after-success
            gh-edit-original-values values
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
           (when (fboundp 'gh-ui-refresh) (gh-ui-refresh t)))))
     (lambda (error)
       (when (buffer-live-p buffer)
         (with-current-buffer buffer
           (setq gh-edit-submitting nil
                 header-line-format
                 (propertize (format " Submission failed: %s"
                                     (gh-error-message error))
                             'face 'error))
           (message "gh: %s" (gh-error-message error))))))))

(defun gh-edit-cancel ()
  "Cancel editing, preserving the source page."
  (interactive)
  (when (or (not (buffer-modified-p))
            (yes-or-no-p "Discard this GitHub edit? "))
    (let ((source gh-edit-source-buffer))
      (kill-buffer (current-buffer))
      (when (buffer-live-p source)
        (pop-to-buffer source)))))

(provide 'gh-edit)
;;; gh-edit.el ends here
