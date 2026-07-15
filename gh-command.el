;;; gh-command.el --- Generic GitHub CLI and API entry points -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; Author: gh.el contributors
;; Keywords: tools, vc, github
;; Package-Requires: ((emacs "29.1"))

;;; Commentary:

;; PTY access preserves coverage of every gh command and installed extension.
;; The generic API page provides asynchronous REST and GraphQL access without
;; bypassing the unified client transport.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'seq)
(require 'subr-x)
(require 'gh-api)
(require 'gh-client)
(require 'gh-core)

(defvar-local gh-command--generation 0)

(defun gh-command--read-argv ()
  "Read a GitHub CLI command line and safely return argv."
  (let ((line (read-shell-command "gh ")))
    (condition-case error
        (split-string-and-unquote line)
      (error (user-error "Cannot parse command: %s"
                         (error-message-string error))))))

;;;###autoload
(defun gh-command (argv &optional context)
  "Run arbitrary GitHub CLI ARGV in an interactive Emacs PTY.
ARGV is a list of strings.  No shell evaluates the command."
  (interactive (list (gh-command--read-argv)))
  (setq context (gh-context-resolve context))
  (let* ((label (if argv (string-join (seq-take argv 2) " ") "gh"))
         (buffer (gh-client--start-pty argv (format "*gh command: %s*" label)
                                       context)))
    (funcall gh-display-buffer-function buffer)
    buffer))

(defun gh-command--parse-fields (text)
  "Parse comma-separated key=value TEXT into an alist."
  (unless (string-empty-p text)
    (mapcar
     (lambda (field)
       (unless (string-match "\\`\\([^=]+\\)=\\(.*\\)\\'" field)
         (user-error "Expected key=value field: %s" field))
       (cons (match-string 1 field) (match-string 2 field)))
     (split-string text "," t "[ \t\n]+"))))

(defun gh-command--json-string (data)
  "Serialize DATA as pretty JSON."
  (with-temp-buffer
    (insert (json-serialize data :null-object nil :false-object :json-false))
    (json-pretty-print-buffer)
    (buffer-string)))

(defun gh-command--api-buffer (endpoint)
  "Create generic API result buffer for ENDPOINT."
  (let ((buffer (get-buffer-create (format "*gh api: %s*" endpoint))))
    (with-current-buffer buffer
      (special-mode)
      (setq-local gh-command--generation 0)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (propertize "Loading GitHub API response…\n"
                            'font-lock-face 'shadow))))
    buffer))

;;;###autoload
(defun gh-api-request (endpoint &optional method fields paginate context)
  "Asynchronously call arbitrary REST ENDPOINT.
METHOD defaults to GET.  FIELDS is an alist and PAGINATE requests all pages."
  (interactive
   (list (read-string "GitHub API endpoint: ")
         (completing-read "Method: " '("GET" "POST" "PATCH" "PUT" "DELETE")
                          nil t nil nil "GET")
         (gh-command--parse-fields
          (read-string "Fields (key=value, comma separated): "))
         current-prefix-arg))
  (setq context (gh-context-resolve context)
        method (or method "GET"))
  (let ((buffer (gh-command--api-buffer endpoint)))
    (with-current-buffer buffer
      (cl-incf gh-command--generation)
      (let ((generation gh-command--generation))
        (gh-api--generic-request
         context endpoint method fields paginate
         (lambda (data)
           (when (= generation gh-command--generation)
             (let ((inhibit-read-only t))
               (erase-buffer)
               (insert (gh-command--json-string data))
               (goto-char (point-min)))))
         (lambda (error)
           (when (= generation gh-command--generation)
             (let ((inhibit-read-only t))
               (erase-buffer)
               (insert (gh-error-message error) "\n")))))))
    (funcall gh-display-buffer-function buffer)
    buffer))

;;;###autoload
(defun gh-graphql-request (query variables &optional context)
  "Asynchronously run GraphQL QUERY with VARIABLES alist."
  (interactive
   (list (read-string "GraphQL query: ")
         (gh-command--parse-fields
          (read-string "Variables (key=value, comma separated): "))))
  (setq context (gh-context-resolve context))
  (let ((buffer (gh-command--api-buffer "graphql")))
    (with-current-buffer buffer
      (cl-incf gh-command--generation)
      (let ((generation gh-command--generation))
        (gh-api--graphql
         context query variables
         (lambda (data)
           (when (= generation gh-command--generation)
             (let ((inhibit-read-only t))
               (erase-buffer)
               (insert (gh-command--json-string data))
               (goto-char (point-min)))))
         (lambda (error)
           (when (= generation gh-command--generation)
             (let ((inhibit-read-only t))
               (erase-buffer)
               (insert (gh-error-message error) "\n")))))))
    (funcall gh-display-buffer-function buffer)
    buffer))

(defun gh-auth-switch (host user)
  "Switch active GitHub CLI account to USER on HOST asynchronously."
  (interactive
   (list (read-string "Host: " (or gh-host "github.com"))
         (read-string "User: ")))
  (let ((context (gh-context-create :host host)))
    (gh-client--mutate-text
     (append (list "auth" "switch" "--hostname" host)
             (unless (string-empty-p user) (list "--user" user)))
     (lambda (_)
       (gh-client-invalidate-account)
       (run-hooks 'gh-auth-post-switch-hook)
       (message "Switched GitHub CLI account on %s" host))
     #'gh-core--user-error :context context)))

(provide 'gh-command)
;;; gh-command.el ends here
