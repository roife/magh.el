;;; magh-command.el --- Generic GitHub CLI and API entry points -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; Author: magh.el contributors
;; Keywords: tools, vc, github

;;; Commentary:

;; PTY access preserves coverage of every gh command and installed extension.
;; The generic API page provides asynchronous REST and GraphQL access without
;; bypassing the unified client transport.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'subr-x)
(require 'magh-api)
(require 'magh-client)
(require 'magh-core)

(defvar-local magh-command--generation 0)

;;;###autoload
(defun magh-command (argv &optional context)
  "Run arbitrary GitHub CLI ARGV in an interactive Emacs PTY.
ARGV is a list of strings.  No shell evaluates the command."
  (interactive
   (list (split-string-shell-command (read-shell-command "gh "))))
  (setq context (magh-context-resolve context))
  (let* ((label (if argv (string-join (take 2 argv) " ") "gh"))
         (buffer (magh-client--start-pty argv (format "*magh command: %s*" label)
                                       context)))
    (funcall magh-display-buffer-function buffer)
    buffer))

(defun magh-command--parse-fields (text)
  "Parse comma-separated key=value TEXT into an alist."
  (unless (string-empty-p text)
    (mapcar
     (lambda (field)
       (unless (string-match "\\`\\([^=]+\\)=\\(.*\\)\\'" field)
         (user-error "Expected key=value field: %s" field))
       (cons (match-string 1 field) (match-string 2 field)))
     (split-string text "," t "[ \t\n]+"))))

(defun magh-command--json-string (data)
  "Serialize DATA as pretty JSON."
  (with-temp-buffer
    (insert (json-serialize data :null-object nil :false-object :json-false))
    (json-pretty-print-buffer)
    (buffer-string)))

(defun magh-command--api-buffer (endpoint)
  "Create generic API result buffer for ENDPOINT."
  (let ((buffer (get-buffer-create (format "*magh api: %s*" endpoint))))
    (with-current-buffer buffer
      (unless (derived-mode-p 'special-mode) (special-mode))
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (propertize "Loading GitHub API response…\n"
                            'font-lock-face 'shadow))))
    buffer))

(defun magh-command--run-api-request (endpoint request)
  "Display the asynchronous API REQUEST in a buffer named for ENDPOINT.
REQUEST receives success and error callbacks."
  (let ((buffer (magh-command--api-buffer endpoint)))
    (with-current-buffer buffer
      (cl-incf magh-command--generation)
      (let ((generation magh-command--generation))
        (funcall
         request
         (lambda (data)
           (when (= generation magh-command--generation)
             (let ((inhibit-read-only t))
               (erase-buffer)
               (insert (magh-command--json-string data))
               (goto-char (point-min)))))
         (lambda (error)
           (when (= generation magh-command--generation)
             (let ((inhibit-read-only t))
               (erase-buffer)
               (insert (magh-error-message error) "\n")))))))
    (funcall magh-display-buffer-function buffer)
    buffer))

;;;###autoload
(defun magh-api-request (endpoint &optional method fields paginate context)
  "Asynchronously call arbitrary REST ENDPOINT.
METHOD defaults to GET.  FIELDS is an alist and PAGINATE requests all pages."
  (interactive
   (list (read-string "GitHub API endpoint: ")
         (completing-read "Method: " '("GET" "POST" "PATCH" "PUT" "DELETE")
                          nil t nil nil "GET")
         (magh-command--parse-fields
          (read-string "Fields (key=value, comma separated): "))
         current-prefix-arg))
  (setq context (magh-context-resolve context)
        method (or method "GET"))
  (magh-command--run-api-request
   endpoint
   (lambda (success error)
     (magh-api--generic-request context endpoint method fields paginate
                              success error))))

;;;###autoload
(defun magh-graphql-request (query variables &optional context)
  "Asynchronously run GraphQL QUERY with VARIABLES alist."
  (interactive
   (list (read-string "GraphQL query: ")
         (magh-command--parse-fields
          (read-string "Variables (key=value, comma separated): "))))
  (setq context (magh-context-resolve context))
  (magh-command--run-api-request
   "graphql"
   (lambda (success error)
     (magh-api--graphql context query variables success error))))

(defun magh-auth-switch (host user)
  "Switch active GitHub CLI account to USER on HOST asynchronously."
  (interactive
   (list (read-string "Host: " (or magh-host "github.com"))
         (read-string "User: ")))
  (let ((context (magh-context-create :host host)))
    (magh-client--mutate-text
     (append (list "auth" "switch" "--hostname" host)
             (unless (string-empty-p user) (list "--user" user)))
     (lambda (_)
       (magh-client-invalidate-account)
       (run-hooks 'magh-auth-post-switch-hook)
       (message "Switched GitHub CLI account on %s" host))
     #'magh-core--user-error :context context)))

(provide 'magh-command)
;;; magh-command.el ends here
