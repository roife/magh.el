;;; gh-test-helper.el --- Shared helpers for gh.el tests -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'ert)

(defconst gh-test-root
  (file-name-parent-directory
   (file-name-directory (or load-file-name buffer-file-name))))

(add-to-list 'load-path gh-test-root)
(setq load-prefer-newer t)

(defconst gh-test-fake-executable
  (expand-file-name "test/fake-gh" gh-test-root))

(defun gh-test-wait (predicate &optional seconds)
  "Wait up to SECONDS for PREDICATE while servicing processes and timers."
  (let ((deadline (+ (float-time) (or seconds 3))))
    (while (and (not (funcall predicate)) (< (float-time) deadline))
      (accept-process-output nil 0.02))
    (should (funcall predicate))))

(defmacro gh-test-with-clean-client (&rest body)
  "Run BODY with isolated gh.el client state and the fake executable."
  (declare (indent 0) (debug t))
  `(let ((gh-executable gh-test-fake-executable)
         (gh-client-cache-ttl 60))
     (clrhash gh-client--cache)
     (clrhash gh-client--inflight)
     (unwind-protect
         (progn ,@body)
       (maphash
        (lambda (_key request)
          (ignore-errors (gh-client-cancel request)))
        gh-client--inflight)
       (clrhash gh-client--cache)
       (clrhash gh-client--inflight))))

(provide 'gh-test-helper)
;;; gh-test-helper.el ends here
