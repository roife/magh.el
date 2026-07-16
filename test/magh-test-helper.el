;;; magh-test-helper.el --- Shared helpers for magh.el tests -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'ert)

(defconst magh-test-root
  (file-name-parent-directory
   (file-name-directory (or load-file-name buffer-file-name))))

(add-to-list 'load-path magh-test-root)
(setq load-prefer-newer t)

(defconst magh-test-fake-executable
  (expand-file-name "test/fake-gh" magh-test-root))

(defun magh-test-wait (predicate &optional seconds)
  "Wait up to SECONDS for PREDICATE while servicing processes and timers."
  (let ((deadline (+ (float-time) (or seconds 3))))
    (while (and (not (funcall predicate)) (< (float-time) deadline))
      (accept-process-output nil 0.02))
    (should (funcall predicate))))

(defmacro magh-test-with-clean-client (&rest body)
  "Run BODY with isolated magh.el client state and the fake executable."
  (declare (indent 0) (debug t))
  `(let ((magh-executable magh-test-fake-executable)
         (magh-client-cache-ttl 60))
     (clrhash magh-client--cache)
     (clrhash magh-client--inflight)
     (unwind-protect
         (progn ,@body)
       (maphash
        (lambda (_key request)
          (ignore-errors (magh-client-cancel request)))
        magh-client--inflight)
       (clrhash magh-client--cache)
       (clrhash magh-client--inflight))))

(provide 'magh-test-helper)
;;; magh-test-helper.el ends here
