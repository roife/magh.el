;;; magh-candidate-test.el --- Tests for magh-candidate -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for shared candidate selection helpers.

;;; Code:

(require 'magh-test-helper)
(require 'magh-candidate)

(ert-deftest magh-candidate-read-empty-resources-signals-user-error ()
  (let ((condition (should-error
                    (magh-candidate-read "Resource: " nil)
                    :type 'user-error)))
    (should (equal (cadr condition) "No GitHub resources available"))))

(ert-deftest magh-candidate-read-empty-resources-skips-consult ()
  (let ((consult-called nil))
    (cl-letf (((symbol-function 'consult--read)
               (lambda (&rest _args)
                 (setq consult-called t))))
      (should-error (magh-candidate-read "Resource: " nil)
                    :type 'user-error))
    (should-not consult-called)))

(provide 'magh-candidate-test)
;;; magh-candidate-test.el ends here
