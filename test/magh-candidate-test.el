;;; magh-candidate-test.el --- Tests for magh-candidate -*- lexical-binding: t; -*-

(require 'magh-test-helper)
(require 'magh-candidate)

(ert-deftest magh-candidate-read-rejects-empty-resources-before-consult ()
  (let ((consult-called nil))
    (cl-letf (((symbol-function 'consult--read)
               (lambda (&rest _args)
                 (setq consult-called t))))
      (let ((condition (should-error
                        (magh-candidate-read "Resource: " nil)
                        :type 'user-error)))
        (should (equal (cadr condition)
                       "No GitHub resources available"))))
    (should-not consult-called)))

(provide 'magh-candidate-test)
;;; magh-candidate-test.el ends here
