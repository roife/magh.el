;;; gh-edit-test.el --- Structured editor tests for gh.el -*- lexical-binding: t; -*-

(require 'gh-test-helper)
(require 'gh-edit)

(ert-deftest gh-edit-round-trips-typed-fields-and-body ()
  (with-temp-buffer
    (gh-edit-mode)
    (setq gh-edit-fields
          '((:name title :required t)
            (:name labels :multiple t)
            (:name draft :type boolean)
            (:name count :type integer)))
    (insert "title: Hello\nlabels: bug, help wanted\ndraft: false\ncount: 3\n---\nBody\n")
    (pcase-let ((`(,values ,body) (gh-edit--parse)))
      (should (equal (plist-get values :title) "Hello"))
      (should (equal (plist-get values :labels) '("bug" "help wanted")))
      (should (eq (plist-get values :draft) :json-false))
      (should (= (plist-get values :count) 3))
      (should (equal body "Body")))))

(ert-deftest gh-edit-rejects-unknown-and-missing-required-fields ()
  (with-temp-buffer
    (gh-edit-mode)
    (setq gh-edit-fields '((:name title :required t)))
    (insert "unknown: value\n---\n")
    (should-error (gh-edit--parse) :type 'gh-invalid-input))
  (with-temp-buffer
    (gh-edit-mode)
    (setq gh-edit-fields '((:name title :required t)))
    (insert "title: \n---\n")
    (pcase-let ((`(,values ,_body) (gh-edit--parse)))
      (should-error (gh-edit--validate values) :type 'gh-invalid-input))))

(ert-deftest gh-edit-capf-never-starts-a-network-request ()
  (with-temp-buffer
    (gh-edit-mode)
    (setq gh-edit-fields '((:name labels :multiple t))
          gh-edit--completion-values (make-hash-table :test #'eq))
    (puthash 'labels '("bug" "feature") gh-edit--completion-values)
    (insert "labels: bu")
    (let ((capf (gh-edit-completion-at-point)))
      (should capf)
      (should (equal (nth 2 capf) '("bug" "feature"))))))

(provide 'gh-edit-test)
;;; gh-edit-test.el ends here
