;;; magh-edit-test.el --- Structured editor tests for magh.el -*- lexical-binding: t; -*-

(require 'magh-test-helper)
(require 'magh-edit)

(ert-deftest magh-edit-round-trips-typed-fields-and-body ()
  (with-temp-buffer
    (magh-edit-mode)
    (setq magh-edit-fields
          '((:name title :required t)
            (:name labels :multiple t)
            (:name draft :type boolean)
            (:name count :type integer)))
    (insert "title: Hello\nlabels: bug, help wanted\ndraft: false\ncount: 3\n---\nBody\n")
    (pcase-let ((`(,values ,body) (magh-edit--parse)))
      (should (equal (plist-get values :title) "Hello"))
      (should (equal (plist-get values :labels) '("bug" "help wanted")))
      (should (eq (plist-get values :draft) :json-false))
      (should (= (plist-get values :count) 3))
      (should (equal body "Body")))))

(ert-deftest magh-edit-decodes-blank-fields-with-field-semantics ()
  (with-temp-buffer
    (magh-edit-mode)
    (setq magh-edit-fields
          '((:name milestone)
            (:name description :allow-empty t)
            (:name labels :multiple t)
            (:name draft :type boolean)
            (:name count :type integer)))
    (insert "milestone:   \ndescription: \nlabels: \ndraft: \ncount: \n---\n")
    (pcase-let ((`(,values ,_body) (magh-edit--parse)))
      (should-not (plist-get values :milestone))
      (should (equal (plist-get values :description) ""))
      (should-not (plist-get values :labels))
      (should (eq (plist-get values :draft) :json-false))
      (should-not (plist-get values :count)))))

(ert-deftest magh-edit-rejects-unknown-and-missing-required-fields ()
  (with-temp-buffer
    (magh-edit-mode)
    (setq magh-edit-fields '((:name title :required t)))
    (insert "unknown: value\n---\n")
    (should-error (magh-edit--parse) :type 'magh-invalid-input))
  (with-temp-buffer
    (magh-edit-mode)
    (setq magh-edit-fields '((:name title :required t)))
    (insert "title: \n---\n")
    (pcase-let ((`(,values ,_body) (magh-edit--parse)))
      (should-not (plist-get values :title))
      (should-error (magh-edit--validate values) :type 'magh-invalid-input))))

(ert-deftest magh-edit-capf-never-starts-a-network-request ()
  (with-temp-buffer
    (magh-edit-mode)
    (setq magh-edit-fields '((:name labels :multiple t))
          magh-edit--completion-values (make-hash-table :test #'eq))
    (puthash 'labels '("bug" "feature") magh-edit--completion-values)
    (insert "labels: bu")
    (let ((capf (magh-edit-completion-at-point)))
      (should capf)
      (should (equal (nth 2 capf) '("bug" "feature"))))))

(provide 'magh-edit-test)
;;; magh-edit-test.el ends here
