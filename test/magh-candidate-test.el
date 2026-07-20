;;; magh-candidate-test.el --- Tests for magh-candidate -*- lexical-binding: t; -*-

(require 'magh-test-helper)
(require 'magh-candidate)
(require 'magh-project)

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

(ert-deftest magh-project-items-route-topics-and-keep-drafts-in-projects ()
  (let* ((context (magh-context-from-repository "acme/board"))
         (issue
          (magh-project--item-resource
           context "acme" 4
           '((id . "ITEM-I")
             (content . ((type . "Issue") (number . 9) (title . "Issue")
                         (url . "https://github.com/acme/widgets/issues/9"))))))
         (pr
          (magh-project--item-resource
           context "acme" 4
           '((id . "ITEM-P")
             (content . ((type . "PullRequest") (number . 8) (title . "PR")
                         (url . "https://github.com/acme/widgets/pull/8"))))))
         (draft
          (magh-project--item-resource
           context "acme" 4
           '((id . "ITEM-D")
             (content . ((type . "DraftIssue") (title . "Draft")))))))
    (should (eq (plist-get issue :kind) 'issue))
    (should (= (plist-get issue :number) 9))
    (should (equal (plist-get issue :project-item-id) "ITEM-I"))
    (should (eq (plist-get pr :kind) 'pr))
    (should (= (plist-get pr :number) 8))
    (should (eq (plist-get draft :kind) 'project-item))
    (should (plist-get draft :draft))
    (should (= (plist-get draft :number) 4))))

(ert-deftest magh-discussion-urls-and-notifications-route-natively ()
  (let* ((resource
          (magh-resource-from-url
           "https://github.com/acme/widgets/discussions/27"))
         (notification
          (magh-candidate--notification-resource
           (magh-context-create :host "github.com")
           '((id . "N")
             (repository . ((full_name . "acme/widgets")))
             (subject . ((type . "Discussion") (title . "Topic")
                         (url . "https://api.github.com/repos/acme/widgets/discussions/27"))))))
         (subject (plist-get notification :subject-resource)))
    (should (eq (plist-get resource :kind) 'discussion))
    (should (= (plist-get resource :number) 27))
    (should (eq (plist-get subject :kind) 'discussion))
    (should (= (plist-get subject :number) 27))))

(provide 'magh-candidate-test)
;;; magh-candidate-test.el ends here
