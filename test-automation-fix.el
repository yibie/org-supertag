;;; test-automation-fix.el --- Test automation data integrity fixes -*- lexical-binding: t; -*-

;;; Commentary:
;; Test script to verify that automation rules no longer cause data loss.

;;; Code:

(require 'supertag-automation)
(require 'supertag-automation-sync)

(defun test-automation-data-preservation ()
  "Test that automation preserves node data during field updates."
  (interactive)
  (let ((test-node-id "test-node-123")
        (initial-data '(:id "test-node-123"
                         :title "Test Node"
                         :tags ("Project" "Task")
                         :properties (:status "active" :priority "high")
                         :file "/tmp/test.org"
                         :position 1))
        (final-data nil))

    ;; Create test node
    (supertag-store-put-entity :nodes test-node-id initial-data)

    ;; Simulate automation field update
    (supertag-automation-action-update-property
     test-node-id
     :property :status
     :value "completed")

    ;; Check result
    (setq final-data (supertag-node-get test-node-id))

    ;; Verify data integrity
    (let ((tags-preserved (equal (plist-get final-data :tags) '("Project" "Task")))
          (priority-preserved (equal (plist-get final-data :properties) '(:status "completed" :priority "high")))
          (title-preserved (equal (plist-get final-data :title) "Test Node")))

      (message "Test Results:")
      (message "  Tags preserved: %s" tags-preserved)
      (message "  Priority preserved: %s" priority-preserved)
      (message "  Title preserved: %s" title-preserved)
      (message "  Status updated: %s" (equal (plist-get (plist-get final-data :properties) :status) "completed"))

      ;; Cleanup
      (supertag-store-remove-entity :nodes test-node-id)

      (if (and tags-preserved priority-preserved title-preserved)
          (message "✓ All tests passed - automation data preservation is working!")
        (message "✗ Some tests failed - data loss still occurs")))))

(defun test-sync-data-preservation ()
  "Test that field sync preserves node data."
  (interactive)
  (let ((from-node-id "from-node-456")
        (to-node-id "to-node-789")
        (from-data '(:id "from-node-456"
                     :title "Source Node"
                     :tags ("Project")
                     :properties (:status "active"))
        (to-data '(:id "to-node-789"
                   :title "Target Node"
                   :tags ("Task" "Review")
                   :properties (:priority "low" :assigned-to "john"))))

    ;; Create test nodes
    (supertag-store-put-entity :nodes from-node-id from-data)
    (supertag-store-put-entity :nodes to-node-id to-data)

    ;; Simulate field sync
    (supertag-automation-sync-field from-node-id to-node-id "status" "completed" nil)

    ;; Check result
    (let ((final-data (supertag-node-get to-node-id)))

      ;; Verify data integrity
      (let ((tags-preserved (equal (plist-get final-data :tags) '("Task" "Review")))
            (priority-preserved (equal (plist-get (plist-get final-data :properties) :priority) "low"))
            (assigned-to-preserved (equal (plist-get (plist-get final-data :properties) :assigned-to) "john"))
            (status-synced (equal (plist-get (plist-get final-data :properties) :status) "completed")))

        (message "Sync Test Results:")
        (message "  Tags preserved: %s" tags-preserved)
        (message "  Priority preserved: %s" priority-preserved)
        (message "  Assigned-to preserved: %s" assigned-to-preserved)
        (message "  Status synced: %s" status-synced)

        ;; Cleanup
        (supertag-store-remove-entity :nodes from-node-id)
        (supertag-store-remove-entity :nodes to-node-id)

        (if (and tags-preserved priority-preserved assigned-to-preserved status-synced)
            (message "✓ Sync test passed - field sync preserves data!")
          (message "✗ Sync test failed - data loss during sync"))))))

(provide 'test-automation-fix)
;;; test-automation-fix.el ends here