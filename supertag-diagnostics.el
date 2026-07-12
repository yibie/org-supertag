;;; supertag-diagnostics.el --- Interactive diagnostic helpers for Org-Supertag -*- lexical-binding: t; -*-

;;; Commentary:
;; Interactive, read-only diagnostic utilities for inspecting node logic,
;; previewing automation rule matches, and running ad-hoc query-engine
;; lookups.  These are developer/debugging aids, not automated tests
;; (no `ert-deftest' forms live here).
;; Load manually when needed:
;;   (load-file "supertag-diagnostics.el")
;; Optional verbosity:
;;   (setq supertag-automation-verbose t
;;         supertag-sync-verbose t)
;;   M-x supertag-diagnostics-explain-current-node
;;   M-x supertag-diagnostics-automation-dry-run
;;   M-x supertag-diagnostics-view-query-sexp

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'supertag-core-store)
(require 'supertag-ops-node)

(defgroup supertag-diagnostics nil
  "Experimental helpers for Org-Supertag."
  :group 'org-supertag)

(defcustom supertag-diagnostics-ref-stale-days 7
  "Days after which a `ref' node is considered stale if not done."
  :type 'integer
  :group 'supertag-diagnostics)

(defconst supertag-diagnostics--event-choices
  '((:label "node-change" :type :node-change)
    (:label "property-change" :type :property-change)
    (:label "field-change (legacy)" :type :field-change)
    (:label "global-field-change" :type :global-field-change)
    (:label "tag-added" :type :tag-change :op :added)
    (:label "tag-removed" :type :tag-change :op :removed))
  "Interactive event choices for dry-run previews.")

(defun supertag-diagnostics--ensure-automation ()
  "Load automation modules used by the experimental helpers."
  (require 'supertag-automation)
  (require 'supertag-automation-sync))

(defun supertag-diagnostics--hash-keys (table)
  "Return hash TABLE keys as a list."
  (let (keys)
    (when (hash-table-p table)
      (maphash (lambda (key _value) (push key keys)) table))
    (nreverse keys)))

(defun supertag-diagnostics--list-node-ids ()
  "Return all node IDs from the store."
  (supertag-diagnostics--hash-keys (supertag-store-get-collection :nodes)))

(defun supertag-diagnostics--node-id-at-point ()
  "Return the Org ID at point when available."
  (when (derived-mode-p 'org-mode)
    (require 'org-id)
    (org-id-get nil nil)))

(defun supertag-diagnostics--read-node-id (&optional prompt)
  "Read a node ID, preferring the ID at point."
  (or (supertag-diagnostics--node-id-at-point)
      (let ((ids (supertag-diagnostics--list-node-ids)))
        (unless ids
          (error "No nodes found in store"))
        (completing-read (or prompt "Node ID: ") ids nil t))))

(defun supertag-diagnostics--normalize-property-key (input)
  "Normalize INPUT into a keyword property key."
  (let* ((trimmed (string-trim (or input ""))))
    (if (string-prefix-p ":" trimmed)
        (intern trimmed)
      (intern (concat ":" trimmed)))))

(defun supertag-diagnostics--time-before-now-p (time-value)
  "Return non-nil when TIME-VALUE is a timestamp before now."
  (and (listp time-value)
       (time-less-p time-value (current-time))))

(defun supertag-diagnostics--todo-done-p (todo)
  "Return non-nil when TODO is a completed state."
  (and (stringp todo)
       (member (upcase todo) '("DONE" "CANCELLED" "CANCELED" "ARCHIVE"))))

(defun supertag-diagnostics--node-has-tag-p (node tag)
  "Return non-nil when NODE has TAG."
  (and (stringp tag)
       (member tag (plist-get node :tags))))

(defun supertag-diagnostics--days-since (time-value)
  "Return integer days since TIME-VALUE, or nil when TIME-VALUE invalid."
  (when (and (listp time-value) (= (length time-value) 4))
    (floor (/ (float-time (time-subtract (current-time) time-value)) 86400.0))))

(defun supertag-diagnostics--node-overdue-p (node)
  "Return non-nil when NODE is overdue based on :deadline and :todo."
  (let ((deadline (plist-get node :deadline))
        (todo (plist-get node :todo)))
    (and deadline
         (supertag-diagnostics--time-before-now-p deadline)
         (not (supertag-diagnostics--todo-done-p todo)))))

(defun supertag-diagnostics--ref-stale-p (node)
  "Return non-nil when NODE (tagged ref) is stale and not done."
  (when (supertag-diagnostics--node-has-tag-p node "ref")
    (let* ((todo (plist-get node :todo))
           (created-at (or (plist-get node :created-at)
                           (plist-get node :modified-at)))
           (days (supertag-diagnostics--days-since created-at)))
      (and (not (supertag-diagnostics--todo-done-p todo))
           (integerp days)
           (>= days supertag-diagnostics-ref-stale-days)))))

(defvar supertag-diagnostics-logic-rules
  (list
   (list :id :is-ref
         :title "Reference"
         :predicate (lambda (node) (supertag-diagnostics--node-has-tag-p node "ref"))
         :explain "node tags include ref")
   (list :id :overdue
         :title "Overdue"
         :predicate #'supertag-diagnostics--node-overdue-p
         :explain "deadline earlier than now and TODO not done")
   (list :id :stale-ref
         :title "Stale reference"
         :predicate #'supertag-diagnostics--ref-stale-p
         :explain "tag ref, not done, and older than N days")
   (list :id :has-deadline
         :title "Has deadline"
         :predicate (lambda (node) (plist-get node :deadline))
         :explain "node has a deadline timestamp"))
  "Experimental logic rules for read-only derivations.")

(defun supertag-diagnostics--evaluate-logic (node)
  "Evaluate `supertag-diagnostics-logic-rules' against NODE."
  (let (results)
    (dolist (rule supertag-diagnostics-logic-rules (nreverse results))
      (let ((pred (plist-get rule :predicate)))
        (when (functionp pred)
          (condition-case err
              (when (funcall pred node)
                (push rule results))
            (error
             (message "supertag-diagnostics: logic rule %s failed: %s"
                      (plist-get rule :id) err))))))))

(defun supertag-diagnostics--event-description (event)
  "Return a readable description for EVENT."
  (pcase (plist-get event :type)
    (:node-change "node-change")
    (:property-change (format "property-change %s" (plist-get event :property)))
    (:field-change (format "field-change %s (tag %s)"
                           (plist-get event :field)
                           (plist-get event :tag)))
    (:global-field-change (format "global-field-change %s" (plist-get event :field)))
    (:tag-change (format "tag-%s %s"
                         (if (eq (plist-get event :op) :added) "added" "removed")
                         (plist-get event :tag)))
    (_ "custom-event")))

(defun supertag-diagnostics--read-event (_node-id)
  "Read an event descriptor for dry-run evaluation."
  (let* ((labels (mapcar (lambda (item) (plist-get item :label))
                         supertag-diagnostics--event-choices))
         (choice (completing-read "Event: " labels nil t))
         (spec (cl-find choice supertag-diagnostics--event-choices
                        :key (lambda (item) (plist-get item :label))
                        :test #'string=))
         (type (plist-get spec :type)))
    (pcase type
      (:tag-change
       (list :type :tag-change
             :op (plist-get spec :op)
             :tag (read-string "Tag (id/name): ")))
      (:field-change
       (list :type :field-change
             :field (read-string "Field name: ")
             :tag (read-string "Tag id (legacy storage): ")))
      (:global-field-change
       (list :type :global-field-change
             :field (read-string "Global field id: ")))
      (:property-change
       (list :type :property-change
             :property (supertag-diagnostics--normalize-property-key
                        (read-string "Property (without :): "))))
      (_ (list :type type)))))

(defun supertag-diagnostics--event-context (node-id event)
  "Build a `supertag-automation--current-event' context for EVENT."
  (pcase (plist-get event :type)
    (:node-change
     (list :path (list :nodes node-id)))
    (:property-change
     (list :path (list :nodes node-id :properties (plist-get event :property))))
    (:field-change
     (list :path (list :fields node-id (plist-get event :tag) (plist-get event :field))))
    (:global-field-change
     (list :path (list :field-values node-id (plist-get event :field))))
    (:tag-change
     (list :tag-event (plist-get event :op) :tag (plist-get event :tag)))
    (_ nil)))

(defun supertag-diagnostics--normalize-trigger (trigger)
  "Normalize automation TRIGGER when possible."
  (if (fboundp 'supertag-automation--normalize-trigger)
      (supertag-automation--normalize-trigger trigger)
    trigger))

(defun supertag-diagnostics--trigger-match-p (trigger event)
  "Return non-nil when TRIGGER matches EVENT."
  (let* ((event-type (plist-get event :type))
         (op (plist-get event :op))
         (tag (plist-get event :tag)))
  (pcase trigger
      (`(:on-tag-added ,tag-name)
       (and (eq event-type :tag-change)
            (eq op :added)
            (equal tag-name tag)))
      (`(:on-tag-removed ,tag-name)
       (and (eq event-type :tag-change)
            (eq op :removed)
            (equal tag-name tag)))
      (:manual nil)
      (:on-schedule nil)
      (:always t)
      (:on-change (memq event-type '(:node-change :property-change :field-change
                                                  :global-field-change :tag-change)))
      (:on-property-change (memq event-type '(:property-change :field-change
                                                               :global-field-change)))
      (:on-field-change (memq event-type '(:field-change :global-field-change)))
      (_ nil))))

(defun supertag-diagnostics--automation-dry-run (node-id event)
  "Return automation evaluation results for NODE-ID and EVENT."
  (supertag-diagnostics--ensure-automation)
  (let* ((rules (supertag-automation-list))
         (context (supertag-diagnostics--event-context node-id event))
         results)
    (dolist (rule rules (nreverse results))
      (let* ((enabled (plist-get rule :enabled))
             (trigger (supertag-diagnostics--normalize-trigger (plist-get rule :trigger)))
             (trigger-match (supertag-diagnostics--trigger-match-p trigger event))
             (condition (plist-get rule :condition))
             (condition-pass nil)
             (condition-error nil))
        (when enabled
          (condition-case err
              (let ((supertag-automation--current-event context))
                (setq condition-pass
                      (supertag-automation--evaluate-condition condition node-id)))
            (error
             (setq condition-error err))))
        (push (list :rule rule
                    :enabled enabled
                    :trigger trigger
                    :trigger-match trigger-match
                    :condition-pass condition-pass
                    :condition-error condition-error
                    :would-run (and enabled trigger-match condition-pass))
              results)))))

(defun supertag-diagnostics--format-actions (actions)
  "Return a concise string for ACTIONS."
  (if (listp actions)
      (string-join
       (delq nil
             (mapcar (lambda (action)
                       (when (and (plistp action)
                                  (plist-get action :action))
                         (symbol-name (plist-get action :action))))
                     actions))
       ", ")
    ""))

(defun supertag-diagnostics--logic-rule-choices ()
  "Return completing choices for logic rules."
  (mapcar (lambda (rule)
            (format "%s - %s"
                    (plist-get rule :id)
                    (plist-get rule :title)))
          supertag-diagnostics-logic-rules))

(defun supertag-diagnostics--find-logic-rule (choice)
  "Return a logic rule plist from CHOICE string."
  (let* ((id-str (car (split-string choice " " t)))
         (id (and id-str (intern id-str))))
    (cl-find id supertag-diagnostics-logic-rules
             :key (lambda (rule) (plist-get rule :id))
             :test #'eq)))

(defun supertag-diagnostics-view-logic (rule-choice)
  "Show a simple list view of nodes matching RULE-CHOICE.
RULE-CHOICE is chosen interactively from `supertag-diagnostics-logic-rules'."
  (interactive
   (list (completing-read "Logic view: " (supertag-diagnostics--logic-rule-choices) nil t)))
  (let* ((rule (supertag-diagnostics--find-logic-rule rule-choice))
         (pred (plist-get rule :predicate)))
    (unless (and rule (functionp pred))
      (error "Invalid logic rule: %s" rule-choice))
    (let* ((nodes (supertag-store-get-collection :nodes))
           (items '()))
      (maphash
       (lambda (node-id node)
         (when (and (plistp node)
                    (condition-case nil
                        (funcall pred node)
                      (error nil)))
           (push (list :id node-id
                       :title (or (plist-get node :title) "")
                       :tags (or (plist-get node :tags) '()))
                 items)))
       nodes)
      (setq items
            (sort items (lambda (a b)
                          (string< (plist-get a :title) (plist-get b :title)))))
      (let ((buf (get-buffer-create "*Supertag Logic View*")))
        (with-current-buffer buf
          (setq buffer-read-only nil)
          (erase-buffer)
          (insert (format "Logic view: %s (%s)\n"
                          (plist-get rule :title)
                          (plist-get rule :id)))
          (insert (format "Explain: %s\n" (or (plist-get rule :explain) "")))
          (insert (format "Matches: %d\n\n" (length items)))
          (if (null items)
              (insert "<none>\n")
            (dolist (item items)
              (insert (format "- %s\n  id: %s\n  tags: %s\n"
                              (plist-get item :title)
                              (plist-get item :id)
                              (if (plist-get item :tags)
                                  (string-join (plist-get item :tags) ", ")
                                "<none>")))))
          (setq buffer-read-only t)
          (goto-char (point-min)))
        (pop-to-buffer buf)))))

(defun supertag-diagnostics--insert-automation-results (event results show-all)
  "Insert dry-run RESULT details for EVENT."
  (let* ((run-items (cl-remove-if-not (lambda (item) (plist-get item :would-run)) results))
         (disabled (cl-count-if (lambda (item) (not (plist-get item :enabled))) results))
         (trigger-miss (cl-count-if (lambda (item)
                                      (and (plist-get item :enabled)
                                           (not (plist-get item :trigger-match))))
                                    results))
         (condition-fail (cl-count-if (lambda (item)
                                        (and (plist-get item :enabled)
                                             (plist-get item :trigger-match)
                                             (not (plist-get item :condition-pass))
                                             (not (plist-get item :condition-error))))
                                      results))
         (condition-error (cl-count-if (lambda (item) (plist-get item :condition-error)) results)))
    (insert (format "Automation dry-run (%s)\n"
                    (supertag-diagnostics--event-description event)))
    (insert (format "Summary: %d would run, %d disabled, %d trigger-miss, %d condition-fail, %d errors\n\n"
                    (length run-items) disabled trigger-miss condition-fail condition-error))
    (if (null run-items)
        (insert "Would run: <none>\n\n")
      (insert "Would run:\n")
      (dolist (item run-items)
        (let* ((rule (plist-get item :rule))
               (name (plist-get rule :name))
               (trigger (plist-get item :trigger))
               (actions (supertag-diagnostics--format-actions (plist-get rule :actions))))
          (insert (format "- %s (trigger: %s; actions: %s)\n"
                          name trigger (if (string-empty-p actions) "<none>" actions)))))
      (insert "\n"))
    (when show-all
      (insert "All rules:\n")
      (dolist (item results)
        (let* ((rule (plist-get item :rule))
               (name (plist-get rule :name))
               (reason
                (cond
                 ((not (plist-get item :enabled)) "disabled")
                 ((not (plist-get item :trigger-match)) "trigger-miss")
                 ((plist-get item :condition-error) "condition-error")
                 ((not (plist-get item :condition-pass)) "condition-fail")
                 (t "would-run"))))
          (insert (format "- %s (%s)\n" name reason))))
      (insert "\n"))))

(defun supertag-diagnostics-automation-dry-run (node-id event &optional show-all)
  "Preview automation matches for NODE-ID and EVENT.
With SHOW-ALL (prefix arg), include all rules with reasons."
  (interactive (let* ((id (supertag-diagnostics--read-node-id))
                      (ev (supertag-diagnostics--read-event id)))
                 (list id ev current-prefix-arg)))
  (let* ((node (supertag-node-get node-id)))
    (unless node
      (error "Node not found: %s" node-id))
    (let* ((results (supertag-diagnostics--automation-dry-run node-id event))
           (buf (get-buffer-create "*Supertag Automation Dry Run*")))
      (with-current-buffer buf
        (setq buffer-read-only nil)
        (erase-buffer)
        (insert (format "Node: %s\nTitle: %s\n\n"
                        node-id (or (plist-get node :title) "")))
        (supertag-diagnostics--insert-automation-results event results show-all)
        (setq buffer-read-only t)
        (goto-char (point-min)))
      (pop-to-buffer buf))))

(defun supertag-diagnostics-explain-current-node (node-id &optional event)
  "Explain derived logic and automation preview for NODE-ID."
  (interactive
   (let* ((id (supertag-diagnostics--read-node-id))
          (ev (when current-prefix-arg (supertag-diagnostics--read-event id))))
     (list id ev)))
  (let* ((node (supertag-node-get node-id)))
    (unless node
      (error "Node not found: %s" node-id))
    (let* ((logic-results (supertag-diagnostics--evaluate-logic node))
           (event (or event (list :type :node-change)))
           (automation-results (supertag-diagnostics--automation-dry-run node-id event))
           (buf (get-buffer-create "*Supertag Explain*")))
      (with-current-buffer buf
        (setq buffer-read-only nil)
        (erase-buffer)
        (insert (format "Node: %s\nTitle: %s\nTags: %s\nTodo: %s\n\n"
                        node-id
                        (or (plist-get node :title) "")
                        (if (plist-get node :tags)
                            (string-join (plist-get node :tags) ", ")
                          "<none>")
                        (or (plist-get node :todo) "")))
        (insert "Derived facts (logic):\n")
        (if (null logic-results)
            (insert "- <none>\n\n")
          (dolist (rule logic-results)
            (insert (format "- %s: %s\n"
                            (plist-get rule :title)
                            (plist-get rule :explain))))
          (insert "\n"))
        (supertag-diagnostics--insert-automation-results event automation-results nil)
        (setq buffer-read-only t)
        (goto-char (point-min)))
      (pop-to-buffer buf))))

(defun supertag-diagnostics-view-query-sexp (query-sexp)
  "Show a simple list view for QUERY-SEXP (supertag query engine)."
  (interactive
   (let* ((raw (read-from-minibuffer "Query sexp (e.g. (and (tag \"ref\") (term \"ai\"))): ")))
     (list (read raw))))
  (require 'supertag-services-query)
  (require 'supertag-view-api)
  (let* ((node-ids (supertag-query-sexp query-sexp))
         (nodes (supertag-view-api-get-entities :nodes node-ids))
         (buf (get-buffer-create "*Supertag Query (Test)*")))
    (with-current-buffer buf
      (setq buffer-read-only nil)
      (erase-buffer)
      (insert (format "Query: %S\nMatches: %d\n\n" query-sexp (length nodes)))
      (if (null nodes)
          (insert "<none>\n")
        (dolist (node nodes)
          (let ((id (plist-get node :id))
                (title (or (plist-get node :title) ""))
                (tags (plist-get node :tags)))
            (insert (format "- %s\n  id: %s\n  tags: %s\n"
                            title
                            id
                            (if tags (string-join tags ", ") "<none>"))))))
      (setq buffer-read-only t)
      (goto-char (point-min)))
    (pop-to-buffer buf)))

(provide 'supertag-diagnostics)

;;; supertag-diagnostics.el ends here
