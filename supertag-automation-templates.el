;;; supertag-automation-templates.el --- Ready-made rule templates for Automation 2.0 -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Automation System 2.0 (see `supertag-automation.el') is powerful but, in
;; its raw form, requires hand-writing an Elisp plist for every rule:
;; picking a valid :trigger, a condition s-expression, and one or more
;; :action specs from a vocabulary that only lives in the engine's source
;; code.  This file lowers that barrier by shipping:
;;
;;  1. `supertag-automation-templates' - a catalog of ready-made rule
;;     "shapes".  Each template knows which small set of values a user
;;     must supply (a tag name, a TODO state, ...) and how to turn those
;;     values into the exact arguments `supertag-automation-create'
;;     expects.
;;  2. `supertag-automation-insert-template' - an interactive command that
;;     lets the user pick a template, fills in the parameters with
;;     `completing-read'/`read-string' (using tag completion sourced from
;;     `supertag-query' when available), previews the resulting rule
;;     plist, and only calls `supertag-automation-create' after explicit
;;     confirmation.
;;  3. `supertag-automation-list-templates' - a read-only catalog browser.
;;
;; Every :trigger, condition operator, and :action keyword used below was
;; verified by reading `supertag-automation.el' directly (see the keyword
;; list in the Commentary of that file's own docstrings, in particular:
;;   - `supertag-automation--trigger-match-p'      (valid triggers)
;;   - `supertag-automation--eval-single-condition' (condition DSL)
;;   - `supertag-automation-execute-action'         (action DSL)
;; No keyword is invented here; anything the engine cannot express is
;; called out explicitly instead of being faked.
;;
;; Notable engine behavior this file relies on / works around:
;;  - `supertag-automation-create' derives a rule's storage :id from its
;;    :name, and SILENTLY REPLACES any existing rule with the same
;;    derived id (it deletes the old one first).  It never signals an
;;    error for a duplicate name.  Since silently clobbering an existing,
;;    possibly-unrelated rule would be a bad user experience, this file
;;    adds its own pre-flight check (`supertag-automation-get-by-name')
;;    and offers to rename before calling into the engine.
;;  - A :call-function action attached to an :on-schedule rule is invoked
;;    by the scheduler bridge as
;;    (apply FUNCTION node-id context ARGS) with NODE-ID nil (see
;;    `supertag-automation--register-scheduled-rule').  Scheduled rules
;;    can therefore only run :call-function actions; other action types
;;    listed in an :on-schedule rule's :actions are silently ignored by
;;    the runner.  The scheduled template below (`daily-set-property-for-tag')
;;    accounts for this by shipping its own small :call-function target
;;    that fans a property update out across every node with a given tag.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'supertag-automation)
(require 'supertag-core-scan)      ; For `supertag-index-get-nodes-by-tag'
(require 'supertag-services-query) ; For `supertag-query' (tag completion)

;;; --- Small helpers ---

(defun supertag-automation-templates--param (params key)
  "Return the value of KEY in the PARAMS alist collected from the user."
  (cdr (assq key params)))

(defun supertag-automation-templates--keywordize (name)
  "Turn NAME (a string, symbol, or keyword) into a keyword like :FOO."
  (cond
   ((keywordp name) name)
   ((symbolp name) (intern (concat ":" (symbol-name name))))
   ((stringp name) (intern (concat ":" (string-remove-prefix ":" (string-trim name)))))
   (t (error "supertag-automation-templates: cannot keywordize %S" name))))

(defun supertag-automation-templates--split-tags (raw)
  "Split RAW (a comma/space separated string) into a list of tag names."
  (split-string (or raw "") "[,\s]+" t "[ \t]+"))

;;; --- :call-function target used by the scheduled template ---
;;
;; Scheduled (:on-schedule) rules only ever run :call-function actions
;; (see Commentary above), so any "update a property/field on every node
;; with a tag, once a day" template must go through a real function
;; symbol rather than a plain :update-property action spec.

(defun supertag-automation-templates--scheduled-set-property (node-id context tag property value)
  "Set PROPERTY to VALUE on every node tagged TAG.
Intended to be used as the :function of a :call-function action on an
:on-schedule rule.  NODE-ID and CONTEXT are supplied by the scheduler
runner (see `supertag-automation--register-scheduled-rule') and are
unused here because scheduled rules are not scoped to a single node."
  (ignore node-id context)
  (dolist (nid (supertag-index-get-nodes-by-tag tag))
    (supertag-automation-action-update-property nid (list :property property :value value))))

;;; --- Template catalog ---
;;
;; Each template is a plist:
;;   :id          unique symbol
;;   :name        short human-readable name
;;   :description one sentence describing what the rule automates
;;   :params      list of (KEY PROMPT TYPE)
;;                  KEY    - symbol used to look the value up in the alist
;;                           passed to :build
;;                  PROMPT - string shown to the user
;;                  TYPE   - one of `tag', `todo-state', `property',
;;                           `value', `string', `file' (drives which
;;                           reader `supertag-automation-insert-template'
;;                           uses)
;;   :build       function: (alist-of (KEY . VALUE)) -> plist suitable
;;                for `supertag-automation-create'

(defconst supertag-automation-templates
  (list
   ;; 1. Tag added -> set TODO state ------------------------------------
   (list
    :id :tag-added-set-todo-state
    :name "Tag added -> set TODO state"
    :description "When a tag is added to a node, set its TODO keyword (e.g. adding #done sets TODO state to DONE)."
    :params '((tag "Tag that triggers the rule (e.g. done)" tag)
              (state "TODO keyword to apply (e.g. DONE)" todo-state))
    :build (lambda (params)
             (let* ((tag (supertag-automation-templates--param params 'tag))
                    (state (supertag-automation-templates--param params 'state)))
               (list :name (format "Tag '%s' sets TODO %s" tag state)
                     :description (format "Adding tag '%s' sets the TODO state to %s." tag state)
                     :trigger (list :on-tag-added tag)
                     :actions (list (list :action :update-todo-state
                                          :params (list :state state)))))))

   ;; 2. Tag added -> set a property -------------------------------------
   (list
    :id :tag-added-set-property
    :name "Tag added -> set a property"
    :description "When a tag is added to a node, set an Org property to a fixed value (e.g. adding #urgent sets PRIORITY to A)."
    :params '((tag "Tag that triggers the rule (e.g. urgent)" tag)
              (property "Property name to set (e.g. PRIORITY)" property)
              (value "Value to assign (e.g. A)" value))
    :build (lambda (params)
             (let* ((tag (supertag-automation-templates--param params 'tag))
                    (property (supertag-automation-templates--param params 'property))
                    (value (supertag-automation-templates--param params 'value))
                    (prop-kw (supertag-automation-templates--keywordize property)))
               (list :name (format "Tag '%s' sets %s" tag property)
                     :description (format "Adding tag '%s' sets property %s to %s." tag property value)
                     :trigger (list :on-tag-added tag)
                     :actions (list (list :action :update-property
                                          :params (list :property prop-kw :value value)))))))

   ;; 3. Tag added -> add another tag (implication) ----------------------
   (list
    :id :tag-added-add-tag
    :name "Tag added -> add another tag (implication)"
    :description "When a tag is added, automatically add a second tag too (e.g. #bug implies #needs-triage)."
    :params '((trigger-tag "Tag that triggers the rule (e.g. bug)" tag)
              (implied-tag "Tag to add automatically (e.g. needs-triage)" tag))
    :build (lambda (params)
             (let* ((trigger-tag (supertag-automation-templates--param params 'trigger-tag))
                    (implied-tag (supertag-automation-templates--param params 'implied-tag)))
               (list :name (format "Tag '%s' implies '%s'" trigger-tag implied-tag)
                     :description (format "Adding tag '%s' automatically adds tag '%s'." trigger-tag implied-tag)
                     :trigger (list :on-tag-added trigger-tag)
                     :actions (list (list :action :add-tag
                                          :params (list :tag implied-tag)))))))

   ;; 4. Tag removed -> remove a derived tag -----------------------------
   (list
    :id :tag-removed-remove-tag
    :name "Tag removed -> remove a derived tag"
    :description "When a tag is removed from a node, remove a second, dependent tag as well (e.g. removing #active also removes #on-hold)."
    :params '((trigger-tag "Tag whose removal triggers the rule (e.g. active)" tag)
              (derived-tag "Tag to remove along with it (e.g. on-hold)" tag))
    :build (lambda (params)
             (let* ((trigger-tag (supertag-automation-templates--param params 'trigger-tag))
                    (derived-tag (supertag-automation-templates--param params 'derived-tag)))
               (list :name (format "Removing '%s' removes '%s'" trigger-tag derived-tag)
                     :description (format "Removing tag '%s' automatically removes tag '%s'." trigger-tag derived-tag)
                     :trigger (list :on-tag-removed trigger-tag)
                     :actions (list (list :action :remove-tag
                                          :params (list :tag derived-tag)))))))

   ;; 5. Field change -> update a tag field on the same node -------------
   (list
    :id :field-change-update-field
    :name "Field change -> update another field"
    :description "Scoped to a tag: whenever one field on a node changes, set another field on that node to a fixed value."
    :params '((scope-tag "Only run for nodes with this tag (e.g. task)" tag)
              (source-field "Field whose change triggers the rule (e.g. status)" string)
              (target-tag "Tag ID that defines the field to update (usually same as scope tag)" tag)
              (target-field "Field to update (e.g. last-touched)" string)
              (target-value "Value to set on the target field" value))
    :build (lambda (params)
             (let* ((scope-tag (supertag-automation-templates--param params 'scope-tag))
                    (source-field (supertag-automation-templates--param params 'source-field))
                    (target-tag (supertag-automation-templates--param params 'target-tag))
                    (target-field (supertag-automation-templates--param params 'target-field))
                    (target-value (supertag-automation-templates--param params 'target-value)))
               (list :name (format "%s/%s change updates %s" scope-tag source-field target-field)
                     :description (format "On nodes tagged '%s', a change to field '%s' sets field '%s' to %s."
                                          scope-tag source-field target-field target-value)
                     :trigger :on-field-change
                     :condition (list 'and
                                      (list 'has-tag scope-tag)
                                      (list 'field-changed source-field))
                     :actions (list (list :action :update-field
                                          :params (list :tag target-tag
                                                        :field target-field
                                                        :value target-value)))))))

   ;; 6. Field equals value -> move node to an archive file --------------
   (list
    :id :field-equals-move-node
    :name "Field equals value -> move node to file"
    :description "Whenever a field is set to a specific value, move the node into an archive (or any target) file."
    :params '((scope-tag "Only run for nodes with this tag (leave blank for any node)" tag)
              (field "Field to test (e.g. status)" string)
              (value "Value that triggers the move (e.g. archived)" value)
              (target-file "Absolute path of the file to move matching nodes into" file))
    :build (lambda (params)
             (let* ((scope-tag (string-trim (or (supertag-automation-templates--param params 'scope-tag) "")))
                    (field (supertag-automation-templates--param params 'field))
                    (value (supertag-automation-templates--param params 'value))
                    (target-file (supertag-automation-templates--param params 'target-file))
                    (field-cond (list 'field-equals field value))
                    (condition (if (string-empty-p scope-tag)
                                   field-cond
                                 (list 'and (list 'has-tag scope-tag) field-cond))))
               (list :name (format "%s=%s moves to %s" field value (file-name-nondirectory target-file))
                     :description (format "When field '%s' equals %s, move the node into %s."
                                          field value target-file)
                     :trigger :on-field-change
                     :condition condition
                     :actions (list (list :action :move-node
                                          :params (list :target-file target-file)))))))

   ;; 7. Property equals value -> add a tag -------------------------------
   (list
    :id :property-equals-add-tag
    :name "Property equals value -> add tag"
    :description "Whenever a node property equals a specific value, automatically add a tag (e.g. priority = A adds #urgent)."
    :params '((property "Property to test, without colon (e.g. priority)" property)
              (value "Value that triggers the tag add (e.g. A)" value)
              (tag "Tag to add when the condition matches" tag))
    :build (lambda (params)
             (let* ((property (supertag-automation-templates--param params 'property))
                    (value (supertag-automation-templates--param params 'value))
                    (tag (supertag-automation-templates--param params 'tag))
                    (prop-kw (supertag-automation-templates--keywordize property)))
               (list :name (format "%s=%s adds '%s'" property value tag)
                     :description (format "When property %s equals %s, add tag '%s'." property value tag)
                     :trigger :on-property-change
                     :condition (list 'property-equals prop-kw value)
                     :actions (list (list :action :add-tag
                                          :params (list :tag tag)))))))

   ;; 8. Scheduled daily rule -> update a property on all tagged nodes ---
   (list
    :id :daily-set-property-for-tag
    :name "Scheduled daily -> set property on tagged nodes"
    :description "Once a day, set a property to a fixed value on every node that carries a given tag."
    :params '((tag "Nodes with this tag get updated every day (e.g. weekly-review)" tag)
              (property "Property to set (e.g. LAST-REVIEWED)" property)
              (value "Value to assign" value)
              (time "Time of day to run, 24h HH:MM (e.g. 06:00)" string))
    :build (lambda (params)
             (let* ((tag (supertag-automation-templates--param params 'tag))
                    (property (supertag-automation-templates--param params 'property))
                    (value (supertag-automation-templates--param params 'value))
                    (time (supertag-automation-templates--param params 'time))
                    (prop-kw (supertag-automation-templates--keywordize property)))
               (list :name (format "Daily %s: set %s on '%s' nodes" time property tag)
                     :description (format "Every day at %s, set property %s to %s on every node tagged '%s'."
                                          time property value tag)
                     :trigger :on-schedule
                     :schedule (list :time time)
                     :actions (list (list :action :call-function
                                          :params (list :function #'supertag-automation-templates--scheduled-set-property
                                                        :args (list tag prop-kw value))))))))

   ;; 9. Tag added -> create a follow-up node -----------------------------
   (list
    :id :tag-added-create-followup-node
    :name "Tag added -> create follow-up node"
    :description "When a tag is added, create a brand-new node with a given title and tags (the engine does not support linking it back to the source node)."
    :params '((tag "Tag that triggers creation of a follow-up node (e.g. needs-followup)" tag)
              (title "Title for the new node" string)
              (followup-tags "Comma-separated tags to apply to the new node (e.g. task, followup)" string))
    :build (lambda (params)
             (let* ((tag (supertag-automation-templates--param params 'tag))
                    (title (supertag-automation-templates--param params 'title))
                    (followup-tags (supertag-automation-templates--split-tags
                                    (supertag-automation-templates--param params 'followup-tags))))
               (list :name (format "Tag '%s' creates follow-up" tag)
                     :description (format "Adding tag '%s' creates a new node titled '%s'." tag title)
                     :trigger (list :on-tag-added tag)
                     :actions (list (list :action :create-node
                                          :params (list :title title
                                                        :tags followup-tags))))))))
  "Catalog of ready-made Automation 2.0 rule templates.

Every :trigger/condition/action keyword used by these templates was
verified against `supertag-automation.el'.  One idea from the original
brief was intentionally dropped:

  - A `:case' (multi-branch) action template.  `:case' branches need a
    nested :on spec plus a list of :equals/:in/:match/:test branches,
    each with its own :actions - that is a small program, not a handful
    of scalar parameters, so it does not fit this template's
    (key prompt type) shape.  Users who need branching logic are
    expected to compose it by hand with `supertag-automation-create',
    using the templates above as worked examples of the surrounding
    plist shape.

The `:manual' trigger was also left out: it never fires automatically
\(`supertag-automation--trigger-match-p' always returns nil for it\), so
a rule built on it would need a bespoke call to `supertag-rule-execute'
with no UI entry point provided elsewhere in the codebase to invoke it.")

;;; --- Interactive instantiation ---

(defun supertag-automation-templates--all-tag-names ()
  "Return known tag names for completion, or nil if unavailable."
  (ignore-errors (mapcar #'car (supertag-query :tags))))

(defun supertag-automation-templates--read-tag (prompt)
  "Read a tag name for PROMPT, offering existing tags as completion."
  (string-trim
   (completing-read (concat prompt ": ")
                     (supertag-automation-templates--all-tag-names)
                     nil nil)))

(defun supertag-automation-templates--read-todo-state (prompt)
  "Read a TODO keyword for PROMPT, offering `org-todo-keywords-1' when known."
  (let ((states (if (and (boundp 'org-todo-keywords-1) org-todo-keywords-1)
                     org-todo-keywords-1
                   '("TODO" "NEXT" "DONE" "CANCELLED"))))
    (string-trim (completing-read (concat prompt ": ") states nil nil))))

(defun supertag-automation-templates--read-param (prompt type)
  "Read one parameter value for PROMPT according to TYPE."
  (pcase type
    ('tag (supertag-automation-templates--read-tag prompt))
    ('todo-state (supertag-automation-templates--read-todo-state prompt))
    ('file (expand-file-name (read-file-name (concat prompt ": "))))
    ((or 'property 'value 'string _) (string-trim (read-string (concat prompt ": "))))))

(defun supertag-automation-templates--collect-params (template)
  "Prompt the user for every entry in TEMPLATE's :params.
Returns an alist of (KEY . VALUE)."
  (mapcar (lambda (spec)
            (cl-destructuring-bind (key prompt type) spec
              (cons key (supertag-automation-templates--read-param prompt type))))
          (plist-get template :params)))

(defun supertag-automation-templates--choose ()
  "Prompt the user to choose a template, annotated with its description.
Returns the chosen template plist."
  (let* ((candidates (mapcar (lambda (tpl) (cons (plist-get tpl :name) tpl))
                             supertag-automation-templates))
         (completion-extra-properties
          (list :annotation-function
                (lambda (name)
                  (let ((tpl (cdr (assoc name candidates))))
                    (when tpl (concat "  -- " (plist-get tpl :description)))))))
         (choice (completing-read "Automation template: " candidates nil t)))
    (or (cdr (assoc choice candidates))
        (user-error "Unknown automation template: %s" choice))))

(defun supertag-automation-templates--preview-buffer (template rule-args)
  "Render RULE-ARGS (built from TEMPLATE) into a preview buffer and return it."
  (let ((buf (get-buffer-create "*Supertag Automation Preview*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (format ";; Template: %s\n" (plist-get template :name)))
        (insert (format ";; %s\n\n" (plist-get template :description)))
        (insert ";; Arguments that will be passed to `supertag-automation-create':\n")
        (pp rule-args (current-buffer)))
      (emacs-lisp-mode)
      (setq buffer-read-only t)
      (goto-char (point-min)))
    buf))

(defun supertag-automation-templates--create-with-retry (rule-args)
  "Create the automation described by RULE-ARGS, renaming on name clash.

`supertag-automation-create' derives a rule's storage id from its
:name and silently REPLACES any existing rule sharing that id -- it
never signals a duplicate-name error.  To avoid surprising the user by
clobbering an unrelated existing rule, check `supertag-automation-get-by-name'
first and offer to pick a different name."
  (let ((name (plist-get rule-args :name)))
    (while (and name
                (supertag-automation-get-by-name name)
                (not (y-or-n-p
                      (format "An automation named '%s' already exists and would be REPLACED. Continue? "
                              name))))
      (setq name (read-string "New automation name: " name))
      (setq rule-args (plist-put rule-args :name name)))
    (condition-case err
        (let ((created (supertag-automation-create rule-args)))
          (message "Created automation '%s' (id=%s)."
                   (plist-get created :name) (plist-get created :id))
          created)
      (error
       (message "Failed to create automation: %s" (error-message-string err))
       nil))))

;;;###autoload
(defun supertag-automation-insert-template ()
  "Instantiate one of `supertag-automation-templates' interactively.

Prompts for a template, then for each of its declared parameters,
previews the resulting rule plist in a temporary buffer, and only
calls `supertag-automation-create' after explicit confirmation."
  (interactive)
  (let* ((template (supertag-automation-templates--choose))
         (params (supertag-automation-templates--collect-params template))
         (rule-args (funcall (plist-get template :build) params))
         (buf (supertag-automation-templates--preview-buffer template rule-args)))
    (display-buffer buf)
    (if (y-or-n-p (format "Create automation '%s'? " (plist-get rule-args :name)))
        (supertag-automation-templates--create-with-retry rule-args)
      (message "Automation creation cancelled."))))

;;;###autoload
(defun supertag-automation-list-templates ()
  "Browse the `supertag-automation-templates' catalog in a read-only buffer."
  (interactive)
  (let ((buf (get-buffer-create "*Supertag Automation Templates*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (dolist (tpl supertag-automation-templates)
          (insert (format "%s  [%s]\n" (plist-get tpl :name) (plist-get tpl :id)))
          (insert (format "    %s\n" (plist-get tpl :description)))
          (insert (format "    Params: %s\n\n"
                          (mapconcat (lambda (p) (symbol-name (car p)))
                                     (plist-get tpl :params) ", ")))))
      (goto-char (point-min))
      (special-mode))
    (display-buffer buf)))

(provide 'supertag-automation-templates)

;;; supertag-automation-templates.el ends here
