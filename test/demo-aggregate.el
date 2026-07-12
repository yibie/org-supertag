;;; demo-aggregate.el --- Aggregate virtual column demo -*- lexical-binding: t; -*-

;;; Commentary:

;; Interactive demo of the aggregate virtual column type
;; Usage: M-x supertag-demo-aggregate

;;; Code:

(require 'supertag-virtual-column)

(defun supertag-demo-aggregate ()
  "Interactive demo of aggregate virtual columns."
  (interactive)
  (switch-to-buffer "*Aggregate Demo*")
  (erase-buffer)
  (org-mode)
  
  (insert "# Aggregate Virtual Column Demo\n\n")
  
  (insert "## What is Aggregate?\n\n")
  (insert "Aggregate virtual columns compute values across *all* nodes with a specific tag.\n")
  (insert "Unlike Rollup (which aggregates related nodes), Aggregate looks at the entire database.\n\n")
  
  (insert "## Use Cases\n\n")
  (insert "- **Total Project Budget**: Sum of budget across all #project nodes\n")
  (insert "- **Task Count**: Count of all #task nodes\n")
  (insert "- **Average Completion**: Average progress across all projects\n")
  (insert "- **Maximum Priority**: Highest priority among all tasks\n\n")
  
  ;; Initialize and create columns
  (supertag-virtual-column-init)
  
  (insert "## Creating Aggregate Columns\n\n")
  (insert "#+begin_src elisp\n")
  (insert ";; Total effort across all projects\n")
  (insert "(supertag-virtual-column-create\n")
  (insert " (list :id \"total-project-effort\"\n")
  (insert "       :name \"Total Project Effort\"\n")
  (insert "       :type :aggregate\n")
  (insert "       :params (list :tag \"project\"\n")
  (insert "                     :field \"effort\"\n")
  (insert "                     :function :sum)))\n\n")
  
  (insert ";; Count of all projects\n")
  (insert "(supertag-virtual-column-create\n")
  (insert " (list :id \"project-count\"\n")
  (insert "       :name \"Project Count\"\n")
  (insert "       :type :aggregate\n")
  (insert "       :params (list :tag \"project\"\n")
  (insert "                     :field \"effort\"\n")
  (insert "                     :function :count)))\n\n")
  
  (insert ";; Average effort per project\n")
  (insert "(supertag-virtual-column-create\n")
  (insert " (list :id \"avg-project-effort\"\n")
  (insert "       :name \"Average Project Effort\"\n")
  (insert "       :type :aggregate\n")
  (insert "       :params (list :tag \"project\"\n")
  (insert "                     :field \"effort\"\n")
  (insert "                     :function :avg)))\n")
  (insert "#+end_src\n\n")
  
  ;; Create the columns
  (supertag-virtual-column-create
   (list :id "total-project-effort"
         :name "Total Project Effort"
         :type :aggregate
         :params (list :tag "project"
                       :field "effort"
                       :function :sum)))
  
  (supertag-virtual-column-create
   (list :id "project-count"
         :name "Project Count"
         :type :aggregate
         :params (list :tag "project"
                       :field "effort"
                       :function :count)))
  
  (supertag-virtual-column-create
   (list :id "avg-project-effort"
         :name "Average Project Effort"
         :type :aggregate
         :params (list :tag "project"
                       :field "effort"
                       :function :avg)))
  
  ;; Show all columns
  (insert "## Created Columns\n\n")
  (dolist (col (supertag-virtual-column-list))
    (let ((params (plist-get col :params)))
      (insert (format "- **%s** (%s)\n"
                      (plist-get col :name)
                      (plist-get col :id)))
      (insert (format "  - Type: ~%s~\n" (plist-get col :type)))
      (insert (format "  - Tag: ~%s~\n" (plist-get params :tag)))
      (insert (format "  - Field: ~%s~\n" (plist-get params :field)))
      (insert (format "  - Function: ~%s~\n\n" (plist-get params :function)))))
  
  ;; Show aggregate functions
  (insert "## Supported Aggregate Functions\n\n")
  (insert "| Function | Description |\n")
  (insert "|----------|-------------|\n")
  (insert "| ~:sum~ | Sum of all values |\n")
  (insert "| ~:count~ | Count of nodes |\n")
  (insert "| ~:avg~ | Average of values |\n")
  (insert "| ~:max~ | Maximum value |\n")
  (insert "| ~:min~ | Minimum value |\n")
  (insert "| ~:first~ | First value |\n")
  (insert "| ~:last~ | Last value |\n\n")
  
  ;; Rollup vs Aggregate comparison
  (insert "## Rollup vs Aggregate\n\n")
  (insert "| Feature | Rollup | Aggregate |\n")
  (insert "|---------|--------|-----------|\n")
  (insert "| Scope | Related nodes | All nodes with tag |\n")
  (insert "| Use case | Sum child task effort | Sum all project budgets |\n")
  (insert "| Requires | Relation type | Tag name |\n")
  (insert "| Example | Total effort of project X | Total effort of ALL projects |\n\n")
  
  ;; Try it
  (insert "## Try It Yourself\n\n")
  (insert "Evaluate these expressions:\n\n")
  (insert "#+begin_src elisp\n")
  (insert ";; Get aggregate value for any node\n")
  (insert "(supertag-virtual-column-get \"any-node-id\" \"total-project-effort\")\n\n")
  (insert ";; Create your own aggregate column\n")
  (insert "(supertag-virtual-column-create\n")
  (insert " (list :id \"total-tasks\"\n")
  (insert "       :name \"Total Tasks\"\n")
  (insert "       :type :aggregate\n")
  (insert "       :params (list :tag \"task\"\n")
  (insert "                     :field \"status\"\n")
  (insert "                     :function :count)))\n")
  (insert "#+end_src\n")
  
  (goto-char (point-min)))

(provide 'demo-aggregate)

;;; demo-aggregate.el ends here
