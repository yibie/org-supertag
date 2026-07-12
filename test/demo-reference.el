;;; demo-reference.el --- Reference virtual column demo -*- lexical-binding: t; -*-

;;; Commentary:

;; Interactive demo of the reference virtual column type
;; Usage: M-x supertag-demo-reference

;;; Code:

(require 'supertag-virtual-column)

(defun supertag-demo-reference ()
  "Interactive demo of reference virtual columns."
  (interactive)
  (switch-to-buffer "*Reference Demo*")
  (erase-buffer)
  (org-mode)
  
  (insert "# Reference Virtual Column Demo\n\n")
  
  (insert "## What is Reference?\n\n")
  (insert "Reference virtual columns pull field values from *related* nodes.\n")
  (insert "Unlike Rollup (which aggregates multiple values), Reference returns a single value.\n\n")
  
  (insert "## Use Cases\n\n")
  (insert "- **Parent Deadline**: Show parent project's deadline on child tasks\n")
  (insert "- **Owner Name**: Display the name of the person who owns this task\n")
  (insert "- **Category Color**: Inherit color from the assigned category\n")
  (insert "- **Manager Email**: Reference manager's email from department node\n\n")
  
  ;; Initialize and create columns
  (supertag-virtual-column-init)
  
  (insert "## Creating Reference Columns\n\n")
  (insert "#+begin_src elisp\n")
  (insert ";; Reference parent node's deadline\n")
  (insert "(supertag-virtual-column-create\n")
  (insert " (list :id \"parent-deadline\"\n")
  (insert "       :name \"Parent Deadline\"\n")
  (insert "       :type :reference\n")
  (insert "       :params (list :relation \"parent\"\n")
  (insert "                     :field \"deadline\")))\n\n")
  
  (insert ";; Reference owner's name\n")
  (insert "(supertag-virtual-column-create\n")
  (insert " (list :id \"owner-name\"\n")
  (insert "       :name \"Owner Name\"\n")
  (insert "       :type :reference\n")
  (insert "       :params (list :relation \"owned-by\"\n")
  (insert "                     :field \"name\")))\n\n")
  
  (insert ";; Reference with index (second parent)\n")
  (insert "(supertag-virtual-column-create\n")
  (insert " (list :id \"second-parent-status\"\n")
  (insert "       :name \"Second Parent Status\"\n")
  (insert "       :type :reference\n")
  (insert "       :params (list :relation \"parent\"\n")
  (insert "                     :field \"status\"\n")
  (insert "                     :index 1)))\n")
  (insert "#+end_src\n\n")
  
  ;; Create the columns
  (supertag-virtual-column-create
   (list :id "parent-deadline"
         :name "Parent Deadline"
         :type :reference
         :params (list :relation "parent"
                       :field "deadline")))
  
  (supertag-virtual-column-create
   (list :id "owner-name"
         :name "Owner Name"
         :type :reference
         :params (list :relation "owned-by"
                       :field "name")))
  
  (supertag-virtual-column-create
   (list :id "second-parent-status"
         :name "Second Parent Status"
         :type :reference
         :params (list :relation "parent"
                       :field "status"
                       :index 1)))
  
  ;; Show all columns
  (insert "## Created Columns\n\n")
  (dolist (col (supertag-virtual-column-list))
    (let ((params (plist-get col :params)))
      (insert (format "- **%s** (%s)\n"
                      (plist-get col :name)
                      (plist-get col :id)))
      (insert (format "  - Type: ~%s~\n" (plist-get col :type)))
      (insert (format "  - Relation: ~%s~\n" (plist-get params :relation)))
      (insert (format "  - Field: ~%s~\n" (plist-get params :field)))
      (when (plist-get params :index)
        (insert (format "  - Index: ~%s~\n" (plist-get params :index))))
      (insert "\n")))
  
  ;; Parameters
  (insert "## Reference Parameters\n\n")
  (insert "| Parameter | Required | Description |\n")
  (insert "|-----------|----------|-------------|\n")
  (insert "| ~:relation~ | Yes | Relation type to follow (e.g., ~\"parent\"~) |\n")
  (insert "| ~:field~ | Yes | Field name to retrieve from target node |\n")
  (insert "| ~:index~ | No | Which relation to use (0=first, 1=second, default: 0) |\n\n")
  
  ;; Comparison
  (insert "## Reference vs Rollup vs Aggregate\n\n")
  (insert "| Feature | Reference | Rollup | Aggregate |\n")
  (insert "|---------|-----------|--------|-----------|\n")
  (insert "| Input | Single related node | Multiple related nodes | All nodes with tag |\n")
  (insert "| Output | Field value | Aggregated value | Aggregated value |\n")
  (insert "| Use case | Show parent's deadline | Sum children's effort | Total of all projects |\n")
  (insert "| Example | Task's project deadline | Project's total effort | All projects budget |\n\n")
  
  ;; Try it
  (insert "## Try It Yourself\n\n")
  (insert "Evaluate these expressions:\n\n")
  (insert "#+begin_src elisp\n")
  (insert ";; Get referenced value\n")
  (insert "(supertag-virtual-column-get \"child-node-id\" \"parent-deadline\")\n\n")
  (insert ";; Create your own reference\n")
  (insert "(supertag-virtual-column-create\n")
  (insert " (list :id \"category-color\"\n")
  (insert "       :name \"Category Color\"\n")
  (insert "       :type :reference\n")
  (insert "       :params (list :relation \"belongs-to\"\n")
  (insert "                     :field \"color\")))\n")
  (insert "#+end_src\n")
  
  (goto-char (point-min)))

(provide 'demo-reference)

;;; demo-reference.el ends here
