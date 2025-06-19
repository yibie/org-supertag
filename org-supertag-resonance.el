;;; org-supertag-resonance.el --- Proactive resonance discovery for org-supertag  -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; This package implements the "Core Resonance Loop" for org-supertag.
;; It provides different interaction modes for proactively discovering
;; connections between the current note and the existing knowledge base.
;;
;; The user can choose between several 'trigger modes':
;; - 'manual: Suggestions are only fetched when explicitly requested.
;; - 'whisper: The system indicates potential resonance silently in the mode-line.
;; - 'proactive: The system automatically fetches and displays suggestions on idle.

;;; Code:

(require 'org-supertag-util)
(require 'org-supertag-api)

(defgroup org-supertag-resonance nil
  "Settings for the proactive resonance engine."
  :group 'org-supertag)

(defcustom org-supertag-resonance-trigger-mode 'manual
  "The trigger mode for the conceptual resonance feature.
This variable determines how and when resonance suggestions are fetched.

Possible values are:
- 'manual:     Suggestions are only fetched when you run the command
              `org-supertag-resonance-discover`. This is the least
              intrusive mode.
- 'whisper:    When idle, the system checks for resonance potential. If
              found, a small indicator appears in the mode-line. You can
              then run a command to see the results. (Not yet implemented)
- 'proactive:  When idle, the system automatically fetches suggestions
              and displays them in the sidebar. This is the most
              automated mode."
  :type '(choice (const :tag "Manual" manual)
                 (const :tag "Whisper" whisper)
                 (const :tag "Proactive" proactive))
  :group 'org-supertag-resonance)

(defcustom org-supertag-resonance-idle-delay 2
  "The idle delay in seconds before triggering resonance discovery.
This only applies to 'whisper' and 'proactive' modes."
  :type 'integer
  :group 'org-supertag-resonance)

(defvar org-supertag-resonance--timer nil
  "The timer object for idle-based resonance discovery.")

(defun org-supertag-resonance--fetch-and-display ()
  "Fetch and display resonance for the current node."
  (let* ((node (org-supertag-node-get-at-point))
         (node-id (and node (plist-get node :id)))
         (content (and node-id (org-supertag-get-node-content node-id))))
    (if (and node-id content)
        (progn
          (message "Resonance: Analyzing current node for connections...")
          (org-supertag-api-proactive-get-resonance
           node-id
           content
           (lambda (response)
             (if (and response (eq (plist-get response :status) 'success))
                 (message "Resonance Found: %s" response) ; Placeholder UI
               (message "Resonance: No significant connections found or an error occurred.")))))
      (message "Resonance: Could not find a valid org-supertag node at point."))))

;;;###autoload
(defun org-supertag-resonance-discover ()
  "Manually trigger the resonance discovery for the current node."
  (interactive)
  (org-supertag-resonance--fetch-and-display))

(provide 'org-supertag-resonance)

;;; org-supertag-resonance.el ends here 
