;;; widget-music-player-prototype.el --- Interactive widget music-list experiment -*- lexical-binding: t; -*-

;; A standalone music-list prototype combining cover art, widget buttons,
;; a simulated playback timer, and row-integrated progress.
;;
;; Run from the repository root:
;;   emacs -Q -l doc/examples/widget-music-player-prototype.el \
;;     --eval "(widget-music-player-prototype)"
;;
;; Or load this file in an existing Emacs, then run:
;;   M-x widget-music-player-prototype

;;; Code:

(require 'cl-lib)
(require 'widget)
(require 'wid-edit)

(defgroup widget-music-player-prototype nil
  "Throwaway widget music-list prototype."
  :group 'applications)

(defconst widget-music-player--cover
  (expand-file-name "Pictures/人文/人文-神经浪游者.jpeg" "~"))

(defconst widget-music-player--tracks
  '((:title "没了"         :artist "野外合作社" :duration 238)
    (:title "不速之客"     :artist "野外合作社" :duration 267)
    (:title "明天"         :artist "野外合作社" :duration 252)
    (:title "148"          :artist "野外合作社" :duration 214)
    (:title "优越的啸杂"   :artist "野外合作社" :duration 286)
    (:title "哑巴"         :artist "野外合作社" :duration 231)
    (:title "Mind the Gap" :artist "野外合作社" :duration 303)
    (:title "你"           :artist "野外合作社" :duration 245)))

(defvar widget-music-player--index 0)
(defvar widget-music-player--position 0)
(defvar widget-music-player--playing nil)
(defvar widget-music-player--current-item nil)
(defvar widget-music-player--progress-overlay nil)
(defvar widget-music-player--timer nil)
(defvar widget-music-player--saved-window-configuration nil)

(defface widget-music-player-heading
  '((t :foreground "#ead9ad" :weight bold))
  "Primary text in the music-list prototype."
  :group 'widget-music-player-prototype)

(defface widget-music-player-muted
  '((t :foreground "#8f8477"))
  "Secondary text in the music-list prototype."
  :group 'widget-music-player-prototype)

(defface widget-music-player-accent
  '((t :foreground "#d4bd64" :weight bold))
  "Accent text in the music-list prototype."
  :group 'widget-music-player-prototype)

(defface widget-music-player-active
  '((t :foreground "#d4bd64" :weight bold :box nil))
  "Active operation button in the music-list prototype."
  :group 'widget-music-player-prototype)

(defface widget-music-player-progress
  '((t :background "#454131"))
  "Blended background used as simulated translucent progress."
  :group 'widget-music-player-prototype)

(defun widget-music-player--current-track ()
  (nth widget-music-player--index widget-music-player--tracks))

(defun widget-music-player--format-time (seconds)
  (format "%02d:%02d" (/ seconds 60) (% seconds 60)))

(defun widget-music-player--position (position)
  (if (markerp position) (marker-position position) position))

(defun widget-music-player--button (label callback &optional face help)
  (widget-create 'push-button
                 :tag label
                 :format "%[%t%]"
                 :button-face (or face 'widget-music-player-heading)
                 :help-echo help
                 :notify callback))

(defun widget-music-player--align-to (column)
  (indent-to column)
  (insert (propertize " " 'display `(space :align-to ,column))))

(defun widget-music-player--update-progress ()
  (when-let* ((item widget-music-player--current-item)
              (buffer (get-buffer "*Widget Music List*"))
              (start (or (plist-get item :progress-from)
                         (plist-get item :from)))
              (finish (plist-get item :to)))
    (with-current-buffer buffer
      (let* ((start (widget-music-player--position start))
             (finish (widget-music-player--position finish))
             (duration (plist-get (widget-music-player--current-track) :duration))
             (ratio (min 1.0 (/ (float widget-music-player--position) duration)))
             (width (string-width
                     (buffer-substring-no-properties start finish)))
             (progress-width
              (if (zerop widget-music-player--position)
                  0
                (max 1 (floor (* width ratio)))))
             (progress-end
              (save-excursion
                (goto-char start)
                (move-to-column (+ (current-column) progress-width))
                (min (point) finish))))
        (unless (and (overlayp widget-music-player--progress-overlay)
                     (overlay-buffer widget-music-player--progress-overlay))
          (setq widget-music-player--progress-overlay
                (make-overlay start progress-end buffer))
          (overlay-put widget-music-player--progress-overlay
                       'face 'widget-music-player-progress)
          (overlay-put widget-music-player--progress-overlay 'priority 20))
        (move-overlay widget-music-player--progress-overlay
                      start progress-end buffer)
        (overlay-put widget-music-player--progress-overlay
                     'help-echo
                     (format "%s / %s"
                             (widget-music-player--format-time
                              widget-music-player--position)
                             (widget-music-player--format-time duration)))
        (setf (plist-get widget-music-player--current-item :progress) ratio
              (plist-get widget-music-player--current-item :progress-overlay)
              widget-music-player--progress-overlay)))))

(defun widget-music-player--render-row (track index cover)
  (let* ((current-p (= index widget-music-player--index))
         (start (copy-marker (point)))
         (progress-start nil)
         (selected-index index))
    (if cover
        (insert-image cover "      ")
      (insert (propertize "  ▣   " 'face 'widget-music-player-muted)))
    (setq progress-start (copy-marker (point)))
    (insert "  " (if current-p
                      (propertize "▶ " 'face 'widget-music-player-accent)
                    "  "))
    (widget-music-player--button
     (plist-get track :title)
     (lambda (&rest _) (widget-music-player-play selected-index))
     (if current-p 'widget-music-player-accent 'widget-music-player-heading)
     "Play this track")
    (insert (propertize (format " — %s  %s"
                                (plist-get track :artist)
                                (widget-music-player--format-time
                                 (plist-get track :duration)))
                        'face 'widget-music-player-muted))
    (widget-music-player--align-to 56)
    (widget-music-player--button
     (if (and current-p widget-music-player--playing) "⏸" "▶")
     (lambda (&rest _) (widget-music-player-play selected-index))
     (when current-p 'widget-music-player-active)
     "Play or pause")
    (widget-music-player--align-to 60)
    (widget-music-player--button
     "ⓘ"
     (lambda (&rest _)
       (message "%s — %s · %s"
                (plist-get track :title)
                (plist-get track :artist)
                 (widget-music-player--format-time
                 (plist-get track :duration))))
     nil "Show track information")
    (insert "    ")
    (let ((finish (copy-marker (point))))
      (add-text-properties start finish `(widget-music-player-index ,index))
      (when current-p
        (setq widget-music-player--current-item
              `(:from ,start :progress-from ,progress-start :to ,finish))))
    (insert "\n")))

(defun widget-music-player--render-list ()
  (let ((buffer (get-buffer-create "*Widget Music List*")))
    (with-current-buffer buffer
      (widget-music-player-mode)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (remove-overlays)
        (setq widget-music-player--current-item nil
              widget-music-player--progress-overlay nil)
        (insert (propertize "Music Library" 'face '(:inherit widget-music-player-heading
                                                    :height 1.25))
                (propertize "    [n next] [b previous] [SPC play/pause] [q quit]\n\n"
                            'face 'widget-music-player-muted))
        (let ((cover (when (file-readable-p widget-music-player--cover)
                       (create-image widget-music-player--cover nil nil
                                     :width 48 :height 48 :ascent 'center))))
          (cl-loop for track in widget-music-player--tracks
                   for index from 0
                   do (widget-music-player--render-row track index cover)))
        (widget-setup)
        (setq buffer-read-only t)
        (widget-music-player--update-progress)
        (goto-char (point-min))))
    buffer))

(defun widget-music-player-play (index)
  (if (= index widget-music-player--index)
      (setq widget-music-player--playing (not widget-music-player--playing))
    (setq widget-music-player--index index
          widget-music-player--position 0
          widget-music-player--playing t))
  (widget-music-player--render-list))

(defun widget-music-player-next ()
  (interactive)
  (setq widget-music-player--index
        (mod (1+ widget-music-player--index)
             (length widget-music-player--tracks))
        widget-music-player--position 0
        widget-music-player--playing t)
  (widget-music-player--render-list))

(defun widget-music-player-previous ()
  (interactive)
  (setq widget-music-player--index
        (mod (1- widget-music-player--index)
             (length widget-music-player--tracks))
        widget-music-player--position 0
        widget-music-player--playing t)
  (widget-music-player--render-list))

(defun widget-music-player-toggle ()
  (interactive)
  (widget-music-player-play widget-music-player--index))

(defun widget-music-player--tick ()
  (when widget-music-player--playing
    (let ((duration (plist-get (widget-music-player--current-track) :duration)))
      (if (< widget-music-player--position duration)
          (progn
            (cl-incf widget-music-player--position)
            (widget-music-player--update-progress))
        (widget-music-player-next)))))

(defun widget-music-player--stop-timer ()
  (when (timerp widget-music-player--timer)
    (cancel-timer widget-music-player--timer))
  (setq widget-music-player--timer nil))

(defun widget-music-player--start-timer ()
  (widget-music-player--stop-timer)
  (setq widget-music-player--timer
        (run-at-time 1 1 #'widget-music-player--tick)))

(defun widget-music-player-quit ()
  (interactive)
  (widget-music-player--stop-timer)
  (let ((configuration widget-music-player--saved-window-configuration)
        (buffer (get-buffer "*Widget Music List*")))
    (when (window-configuration-p configuration)
      (set-window-configuration configuration))
    (when buffer (kill-buffer buffer))))

(defvar widget-music-player-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map widget-keymap)
    (define-key map (kbd "q") #'widget-music-player-quit)
    (define-key map (kbd "n") #'widget-music-player-next)
    (define-key map (kbd "b") #'widget-music-player-previous)
    (define-key map (kbd "SPC") #'widget-music-player-toggle)
    map))

(define-derived-mode widget-music-player-mode special-mode "Widget-Music-List"
  "Major mode for the throwaway widget music-list prototype."
  (setq-local truncate-lines t
              indent-tabs-mode nil
              cursor-type nil
              line-spacing 0.16
              mode-line-format nil
              frame-title-format "Widget Music List")
  (buffer-face-set '(:family "Menlo"
                     :height 125
                     :foreground "#ead9ad"
                     :background "#262626"))
  (add-hook 'kill-buffer-hook #'widget-music-player--stop-timer nil t))

;;;###autoload
(defun widget-music-player-prototype ()
  "Open the standalone widget music-list prototype."
  (interactive)
  (setq widget-music-player--saved-window-configuration
        (current-window-configuration)
        widget-music-player--index 0
        widget-music-player--position 0
        widget-music-player--playing nil)
  (switch-to-buffer (widget-music-player--render-list))
  (widget-music-player--start-timer))

(defun widget-music-player-prototype-self-check ()
  "Run the smallest state-transition check for the prototype."
  (let ((widget-music-player--index 0)
        (widget-music-player--position 10)
        (widget-music-player--playing t))
    (cl-letf (((symbol-function 'widget-music-player--render-list) #'ignore)
              ((symbol-function 'widget-music-player--update-progress) #'ignore))
      (widget-music-player-next)
      (cl-assert (= widget-music-player--index 1))
      (widget-music-player-previous)
      (cl-assert (= widget-music-player--index 0))
      (setq widget-music-player--position 10)
      (widget-music-player--tick)
      (cl-assert (= widget-music-player--position 11))
      (setq widget-music-player--playing nil)
      (widget-music-player--tick)
      (cl-assert (= widget-music-player--position 11)))
    t))

(provide 'widget-music-player-prototype)

;;; widget-music-player-prototype.el ends here
