;;; supertag-view-svg-tag.el --- SVG tag rendering for org-supertag -*- lexical-binding: t; -*-

;;; Commentary:
;; Renders #tag inline tags as SVG pill badges using Emacs' built-in svg library.
;; Integrates with supertag-view-style-mode for seamless switching between
;; face-based and SVG-based rendering.

;;; Code:

(require 'svg)
(require 'cl-lib)
(require 'supertag-view-helper)

;;;----------------------------------------------------------------------
;;; Customization
;;;----------------------------------------------------------------------

(defgroup supertag-view-svg-tag nil
  "SVG tag rendering for org-supertag inline #tags."
  :group 'org-supertag)

(defcustom supertag-svg-tag-enable t
  "When non-nil, render #tags as SVG pill badges.
When nil, falls back to face-based rendering via `supertag-inline-face'."
  :type 'boolean
  :group 'supertag-view-svg-tag)

(defcustom supertag-svg-tag-style 'colored
  "Visual style of SVG tags.
`colored' uses a deterministic pastel color per tag name.
`neutral' uses a subtle gray pill like typical note apps."
  :type '(choice (const :tag "Colored per-tag" colored)
                 (const :tag "Neutral gray pill" neutral))
  :group 'supertag-view-svg-tag)

(defcustom supertag-svg-tag-padding-x 8
  "Horizontal padding (px) inside the SVG tag."
  :type 'integer
  :group 'supertag-view-svg-tag)

(defcustom supertag-svg-tag-radius 100
  "Corner radius (px) of SVG tag badges.
Values larger than half the badge height are capped, so the default
creates a fully rounded pill."
  :type 'integer
  :group 'supertag-view-svg-tag)

(defcustom supertag-svg-tag-stroke-width 0
  "Stroke width for SVG tag borders.
Set to 0 to draw no border."
  :type 'number
  :group 'supertag-view-svg-tag)

(defcustom supertag-svg-tag-font-scale 0.78
  "Font size scale factor relative to the frame character height."
  :type 'number
  :group 'supertag-view-svg-tag)

(defcustom supertag-svg-tag-show-hash nil
  "When non-nil, include the leading '#' in the SVG badge.
When nil, only the tag name is shown."
  :type 'boolean
  :group 'supertag-view-svg-tag)

(defcustom supertag-svg-tag-font-weight "500"
  "Font weight used inside SVG tags (e.g. \"normal\", \"500\", \"bold\")."
  :type 'string
  :group 'supertag-view-svg-tag)

(defcustom supertag-svg-tag-color-alpha 0.18
  "Opacity of the colored style background (0 = invisible, 1 = opaque)."
  :type 'number
  :group 'supertag-view-svg-tag)

(require 'color)

;;;----------------------------------------------------------------------
;;; Color generation
;;;----------------------------------------------------------------------

(defun supertag-svg-tag--is-light-theme-p ()
  "Return non-nil if the current frame has a light background."
  (eq (frame-parameter nil 'background-mode) 'light))

(defun supertag-svg-tag--hash-to-index (str max)
  "Hash STR to an integer in [0, MAX)."
  (mod (abs (sxhash str)) max))

(defun supertag-svg-tag--hsl-color (hue saturation lightness)
  "Return a #rrggbb string from HUE (0-360), SATURATION and LIGHTNESS (0-1)."
  (apply #'color-rgb-to-hex
         (append (color-hsl-to-rgb (/ hue 360.0) saturation lightness)
                 '(2))))

(defun supertag-svg-tag--hsl-rgba (hue saturation lightness alpha)
  "Return an rgba(...) string from HUE, SATURATION, LIGHTNESS and ALPHA."
  (let ((rgb (color-hsl-to-rgb (/ hue 360.0) saturation lightness)))
    (format "rgba(%d,%d,%d,%s)"
            (round (* (nth 0 rgb) 255))
            (round (* (nth 1 rgb) 255))
            (round (* (nth 2 rgb) 255))
            alpha)))

(defun supertag-svg-tag--neutral-colors ()
  "Return (bg border fg) for the neutral gray style."
  (if (supertag-svg-tag--is-light-theme-p)
      (list "#f3f4f6" "#f3f4f6" "#374151")
    (list "#374151" "#374151" "#f3f4f6")))

(defun supertag-svg-tag--colored-colors (tag-name)
  "Return (bg border fg) for the colored style based on TAG-NAME."
  (let* ((idx (supertag-svg-tag--hash-to-index tag-name 20))
         (hue (* idx 18))
         (light-p (supertag-svg-tag--is-light-theme-p))
         (base-bg (supertag-svg-tag--hsl-color hue 0.55 (if light-p 0.90 0.30)))
         (bg (supertag-svg-tag--hsl-rgba hue 0.55 (if light-p 0.90 0.30)
                                         supertag-svg-tag-color-alpha))
         (border (supertag-svg-tag--hsl-color hue 0.60 (if light-p 0.72 0.45)))
         (fg (supertag-svg-tag--text-color base-bg)))
    (list bg border fg)))

(defun supertag-svg-tag--get-colors (tag-name)
  "Return (bg border fg) color triple for TAG-NAME."
  (if (eq supertag-svg-tag-style 'neutral)
      (supertag-svg-tag--neutral-colors)
    (supertag-svg-tag--colored-colors tag-name)))

(defun supertag-svg-tag--text-color (bg-color)
  "Derive a readable text color from BG-COLOR."
  (let* ((rgb (color-name-to-rgb bg-color))
         (r (nth 0 rgb))
         (g (nth 1 rgb))
         (b (nth 2 rgb))
         ;; Perceived brightness: 0.299R + 0.587G + 0.114B
         (brightness (+ (* 0.299 r) (* 0.587 g) (* 0.114 b))))
    (if (> brightness 0.65) "#1e293b" "#f8fafc")))

;;;----------------------------------------------------------------------
;;; SVG tag builder
;;;----------------------------------------------------------------------

(defvar supertag-svg-tag--cache (make-hash-table :test 'equal)
  "Cache of SVG images keyed by (display-text char-height style background-mode alpha).")

(defun supertag-svg-tag--char-width ()
  "Return a usable character width in pixels."
  (if (display-graphic-p)
      (max 1 (frame-char-width))
    8))

(defun supertag-svg-tag--char-height ()
  "Return a usable character height in pixels."
  (if (display-graphic-p)
      (max 1 (frame-char-height))
    16))

(defun supertag-svg-tag--text-pixel-width (text)
  "Estimate pixel width for TEXT using frame character metrics.
Accounts for CJK/double-width characters via `string-width'."
  (max 1 (round (* (string-width text)
                   (supertag-svg-tag--char-width)
                   supertag-svg-tag-font-scale))))

(defun supertag-svg-tag--default-font-family ()
  "Return the best available font family string for SVG."
  (let ((family (face-attribute 'default :family nil 'default)))
    (if (or (not family) (eq family 'unspecified))
        "sans-serif"
      family)))

(defun supertag-svg-tag--make-svg (text display-text)
  "Create an SVG image for tag TEXT, showing DISPLAY-TEXT.
Returns an Emacs image object suitable for the `display' text property."
  (let* ((char-h (supertag-svg-tag--char-height))
         (font-size-px (round (* char-h supertag-svg-tag-font-scale)))
         (text-w (supertag-svg-tag--text-pixel-width display-text))
         (pad-x supertag-svg-tag-padding-x)
         (img-h (max char-h (round (* char-h 1.15))))
         (img-w (+ text-w (* 2 pad-x)))
         (radius (min supertag-svg-tag-radius (/ img-h 2)))
         (colors (supertag-svg-tag--get-colors text))
         (bg (nth 0 colors))
         (border-clr (nth 1 colors))
         (fg (nth 2 colors))
         (svg (svg-create img-w img-h)))
    ;; Pill/capsule background
    (svg-rectangle svg 0 0 img-w img-h
                   :rx radius :ry radius
                   :fill bg
                   :stroke border-clr
                   :stroke-width supertag-svg-tag-stroke-width)
    ;; Vertically centered label
    (svg-text svg display-text
              :x (/ img-w 2)
              :y (/ img-h 2)
              :font-family (supertag-svg-tag--default-font-family)
              :font-size font-size-px
              :fill fg
              :text-anchor "middle"
              :dominant-baseline "central"
              :font-weight supertag-svg-tag-font-weight)
    (svg-image svg :scale 1 :ascent 'center)))

(defun supertag-svg-tag--get-cached (tag-name)
  "Get or create a cached SVG image for TAG-NAME."
  (let* ((display-text (if supertag-svg-tag-show-hash
                           tag-name
                         (if (string-prefix-p "#" tag-name)
                             (substring tag-name 1)
                           tag-name)))
         (key (list display-text
                    (supertag-svg-tag--char-height)
                    supertag-svg-tag-style
                    (frame-parameter nil 'background-mode)
                    supertag-svg-tag-color-alpha)))
    (or (gethash key supertag-svg-tag--cache)
        (let ((img (supertag-svg-tag--make-svg tag-name display-text)))
          (puthash key img supertag-svg-tag--cache)
          img))))

(defun supertag-svg-tag--clear-cache ()
  "Clear the SVG image cache (call after theme changes)."
  (interactive)
  (clrhash supertag-svg-tag--cache)
  (message "SVG tag cache cleared"))

;;;----------------------------------------------------------------------
;;; Font-lock integration
;;;----------------------------------------------------------------------

(defun supertag-svg-tag--match-handler ()
  "Font-lock match handler for #tag patterns.
Returns the appropriate display spec for the matched tag."
  (let ((tag (match-string 0)))
    (if (and supertag-svg-tag-enable
             (display-graphic-p)
             (fboundp 'svg-create))
        `(face nil display ,(supertag-svg-tag--get-cached tag))
      'supertag-inline-face)))

(defvar supertag-view-svg-tag--font-lock-keywords
  `((,(concat "#[" supertag-view-helper--valid-tag-chars "]+")
     (0 (if (and (not (supertag-view-helper--in-src-block-p))
                 (not (supertag-view-helper--at-table-p))
                 (not (supertag-view-helper--at-commented-p))
                 (not (eq (get-text-property (match-beginning 0) 'face) 'org-verbatim)))
            (supertag-svg-tag--match-handler))
        t)))
  "Font-lock keywords for SVG tag rendering.")

;;;----------------------------------------------------------------------
;;; Theme change hook
;;;----------------------------------------------------------------------

(defun supertag-svg-tag--on-theme-change (&rest _)
  "Clear SVG cache and refresh font-lock on theme change."
  (supertag-svg-tag--clear-cache)
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when supertag-view-style-mode
        (supertag-view-helper--refresh-fontification)))))

;; Register theme-change hook once
(when (boundp 'enable-theme-functions)
  (add-hook 'enable-theme-functions #'supertag-svg-tag--on-theme-change))

;;;###autoload
(defun supertag-svg-tag-mode-enable ()
  "Enable SVG tag rendering for org-supertag."
  (interactive)
  (setq supertag-svg-tag-enable t)
  (supertag-svg-tag--refresh-all-buffers)
  (message "SVG tag rendering enabled"))

;;;###autoload
(defun supertag-svg-tag-mode-disable ()
  "Disable SVG tag rendering, revert to face-based."
  (interactive)
  (setq supertag-svg-tag-enable nil)
  (supertag-svg-tag--refresh-all-buffers)
  (message "SVG tag rendering disabled"))

;;;###autoload
(defun supertag-svg-tag-mode-toggle ()
  "Toggle SVG tag rendering on/off."
  (interactive)
  (if supertag-svg-tag-enable
      (supertag-svg-tag-mode-disable)
    (supertag-svg-tag-mode-enable)))

(defun supertag-svg-tag--refresh-all-buffers ()
  "Toggle between SVG and face keywords in all active mode buffers."
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when supertag-view-style-mode
        ;; Remove both keyword sets, then add back the right one
        (font-lock-remove-keywords nil supertag-view-helper--font-lock-keywords)
        (font-lock-remove-keywords nil supertag-view-svg-tag--font-lock-keywords)
        (font-lock-add-keywords nil (supertag-view-helper--get-font-lock-keywords) t)
        (supertag-view-helper--refresh-fontification)))))

(provide 'supertag-view-svg-tag)
;;; supertag-view-svg-tag.el ends here
