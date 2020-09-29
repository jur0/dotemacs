;;; Code:

(require 'init-const)

(defconst my/font-sizes-families-alist
  '(("phone" . (110 "Hack" "Source Serif Variable"))
    ("laptop" . (120 "Hack" "Source Serif Variable"))
    ("desktop" . (130 "Hack" "Source Serif Variable"))
    ("presentation" . (180 "Iosevka Nerd Font Mono" "Source Serif Pro")))
  "Alist of desired typefaces and their point sizes.

Each association consists of a display type mapped to a point
size, followed by monospaced and proportionately spaced font
names.  The monospaced typeface is meant to be applied to the
`default' and `fixed-pitch' faces.  The proportionately spaced
font is intended for the `variable-pitch' face.")

(defun my/set-font-face-attributes (height fixed-font variable-font)
  "Set font face attributes.

HEIGHT is the font's point size, represented as either '10' or
'10.5'.  FIXED-FONT is a fixed pitch typeface (also the default
one).  VARIABLE-FONT is proportionally spaced type face."
  (set-face-attribute 'default nil :family fixed-font :height height)
  (set-face-attribute 'fixed-pitch nil :family fixed-font)
  (set-face-attribute 'variable-pitch nil :family variable-font))

(defun my/set-font-for-display (display)
  "Set defaults based on DISPLAY."
  (let* ((font-data (assoc display my/font-sizes-families-alist))
         (height (nth 1 font-data))
         (fixed-font (nth 2 font-data))
         (variable-font (nth 3 font-data)))
    (my/set-font-face-attributes height fixed-font variable-font)))

;; TODO: determine pixel width for phone.
(defun my/get-display ()
  "Get display size."
  (if (<= (display-pixel-width) 1280)
	  "laptop"
    "desktop"))

(defun my/set-font-init ()
  "Set font for the current display."
  (if sys/guip
      (my/set-font-for-display (my/get-display))
    (user-error "Not running a graphical Emacs, cannot set font")))

(add-hook 'after-init-hook #'my/set-font-init)

(defun my/font-mono-p (font)
  "Check if FONT is monospaced."
  (when-let ((info (font-info font)))
    ;; If the string is found the match function returns an integer.
    (integerp (string-match-p "spacing=100" (aref info 1)))))

;; Set fixed-pitch and variable-pitch fonts and font height
;; interactively. Mainly for testing purposes to check different font families.
(defun my/set-font ()
  "Set font."
  (interactive)
  (when sys/guip
    (let* ((font-groups (seq-group-by #'my/font-mono-p (font-family-list)))
           fixed-fonts
           variable-fonts
           all-fonts
           fixed-font
           variable-font
           (heights (mapcar #'number-to-string (list 110 115 120 125 130 135 140)))
           height)
           (if (caar font-groups)
               (setq fixed-fonts (cdar font-groups)
                     variable-fonts (cdadr font-groups))
             (setq fixed-fonts (cdadr font-groups)
                   variable-fonts (cdar font-groups)))
           (setq all-fonts (append variable-fonts fixed-fonts))
           (setq fixed-font (completing-read "Select fixed pitch font: " fixed-fonts nil t))
           (setq variable-font (completing-read "Select variable pitch font: " all-fonts nil t))
           (setq height (completing-read "Select or insert font height: " heights nil))
           (my/set-font-face-attributes (string-to-number height) fixed-font variable-font))))

(provide 'init-font)

;;; init-font ends here
