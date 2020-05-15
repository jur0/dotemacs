;;; Code:

(require 'init-const)

(defconst my/fixed-pitch-font "Iosevka Nerd Font Mono"
  "The default fixed-pitch typeface.")

;; The height of 105 is the same as 10.5pt.
(defun my/set-default-font (family size)
  "Set `default' face font to FAMILY at SIZE."
  (set-face-attribute 'default nil :family family :height size))

(defun my/set-laptop-fonts ()
  "Set fonts for a laptop screen."
  (interactive)
  (when sys/guip (my/set-default-font my/fixed-pitch-font 125)))

(defun my/set-desktop-fonts ()
  "Set fonts for a desktop screen."
  (interactive)
  (when sys/guip (my/set-default-font my/fixed-pitch-font 135)))

(defun my/set-font-for-screen ()
  "Use font settings based on screen size.

Choose between `prot/laptop-fonts' and `prot/desktop-fonts'
depending on the width of the monitor.  The calculation is based
on the maximum width of my laptop's screen.  So if an external
display is attached, then it is considered a desktop scenario.

While this function is interactive, it is best to run it with the
`after-init-hook' or perhaps some other event that tracks
monitor-related events."
    (interactive)
    (when sys/guip
	  (if (<= (display-pixel-width) 1280)
	      (my/set-laptop-fonts)
	    (my/set-desktop-fonts))))

(add-hook 'after-init-hook #'my/set-font-for-screen)

(provide 'init-font)
