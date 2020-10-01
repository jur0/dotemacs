;;; Code:

(use-package window
  :init
  (setq display-buffer-alist
        '(
          ;; TODO: Add more buffers!
          ;; Bottom side of display.
          ("\\*Calendar.*"
           (display-buffer-in-side-window)
           ;; Integer in `window-height' specifies number of lines.
           (window-height . 20)
           (side . bottom)
           (slot . -1))
          ("\\*ielm.*"
           (display-buffer-in-side-window)
           (window-height . 20)
           (side . bottom)
           (slot . 0))
          (".*\\*Completions.*"
           (display-buffer-in-side-window)
           (window-height . 20)
           (side . bottom)
           (slot . 1)
           (window-parameters . ((no-other-window . t))))
          ("\\*\\(Backtrace\\|Warnings\\|Compile-Log\\|Messages\\)\\*"
           (display-buffer-in-side-window)
           (window-height . 20)
           (side . bottom)
           (slot . 2))
          ;; Right side of display.
          ("\\*\\(Help\\|Apropos|Man\\)\\*"
           (display-buffer-in-side-window)
           ;; Adjust the width of the window based on buffer's content width.
           (window-width . fit-window-to-buffer)
           (side . right)
           (slot . 0))))
  (setq window-resize-pixelwise t)
  ;; Resize window combinations proportionally.
  (setq window-combination-resize t)
  (setq fit-window-to-buffer-horizontally t)
  (setq even-window-sizes 'height-only)
  (setq window-sides-vertical nil)
  :bind
  (("s-n" . next-buffer)
   ("s-p" . previous-buffer)
   ("s-o" . other-window)
   ("s-2" . split-window-below)
   ("s-3" . split-window-right)
   ("s-0" . delete-window)
   ("s-1" . delete-other-windows)
   ("s-5" . delete-frame)
   ("C-x +" . balance-windows-area)
   ("s-q" . window-toggle-side-windows)))

;; Keep a record of buffer and window layout changes.
(use-package winner
  :commands
  (winner-undo winner-redo)
  :config
  (winner-mode t)
  :bind
  ("<s-right>" . winner-redo)
  ("<s-left>" . winner-undo))

(provide 'init-window)

;;; init-window.el ends here
