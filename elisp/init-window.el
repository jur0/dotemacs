;;; Code:

(use-package window
  :init
  ;; Resize window combinations proportionally.
  (setq window-combination-resize t)
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
  :hook
  (after-init-hook . winner-mode)
  :bind
  ("<s-right>" . winner-redo)
  ("<s-left>" . winner-undo))

(provide 'init-window)
