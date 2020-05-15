;;; Code:

(use-package window
  :init
  (setq display-buffer-alist
        ;; TODO: setup other buffers.
        '(("\\*\\(Backtrace\\|Warnings\\|Compile-Log\\|Messages\\)\\*"
           (display-buffer-in-side-window)
           (window-height . 0.25)
           (side . bottom)
           (slot . 0)
           ;; Make the window no-selectable by `other-window' (C-x o).
           (window-parameters . ((no-other-window . t))))))
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
  :custom
  ;; List of buffer names which won't be restored.
  (winner-boring-buffers
   '("*Completions*"
     "*Compile-Log*"
     "*inferior-lisp*"
     "*Fuzzy Completions*"
     "*Apropos*"
     "*Help*"
     "*cvs*"
     "*Buffer List*"
     "*Ibuffer*"
     "*esh command on file*"))
  :config
  (winner-mode t)
  :commands
  (winner-undo winner-redo)
  :bind
  ("<s-right>" . winner-redo)
  ("<s-left>" . winner-undo))

(provide 'init-window)
