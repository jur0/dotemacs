;;; Code:

;; Determine the major mode to use for files that do not specify a
;; major mode.
(setq-default major-mode 'text-mode)

;; Set default line length.
(setq-default fill-column 80)

;; Set default tab width in spaces.
(setq-default tab-width 4)

;; Permanently indent with spaces, never with TABs.
(setq-default indent-tabs-mode nil)

;; Tab first tries to indent the current line, and if the line was
;; already indented, then try to complete the thing at point.
(setq-default tab-always-indent 'complete)

;; Add a newline automatically at the end of the file upon save.
(setq require-final-newline t)

;; Delete files by moving them into OS's trash directory.
(setq delete-by-moving-to-trash t)

;; Delete trailing whitespace before saving buffer.
(add-hook 'before-save-hook #'delete-trailing-whitespace)

;; Automatically revert buffers when files on disk change.
(use-package autorevert
  :config
  (setq auto-revert-verbose t)
  ;; Enable autorevert on Dired buffers.
  (setq global-auto-revert-non-file-buffers t)
  (global-auto-revert-mode t))

;; Delete selection if you insert.
(use-package delsel
  :config
  (delete-selection-mode t))

;; Move back to the last change.
(use-package goto-last-change
  :ensure t
  :bind
  ("C-z" . goto-last-change))

;; M-w/C-w copies/kills the whole line if region is not active.
(use-package whole-line-or-region
  :ensure t
  :config
  (unbind-key "s-v" whole-line-or-region-local-mode-map)
  (unbind-key "s-x" whole-line-or-region-local-mode-map)
  (whole-line-or-region-global-mode t))

;; Expand/contract sexp.
(use-package expand-region
  :ensure t
  :bind
  (("C-." . er/expand-region)
   ("C-," . er/contract-region)))

(provide 'init-edit)
