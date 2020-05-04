;;; Code:

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

;; Add a newline automatically at the end of the file upon save.
(setq require-final-newline t)

(provide 'init-edit)
