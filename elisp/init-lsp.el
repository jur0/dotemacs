;;; Code:

(use-package lsp-mode
  :ensure t
  :config
  (setq lsp-keymap-prefix "C-c p")
  (setq lsp-keep-workspace-alive nil)
  ;;(setq lsp-prefer-capf t)
  ;; Disable doc helper buffer.
  ;; (setq lsp-signature-auto-activate nil)
  (setq lsp-enable-xref t))

(use-package lsp-ui
  :ensure t
  :after
  (lsp-mode flycheck)
  :config
  (setq lsp-ui-doc-position 'bottom)
  (setq lsp-ui-doc-delay 1.5)
  (setq lsp-ui-sideline-delay 1.5)
  :hook
  (lsp-mode . lsp-ui-mode))

(provide 'init-lsp)

;;; init-lsp.el ends here
