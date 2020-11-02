;;; Code:

(use-package rust-mode
  :ensure t
  :init
  (setq lsp-rust-server 'rust-analyzer)
  :config
  (setq rust-format-on-save t)
  :hook
  (rust-mode . lsp-deferred))

(use-package cargo
  :ensure t
  :hook
  (rust-mode . cargo-minor-mode))

(provide 'init-rust)

;;; init-rust.el ends here
