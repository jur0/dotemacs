;;; Code:

(use-package lsp-mode
  :ensure t
  :init
  ;; Increase max number of bytes read from subprocess in a single chunk.
  (setq read-process-output-max (* 1024 1024)):config
  :config
  (setq lsp-keymap-prefix "C-c p")
  (setq lsp-keep-workspace-alive nil)
  (setq lsp-prefer-capf t)
  (setq lsp-enable-semantic-highlighting t)
  (setq lsp-diagnostics-modeline-scope :project)
  ;; Disable doc helper buffer.
  ;; (setq lsp-signature-auto-activate nil)
  :hook
  (lsp-managed-mode . lsp-diagnostics-modeline-mode)
  :bind
  (:map lsp-mode-map
        ([remap xref-find-definitions] . lsp-find-definition)
        ([remap xref-find-references] . lsp-find-references)))

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
