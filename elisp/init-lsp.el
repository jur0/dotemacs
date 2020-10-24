;;; Code:

(use-package lsp-mode
  :ensure t
  :init
  ;; Increase max number of bytes read from subprocess in a single chunk.
  (setq read-process-output-max (* 1024 1024))
  :config
  (setq lsp-keymap-prefix "C-c p")
  (setq lsp-keep-workspace-alive nil)
  (setq lsp-enable-semantic-highlighting t)
  (setq lsp-modeline-diagnostics-enable t)
  (setq lsp-modeline-diagnostics-scope :project)
  (setq lsp-headerline-breadcrumb-enable t)
  (setq lsp-headerline-breadcrumb-segments '(project file symbols))
  :hook
  (lsp-mode . lsp-enable-which-key-integration)
  :bind
  (:map lsp-mode-map
        ([remap xref-find-definitions] . lsp-find-definition)
        ([remap xref-find-references] . lsp-find-references)))

(use-package lsp-ui
  :ensure t
  :commands
  (lsp-ui-mode)
  :after
  (lsp-mode flycheck)
  :config
  (setq lsp-ui-doc-position 'bottom)
  (setq lsp-ui-doc-delay 2.0)
  (setq lsp-ui-sideline-delay 1.5))

(provide 'init-lsp)

;;; init-lsp.el ends here
