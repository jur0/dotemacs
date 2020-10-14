;;; Code:

(require 'init-const)

;; On-the-fly syntax checking/linting.
(use-package flycheck
  :ensure t
  :config
  (setq flycheck-emacs-lisp-load-path 'inherit)
  ;; Only check while saving and opening files.
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (global-flycheck-mode t))

(provide 'init-linter)

;;; init-linter.el ends here
