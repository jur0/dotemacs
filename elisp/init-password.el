;;; Code:

(require 'init-const)

;; Major mode for pass.
(use-package pass
  :if exec/pass
  :ensure t
  :commands
  (pass))

;; Interface for common actions related to password manipulation.
(use-package password-store
  :if exec/pass
  :ensure t
  :commands
  (password-store-copy
   password-store-edit
   password-store-insert)
  :config
  (setq password-store-time-before-clipboard-restore 30))

(provide 'init-password)

;;; init-password.el ends here
