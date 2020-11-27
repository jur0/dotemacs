;;; Code:

(require 'init-const)

;; Force Emacs to use its own internal password prompt instead of an external
;; pin entry program.
(use-package epg-config
  :init
  (setq epg-pinentry-mode 'loopback))

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

;; Major mode for pass, it depends on `password-store'. It shows
;; password tree which can be manipulated.
(use-package pass
  :if exec/pass
  :ensure t
  :commands
  (pass))

(provide 'init-password)

;;; init-password.el ends here
