;;; Code:

(require 'init-const)

;; Interface for pass command.
(use-package pass
  :if exec/pass
  :ensure t
  :commands
  (pass))

(provide 'init-password)

;;; init-password.el ends here
