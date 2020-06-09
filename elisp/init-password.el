;;; Code:

;; Interface for pass command.
(use-package pass
  :if (executable-find "pass")
  :ensure t
  :commands
  (pass))

(provide 'init-password)

;;; init-password.el ends here
