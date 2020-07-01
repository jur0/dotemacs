;;; Code:

;; Built-in Emacs tool to monitor running processes.
(use-package proced
  :commands
  (proced)
  :config
  (setq proced-auto-update-flag t)
  (setq proced-auto-update-interval 1)
  (setq proced-descend t)
  (setq proced-filter 'user))

(use-package proced-narrow
  :ensure t
  :after
  (proced)
  :bind
  (:map proced-mode-map
        ("/" . proced-narrow)))

(provide 'init-monitor)

;;; init-monitor.el ends here
