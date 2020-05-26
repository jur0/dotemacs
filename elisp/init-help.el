;;; Code:

;; Display the key bindings following your currently entered incomplete command
;; (a prefix) in minibuffer.
(use-package which-key
  :ensure t
  :config
  (setq which-key-popup-type 'minibuffer)
  (which-key-mode t))

(provide 'init-help)

;;; init-help.el ends here
