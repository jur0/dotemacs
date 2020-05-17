;;; Code:

(require 'init-const)

;; On-the-fly syntax checking/linting.
(use-package flycheck
  :ensure t
  :config
  ;; Enable/disable Flycheck depending on a mode.
  (setq flycheck-global-modes
        '(not text-mode outline-mode fundamental-mode lisp-interaction-mode
              org-mode diff-mode shell-mode eshell-mode term-mode vterm-mode))
  (setq flycheck-emacs-lisp-load-path 'inherit)
  ;; Only check while saving and opening files.
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (global-flycheck-mode t))

;; Display Flycheck errors in GUI tooltips.
(use-package flycheck-posframe
  :if sys/guip
  :ensure t
  :after
  (flycheck)
  :custom-face
  (flycheck-posframe-info-face ((t (:background "#4d6600"))))
  (flycheck-posframe-warning-face ((t (:background "#804d00"))))
  (flycheck-posframe-error-face ((t (:background "#650F0F"))))
  :init
  ;; ℹ
  (setq flycheck-posframe-info-prefix "\u2139 ")
  ;; ⚠
  (setq flycheck-posframe-warning-prefix "\u26a0 ")
  ;; ✖
  (setq flycheck-posframe-error-prefix "\u2716 ")
  (setq flycheck-posframe-border-width 1)
  ;; (setq flycheck-posframe-inhibit-functions
  ;;       '((lambda (&rest _) (bound-and-true-p company-backend))))
  :hook
  (flycheck-mode . flycheck-posframe-mode))

(provide 'init-linter)

;;; init-linter.el ends here
