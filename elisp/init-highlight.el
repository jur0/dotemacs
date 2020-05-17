;;; Code:

;; Highlight the current line.
(use-package hl-line
  :hook
  (after-init . global-hl-line-mode))

;; Sets background of a color found in text e.g. RGB. The mode is activated
;; manually.
(use-package rainbow-mode
  :ensure t
  :commands
  (rainbow-mode)
  :config
  (setq rainbow-ansi-colors nil)
  (setq rainbow-x-colors nil))

(provide 'init-highlight)
