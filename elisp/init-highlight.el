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

;; Hightlight nested parentheses and braces according to their depth.
(use-package rainbow-delimiters
  :ensure t
  :hook
  (prog-mode . rainbow-delimiters-mode))

;; Highlight entire code blocks. The mode is activated manually.
(use-package rainbow-blocks
  :ensure t
  :commands
  (rainbow-blocks-mode)
  :config
  (setq rainbow-blocks-highlight-braces-p t)
  (setq rainbow-blocks-highlight-brackets-p t)
  (setq rainbow-blocks-highlight-parens-p t))

(provide 'init-highlight)
