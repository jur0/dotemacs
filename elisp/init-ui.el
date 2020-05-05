;;; Code:

(require 'init-const)

;; Make Emacs frame fullscreen.
(when sys/guip
  (add-to-list 'default-frame-alist '(fullscreen . maximized)))

;; Change frame size by pixels and not by char size.
(setq frame-resize-pixelwise t)

;; Turn Off Cursor Alarms.
(setq ring-bell-function 'ignore)

;; Shorten 'yes' to 'y' and 'no' to 'n'.
(fset 'yes-or-no-p 'y-or-n-p)

;; Allow narrowing to region.
(put 'narrow-to-region 'disabled nil)
;; Allow up/downcase region.
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
;; With `a' command in dired buffer the dired buffer is killed and the file or
;; directory on the current line is visited.
(put 'dired-find-alternate-file 'disabled nil)
;; Disable overwrite-mode.
(put 'overwrite-mode 'disabled t)

;; Disable bell.
(setq ring-bell-function 'ignore)

;; Suppress GUI features.
(setq use-file-dialog nil)
(setq use-dialog-box nil)
(setq inhibit-startup-screen t)
(setq inhibit-startup-echo-area-message t)

;; Disable blinking cursor.
(blink-cursor-mode -1)

;; Display dividers between windows
(setq window-divider-default-places t)
(setq window-divider-default-bottom-width 1)
(setq window-divider-default-right-width 1)
(add-hook 'window-setup-hook #'window-divider-mode)

;; Display line numbers when programming.
(use-package display-line-numbers
  :hook
  (prog-mode . display-line-numbers-mode))

;; Display line and column number of cursor in mode line.
(use-package simple
  :init
  ;; Show column number in mode line.
  (setq column-number-mode t)
  ;; Show line number in mode line.
  (setq line-number-mode t))

;; Frame title.
(setq-default frame-title-format '("" user-login-name "@" system-name ": %b"))
(setq-default icon-title-format frame-title-format)

;; Keep the `ns-appearance' frame parameter correctly set in GUI frames so that
;; it matches the currently-enabled theme, whether it is light or dark.
(when (or sys/mac-x-p sys/mac-cocoa-p)
  (use-package ns-auto-titlebar
    :ensure t
    :config
    (ns-auto-titlebar-mode t)))

(provide 'init-ui)
