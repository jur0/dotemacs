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

;; Echo unfinished commands after a delay (in seconds).
(setq echo-keystrokes 0.15)

;; Display dividers between windows.
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

;; Fringes are areas on both left and right side of Emacs frame. They
;; are used for showing indicators, such as linting errors, continuation
;; lines, etc.
(use-package fringe
  :config
  ;; `nil' means the default width of 8 px.
  (fringe-mode nil)
  (setq-default fringes-outside-margins nil)
  (setq-default indicate-buffer-boundaries nil)
  (setq-default indicate-empty-lines nil)
  (setq-default overflow-newline-into-fringe t))

;; Move mouse pointer away from to screen (to the top right corner)
;; automatically.
(use-package avoid
  :config
  (setq mouse-avoidance-banish-position
        '((frame-or-window . frame)
          (side . right)
          (side-pos . 0)
          (top-or-bottom . top)
          (top-or-bottom-pos . 0)))
  (mouse-avoidance-mode 'animate))

;; Frame title.
(setq frame-title-format
      `(,(user-login-name) "@" ,(system-name) "  ◉  %f"))
(setq-default icon-title-format frame-title-format)

;; Keep the `ns-appearance' frame parameter correctly set in GUI frames so that
;; it matches the currently-enabled theme, whether it is light or dark.
(use-package ns-auto-titlebar
  :if (eq 'ns (window-system))
  :ensure t
  :config
  (ns-auto-titlebar-mode t))

(use-package modus-vivendi-theme
  :ensure t
  :config
  (setq modus-vivendi-theme-fringes 'subtle)
  (setq modus-vivendi-theme-bold-constructs t)
  (load-theme 'modus-vivendi t))

(use-package doom-modeline
  :ensure t
  :config
  (setq doom-modeline-height 1)
  (setq doom-modeline-bar-width 6)
  (setq doom-modeline-major-mode-icon t)
  (setq doom-modeline-icon t)
  (setq doom-modeline-buffer-modification-icon t)
  (setq doom-modeline-major-mode-color-icon t)
  (setq doom-modeline-buffer-state-icon t)
  (setq doom-modeline-buffer-file-name-style 'truncate-with-project)
  (setq doom-modeline-minor-modes nil)
  (setq doom-modeline-indent-info nil)
  (setq doom-modeline-buffer-encoding nil)
  (setq doom-modeline-env-version nil)
  :hook
  (after-init . doom-modeline-mode))

(defun my/font-installed-p (font-name)
  "Check if font with FONT-NAME is available."
  (find-font (font-spec :name font-name)))

(use-package all-the-icons
  :if sys/guip
  :ensure t
  :init
  (unless (or sys/win32p (my/font-installed-p "all-the-icons"))
    (all-the-icons-install-fonts t)))

(provide 'init-ui)

;;; init-ui.el ends here
