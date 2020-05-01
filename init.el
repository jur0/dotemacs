(require 'package)

(setq package-archives
      '(("melpa" . "http://melpa.org/packages/")
        ("org" . "http://orgmode.org/elpa/")
        ("gnu" . "http://elpa.gnu.org/packages/")))

(setq-default network-security-level 'high)
(setq-default tls-checktrust t)
(setq-default gnutls-verify-error t)

;; Initialise the packages, avoiding a re-initialisation.
(unless (bound-and-true-p package--initialized)
  (setq package-enable-at-startup nil)
  (package-initialize))

;; Use use-package for package management.
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Configure `use-package' prior to loading it.
(eval-and-compile
  (setq use-package-always-ensure nil)
  (setq use-package-always-defer nil)
  (setq use-package-always-demand nil)
  (setq use-package-expand-minimally nil)
  (setq use-package-enable-imenu-support t))

(eval-when-compile
  (require 'use-package))

;; https://bling.github.io/blog/2016/01/18/why-are-you-changing-gc-cons-threshold/
;;  While the minibuffer is open, garbage collection will never occur, but once
;;  make a selection, or cancel, garbage collection will kick off immediately
;;  and then revert back to the default, sensible behavior.
(use-package emacs
  :config
  (setq gc-cons-threshold-original gc-cons-threshold)
  (defun my/minibuffer-setup-hook ()
    (setq gc-cons-threshold most-positive-fixnum))
  (defun my/minibuffer-exit-hook ()
    (setq gc-cons-threshold gc-cons-threshold-original))
  ;; Warn when opening files bigger than 100MB.
  (setq large-file-warning-threshold 100000000)
  :hook
  ((minibuffer-setup . my/minibuffer-setup-hook)
   (minibuffer-exit . my/minibuffer-exit-hook)))

;; Set encoding to UTF-8 everywhere (just in case the OS does not use UTF-8).
(use-package emacs
  :config
  (setq prefer-coding-system 'utf-8)
  (setq set-default-coding-systems 'utf-8)
  (setq set-terminal-coding-system 'utf-8)
  (setq set-keyboard-coding-system 'utf-8))

;; The first running process of Emacs is started as server so Emacs clients can
;; connect to it. Calling emacsclient (with or without --create-frame), will
;; share the same buffer list and data as the original running process
;; (server). The server persists for as long as there is an Emacs frame attached
;; to it.
(use-package server
  :hook
  (after-init . server-start))

;; Save the state of Emacs from one session to another. Emacs will save the
;; desktop when it exits, the next time Emacs starts, it will restore the
;; desktop. "Desktop" is the state of the available buffers and the values of
;; each register.
(use-package desktop
  :config
  (setq desktop-dirname (file-name-directory user-emacs-directory))
  (setq desktop-base-file-name "desktop")
  (setq desktop-base-lock-name "desktop.lock")
  (setq desktop-auto-save-timeout 60)
  ;; Number of buffers to restore immediately.
  (setq desktop-restore-eager 10)
  ;; Do not restore frames TODO
  (setq desktop-restore-frames nil)
  ;; Regexp identifying files to be excluded from saving.
  (setq desktop-files-not-to-save nil)
  ;; List of global variables to clear.
  (setq desktop-globals-to-clear nil)
  ;; Load dektop file even if it's locked.
  (setq desktop-load-locked-desktop t)
  ;; Offer to recreate the buffer of a deleted file.
  (setq desktop-missing-file-warning t)
  ;; Ask to save the desktop file if no such file exists.
  (setq desktop-save 'ask-if-new)
  (desktop-save-mode t))

;; Disable some unused global keybindings.
(use-package emacs
  :config
  ;; Disable suspend-emacs.
  (global-unset-key (kbd "C-z"))
  (global-unset-key (kbd "C-x C-z"))
  ;; Disable view-hello-file.
  (global-unset-key (kbd "C-h h"))
  ;; Disable print buffer using a printer.
  (global-unset-key (kbd "s-p"))
  ;; Disable iconify-frame.
  (global-unset-key (kbd "s-m")))

;; Make UI minimal.
(use-package emacs
  :init
  (tool-bar-mode -1)
  (tooltip-mode -1)
  (menu-bar-mode -1)
  (blink-cursor-mode -1)
  (toggle-scroll-bar -1)
  (scroll-bar-mode -1)
  :config
  ;; Do not show annoying startup screen.
  (setq inhibit-startup-message t)
  (setq inhibit-splash-screen t)
  (setq use-file-dialog nil)
  ;; Allow mouse commands to use dialog boxes.
  (setq use-dialog-box t))

;; Generic feedback settings.
(use-package emacs
  :config
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
  ;; Yes == y, No == n.
  (defalias 'yes-or-no-p 'y-or-n-p)
  ;; Show full path of a file in the title bar.
  (setq frame-title-format
        '((:eval (if (buffer-file-name)
                     (abbreviate-file-name (buffer-file-name))
                   "%b"))))
  ;; Disable bell
  (setq ring-bell-function 'ignore))

;; Scrolling
(use-package emacs
  :config
  ;; Leave some lines at the top/botton of the page as margin while scrolling.
  (setq scroll-margin 5)
  ;; Keep cursor at the same position while scrolling.
  (setq scroll-preserve-screen-position t)
  (setq scroll-conservatively 1)

  (defun window-half-window-body-height ()
    "Return half of the height of WINDOW's test area."
    (/ (window-body-height) 2))

  ;; Scroll just half page down using C-v.
  (defun scroll-half-down ()
    "Scroll down half the page."
    (interactive)
    (scroll-down (window-half-window-body-height)))

  ;; Scroll just half page up using M-v.
  (defun scroll-half-up ()
    "Scroll up half the page."
    (interactive)
    (scroll-up (window-half-window-body-height)))

  (define-key global-map [remap scroll-down-command] 'scroll-half-down)
  (define-key global-map [remap scroll-up-command] 'scroll-half-up))

;; MacOS settings.
(when (equal system-type 'darwin)
  (setq mac-option-modifier 'super)
  (setq mac-command-modifier 'meta)
  ;; Maximase the emacs application window.
  (add-to-list 'default-frame-alist '(fullscreen . maximized))
  (setq initial-frame-alist
        (append
         '((ns-transparent-titlebar . t)
           (ns-appearance . dark)
           (vertical-scroll-bars . nil)
           (internal-border-width . 0)))))

;; Use system environment variables in Emacs. This is useful when Emacs is not
;; started by typing a shell command (clicking an icon on the screen).
(use-package exec-path-from-shell
  :ensure t
  :config
  (exec-path-from-shell-initialize))

;; Setup TAB behaviour.
(use-package emacs
  :config
  ;; TAB first tries to indent the current line, and if the line was already
  ;; indented, then try to complete the thing at point.
  (setq-default tab-always-indent 'complete)
  ;; Set tab width to 4 spaces.
  (setq-default tab-width 4)
  ;; Use spaces instead of tabs for indentation.
  (setq-default indent-tabs-mode nil))

;; M-w/C-w copies/kills the whole line if region is not active.
(use-package whole-line-or-region
  :ensure t
  :config
  (whole-line-or-region-global-mode t))

;; Expand/contract sexp.
(use-package expand-region
  :ensure t
  :bind
  (("C-." . er/expand-region)
   ("C-," . er/contract-region)))
