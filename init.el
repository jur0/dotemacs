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
  (setq use-package-always-ensure nil
        use-package-always-defer nil
        use-package-always-demand nil
        use-package-expand-minimally nil
        use-package-enable-imenu-support t))

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
  (setq prefer-coding-system 'utf-8
        set-default-coding-systems 'utf-8
        set-terminal-coding-system 'utf-8
        set-keyboard-coding-system 'utf-8))

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
  (setq desktop-dirname (file-name-directory user-emacs-directory)
        desktop-base-file-name "desktop"
        desktop-base-lock-name "desktop.lock"
        desktop-auto-save-timeout 60
        ;; Number of buffers to restore immediately.
        desktop-restore-eager 10
        ;; Do not restore frames TODO
        desktop-restore-frames nil
        ;; Regexp identifying files to be excluded from saving.
        desktop-files-not-to-save nil
        ;; List of global variables to clear.
        desktop-globals-to-clear nil
        ;; Load dektop file even if it's locked.
        desktop-load-locked-desktop t
        ;; Offer to recreate the buffer of a deleted file.
        desktop-missing-file-warning t
        ;; Ask to save the desktop file if no such file exists.
        desktop-save 'ask-if-new)
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
  (setq inhibit-startup-message t
        inhibit-splash-screen t
        use-file-dialog nil
        ;; Allow mouse commands to use dialog boxes.
        use-dialog-box t))

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
