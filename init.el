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
  :hook
  ((minibuffer-setup . my/minibuffer-setup-hook)
   (minibuffer-exit . my/minibuffer-exit-hook)))

;; Warn when opening files bigger than 100MB.
(setq large-file-warning-threshold 100000000)

;; Set encoding to UTF-8 everywhere (just in case the OS does not use UTF-8).
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

;; The first running process of Emacs is started as server so Emacs clients can
;; connect to it. Calling emacsclient (with or without --create-frame), will
;; share the same buffer list and data as the original running process
;; (server). The server persists for as long as there is an Emacs frame attached
;; to it.
(use-package server
  :hook
  (after-init . server-start))
