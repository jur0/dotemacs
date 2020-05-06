;;; Code:

(require 'init-const)

;; Set UTF-8 as the default coding system.
(when (fboundp 'set-charset-priority)
  (set-charset-priority 'unicode))

;; Explicitly set the prefered coding systems (UTF-8).
(prefer-coding-system 'utf-8)
(setq locale-coding-system 'utf-8)

(set-language-environment 'utf-8)
(set-default-coding-systems 'utf-8)
(set-buffer-file-coding-system 'utf-8)
(set-clipboard-coding-system 'utf-8)
(set-file-name-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(modify-coding-system-alist 'process "*" 'utf-8)

;; Disable some unused global keybindings.
;; Disable suspend-emacs.
(global-unset-key (kbd "C-z"))
(global-unset-key (kbd "C-x C-z"))
;; Disable view-hello-file.
(global-unset-key (kbd "C-h h"))
;; Disable print buffer using a printer.
(global-unset-key (kbd "s-p"))
;; Disable iconify-frame.
(global-unset-key (kbd "s-m"))

;; Environment (PATH and MANPATH) for MacOS and Linux. It's useful when Emacs is
;; not started from the console, but rather from GUI i.e. clicking on icon.
(when (or sys/mac-cocoa-p sys/linux-x-p sys/mac-x-p)
  (use-package exec-path-from-shell
    :ensure t
    :init
    (setq exec-path-from-shell-check-startup-files nil)
    (setq exec-path-from-shell-variables '("PATH" "MANPATH"))
    (setq exec-path-from-shell-arguments '("-l"))
    (exec-path-from-shell-initialize)))

;; The first running process of Emacs is started as server so Emacs clients can
;; connect to it. Calling emacsclient (with or without --create-frame), will
;; share the same buffer list and data as the original running process
;; (server). The server persists for as long as there is an Emacs frame attached
;; to it.
(use-package server
  :hook
  (after-init . server-mode))

;; Setup super and meta keys for different operating systems.
(cond (sys/mac-cocoa-p
       (setq mac-option-modifier 'super)
       (setq mac-command-modifier 'meta))
      (t nil))

;; Do not compact font caches during garbage collection.
(setq inhibit-compacting-font-caches t)

(provide 'init-global)
