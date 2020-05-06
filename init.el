;;; Code:

(when (version< emacs-version "27.0.90")
  (error "Emacs 27.0.90 or newer versions are required!"))

(defvar gc-cons-threshold-default
  (if (display-graphic-p) 16000000 1600000)
  "The default value for `gc-cons-threshold'.")

(defvar gc-cons-threshold-upper-limit
  (if (display-graphic-p) 128000000 32000000)
  "The upper limit value for `gc-cons-threshold' to defer it.")

(defun my/setup-default-startup-values ()
  (setq gc-cons-threshold gc-cons-threshold-default)
  (setq gc-cons-percentage gc-cons-percentage-default)
  (setq file-name-handler-alist file-name-handler-alist-default)
  (makunbound 'gc-cons-percentage-default)
  (makunbound 'file-name-handler-alist-default))

;; https://bling.github.io/blog/2016/01/18/why-are-you-changing-gc-cons-threshold/
(defun my/minibuffer-setup-hook ()
  (setq gc-cons-threshold gc-cons-threshold-upper-limit))

(defun my/minibuffer-exit-hook ()
  (setq gc-cons-threshold gc-cons-threshold-default))

(defun my/garbage-collect-when-minibuffer-exit ()
  (add-hook 'minibuffer-setup-hook #'my/minibuffer-setup-hook)
  (add-hook 'minibuffer-exit-hook #'my/minibuffer-exit-hook))

(defun my/garbage-collect-when-unfocused ()
  (if (boundp 'after-focus-change-function)
      (add-function :after after-focus-change-function
                    (lambda ()
                      (unless (frame-focus-state)
                        (garbage-collect))))
    (add-hook 'after-focus-change-function #'garbage-collect)))

(add-hook 'emacs-startup-hook #'my/setup-default-startup-values)
(add-hook 'emacs-startup-hook #'my/garbage-collect-when-minibuffer-exit)
(add-hook 'emacs-startup-hook #'my/garbage-collect-when-unfocused)

;; Add elisp directory and its subdirectories to the load path.
(defun my/add-to-load-path (directory)
  "Add DIRECTORY and its subdirectories to `load-path'."
  (let ((base directory))
    (unless (member base load-path)
      (add-to-list 'load-path base))
    (dolist (f (directory-files base))
      (let ((name (concat base "/" f)))
        (when (and (file-directory-p name)
                   (not (equal f ".."))
                   (not (equal f ".")))
          (unless (member base load-path)
            (add-to-list 'load-path name)))))))

(my/add-to-load-path (expand-file-name "elisp" user-emacs-directory))

;; Packages
(require 'init-package)

;; Global
(require 'init-global)

(require 'init-edit)

(require 'init-icomplete)

(require 'init-ui)

(require 'init-highlight)

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

  :bind
  (([remap scroll-down-command] . scroll-half-down)
   ([remap scroll-up-command] . scroll-half-up)))


