;;; Code:

(when (version< emacs-version "27.1")
  (error "Emacs 27.1 or newer versions are required!"))

(defvar gc-cons-threshold-default
  (if (display-graphic-p) 16000000 1600000)
  "The default value for `gc-cons-threshold'.")

(defvar gc-cons-threshold-upper-limit
  (if (display-graphic-p) 128000000 32000000)
  "The upper limit value for `gc-cons-threshold' to defer it.")

(defun my/setup-default-startup-values ()
  "Setup default startup values."
  (setq gc-cons-threshold gc-cons-threshold-default)
  (setq gc-cons-percentage gc-cons-percentage-default)
  (setq file-name-handler-alist file-name-handler-alist-default)
  (makunbound 'gc-cons-percentage-default)
  (makunbound 'file-name-handler-alist-default))

;; https://bling.github.io/blog/2016/01/18/why-are-you-changing-gc-cons-threshold/
(defun my/minibuffer-setup-hook ()
  "Setup `gc-cons-threshold' to a large number.

When minibuffer is open, garbage collection never occurs so there
is no freezing."
  (setq gc-cons-threshold gc-cons-threshold-upper-limit))

(defun my/minibuffer-exit-hook ()
  "Setup `gc-cons-threshold' to a small number.

When a selection is made or minibuffer is cancelled, garbage
collection kicks off."
  (setq gc-cons-threshold gc-cons-threshold-default))

(defun my/garbage-collect-when-minibuffer-exit ()
  "Setup setup and exit hooks for minibuffer."
  (add-hook 'minibuffer-setup-hook #'my/minibuffer-setup-hook)
  (add-hook 'minibuffer-exit-hook #'my/minibuffer-exit-hook))

(defun my/garbage-collect-when-unfocused ()
  "Setup garbage collection when unfocused."
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

(require 'init-font)

(require 'init-global)

(require 'init-window)

(require 'init-edit)

(require 'init-search)

(require 'init-completion)

(require 'init-calendar)

(require 'init-workspace)

(require 'init-help)

(require 'init-backup)

(require 'init-ui)

(require 'init-linter)

(require 'init-spell)

(require 'init-password)

(require 'init-file-manager)

(require 'init-buffer)

(require 'init-highlight)

(require 'init-scroll)

(require 'init-org)

(require 'init-vc)

(require 'init-os)

(require 'init-lsp)

(require 'init-rust)

(provide 'init)

;;; init.el ends here
