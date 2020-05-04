;;; Code:

;; Specify directory for storing packages.
(setq package-user-dir (expand-file-name "elpa" user-emacs-directory))
(setq package-archives
      '(("melpa" . "http://melpa.org/packages/")
        ;; ("org" . "http://orgmode.org/elpa/")
        ("gnu" . "http://elpa.gnu.org/packages/")))

(setq-default network-security-level 'high)
(setq-default tls-checktrust t)
(setq-default gnutls-verify-error t)

;; Initialise the packages, avoiding a re-initialisation.
(unless (bound-and-true-p package--initialized)
  ;; Prevent initializing twice.
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
  ;; More verbose messages from packages, could be set to t after the
  ;; configuration works.
  (setq use-package-expand-minimally nil)
  (setq use-package-enable-imenu-support t))

(eval-when-compile
  (require 'use-package)
  ;; Required by use-package for :bind.
  (require 'bind-key))

;; Required by `use-package' for :diminish.
(use-package diminish
  :ensure t)

(use-package auto-package-update
  :ensure t
  :custom
  ;; Update each 7 days.
  (auto-package-update-interval 7)
  ;; Ask before updating.
  (auto-package-update-prompt-before-update t)
  (auto-package-update-delete-old-versions t)
  ;; Show results of auto update.
  (auto-package-update-hide-results t)
  :config
  ;; Allow 'M-x upgrade-packages' command.
  (defalias 'upgrade-packages #'auto-package-update-now)
  ;; Check whether to update packages at startup.
  (auto-package-update-maybe))

(provide 'init-package)
