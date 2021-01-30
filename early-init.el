;;; Code:

(defvar gc-cons-percentage-default gc-cons-percentage
  "The default value for `gc-cons-percentage'.")

(defvar file-name-handler-alist-default file-name-handler-alist
  "The default value for `file-name-handler-alist'.")

;; Defer garbage collection further back in the startup process.
(setq gc-cons-threshold most-positive-fixnum)
(setq gc-cons-percentage 0.5)

;; For every .el and .elc file loaded during start up, Emacs runs those
;; regexps against it.
(setq file-name-handler-alist nil)

;; Do not load site-wide runtime initializations.
(setq site-run-file nil)

;; Do not initialise the package manager.  This is done later.
(setq package-enable-at-startup nil)

;; Do not resize the frame at this early stage.
(setq frame-inhibit-implied-resize t)

;; Faster to disable GUI elements here (before they've been
;; initialized).
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(tooltip-mode -1)
(setq use-file-dialog nil)
(setq use-dialog-box nil)

(setq inhibit-startup-screen t)

(provide 'early-init)

;;; early-init.el ends here
