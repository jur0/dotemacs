;;; Code:

(defvar gc-cons-percentage-default gc-cons-percentage
  "The default value for `gc-cons-percentage'.")

(defvar file-name-handler-alist-default file-name-handler-alist
  "The default value for `file-name-handler-alist'.")

;; Defer garbage collection further back in the startup process.
(setq gc-cons-threshold most-positive-fixnum)
(setq gc-cons-percentage 0.5)

;; For every .el and .elc file loaded during start up, Emacs runs those regexps
;; against it.
(setq file-name-handler-alist nil)

;; Do not load site-wide runtime initializations.
(setq site-run-file nil)

;; Do not initialise the package manager. This is done later.
(setq package-enable-at-startup nil)

;; Do not resize the frame at this early stage.
(setq frame-inhibit-implied-resize t)

;; Faster to disable these here (before they've been initialized).
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
(when (featurep 'ns)
  (push '(ns-transparent-titlebar . t) default-frame-alist))

(provide 'early-init)

;;; early-init.el ends here
