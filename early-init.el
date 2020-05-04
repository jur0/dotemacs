;;; Code:

;; Defer garbage collection further back in the startup process.
(setq gc-cons-threshold most-positive-fixnum)

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

