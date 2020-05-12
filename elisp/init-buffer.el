;;; Code:

;; Make buffers with same names unique by showing their paths to distinguish
;; them.
(use-package uniquify
  :config
  (setq uniquify-buffer-name-style 'post-forward-angle-brackets)
  (setq uniquify-strip-common-suffix t)
  (setq uniquify-after-kill-buffer-p t))

;; Show list of buffers in similar way to dired.
(use-package ibuffer
  :config
  ;; Do not ask for confirmation of "dangerous" operations.
  (setq ibuffer-expert t)
  (setq ibuffer-display-summary nil)
  (setq ibuffer-use-other-window nil)
  (setq ibuffer-show-empty-filter-groups nil)
  (setq ibuffer-movement-cycle nil)
  (setq ibuffer-default-sorting-mode 'filename/process)
  ;; Display a header line with the current filters.
  (setq ibuffer-use-header-line t)
  (setq ibuffer-default-shrink-to-minimum-size nil)
  ;; Format of buffer list (columns).
  (setq ibuffer-formats
        '((mark modified read-only locked " "
                (name 30 30 :left :elide)
                " "
                (size 9 -1 :right)
                " "
                (mode 16 16 :left :elide)
                " " filename-and-process)
          (mark " "
                (name 16 -1)
                " " filename)))
  (setq ibuffer-saved-filter-groups nil)
  :bind
  (("C-x C-b" . ibuffer)
   :map ibuffer-mode-map
   ("* f" . ibuffer-mark-by-file-name-regexp)
   ;; "g" is for "grep.
   ("* g" . ibuffer-mark-by-content-regexp)
   ("* n" . ibuffer-mark-by-name-regexp)
   ;; "sort name"
   ("s n" . ibuffer-do-sort-by-alphabetic)
   ("/ g" . ibuffer-filter-by-content)))

(provide 'init-buffer)
