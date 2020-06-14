;;; Code:

;; Determine the major mode to use for files that do not specify a
;; major mode.
(setq-default major-mode 'text-mode)

;; Set default line length.
(setq-default fill-column 80)
;; Determine a paragraph's fill prefix from its text (bulleted and numbered
;; lists, where it recognises the text's prefix).
(setq adaptive-fill-mode t)
;; Keep paragraphs witbin `fill-column' length. This affects text mode and all
;; major modes derived from it.
(add-hook 'text-mode-hook #'turn-on-auto-fill)

;; Set default tab width in spaces.
(setq-default tab-width 4)

;; Permanently indent with spaces, never with TABs.
(setq-default indent-tabs-mode nil)

;; Tab first tries to indent the current line, and if the line was already
;; indented, then try to complete the thing at point (check `init-completion'
;; and company's binding of `indent-for-tab-command').
(setq-default tab-always-indent 'complete)

;; Add a newline automatically at the end of the file upon save.
(setq require-final-newline t)

;; Delete files by moving them into OS's trash directory.
(setq delete-by-moving-to-trash t)

;; https://stackoverflow.com/questions/3533703/emacs-delete-trailing-whitespace-except-current-line
;; Delete trailing whitespace expect the current line.  It's annoying when the
;; last char on the line is a whitespace and when switching to a browser
;; triggers save which also removes this whitespace (using `super-save').
(defun my/delete-trailing-whitespace-except-current-line ()
  "Delete trailing whitespace in the buffer expect the current line."
  (interactive)
  (let ((begin (line-beginning-position))
        (end (line-end-position)))
    (save-excursion
      (when (< (point-min) begin)
        (save-restriction
          (narrow-to-region (point-min) (1- begin))
          (delete-trailing-whitespace)))
      (when (> (point-max) end)
        (save-restriction
          (narrow-to-region (1+ end) (point-max))
          (delete-trailing-whitespace))))))

;; Delete trailing whitespace before saving buffer.
(add-hook 'before-save-hook #'my/delete-trailing-whitespace-except-current-line)

;; Automatically revert buffers when files on disk change.
(use-package autorevert
  :config
  (setq auto-revert-verbose t)
  ;; Enable autorevert on Dired buffers.
  (setq global-auto-revert-non-file-buffers t)
  (global-auto-revert-mode t))

;; Delete selection if you insert.
(use-package delsel
  :config
  (delete-selection-mode t))

;; Move back to the last change.
(use-package goto-last-change
  :ensure t
  :bind
  ("C-z" . goto-last-change))

;; Change the behaviour of M-< and M-> to move to the first/last actionable
;; point in a buffer (DWIM style).
(use-package beginend
  :ensure t
  :demand t
  :config
  (beginend-global-mode t))

;; Change the behaviour of C-a and C-e to move to the beginning/end of line or
;; code.
(use-package mwim
  :ensure t
  :bind
  (([remap move-beginning-of-line] . mwim-beginning-of-code-or-line)
   ([remap move-end-of-line] . mwim-end-of-code-or-line)))

;; M-w/C-w copies/kills the whole line if region is not active.
(use-package whole-line-or-region
  :ensure t
  :config
  (unbind-key "s-v" whole-line-or-region-local-mode-map)
  (unbind-key "s-x" whole-line-or-region-local-mode-map)
  (whole-line-or-region-global-mode t))

;; Expand/contract sexp.
(use-package expand-region
  :ensure t
  :bind
  (("C-." . er/expand-region)
   ("C-," . er/contract-region)))

;; Delete all whitespace characters until the next non-whitespace character.
(use-package hungry-delete
  :ensure t
  :config
  (setq-default hungry-delete-chars-to-skip " \t\f\v")
  (global-hungry-delete-mode t))

;; Preview when `goto-char'.
(use-package goto-char-preview
  :ensure t
  :bind
  ([remap goto-char] . goto-char-preview))

(provide 'init-edit)
