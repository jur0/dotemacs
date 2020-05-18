;;; Code:

;; TODO: write how to use Isearch.
(use-package isearch
  :config
  ;; Incremental search highlights the current match.
  (setq search-highlight t)
  ;; Regular expression to match a sequence of whitespace chars.
  (setq search-whitespace-regexp ".*?")
  ;; A space will match a sequence of whitespace chars.
  (setq isearch-lax-whitespace t)
  (setq isearch-regexp-lax-whitespace nil)
  (setq isearch-lazy-highlight t)
  ;; Show match numbers in the search prompt.
  (setq isearch-lazy-count t)
  (setq lazy-count-prefix-format nil)
  (setq lazy-count-suffix-format " (%s/%s)")
  ;; Extend the search string by motion commands while holding shift.
  (setq isearch-yank-on-move 'shift)
  ;; Allow unlimited scroll during incremental search.
  (setq isearch-allow-scroll 'unlimited)

  (defun my/isearch-mark-and-exit ()
    "Mark the current search string and exit the search."
    (interactive)
    (push-mark isearch-other-end t 'activate)
    (setq deactivate-mark nil)
    (isearch-done))

  (defun my/isearch-other-end ()
    "End current search in the opposite side of the match.
Particularly useful when the match does not fall within the
confines of word boundaries (e.g. multiple words)."
    (interactive)
    (isearch-done)
    (when isearch-other-end
      (goto-char isearch-other-end)))

  (defun my/isearch-abort ()
    "Remove non-matching `isearch' input, reverting to previous
successful search and continuing with the search.

This is a modified variant of the original `isearch-abort',
mapped to C-g which will remove the failed match if any and only
afterwards exit the search altogether."
    (interactive)
    (discard-input)
    (while (or (not isearch-success) isearch-error)
      (isearch-pop-state))
    (isearch-update))

  (defun my/isearch-query-replace-symbol-at-point ()
    "Run `query-replace-regexp' for the symbol at point."
    (interactive)
    (isearch-forward-symbol-at-point)
    (isearch-query-replace-regexp))

  :bind
  (("M-s M-o" . multi-occur)
   ("M-s %" . my/isearch-query-replace-symbol-at-point)
   :map minibuffer-local-isearch-map
   ("M-/" . isearch-complete-edit)
   :map isearch-mode-map
   ("M-/" . isearch-complete)
   ("C-SPC" . my/isearch-mark-and-exit)
   ("C-d" . my/isearch-abort)
   ("<C-return>" . my/isearch-other-end)))

;; Editable (writable) grep buffer. It possible to have a list of all search
;; matches in a grep/occur buffer and edit those matches using wgrep.
(use-package wgrep
  :ensure t
  :config
  (setq wgrep-auto-save-buffer t)
  (setq wgrep-change-readonly-file t))

;; Visual regexp builder that shows live preview (visually separates regexp
;; groups) and replacement. Build-in packages is `re-builder'.
(use-package visual-regexp
  :ensure
  :config
  (setq vr/default-replace-preview nil)
  (setq vr/match-separator-use-custom-face t))

(provide 'init-search)
;;; init-search.el ends here
