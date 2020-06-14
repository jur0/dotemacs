;;; Code:

(require 'init-const)

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

;; Ripgrep is used instead of grep as it is faster especially for large files
;; with source code.
(use-package rg
  :if exec/rg
  :ensure t
  :after
  (wgrep)
  :config
  ;; Group matches in the same file together.
  (setq rg-group-result t)
  ;; Hide most of rg command line.
  (setq rg-hide-command t)
  (setq rg-show-columns nil)
  (setq rg-show-header t)
  (setq rg-custom-type-aliases nil)
  (setq rg-default-alias-fallback "all")

  (rg-define-search my/rg-vc-or-dir
    "RipGrep in project root or present directory."
    :query ask
    :format regexp
    :files "everything"
    ;; Search root project dir or the current directory.
    :dir (let ((vc (vc-root-dir)))
           (if vc
               vc
             default-directory))
    :confirm prefix
    :flags ("--hidden -g !.git"))

  (rg-define-search my/rg-ref-in-dir
    "RipGrep for thing at point in present directory."
    :query point
    :format regexp
    :files "everything"
    :dir default-directory
    :confirm prefix
    :flags ("--hidden -g !.git"))

  (defun my/rg-save-search-as-name ()
    "Save `rg' buffer, naming it after the current search query.

This function is meant to be mapped to a key in `rg-mode-map'."
    (interactive)
    (let ((pattern (car rg-pattern-history)))
      (rg-save-search-as-name (concat "«" pattern "»"))))

  :bind
  (("M-s g" . my/rg-vc-or-dir)
   ("M-s r" . my/rg-ref-in-dir)
   (:map rg-mode-map
         ("s" . my/rg-save-search-as-name)
         ("C-n" . next-line)
         ("C-p" . previous-line)
         ("M-n" . rg-next-file)
         ("M-p" . rg-prev-file))))

;; Functions for finding projects (version controlled directories) and files in
;; the current directory/project.
(use-package project
  :config
  (defun my/find-file-vc-or-dir (&optional arg)
    "Find file by name that belongs to the current project or dir.
With \\[universal-argument] match files by contents.  This
requires the command-line executable called 'rg' or 'ripgrep'."
    (interactive "P")
    (let* ((default-directory (file-name-directory
                               (or (locate-dominating-file "." ".git" )
                                   default-directory))))
      (if arg
          (let* ((regexp (read-regexp
                          (concat "File contents matching REGEXP in "
                                  (propertize default-directory 'face 'bold)
                                  ": ")))
                 (results (process-lines "rg" "-l" "--hidden" "-m" "1" "-M" "120" regexp)))
            (find-file
             (icomplete-vertical-do ()
               (completing-read (concat
                                 "Files with contents matching "
                                 (propertize regexp 'face 'success)
                                 (format " (%s)" (length results))
                                 ": ")
                                results nil t))))
        (let* ((filenames-all (directory-files-recursively default-directory ".*" nil t))
               (filenames (cl-remove-if (lambda (x)
                                          (string-match-p "\\.git" x))
                                        filenames-all)))
          (icomplete-vertical-do ()
            (find-file
             (completing-read "Find file recursively: " filenames nil t)))))))

  (defun my/find-project (&optional arg)
    "Switch to sub-directory at the specified locations.
With \\[universal-argument] produce a `dired' buffer instead with
all the possible candidates."
    (interactive "P")
    ;; TODO: define a constant with project dirs.
    (let* ((dirs (list "~/data/project/github"))
           (dotless directory-files-no-dot-files-regexp)
           (cands (mapcan (lambda (d)
                            (directory-files d t dotless))
                          dirs))
           (projects (mapcar 'abbreviate-file-name cands))
           (buf "*Projects Dired*"))
      (if arg
          (dired (cons (generate-new-buffer-name buf) projects))
        (icomplete-vertical-do ()
          (dired
           (completing-read "Find project: " projects nil t))))))

  :bind
  (("M-s f" . my/find-file-vc-or-dir)
   ("M-s p" . my/find-project)
   ("M-s l" . find-library)))

(provide 'init-search)

;;; init-search.el ends here
