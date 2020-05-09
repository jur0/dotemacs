;;; Code:

(use-package dired
  :config
  (setq dired-recursive-copies 'always)
  (setq dired-recursive-deletes 'always)
  ;;dired-auto-revert-buffer t
  (setq dired-listing-switches
        "-AGFhlv --group-directories-first --time-style=long-iso")
  ;; If there in another dired buffer, use its current directory as target.
  (setq dired-dwim-target t)
  :hook
  (dired-mode . dired-hide-details-mode))

;; Parts of dired mode that are not used normally.
(use-package dired-aux
  :config
  ;; Isearch matches file names when initial point position is on a file,
  ;; otherwise it searches the whole buffer.
  (setq dired-isearch-filenames 'dwim)
  (setq dired-create-destination-dirs 'ask)
  (setq dired-vc-rename-file t)
  :bind
  (:map dired-mode-map
        ("C-u +" . dired-create-empty-file)
        ("M-s f" . nil)))

;; Lookup for file(s) using a regexp and show result in dired buffer.
;; M-x find-name-dired
(use-package find-dired
  :after
  (dired)
  :config
  (setq find-ls-option
        '("-ls" . "-AGFhlv --group-directories-first --time-style=long-iso"))
  ;; Ignore case.
  (setq find-name-arg "-iname"))

;; Adds ability to call async functions.
(use-package async
  :ensure t)

;; Operations on files can take some time, so they are performed asynchronously.
(use-package dired-async
  :after
  (dired async)
  :hook
  (dired-mode . dired-async-mode))

;; Dynamic filtering of list of files.
(use-package dired-narrow
  :ensure t
  :after
  (dired)
  :config
  ;; Exit when just one file is left.
  (setq dired-narrow-exit-when-one-left t)
  (setq dired-narrow-enable-blinking t)
  (setq dired-narrow-blink-time 0.3)
  :bind
  (:map dired-mode-map
        ("M-s n" . dired-narrow)))

;; Writable dired, activated by C-x C-q.
(use-package wdired
  :after
  (dired)
  :commands
  (wdired-change-to-wdired-mode)
  :config
  (setq wdired-allow-to-change-permissions t)
  ;; Forward slash is treated as a directory.
  (setq wdired-create-parent-directories t))

;; Show path to a file if directories on the path are empty.
(use-package dired-collapse
  :ensure t
  :after
  (dired)
  :hook
  (dired-mode . dired-collapse-mode))

;; Make directories of a dired buffer expandable and form a tree structure.
(use-package dired-subtree
  :ensure t
  :after
  (dired)
  :config
  (setq dired-subtree-use-backgrounds nil)
  :bind
  (:map dired-mode-map
        ("<tab>" . dired-subtree-toggle)
        ("C-<tab>" . dired-subtree-cycle)))

;; Extra colours for dired especially in detailed view.
(use-package diredfl
  :ensure t
  :hook
  (dired-mode . diredfl-mode))

(provide 'init-dired)
