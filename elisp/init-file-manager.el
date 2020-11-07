;;; Code:

(defconst my/ls-args
  "-AGFhlv --group-directories-first --time-style=long-iso")

(use-package dired
  :config
  (setq dired-recursive-copies 'always)
  (setq dired-recursive-deletes 'always)
  ;;dired-auto-revert-buffer t
  (setq dired-listing-switches my/ls-args)
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

;; Reuse the current dired buffer rather than create a new one. It
;; limits the number of dired buffers as they don't stay open for each
;; directory.
(use-package dired-single
  :ensure t
  :demand t
  :after
  (dired)
  :bind
  (:map dired-mode-map
        ([remap dired-up-directory] . dired-single-up-directory)
        ([remap dired-find-file] . dired-single-buffer)))

;; Lookup for file(s) using a regexp and show result in dired buffer.
;; M-x find-name-dired
(use-package find-dired
  :after
  (dired)
  :config
  (setq find-ls-option
        `(,(concat "-print0 | xargs -0 ls " my/ls-args) . ""))
  ;; Ignore case.
  (setq find-name-arg "-iname"))

;; Extra dired functionality (shipped with Emacs).
(use-package dired-x
  :after
  (dired)
  :config
  (setq dired-clean-confirm-killing-deleted-buffers t)
  ;; Do not bind a key to `dired-man'.
  (setq dired-bind-man nil)
  ;; Do not bind a key to `dired-info'.
  (setq dired-bind-info nil)
  :bind
  (("C-x C-j" . dired-jump)
   ("s-j" . dired-jump)
   ("C-x 4 C-j" . dired-jump-other-window)
   ("s-J" . dired-jump-other-window)))

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
        ("/" . dired-narrow-regexp)))

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

;; Enable file previews.
(use-package peep-dired
  :ensure t
  :after
  (dired)
  :config
  (setq peep-dired-cleanup-on-disable t)
  (setq peep-dired-cleanup-eagerly t)
  (setq peep-dired-enable-on-directories nil)
  (setq peep-dired-ignored-extensions
        '("mkv" "webm" "mp4" "mp3" "ogg" "iso"))
  :bind
  (:map dired-mode-map
        ("P" . peep-dired)))

;; Show images in a directory as thumbnails.
;; M-x image-dired
(use-package image-dired
  :commands
  (image-dired)
  :config
  (setq image-dired-external-viewer "open")
  (setq image-dired-thumb-size 80)
  (setq image-dired-thumb-margin 2)
  (setq image-dired-thumb-relief 0)
  (setq image-dired-thumbs-per-row 4)
  :bind
  (:map image-dired-thumbnail-mode-map
        ("<return>" . image-dired-thumbnail-display-external)))

(provide 'init-file-manager)

;;; init-file-manager ends here
