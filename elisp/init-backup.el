;;; Code:

;; Save the state of Emacs from one session to another. Emacs will save the
;; desktop when it exits, the next time Emacs starts, it will restore the
;; desktop. "Desktop" is the state of the available buffers and the values of
;; each register.
(use-package desktop
  :config
  (setq desktop-path (list user-emacs-directory))
  (setq desktop-dirname user-emacs-directory)
  (setq desktop-base-file-name "desktop")
  (setq desktop-base-lock-name "desktop.lock")
  (setq desktop-auto-save-timeout 60)
  ;; Number of buffers to restore immediately.
  (setq desktop-restore-eager 10)
  ;; Restore frames and windows/workspaces (with their positions etc.) -
  ;; works well with `eyebrowse' (init-workspace.el).  Alternatively,
  ;; there is `C-x r f' to store frame/window layout into a register, it
  ;; can be read using `C-x r j'.
  ;; Also, there is `winner-mode'.
  (setq desktop-restore-frames t)
  ;; Regexp identifying files to be excluded from saving.
  (setq desktop-files-not-to-save nil)
  ;; List of global variables to clear.
  (setq desktop-globals-to-clear nil)
  ;; Load dektop file even if it's locked.
  (setq desktop-load-locked-desktop t)
  ;; Offer to recreate the buffer of a deleted file.
  (setq desktop-missing-file-warning t)
  ;; Ask to save the desktop file if no such file exists.
  (setq desktop-save 'ask-if-new)
  (desktop-save-mode t))

;; Save minibuffer history.
(use-package savehist
  :config
  (setq savehist-file (expand-file-name "savehist" user-emacs-directory))
  ;; The maximum length of a minibuffer history list. Once reached, the oldest
  ;; entries get deleted.
  (setq history-length 10000)
  ;; Keep duplicates in the history.
  (setq history-delete-duplicates nil)
  ;; Save every minute.
  (setq savehist-autosave-interval 60)
  ;; Save search entries as well.
  (setq savehist-additional-variables '(search-ring regexp-search-ring))
  (setq savehist-save-minibuffer-history t)
  (savehist-mode t))

;; Save point position in buffer.
(use-package saveplace
  :config
  (setq save-place-file (expand-file-name "saveplace" user-emacs-directory))
  (setq save-place-forget-unreadable-files t)
  (save-place-mode t))

;; Make backups of files.
(setq backup-directory-alist
      `(("." . ,(expand-file-name "file-backup" user-emacs-directory))))
(setq make-backup-files t)
;; Backup also VC controlled files.
(setq vc-make-backup-files t)
(setq backup-by-copying t)
(setq version-control t)
(setq delete-old-versions t)
(setq kept-old-versions 6)
(setq kept-new-versions 9)

;; Auto-save files use hashmarks (#) and shall be written locally within the
;; project directory (along with the actual files). The reason is that auto-save
;; files are just temporary files that Emacs creates until a file is saved
;; again.  Auto-saves are created whenever Emacs crashes, including killing the
;; Emacs job with a shell command.
;; (defconst auto-save-file-directory
;;   (file-name-as-directory
;;    (expand-file-name "auto-save-file" user-emacs-directory)))

;; (unless (file-exists-p auto-save-file-directory)
;;   (make-directory auto-save-file-directory t))

;; (setq auto-save-file-name-transforms
;; (setq `((".*" ,auto-save-file-directory t)))
;; (setq auto-save-default t)
;; (setq auto-save-timeout 20)
;; (setq auto-save-interval 200)

;; Disable auto-save, super-save is used instead.
(setq auto-save-default nil)

;; Super-save auto-saves buffers, when certain events happen - e.g. switch
;; between buffers, an Emacs frame loses focus, etc. It's both something that
;; augments and replaces the standard auto-save-mode.
(use-package super-save
  :ensure t
  :config
  ;; Switch off the default auto-save-mode.
  (setq super-save-auto-save-when-idle t)
  (setq super-save-idle-duration 15)
  (setq super-save-hook-triggers nil)
  (setq super-save-remote-files nil)
  (super-save-mode t))

(use-package recentf
  :init
  (setq recentf-save-file (expand-file-name "recentf" user-emacs-directory))
  (setq recentf-max-saved-items 100)
  ;; Disable recentf-cleanup on Emacs start, because it can cause problems with
  ;; remote files.
  ;; recentf-auto-cleanup 'never
  (setq recentf-exclude
        '(".cache"
          ".cask"
          "bookmarks"
          "cache"
          "recentf"
          "undo-tree-hist"
          "url"
          "COMMIT_EDITMSG\\'"
          "/ssh:"
          "/sudo:"
          "\\.\\(?:gz\\|gif\\|svg\\|png\\|jpe?g\\|bmp\\|xpm\\|zip\\|xz\\)$"
          "^/tmp/"
          (lambda (file) (file-in-directory-p file package-user-dir))))
  :config
  (defun my/recentf ()
    "Select item from `recentf-list' using completion.

The user's $HOME directory is abbreviated as a tilde."
    (interactive)
    (icomplete-vertical-do ()
      (let ((files (mapcar 'abbreviate-file-name recentf-list)))
        (find-file
         (completing-read "Open recentf entry: " files nil t)))))
  :hook
  (after-init . recentf-mode)
  :bind
  (("s-r" . my/recentf)
   ("C-x C-r" . my/recentf)))

;; Undo and redo. The changes of buffer are also saved to a file, so this works
;; between Emacs restarts as well.
(use-package undo-tree
  :ensure t
  :init
  (setq undo-tree-history-directory-alist
        `((".*" . ,(expand-file-name "undo-tree" user-emacs-directory))))
  (setq undo-tree-visualizer-timestamps t)
  (setq undo-tree-visualizer-diff t)
  (setq undo-tree-auto-save-history t)
  :config
  (global-undo-tree-mode t))

(provide 'init-backup)

;;; init-backup.el ends here
