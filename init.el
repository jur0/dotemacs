;;; Code:

(when (version< emacs-version "27.0.90")
  (error "Emacs 27.0.90 or newer versions are required!"))

(defvar gc-cons-threshold-default
  (if (display-graphic-p) 16000000 1600000)
  "The default value for `gc-cons-threshold'.")

(defvar gc-cons-threshold-upper-limit
  (if (display-graphic-p) 128000000 32000000)
  "The upper limit value for `gc-cons-threshold' to defer it.")

(defun my/setup-default-startup-values ()
  (setq gc-cons-threshold gc-cons-threshold-default)
  (setq gc-cons-percentage gc-cons-percentage-default)
  (setq file-name-handler-alist file-name-handler-alist-default)
  (makunbound 'gc-cons-percentage-default)
  (makunbound 'file-name-handler-alist-default))

;; https://bling.github.io/blog/2016/01/18/why-are-you-changing-gc-cons-threshold/
(defun my/minibuffer-setup-hook ()
  (setq gc-cons-threshold gc-cons-threshold-upper-limit))

(defun my/minibuffer-exit-hook ()
  (setq gc-cons-threshold gc-cons-threshold-default))

(defun my/garbage-collect-when-minibuffer-exit ()
  (add-hook 'minibuffer-setup-hook #'my/minibuffer-setup-hook)
  (add-hook 'minibuffer-exit-hook #'my/minibuffer-exit-hook))

(defun my/garbage-collect-when-unfocused ()
  (if (boundp 'after-focus-change-function)
      (add-function :after after-focus-change-function
                    (lambda ()
                      (unless (frame-focus-state)
                        (garbage-collect))))
    (add-hook 'after-focus-change-function #'garbage-collect)))

(add-hook 'emacs-startup-hook #'my/setup-default-startup-values)
(add-hook 'emacs-startup-hook #'my/garbage-collect-when-minibuffer-exit)
(add-hook 'emacs-startup-hook #'my/garbage-collect-when-unfocused)

;; Add elisp directory and its subdirectories to the load path.
(defun my/add-to-load-path (directory)
  "Add DIRECTORY and its subdirectories to `load-path'."
  (let ((base directory))
    (unless (member base load-path)
      (add-to-list 'load-path base))
    (dolist (f (directory-files base))
      (let ((name (concat base "/" f)))
        (when (and (file-directory-p name)
                   (not (equal f ".."))
                   (not (equal f ".")))
          (unless (member base load-path)
            (add-to-list 'load-path name)))))))

(my/add-to-load-path (expand-file-name "elisp" user-emacs-directory))

;; Constants
(require 'init-const)

;; Packages
(require 'init-package)

;; Set encoding to UTF-8 everywhere (just in case the OS does not use UTF-8).
(use-package emacs
  :config
  (setq prefer-coding-system 'utf-8)
  (setq set-default-coding-systems 'utf-8)
  (setq set-terminal-coding-system 'utf-8)
  (setq set-keyboard-coding-system 'utf-8))

;; The first running process of Emacs is started as server so Emacs clients can
;; connect to it. Calling emacsclient (with or without --create-frame), will
;; share the same buffer list and data as the original running process
;; (server). The server persists for as long as there is an Emacs frame attached
;; to it.
(use-package server
  :hook
  (after-init . server-start))

;; Save the state of Emacs from one session to another. Emacs will save the
;; desktop when it exits, the next time Emacs starts, it will restore the
;; desktop. "Desktop" is the state of the available buffers and the values of
;; each register.
(use-package desktop
  :config
  (setq desktop-dirname (file-name-directory user-emacs-directory))
  (setq desktop-base-file-name "desktop")
  (setq desktop-base-lock-name "desktop.lock")
  (setq desktop-auto-save-timeout 60)
  ;; Number of buffers to restore immediately.
  (setq desktop-restore-eager 10)
  ;; Do not restore frames TODO
  (setq desktop-restore-frames nil)
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

;; Disable some unused global keybindings.
(use-package emacs
  :config
  ;; Disable suspend-emacs.
  (global-unset-key (kbd "C-z"))
  (global-unset-key (kbd "C-x C-z"))
  ;; Disable view-hello-file.
  (global-unset-key (kbd "C-h h"))
  ;; Disable print buffer using a printer.
  (global-unset-key (kbd "s-p"))
  ;; Disable iconify-frame.
  (global-unset-key (kbd "s-m")))

;; Make UI minimal.
(use-package emacs
  :init
  (tool-bar-mode -1)
  (tooltip-mode -1)
  (menu-bar-mode -1)
  (blink-cursor-mode -1)
  (toggle-scroll-bar -1)
  (scroll-bar-mode -1)
  :config
  ;; Do not show annoying startup screen.
  (setq inhibit-startup-message t)
  (setq inhibit-splash-screen t)
  (setq use-file-dialog nil)
  ;; Allow mouse commands to use dialog boxes.
  (setq use-dialog-box t))

;; Generic feedback settings.
(use-package emacs
  :config
  ;; Allow narrowing to region.
  (put 'narrow-to-region 'disabled nil)
  ;; Allow up/downcase region.
  (put 'upcase-region 'disabled nil)
  (put 'downcase-region 'disabled nil)
  ;; With `a' command in dired buffer the dired buffer is killed and the file or
  ;; directory on the current line is visited.
  (put 'dired-find-alternate-file 'disabled nil)
  ;; Disable overwrite-mode.
  (put 'overwrite-mode 'disabled t)
  ;; Yes == y, No == n.
  (defalias 'yes-or-no-p 'y-or-n-p)
  ;; Show full path of a file in the title bar.
  (setq frame-title-format
        '((:eval (if (buffer-file-name)
                     (abbreviate-file-name (buffer-file-name))
                   "%b"))))
  ;; Disable bell
  (setq ring-bell-function 'ignore))

;; Scrolling
(use-package emacs
  :config
  ;; Leave some lines at the top/botton of the page as margin while scrolling.
  (setq scroll-margin 5)
  ;; Keep cursor at the same position while scrolling.
  (setq scroll-preserve-screen-position t)
  (setq scroll-conservatively 1)

  (defun window-half-window-body-height ()
    "Return half of the height of WINDOW's test area."
    (/ (window-body-height) 2))

  ;; Scroll just half page down using C-v.
  (defun scroll-half-down ()
    "Scroll down half the page."
    (interactive)
    (scroll-down (window-half-window-body-height)))

  ;; Scroll just half page up using M-v.
  (defun scroll-half-up ()
    "Scroll up half the page."
    (interactive)
    (scroll-up (window-half-window-body-height)))

  :bind
  (([remap scroll-down-command] . scroll-half-down)
   ([remap scroll-up-command] . scroll-half-up)))

;; MacOS settings.
(when (equal system-type 'darwin)
  (setq mac-option-modifier 'super)
  (setq mac-command-modifier 'meta)
  ;; Maximase the emacs application window.
  (add-to-list 'default-frame-alist '(fullscreen . maximized))
  (setq initial-frame-alist
        (append
         '((ns-transparent-titlebar . t)
           (ns-appearance . dark)
           (vertical-scroll-bars . nil)
           (internal-border-width . 0)))))

;; Use system environment variables in Emacs. This is useful when Emacs is not
;; started by typing a shell command (clicking an icon on the screen).
(use-package exec-path-from-shell
  :ensure t
  :config
  (setq exec-path-from-shell-check-startup-files nil)
  (setq exec-path-from-shell-variables '("PATH" "MANPATH"))
  (setq exec-path-from-shell-arguments '("-l"))
  (exec-path-from-shell-initialize))

;; Setup TAB behaviour.
(use-package emacs
  :config
  ;; TAB first tries to indent the current line, and if the line was already
  ;; indented, then try to complete the thing at point.
  (setq-default tab-always-indent 'complete)
  ;; Set tab width to 4 spaces.
  (setq-default tab-width 4)
  ;; Use spaces instead of tabs for indentation.
  (setq-default indent-tabs-mode nil))

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

(use-package orderless
  :ensure t
  :init
  (icomplete-mode)
  :bind
  (:map minibuffer-local-completion-map
        ("SPC" . nil))
  :config
  (setq orderless-component-matching-styles
        '(orderless-regexp orderless-flex))
  (setq orderless-component-separator "[-_/\s]+"))

(use-package minibuffer
  :config
  (setq completion-styles
        '(basic partial-completion initials orderless))
  (setq completion-category-defaults nil)
  ;; Cycling is used if there aren't more candidates than this number.
  (setq completion-cycle-threshold 3)
  (setq completion-flex-nospace nil)
  (setq completion-pcm-complete-word-inserts-delimiters t)
  (setq completion-pcm-word-delimiters "-_/ ")
  (setq completion-show-help nil)
  (setq completion-ignore-case t)
  ;; Ignore case when reading a buffer name.
  (setq read-buffer-completion-ignore-case t)
  ;; Ignore case when reading a file name.
  (setq read-file-name-completion-ignore-case t)
  ;; *Completions* buffer.
  (setq completions-format 'vertical)
  ;; Start something in the minibuffer, switch to another window, call
  ;; minibuffer again, run commands and then move back to the original
  ;; minibuffer.
  (setq enable-recursive-minibuffers t)
  ;; Accept short answers to questions.
  (setq read-answer-short t)
  ;; Resize minibuffer and echo area to fit the text inside.
  (setq resize-mini-windows t)
  ;; Make "unimportant" part of filename in minibuffer visually less noticeable.
  (file-name-shadow-mode t)
  ;; Show recursion depth in minibuffer (related to
  ;; enable-recursive-minibuffers).
  (minibuffer-depth-indicate-mode t)
  ;; Show default value in minibuffer prompt only when it's applicable.
  (minibuffer-electric-default-mode t)

  (defun my/focus-minibuffer ()
    "Focus the active minibuffer.

Bind this to `completion-list-mode-map' to M-v to easily jump
between the list of candidates present in the \\*Completions\\*
buffer and the minibuffer (because by default M-v switches to the
completions if invoked from inside the minibuffer."
    (interactive)
    (let ((minibuffer (active-minibuffer-window)))
      (when minibuffer
        (select-window minibuffer))))

  (defun my/focus-minibuffer-or-completions ()
    "Focus the active minibuffer or the \\*Completions\\*.

If both the minibuffer and the Completions are present, this
command will first move per invocation to the former, then the
latter, and then continue to switch between the two.

The continuous switch is essentially the same as running
`my/focus-minibuffer' and `switch-to-completions' in
succession."
    (interactive)
    (let* ((minibuffer (active-minibuffer-window))
           (completions (get-buffer-window "*Completions*")))
      (cond ((and minibuffer
                  (not (minibufferp)))
             (select-window minibuffer nil))
            ((and completions
                  (not (eq (selected-window)
                           completions)))
             (select-window completions nil)))))

  (defun my/describe-symbol-at-point (&optional arg)
    "Get help (documentation) for the symbol at point.

With a prefix argument, switch to the *Help* window.  If that is
already focused, switch to the most recently used window
instead."
    (interactive "P")
    (let ((symbol (symbol-at-point)))
      (when symbol
        (describe-symbol symbol)))
    (when arg
      (let ((help (get-buffer-window "*Help*")))
        (when help
          (if (not (eq (selected-window) help))
              (select-window help)
            (select-window (get-mru-window)))))))

  (defun my/completions-kill-save-symbol ()
    "Add symbol-at-point to the kill ring.

Intended for use in the \\*Completions\\* buffer.  Bind this to a
key in `completion-list-mode-map'."
    (interactive)
    (kill-new (thing-at-point 'symbol)))

  :bind
  (("s-f" . find-file)
   ("s-F" . find-file-other-window)
   ("s-d" . dired)
   ("s-D" . dired-other-window)
   ("s-b" . switch-to-buffer)
   ("s-B" . switch-to-buffer-other-window)
   ("s-h" . my/describe-symbol-at-point)
   ("s-H" . (lambda ()
              (interactive)
              (my/describe-symbol-at-point '(4))))
   ("s-v" . my/focus-minibuffer-or-completions)
  (:map completion-list-mode-map
        ("n" . next-line)
        ("p" . previous-line)
        ("f" . next-completion)
        ("b" . previous-completion)
        ("h" . my/describe-symbol-at-point)
        ("w" . my/completions-kill-save-symbol)
        ("M-v" . my/focus-minibuffer))))

(use-package icomplete
  :demand t
  :after
  (minibuffer)
  :config
  (setq icomplete-delay-completions-threshold 100)
  (setq icomplete-max-delay-chars 2)
  (setq icomplete-compute-delay 0.2)
  (setq icomplete-show-matches-on-no-input t)
  ;; Hide common prefix from completion candidates.
  (setq icomplete-hide-common-prefix nil)
  ;; Max number of lines to use in minibuffer.
  (setq icomplete-prospects-height 1)
  ;; Candidate separator.
  (setq icomplete-separator (propertize " ┆ " 'face 'shadow))
  (setq icomplete-with-completion-tables t)
  ;; Use Icomplete in non-mini buffers.
  (setq icomplete-in-buffer t)
  (setq icomplete-tidy-shadowed-file-names nil)
  ;; Disable icomplete-mode act like ido.
  (fido-mode -1)
  (icomplete-mode t)

  (defun my/icomplete-kill-or-insert-candidate (&optional arg)
    "Place the matching candidate to the top of the `kill-ring'.

This will keep the minibuffer session active.

With \\[universal-argument] insert the candidate in the most
recently used buffer, while keeping focus on the minibuffer.

With \\[universal-argument] \\[universal-argument] insert the
candidate and immediately exit all recursive editing levels and
active minibuffers.

Bind this function in `icomplete-minibuffer-map'."
    (interactive "*P")
    (let ((candidate (car completion-all-sorted-completions)))
      (when (and (minibufferp)
                 (bound-and-true-p icomplete-mode))
        (cond ((eq arg nil)
               (kill-new candidate))
              ((= (prefix-numeric-value arg) 4)
               (with-minibuffer-selected-window (insert candidate)))
              ((= (prefix-numeric-value arg) 16)
               (with-minibuffer-selected-window (insert candidate))
               (top-level))))))

  (defun my/icomplete-minibuffer-truncate ()
    "Truncate minibuffer lines in `icomplete-mode'.

This should only affect the horizontal layout and is meant to
enforce `icomplete-prospects-height' being set to 1.

Hook it to `icomplete-minibuffer-setup-hook'."
    (when (and (minibufferp)
               (bound-and-true-p icomplete-mode))
      (setq truncate-lines t)))

  :hook
  (icomplete-minibuffer-setup-hook . my/icomplete-minibuffer-truncate)
  :bind
  (:map icomplete-minibuffer-map
        ("<tab>" . icomplete-force-complete)
        ;; Expand the current candidate and exit.
        ("<return>" . icomplete-force-complete-and-exit)
        ;; Insert exactly what is in the minibuffer and exit (no
        ;; expanding).
        ("C-j" . exit-minibuffer)
        ("C-n" . icomplete-forward-completions)
        ("<right>" . icomplete-forward-completions)
        ("<down>" . icomplete-forward-completions)
        ("C-p" . icomplete-backward-completions)
        ("<left>" . icomplete-backward-completions)
        ("<up>" . icomplete-backward-completions)
        ("<C-backspace>" . icomplete-fido-backward-updir)
        ("M-o w" . my/icomplete-kill-or-insert-candidate)
        ("M-o i" . (lambda ()
                     (interactive)
                     (my/icomplete-kill-or-insert-candidate '(4))))
        ("M-o j" . (lambda ()
                     (interactive)
                     (my/icomplete-kill-or-insert-candidate '(16))))))

(use-package icomplete-vertical
  :ensure t
  :demand t
  :after
  (minibuffer icomplete)
  :config
  (setq icomplete-vertical-prospects-height (/ (window-height) 6))
  (icomplete-vertical-mode -1)

  (defun my/icomplete-recentf ()
    "Open `recent-list' item in a new buffer.

The user's $HOME directory is abbreviated as a tilde."
    (interactive)
    (icomplete-vertical-do ()
      (let ((files (mapcar 'abbreviate-file-name recentf-list)))
        (find-file
         (completing-read "Open recentf entry: " files nil t)))))

  (defun my/icomplete-font-family-list ()
    "Add item from the `font-family-list' to the `kill-ring'.

This allows you to save the name of a font, which can then be
used in commands such as `set-frame-font'."
    (interactive)
    (icomplete-vertical-do ()
      (kill-new
       (completing-read "Copy font family: "
                        (print (font-family-list))
                        nil t))))

  (defun my/icomplete-yank-kill-ring ()
    "Insert the selected `kill-ring' item directly at point.
When region is active, `delete-region'.

Sorting of the `kill-ring' is disabled.  Items appear as they
normally would when calling `yank' followed by `yank-pop'."
    (interactive)
    (let ((kills                    ; do not sort items
           (lambda (string pred action)
             (if (eq action 'metadata)
                 '(metadata (display-sort-function . identity)
                            (cycle-sort-function . identity))
               (complete-with-action
                action kill-ring string pred)))))
      (icomplete-vertical-do
          (:separator 'dotted-line :height (/ (window-height) 4))
        (when (use-region-p)
          (delete-region (region-beginning) (region-end)))
        (insert
         (completing-read "Yank from kill ring: " kills nil t)))))

  :bind
  (("s-y" . my/icomplete-yank-kill-ring)
   ("s-r" . my/icomplete-recentf)
   :map icomplete-minibuffer-map
   ("C-v" . icomplete-vertical-toggle)))

