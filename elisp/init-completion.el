;;; Code:

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

;; In buffer completion framework built into Emacs. It reads all the text before
;; the point and tris to find a suitable match (it can be set to search the
;; whole buffer or other buffers, too). It helps to type what has been written
;; before.
(use-package dabbrev
  :after
  (minibuffer icomplete icomplete-vertical)
  :config
  ;; Regexp for skipping characters of an abbreviation. For example, if a
  ;; programming language variables start with '$', it's good to set it to
  ;; "\\$".
  (setq dabbrev-abbrev-skip-leading-regexp "\\$\\|\\*\\|/\\|=")
  ;; Do not search for abbreviations only backward.
  (setq dabbrev-backward-only nil)
  ;; If expansions differ in case, treat them as different.
  (setq dabbrev-case-distinction nil)
  ;; Typed word and expansion don't have to match in case.
  (setq dabbrev-case-fold-search t)
  ;; The expansion's case is applied.
  (setq dabbrev-case-replace nil)
  (setq dabbrev-check-other-buffers t)
  ;; Case sensitive search if uppercase character is used.
  (setq dabbrev-upcase-means-case-search t))

(provide 'init-completion)
