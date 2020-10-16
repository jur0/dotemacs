;;; Code:

(require 'init-const)

;; On-the-fly checking and highlighting of misspelling. It's disabled by
;; default. When it's enabled, it highlights misspelled words. They can
;; be corrected with `C-c s c' (`flyspell-correct' is used for
;; correction).
(use-package flyspell
  :if exec/aspell
  :config
  (setq ispell-program-name "aspell")
  (setq ispell-dictionary "en_GB")
  (setq ispell-silently-savep t)
  ;; --sug-mode=ultra is a suggestion mode, ultra gives the best candidates (but
  ;; is the slowest).
  ;; --run-together can check camel case words. Also aspell process with this
  ;; option can be reused.
  (setq ispell-extra-args '("--sug-mode=ultra" "--run-together"))
  ;; Do not show welcome message when flyspell starts.
  (setq flyspell-issue-welcome-flag nil)
  ;; Do not show messages when checking words.
  (setq flyspell-issue-message-flag nil))

(use-package flyspell-correct
  :if exec/aspell
  :ensure t
  :config

  (defun my/flyspell-mode-dwim ()
    "Toggle `flyspell-mode' or `flyspell-prog-mode'."
    (interactive)
    ;; To spellcheck only comments in `prog-mode', `flyspell-prog-mode'
    ;; is used. Other modes use `flyspell-mode' to turn on
    ;; spellcheck. To disable spellcheck, `flyspell-mode' is used in all
    ;; modes.
    (if (bound-and-true-p flyspell-mode)
          (call-interactively 'flyspell-mode)
      (if (derived-mode-p 'prog-mode)
          (call-interactively 'flyspell-prog-mode)
        (call-interactively 'flyspell-mode))
      (message "Flyspell mode enabled in current buffer")))

  (defun my/flyspell-correct-dwim ()
    "Correct multiple spelling errors."
    (interactive)
    (when (bound-and-true-p flyspell-mode)
      ;; Call with `C-u' prefix (spelling backwards).
      (let ((current-prefix-arg '(4)))
        (call-interactively 'flyspell-correct-wrapper))))

  :bind
  ;; To enable the mode, this keybind must be available even when the
  ;; package is not loaded.
  (("C-c s s" . my/flyspell-mode-dwim)
   (:map flyspell-mode-map
         ;; This keybinding is used by `newcomment'.
         ("C-;" . nil)
         ("C-." . nil)
         ("C-," . nil)
         ("C-c s c" . my/flyspell-correct-dwim))))

(provide 'init-spell)

;;; init-spell.el ends here
