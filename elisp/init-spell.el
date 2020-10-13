;;; Code:

(require 'init-const)

;; On-the-fly checking and highlighting of misspellings.
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
  (setq flyspell-issue-message-flag nil)

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

  :bind
  ;; Toggle flyspell mode.
  (("C-c s s" . my/flyspell-mode-dwim)
   (:map flyspell-mode-map
         ;; This keybinding is used by `newcomment'.
         ("C-;" . nil))))

(use-package flyspell-popup
  :if exec/aspell
  :ensure t
  :config
  (setq flyspell-popup-correct-delay 1.0)
  :hook
  (flyspell-mode . flyspell-popup-auto-correct-mode))

;; TODO: check if flycheck-aspell is better than flyspell.
;; (use-package flycheck-aspell
;;   :if
;;   (and (executable-find "aspell") (executable-find "sed"))
;;   :quelpa
;;   (flycheck-aspell :fetcher github :repo "leotaku/flycheck-aspell")
;;   :custom
;;   (ispell-program-name "aspell")
;;   (ispell-dictionary "en_GB")
;;   (ispell-silently-savep t))

(provide 'init-spell)

;;; init-spell.el ends here
