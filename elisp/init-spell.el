;;; Code:

(require 'init-const)

;; On-the-fly checking and highlighting of misspellings.
(use-package flyspell
  :if exec/aspell
  :custom
  (ispell-program-name "aspell")
  (ispell-dictionary "en_GB")
  (ispell-silently-savep t)
  ;; --sug-mode=ultra is a suggestion mode, ultra gives the best candidates (but
  ;; is the slowest).
  ;; --run-together can check camel case words. Also aspell process with this
  ;; option can be reused.
  (ispell-extra-args '("--sug-mode=ultra" "--run-together"))
  ;; Do not show welcome message when flyspell starts.
  (flyspell-issue-welcome-flag nil)
  ;; Do not show messages when checking words.
  (flyspell-issue-message-flag nil)
  :bind
  (:map flyspell-mode-map
        ("C-;" . nil)
        ("C-." . nil)
        ("C-," . nil))
  :hook
  (((text-mode outline-mode) . flyspell-mode)
   ;; Check comments and strings in source code.
   (prog-mode . flyspell-prog-mode)
   (before-save-hook . flyspell-buffer)))

;; Show popup with available corrections when point is on a misspelled word.
(use-package flyspell-popup
  :if exec/aspell
  :ensure t
  :custom
  (flyspell-popup-correct-delay 1.0)
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
