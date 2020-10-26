;;; Code:

;; Tab bar represents a named persistent window configuration.
(use-package tab-bar
  :config
  (setq tab-bar-close-button-show nil)
  (setq tab-bar-close-last-tab-choice 'tab-bar-mode-disable)
  (setq tab-bar-close-tab-select 'recent)
  ;; Start a new tab with the current buffer.
  (setq tab-bar-new-tab-choice t)
  (setq tab-bar-new-tab-to 'right)
  (setq tab-bar-position nil)
  ;; Keep tab-bar hidden.
  (setq tab-bar-show nil)
  (setq tab-bar-tab-hints nil)
  (setq tab-bar-tab-name-function 'tab-bar-tab-name-all)
  ;; Enable `tab-bar-mode' by default.
  (tab-bar-mode t)
  (global-tab-line-mode -1)
  ;; Check `winner-mode' that keeps track of layout changes.
  (tab-bar-history-mode -1)

  (defun my/tab-bar-select-tab-dwim ()
    "Do-What-I-Mean function for getting to a `tab-bar-mode' tab.
If no other tab exists, create one and switch to it.  If there is
one other tab (so two in total) switch to it without further
questions.  Else use completion to select the tab to switch to."
    (interactive)
    (let ((tabs (mapcar (lambda (tab)
                          (alist-get 'name tab))
                        (tab-bar--tabs-recent))))
      (cond ((eq tabs nil)
             (tab-new))
            ((eq (length tabs) 1)
             (tab-next))
            (t
             (icomplete-vertical-do ()
               (tab-bar-switch-to-tab
                (completing-read "Select tab: " tabs nil t)))))))

  :bind
  (("C-x t t" . my/tab-bar-select-tab-dwim)
   ("s-t" . my/tab-bar-select-tab-dwim)
   ;; Add alias for C-tab.
   ("<s-tab>" . tab-next)
   ;; Add alias for C-S-tab.
   ("<C-s-tab>" . tab-previous)))

(provide 'init-workspace)

;;; init-workspace.el ends here
