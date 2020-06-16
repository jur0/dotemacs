;;; Code:

(use-package magit
  :ensure t
  :bind
  ("C-c g" . magit-status))

(use-package magit-diff
  :config
  (setq magit-diff-refine-hunk 'all))

;; Based on https://chris.beams.io/posts/git-commit/
(use-package git-commit
  :after
  (magit)
  :config
  (setq git-commit-summary-max-length 50)
  (setq fill-column 72)
  (setq git-commit-known-pseudo-headers
        '("Signed-off-by"
          "Acked-by"
          "Modified-by"
          "Cc"
          "Suggested-by"
          "Reported-by"
          "Tested-by"
          "Reviewed-by"))
  (setq git-commit-style-convention-checks
        '(non-empty-second-line overlong-summary-line)))

(provide 'init-vc)

;;; init-vc.el ends here
