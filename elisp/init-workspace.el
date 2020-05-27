;;; Code:

;; Manage workspaces by wrapping C-x r w and C-x r j. It takes care of
;; automatically saving and loading to a separate data structure to allow for
;; features like persistency in combination with desktop.el.
;; TODO: check if this package could be replaced by tab-bar (which is built in)
(use-package eyebrowse
  :ensure t
  :demand t
  :custom
  ;; Keymap prefix must be set before loading eyebrowse.
  (eyebrowse-keymap-prefix (kbd "C-c w"))
  :config
  ;; When creating new workspace, do not clone the last one, but create a clean
  ;; workspace with just *scratch* buffer.
  (setq eyebrowse-new-workspace t)
  (setq eyebrowse-close-window-config-prompt t)
  (eyebrowse-mode t)
  :bind
  (:map eyebrowse-mode-map
        ("C-c w c" . eyebrowse-create-window-config)
        ("C-c w q" . eyebrowse-close-window-config)
        ("C-c w l" . eyebrowse-last-window-config)
        ("C-c w r" . eyebrowse-rename-window-config)
        ("C-c w s" . eyebrowse-switch-to-window-config)
        ("C-c w p" . eyebrowse-prev-window-config)
        ("C-c w n" . eyebrowse-next-window-config)))

(provide 'init-workspace)

;;; init-workspace.el ends here
