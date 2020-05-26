;;; Code:

;; Display the key bindings following your currently entered incomplete command
;; (a prefix) in minibuffer.
(use-package which-key
  :ensure t
  :config
  (setq which-key-popup-type 'minibuffer)
  (which-key-mode t))

;; https://stackoverflow.com/questions/3480173/show-keys-in-emacs-keymap-value
;; Show actual keys instead of numbers when describing a keymap.
(defun my/describe-keymap (keymap)
  "Describe a KEYMAP using `substitute-command-keys'."
  (interactive
   (icomplete-vertical-do ()
     (list (completing-read
            "Keymap: " (let (maps)
                         (mapatoms (lambda (sym)
                                     (and (boundp sym)
                                          (keymapp (symbol-value sym))
                                          (push sym maps))))
                         maps)
            nil t))))
  (with-output-to-temp-buffer (format "*keymap: %s*" keymap)
    (princ (format "%s\n\n" keymap))
    (princ (substitute-command-keys (format "\\{%s}" keymap)))
    (with-current-buffer standard-output ;; temp buffer
      (setq help-xref-stack-item (list #'my/describe-keymap keymap)))))

(define-key global-map (kbd "C-h M-k") 'my/describe-keymap)

(provide 'init-help)

;;; init-help.el ends here
