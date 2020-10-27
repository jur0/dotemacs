;;; Code:

(use-package window
  :init
  (setq display-buffer-alist
        '(
          ;; TODO: Add more buffers!
          ;; Bottom side of display.
          ("\\*Calendar.*"
           (display-buffer-in-side-window)
           (window-height . 20)
           (side . bottom)
           (slot . -1))
          ("\\*ielm.*"
           (display-buffer-in-side-window)
           (window-height . 20)
           (side . bottom)
           (slot . 0))
          (".*\\*Completions.*"
           (display-buffer-in-side-window)
           (window-height . 20)
           (side . bottom)
           (slot . 1)
           (window-parameters . ((no-other-window . t))))
          ("\\*\\(Backtrace\\|Warnings\\|Compile-Log\\|Messages\\)\\*"
           (display-buffer-in-side-window)
           (window-height . 20)
           (side . bottom)
           (slot . 2))
          ("\\*Flycheck errors.*"
           (display-buffer-in-side-window)
           (window-height . 20)
           (side . bottom)
           (slot . 3))
          ("\\*Register Preview.*"
           (display-buffer-in-side-window)
           (window-height . 20)
           (side . bottom)
           (slot . 3))
          ;; Right side of display.
          ("\\*\\(Help\\|Apropos|Man\\)\\*"
           (display-buffer-in-side-window)
           (window-width . fit-window-to-buffer)
           (side . right)
           (slot . 0))))
  (setq window-resize-pixelwise t)
  ;; Resize window combinations proportionally.
  (setq window-combination-resize t)
  (setq fit-window-to-buffer-horizontally 'only)
  (setq even-window-sizes 'height-only)
  (setq window-sides-vertical nil)
  (setq switch-to-buffer-in-dedicated-window 'pop)
  :bind
  (("s-n" . next-buffer)
   ("s-p" . previous-buffer)
   ("s-o" . other-window)
   ("s-2" . split-window-below)
   ("s-3" . split-window-right)
   ("s-0" . delete-window)
   ("s-1" . delete-other-windows)
   ("s-5" . delete-frame)
   ("C-x +" . balance-windows-area)
   ("s-q" . window-toggle-side-windows)))

;; Switch windows more effeciently (if there are more than two windows).
(use-package ace-window
  :ensure t
  :config
  ;; Call `other-window' if there are just 2 windows.
  (setq aw-dispatch-always nil)
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  :bind
  ([remap other-window] . ace-window))

;; Keep a record of buffer and window layout changes.
(use-package winner
  :demand t
  :config
  (winner-mode t)
  :bind
  ("<s-right>" . winner-redo)
  ("<s-left>" . winner-undo))

;; https://protesilaos.com/dotemacs/#h:12591f89-eeea-4b12-93e8-9293504e5a12
(defvar my/window-configuration nil
  "Current window configuration.
Intended for use by my/window-monocle-mode.")

(define-minor-mode my/window-monocle-mode
  "Toggle between single (zoomed in) window and multiple windows.
This makes it possible to maximise a window and later switch back
to original window configuration."
  ;; Not working for doom-modeline (all minor modes are off).
  :lighter " [M]"
  :global nil
  (if (one-window-p)
      (when my/window-configuration
        (set-window-configuration my/window-configuration))
    (setq my/window-configuration (current-window-configuration))
    (delete-other-windows)
    ;; Show monocle indicator in modeline using `mode-line-misc-info'.
    (add-to-list 'mode-line-misc-info
                 '(my/window-monocle-mode "[M]"))))

(define-key global-map (kbd "s-m") 'my/window-monocle-mode)


;;   (defmacro my/with-advice (adlist &rest body)
;;     "Execute BODY with temporary advice in ADLIST.

;; Each element of ADLIST should be a list of the form
;; (SYMBOL WHERE FUNCTION [PROPS])
;; suitable for passing to `advice-add'.  The BODY is wrapped in an
;; `unwind-protect' form, so the advice will be removed even in the
;; event of an error or nonlocal exit."
;;     (declare (debug ((&rest (&rest form)) body))
;;              (indent 1))
;;     `(progn
;;        ,@(mapcar (lambda (adform)
;;                    (cons 'advice-add adform))
;;                  adlist)
;;        (unwind-protect (progn ,@body)
;;          ,@(mapcar (lambda (adform)
;;                      `(advice-remove ,(car adform) ,(nth 2 adform)))
;;                    adlist))))


;;   (defun my/org-todo-fast-selection (orig-fn &rest args)
;;     "Advice to fix window placement in `org-todo-fast-selection'."
;;     (my/with-advice
;;         (;; Make Org-mode respect `display-buffer-alist'.
;;          (#'org-switch-to-buffer-other-window :override #'pop-to-buffer)
;;          (#'split-window :override #'ignore)
;;          ;; And stop Org-mode from messing with our window configuration.
;;          (#'delete-other-windows :override #'ignore))
;;       (apply orig-fn args)))

;;   (advice-add #'org-fast-todo-selection :around #'my/org-todo-fast-selection)

;;   (defun my/org-agenda-commands (orig-fn &rest args)
;;     "Advice to fix window placement in `org-todo-fast-selection'."
;;     (my/with-advice
;;         (;; Make Org-mode respect `display-buffer-alist'.
;;          (#'org-switch-to-buffer-other-window :override #'pop-to-buffer)
;;          ;; And stop Org-mode from messing with our window configuration.
;;          (#'delete-other-windows :override #'ignore))
;;       (apply orig-fn args)))


          ;; ("\\*Org todo.*"
          ;;  (display-buffer-in-side-window)
          ;;  (window-height . fit-window-to-buffer)
          ;;  (side . top)
          ;;  (slot . 0))
          ;; Right side of display.


(provide 'init-window)

;;; init-window.el ends here
