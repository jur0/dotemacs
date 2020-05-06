;;; Code:

(defun my/window-half-window-body-height ()
  "Return half of the height of WINDOW's test area."
  (/ (window-body-height) 2))

(defun my/scroll-half-down ()
  "Scroll down half the page."
  (interactive)
  (scroll-down (my/window-half-window-body-height)))

(defun my/scroll-half-up ()
  "Scroll up half the page."
  (interactive)
  (scroll-up (my/window-half-window-body-height)))

;; Scroll half page down/up instead of a full page to keep context more easily.
(define-key global-map [remap scroll-down-command] 'my/scroll-half-down)
(define-key global-map [remap scroll-up-command] 'my/scroll-half-up)

;; Keep cursor at the same position while scrolling.
(setq scroll-preserve-screen-position t)

;; Redisplay will never recenter point, but will always scroll just enough text
;; to bring point into view, even if you move far away.
(setq scroll-conservatively 101)

;; Number of lines to try scrolling the window when point moves out.
(setq scroll-step 1)
;; Leave some lines at the top/botton of the page as margin while scrolling.
(setq scroll-margin 5)

(setq scroll-up-aggressively 0.01)
(setq scroll-down-aggressively 0.01)

(setq auto-window-vscroll nil)
(setq fast-but-imprecise-scrolling nil)

;; Horizontal scroll.
(setq hscroll-step 1)
(setq hscroll-margin 1)

(provide 'init-scroll)
