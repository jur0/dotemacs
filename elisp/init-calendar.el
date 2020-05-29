;;; Code:

(use-package calendar
  :custom
  ;; Display time in 24h format.
  (setq calendar-time-display-form
        '(24-hours ":" minutes
                   (when time-zone
                     (concat " (" time-zone ")"))))
  :config
  ;; Start with Monday.
  (setq calendar-week-start-day 1)
  ;; The iso style = year/month/day.
  (setq calendar-date-style 'iso)
  :hook
  ;; When the current date is visible, make it.
  (calendar-today-visible-hook . calendar-mark-today))

;; TODO: check calfw - better calendar views.

(provide 'init-calendar)

;;; init-calendar.el ends here
