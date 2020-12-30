;;; Code:

(require 'init-const)

;; Org main directory
(defconst my/org-directory "~/org")

;; Org agenda related files.
(defconst my/org-inbox-file (expand-file-name "inbox.org" my/org-directory))
(defconst my/org-projects-file (expand-file-name "projects.org" my/org-directory))
(defconst my/org-actions-file (expand-file-name "actions.org" my/org-directory))
(defconst my/org-repeaters-file (expand-file-name "repeaters.org" my/org-directory))

;; Org Roam directory
(defconst my/org-roam-directory (expand-file-name "roam/" my/org-directory))
;; Org Roam images directory.
(defconst my/org-roam-images-directory
  (expand-file-name "images/" my/org-roam-directory))

(use-package org
  :config
    ;; https://org-roam.discourse.group/t/update-a-field-last-modified-at-save/321/2
  (defun my/org-find-time-file-property (property &optional anywhere)
    "Return the position of the time file PROPERTY if it exists.
When ANYWHERE is non-nil, search beyond the preamble."
    (save-excursion
      (goto-char (point-min))
      (let ((first-heading
             (save-excursion
               (re-search-forward org-outline-regexp-bol nil t))))
        (when (re-search-forward (format "^#\\+%s:" property)
                                 (if anywhere nil first-heading)
                                 t)
          (point)))))

  (defun my/org-has-time-file-property-p (property &optional anywhere)
    "Return the position of time file PROPERTY if it is defined.
As a special case, return -1 if the time file PROPERTY exists but
is not defined."
    (when-let ((pos (my/org-find-time-file-property property anywhere)))
      (save-excursion
        (goto-char pos)
        (if (and (looking-at-p " ")
                 (progn (forward-char)
                        (org-at-timestamp-p 'lax)))
            pos
          -1))))

  (defun my/org-set-time-file-property (property &optional anywhere pos)
    "Set the time file PROPERTY in the preamble.
When ANYWHERE is non-nil, search beyond the preamble.
If the position of the file PROPERTY has already been computed,
it can be passed in POS."
    (when-let ((pos (or pos
                        (my/org-find-time-file-property property))))
      (save-excursion
        (goto-char pos)
        (if (looking-at-p " ")
            (forward-char)
          (insert " "))
        (delete-region (point) (line-end-position))
        (let* ((now (format-time-string "[%Y-%m-%d %a %H:%M]")))
          (insert now)))))

  (defun my/org-set-last-modified ()
    "Update the LAST_MODIFIED file property in the preamble."
    (when (derived-mode-p 'org-mode)
      (my/org-set-time-file-property "LAST_MODIFIED")))

  :custom
  (org-directory my/org-directory)
  ;; Show emphasis marker characters, for example '*' for bold text.
  (org-hide-emphasis-markers nil)
  (org-special-ctrl-a/e t)
  (org-special-ctrl-k t)
  ;; Follow link when RET is pressed.
  (org-return-follows-link t)
  ;; Show all empty lines between collapsed trees.
  (org-cycle-separator-lines -1)
  ;; Do not use empty lines around headings and list items.
  (org-blank-before-new-entry '((heading . nil) (plain-list-item . nil)))
  ;; Turn on `org-indent-mode' on startup (it indents text according to outline
  ;; structure).
  (org-startup-indented t)
  ;; Display source code buffer in the current window.
  (org-src-window-setup 'current-window)
  ;; Fontify code in code blocks.
  (org-src-fontify-natively t)
  (org-src-preserve-indentation t)
  ;; Treat TAB in a code block according to the major mode of the programming
  ;; language.
  (org-src-tab-acts-natively t)
  ;; Do not ask for confirmation to evaluate code block.
  (org-confirm-babel-evaluate nil)
  (org-edit-src-content-indentation 0)
  ;; Insert state change notes and/or timestamp into a drawer ("LOGBOOK" by
  ;; default).
  (org-log-into-drawer t)
  ;; Closing note can still be added with 'C-u C-t d'.
  (org-log-done 'time)
  ;; Newly created headings have :CREATED: property.
  (org-treat-insert-todo-heading-as-state-change nil)
  ;; Replace "..." with this char.
  (org-ellipsis "⋯")
  (org-todo-keywords
   '(;; TODO an item that needs addressing;
     ;; STARTED being addressed;
     ;; WAITING  dependent on something else happening;
     ;; DELEGATED someone else is doing it and I need to follow up with them;
     ;; ASSIGNED someone else has full, autonomous responsibility for it;
     ;; CANCELLED no longer necessary to finish;
     ;; DONE complete.
     (sequence "TODO(t)" "STARTED(s!)" "WAITING(w@/!)" "DELEGATED(e@/!)" "|"
               "ASSIGNED(a@/!)" "CANCELLED(c@/!)" "DONE(d!)")))
  ;; TODO: consider keeping tags in a separate org file.
  (org-tag-alist
   '(;; ERRAND requires a short trip (deliver package, buy something);
     ;; CALL requires calling via phone, internet, etc.;
     ;; REPLY requires replying to an email;
     ;; VISIT requires a longer trip. ;; TODO
     (:startgroup . nil)
     ("ERRAND" . ?e) ("CALL" . ?c) ("REPLY" . ?r) ("VISIT" . ?v)
     (:endgroup . nil)))
  :bind
  (("C-c l" . org-store-link)
   ("C-c b" . org-switchb)
   (:map org-mode-map
         ("M-n" . outline-next-visible-heading)
         ("M-p" . outline-previous-visible-heading)
         ("<C-M-return>" . org-insert-subheading)))
  :hook
  (before-save . my/org-set-last-modified)
  :mode
  ("\\.org\\'" . org-mode))

(use-package org-habit
  :after
  (org)
  :config
  (add-to-list 'org-modules 'org-habit t)
  :custom
  (org-habit-preceding-days 10)
  (org-habit-following-days 5)
  (org-habit-graph-column 65)
  ;; Show consistency graph for all days.
  (org-habit-show-habits-only-for-today nil))

(use-package org-agenda
  :config
  (unbind-key "N" org-agenda-mode-map)

  (defun my/org-agenda-separating-heading (heading)
    "Print HEADING padded with characters to create a separator."
    (let ((sep ?-))
      (concat (make-string 3 sep)
              " " heading " "
              (make-string (- 75 (length heading)) sep))))

  (defun my/org-agenda-process-inbox-item ()
    "Process a single inbox item in the org-agenda."
    (interactive)
    (org-with-wide-buffer
     (org-agenda-set-tags)
     (org-agenda-priority)
     (org-agenda-refile nil nil nil)))

  :custom
  (org-agenda-files
   (list my/org-inbox-file
         my/org-projects-file
         my/org-actions-file
         my/org-repeaters-file))
  (org-refile-targets
   '((my/org-projects-file :maxlevel . 1)
     (my/org-actions-file :level . 0)
     (my/org-repeaters-file :level . 0)))
  (org-refile-use-outline-path 'file)
  (org-outline-path-complete-in-steps nil)
  (org-refile-allow-creating-parent-nodes 'confirm)
  (org-agenda-window-setup 'current-window)
  (org-agenda-restore-windows-after-quit t)
  (org-agenda-start-day nil)
  (org-agenda-log-mode t)
  ;; (org-agenda-tags-colums -80) ;; TODO: review
  (org-agenda-start-on-weekday 1)
  (org-agenda-custom-commands
   `(("d" "Daily schedule"
      ((agenda ""
               ((org-agenda-span 'day)
                (org-deadline-warning-days 14)))
       (todo "TODO"
             ((org-agenda-overriding-header
               (my/org-agenda-separating-heading "To refile"))
              (org-agenda-files (list my/org-inbox-file))))
       (todo "TODO"
             ((org-agenda-overriding-header
               (my/org-agenda-separating-heading "Projects"))
              (org-agenda-files (list my/org-projects-file))))
       (todo "TODO"
             ((org-agenda-overriding-header
               (my/org-agenda-separating-heading "Actions"))
              (org-agenda-files (list my/org-actions-file))
              (org-agenda-skip-function
               '(org-agenda-skip-entry-if 'deadline 'scheduled)))))
      ((org-agenda-compact-blocks t)
       (org-use-property-inheritance t)))))
  ;; org-agenda-category-icon-alist
  ;; org-agenda-use-time-grid org-agenda-time-grid
  :bind
  (("C-c a" . org-agenda)
   (:map org-agenda-mode-map
         ;; Skip the title between agenda blocks (cursor skips the title).
         ("n" . 'org-agenda-next-item)
         ("p" . 'org-agenda-previous-item)
         ("P" . 'my/org-agenda-process-inbox-item))))

(use-package org-capture
  :bind
  ("C-c c" . org-capture)
  :custom
  ;; Default fallback file for templates that don't specify target file.
  (org-default-notes-file my/org-inbox-file)
  (org-capture-templates
   `(("t" "TODO task" entry
      (file my/org-inbox-file)
      ,(concat "* TODO %?\n"
               ":PROPERTIES:\n"
               ":CREATED:  %U\n"
               ":END:\n")))))

;; TODO: find out how to capture also content from HTML.
;; (use-pacqkage org-protocol-capture-html
;;   :if exec/pandoc
;;   :quelpa
;;   (org-protocol-capture-html
;;    :fetcher github
;;    :repo "alphapapa/org-protocol-capture-html"))

(use-package org-roam
  :if exec/sqlite3
  :ensure t
  :custom
  (org-roam-directory my/org-roam-directory)
  (org-roam-capture-templates
   `(("d" "default" plain
      (function org-roam--capture-get-point)
      "%?"
      :file-name "%<%Y%m%d%H%M%S>-${slug}"
      :head ,(concat "#+TITLE: ${title}\n"
                     "#+ROAM_ALIAS: \n"
                     "#+CREATED: %U\n"
                     "#+LAST_MODIFIED: %U\n\n")
      :unnarrowed t)))
  (org-roam-capture-ref-templates
   `(("r" "ref" plain
      (function org-roam--capture-get-point)
      ""
      :file-name "refs/${slug}"
      :head ,(concat "#+TITLE: ${title}\n"
                     "#+ROAM_KEY: ${ref}\n"
                     "#+CREATED: %U\n"
                     "#+LAST_MODIFIED: %U\n\n")
      :unnarrowed t)))
  :hook
  (after-init . org-roam-mode)
  :bind
  (:map org-roam-mode-map
        (("C-c n l" . org-roam)
         ("C-c n c" . org-roam-capture)
         ("C-c n f" . org-roam-find-file)
         ("C-c n b" . org-roam-switch-to-buffer)
         ("C-c n g" . org-roam-graph))
        :map org-mode-map
        (("C-c n i" . org-roam-insert))))

(use-package org-roam-protocol
  :after
  (org-roam))

;; TODO: review.
;; Download images from websites to be used in Roam's knowledge base.
;; M-x org-download-yank download an image from a link in kill ring.
(use-package org-download
  :ensure t
  :custom
  (org-download-image-dir my/org-roam-images-directory)
  :hook
  ;; Enable drag and drop of images in dired.
  (dired-mode-hook 'org-download-enable))

(provide 'init-org)

;;; init-org.el ends here
