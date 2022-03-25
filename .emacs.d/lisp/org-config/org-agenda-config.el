;;; org-agenda-config.el --- Emacs org agenda config  -*- lexical-binding: t -*-
;;
;; Author: Luiz Tagliaferro <luiz@luiznux.com>
;; URL: https://luiznux.com
;; This file is free software :)
;;
;;; Commentary:
;;
;;  ██████╗ ██████╗  ██████╗      █████╗  ██████╗ ███████╗███╗   ██╗██████╗  █████╗
;; ██╔═══██╗██╔══██╗██╔════╝     ██╔══██╗██╔════╝ ██╔════╝████╗  ██║██╔══██╗██╔══██╗
;; ██║   ██║██████╔╝██║  ███╗    ███████║██║  ███╗█████╗  ██╔██╗ ██║██║  ██║███████║
;; ██║   ██║██╔══██╗██║   ██║    ██╔══██║██║   ██║██╔══╝  ██║╚██╗██║██║  ██║██╔══██║
;; ╚██████╔╝██║  ██║╚██████╔╝    ██║  ██║╚██████╔╝███████╗██║ ╚████║██████╔╝██║  ██║
;;  ╚═════╝ ╚═╝  ╚═╝ ╚═════╝     ╚═╝  ╚═╝ ╚═════╝ ╚══════╝╚═╝  ╚═══╝╚═════╝ ╚═╝  ╚═╝
;;
;; Emacs org agenda config file
;;
;;; Code:

(require 'org-mode-config)

(use-package org-agenda
  :ensure nil
  :commands org-current-level
  :functions (renewOrgBuffer
              org-agenda-files
              org-agenda-maybe-redo
              my-agenda-indent-string
              org-agenda-format-date-aligned)

  :bind ("C-c a" . org-agenda)

  :custom-face
  ;; Fancy style for my `org-agenda' buffer
  (org-agenda-date-today ((t (:foreground "lime green" :height 1.2 :slant italic :weight ultra-bold))))
  (org-agenda-date-weekend ((t (:height 1.2))))
  (org-agenda-date ((t (:height 1.2))))
  (org-scheduled ((t (:foreground "SlateBlue2"))))
  (org-scheduled-previously ((t (:foreground "medium turquoise"))))
  (org-scheduled-today ((t (:foreground "deep sky blue"))))

  :init
  (defconst luiznux-personal-agenda (concat org-directory "/personal/"))
  (defconst luiznux-work-agenda (concat org-directory "/work/work.org"))
  (defconst luiznux-general-agenda (concat org-directory "/agenda/"))

  (defun my-agenda-prefix ()
    (format "%s" (my-agenda-indent-string (org-current-level))))

  (defun my-agenda-indent-string (level)
    (if (= level 1)
        "\n➔"
      (let ((str ""))
        (while (> level 2)
          (setq level (1- level)
                str (concat str "  ")))
        (concat str "  ╰→"))))


  (setq org-agenda-skip-deadline-prewarning-if-scheduled   t
        org-agenda-skip-scheduled-delay-if-deadline        t
        org-agenda-skip-deadline-if-done                   t
        org-agenda-breadcrumbs-separator                   " ❱ "
        org-agenda-prefer-last-repeat                      t
        org-agenda-show-future-repeats                     t
        org-agenda-skip-unavailable-files                  t
        org-agenda-compact-blocks                          nil
        org-agenda-block-separator                         #x2501
        org-agenda-span                                    6
        org-agenda-remove-tags                             t
        calendar-week-start-day                            1
        org-agenda-current-time-string                     " ᐊ┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈ NOW "
        org-agenda-time-grid                               '((daily today require-timed)
                                                             (800 1000 1200 1400 1600 1800 2000)
                                                             " ...... " "----------------")

        org-agenda-format-date                             (lambda (date) (concat  (make-string (window-width) 9472)
                                                                              "\n"
                                                                              (org-agenda-format-date-aligned date)))

        org-agenda-prefix-format                           '((agenda . "%i %-12c%?-12t% s")
                                                             (todo . "%i %-12 c")
                                                             (tags . "%i")
                                                             (search . "%i %-12 c")))

  ;; Set the agenda files and custom commands
  ;; depending witch environment whas chosen
  (pcase luiznux-enviroment-type
    ('work
     (setq org-agenda-files            (list luiznux-work-agenda luiznux-general-agenda)
           org-agenda-custom-commands  '(
                                         ("x" "My Agenda :)"
                                          ((agenda
                                            ""
                                            ((org-agenda-overriding-header        "My Agenda 📅")
                                             (org-agenda-remove-tags              t)
                                             (org-agenda-span                     '2)))

                                           (tags-todo
                                            "work"
                                            ((org-agenda-overriding-header        "Work Stuffs 🖥 ")
                                             (org-agenda-prefix-format            "%e %(my-agenda-prefix)")
                                             (org-agenda-sorting-strategy         '(category-keep))
                                             (org-agenda-remove-tags              t)
                                             (org-tags-match-list-sublevels       t)
                                             (org-enforce-todo-dependencies       t)
                                             (org-agenda-skip-scheduled-if-done   t)
                                             (org-agenda-skip-deadline-if-done    t)
                                             (org-agenda-todo-ignore-scheduled   'all)))
                                           )
                                          )

                                         ("t" "My General Todos"
                                          ((tags-todo
                                            "+CATEGORY=\"task \""
                                            ((org-agenda-overriding-header        "My Tasks  ")
                                             (org-agenda-prefix-format            "%e %(my-agenda-prefix)")
                                             (org-agenda-sorting-strategy         '(category-keep))
                                             (org-agenda-remove-tags              t)
                                             (org-tags-match-list-sublevels       t)
                                             (org-enforce-todo-dependencies       t)
                                             (org-agenda-skip-scheduled-if-done   t)
                                             (org-agenda-skip-deadline-if-done    t)
                                             (org-agenda-todo-ignore-scheduled    'all)))))
                                         )
           )
     )

    ('personal
     (setq org-agenda-files            (list luiznux-personal-agenda luiznux-general-agenda)
           org-agenda-custom-commands  '(
                                         ("x" "My Agenda :)"
                                          ((agenda
                                            ""
                                            ((org-agenda-overriding-header        "My Agenda 📅")
                                             (org-agenda-remove-tags              t)
                                             (org-agenda-span                     '2)))

                                           (tags-todo
                                            "college"
                                            ((org-agenda-overriding-header        "College ")
                                             (org-agenda-prefix-format            "%e %(my-agenda-prefix)")
                                             (org-agenda-sorting-strategy         '(category-keep))
                                             (org-tags-match-list-sublevels       t)
                                             (org-agenda-remove-tags              t)
                                             (org-enforce-todo-dependencies       t)
                                             (org-agenda-skip-scheduled-if-done   t)
                                             (org-agenda-skip-deadline-if-done    t)
                                             (org-agenda-todo-ignore-scheduled    'all)))

                                           (tags-todo
                                            "+CATEGORY=\"project \""
                                            ((org-agenda-overriding-header        "My Projects  ")
                                             (org-agenda-prefix-format            "%e %(my-agenda-prefix)")
                                             (org-agenda-sorting-strategy         '(category-keep))
                                             (org-agenda-remove-tags              t)
                                             (org-tags-match-list-sublevels       t)
                                             (org-enforce-todo-dependencies       t)
                                             (org-agenda-skip-scheduled-if-done   t)
                                             (org-agenda-skip-deadline-if-done    t)
                                             (org-agenda-todo-ignore-scheduled    'all)))
                                           )
                                          )

                                         ("t" "My General Todos"
                                          ((tags-todo
                                            "+CATEGORY=\"task \""
                                            ((org-agenda-overriding-header        "My Tasks  ")
                                             (org-agenda-prefix-format            "%e %(my-agenda-prefix)")
                                             (org-agenda-sorting-strategy         '(category-keep))
                                             (org-agenda-remove-tags              t)
                                             (org-tags-match-list-sublevels       t)
                                             (org-enforce-todo-dependencies       t)
                                             (org-agenda-skip-scheduled-if-done   t)
                                             (org-agenda-skip-deadline-if-done    t)
                                             (org-agenda-todo-ignore-scheduled    'all)))))
                                         )
           )
     )

    ('nil
     (setq org-agenda-files            (list org-directory luiznux-general-agenda)
           org-agenda-custom-commands  '(
                                         ("x" "My Agenda :)"
                                          ((agenda
                                            ""
                                            ((org-agenda-overriding-header        "My Agenda 📅")
                                             (org-agenda-remove-tags              t)
                                             (org-agenda-span                     '2)))))

                                         ("t" "My General Todos"
                                          ((tags-todo
                                            "+CATEGORY=\"task \""
                                            ((org-agenda-overriding-header        "My Tasks  ")
                                             (org-agenda-prefix-format            "%e %(my-agenda-prefix)")
                                             (org-agenda-sorting-strategy         '(category-keep))
                                             (org-agenda-remove-tags              t)
                                             (org-tags-match-list-sublevels       t)
                                             (org-enforce-todo-dependencies       t)
                                             (org-agenda-skip-scheduled-if-done   t)
                                             (org-agenda-skip-deadline-if-done    t)
                                             (org-agenda-todo-ignore-scheduled    'all)))))
                                         )
           )
     )
    )

  (setq org-agenda-ignore-properties      '(effort appt category)
        org-agenda-dim-blocked-tasks      nil
        org-agenda-use-tag-inheritance    nil)

  ;; Set Visual changes on agenda buffer
  (add-hook 'today-visible-calendar-hook 'calendar-mark-today)

  ;; Remove the mouse face in whole agenda buffer
  (add-hook 'org-agenda-finalize-hook
            (lambda () (remove-text-properties
                   (point-min) (point-max) '(mouse-face t))))

  :config
  (defun my/save-all-agenda-buffers ()
    "Function used to save all agenda buffers that are currently open,
based on `org-agenda-files'."
    (interactive)
    (save-current-buffer
      (dolist (buffer (buffer-list t))
        (set-buffer buffer)
        (when (member (buffer-file-name)
                      (mapcar 'expand-file-name (org-agenda-files t)))
          (save-buffer)))))

  (defun renewOrgBuffer ()
    (interactive)
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when (derived-mode-p 'org-agenda-mode)
          (org-agenda-maybe-redo)))))
  (run-with-timer 3 600 #'renewOrgBuffer)

  ;; Refresh org-agenda after rescheduling a task.
  (defun org-agenda-refresh ()
    "Refresh all `org-agenda' buffers."
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when (derived-mode-p 'org-agenda-mode)
          (org-agenda-maybe-redo)))))

  (defadvice org-schedule (after refresh-agenda activate)
    "Refresh org-agenda."
    (org-agenda-refresh)))

;; save all the agenda files after each capture
(add-hook 'org-agenda-finalize-hook 'my/save-all-agenda-buffers)

;; Auto save agenda files
;;(add-hook 'org-agenda-mode-hook
;;          (lambda ()
;;            (add-hook 'auto-save-hook 'org-save-all-org-buffers nil t)
;;            (auto-save-mode)))
;; Auto rebuild agenda buffer after 30 seconds


(provide 'org-agenda-config)
;;; org-agenda-config.el ends here
