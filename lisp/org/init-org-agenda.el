;;; init-org-agenda.el --- Initialize `org-agenda' configurations   -*- lexical-binding: t -*-
;;
;; Author: Luiz Tagliaferro <luiz@luiznux.com>
;; URL: https://luiznux.com
;; This file is free software :)
;;
;;; Commentary:
;;
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

(use-package org-agenda
  :ensure nil
  :commands org-current-level
  :functions (renewOrgBuffer
              org-agenda-files
              org-agenda-maybe-redo
              my-agenda-indent-string
              org-agenda-format-date-aligned)

  :bind (("C-c a"  . org-agenda)
         ("M-<f2>" . org-agenda-hydra/body))

  :custom-face
  ;; Fancy style for my `org-agenda' buffer
  (org-agenda-date-today ((t (:foreground "lime green" :height 1.2 :slant italic :weight ultra-bold))))
  (org-agenda-date-weekend ((t (:height 1.2))))
  (org-agenda-date ((t (:height 1.2))))
  (org-scheduled ((t (:foreground "SlateBlue2"))))
  (org-scheduled-previously ((t (:foreground "medium turquoise"))))
  (org-scheduled-today ((t (:foreground "deep sky blue"))))

  :pretty-hydra
  ((:title (pretty-hydra-title "Org Agenda" 'octicon "nf-oct-checklist" :face 'nerd-icons-lgreen)
    :color blue :quit-key ("q" "C-g"))
   ("View"
    (("a" (org-agenda) "agenda commands")
     ("d" org-agenda-day-view (if (eq 'day (org-agenda-cts)) "[x]" "[ ]"))
     ("w" org-agenda-week-view (if (eq 'week (org-agenda-cts)) "[x]" "[ ]"))
     ("m" org-agenda-month-view (if (eq 'month (org-agenda-cts)) "[x]" "[ ]"))
     ("y" org-agenda-year-view (if (eq 'year (org-agenda-cts)) "[x]" "[ ]")))
    "Motion"
    ((">" (org-agenda-later-week) "next week")
     ("<" (org-agenda-earlier-week) "previous week")
     ("." (org-agenda-later-month) "next month")
     ("," (org-agenda-earlier-month) "previous month"))))

  :init
  (defun org-agenda-cts ()
    (and (eq major-mode 'org-agenda-mode)
         (let ((args (get-text-property
                      (min (1- (point-max)) (point))
                      'org-last-args)))
           (nth 2 args))))

  (defun org-agenda-later-week ()
    "Set `week' current agenda span and call org-agenda-later"
    (interactive)
    (unless (eq 'week org-agenda-current-span)
      (setq org-agenda-current-span 'week) (org-agenda-week-view))
    (org-agenda-later '1))

  (defun org-agenda-earlier-week ()
    "Same as `org-agenda-later-week' but previous"
    (interactive)
    (unless (eq 'week org-agenda-current-span)
      (setq org-agenda-current-span 'week) (org-agenda-week-view))
    (org-agenda-earlier '1))

  (defun org-agenda-later-month ()
    "Set `month' current agenda span and call org-agenda-later"
    (interactive)
    (unless (eq 'month org-agenda-current-span)
      (setq org-agenda-current-span 'month) (org-agenda-month-view))
    (org-agenda-later '1))

  (defun org-agenda-earlier-month ()
    "Same as `org-agenda-later-month' but previous"
    (interactive)
    (unless (eq 'month org-agenda-current-span)
      (setq org-agenda-current-span 'month) (org-agenda-month-view))
    (org-agenda-earlier '1))

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
                                                             (800 1000 1200 1400 1600 1800 2000 2200)
                                                             " ...... " "----------------")

        org-agenda-format-date                             (lambda (date) (concat  (make-string (window-width) 9472)
                                                                              "\n"
                                                                              (org-agenda-format-date-aligned date)))

        org-agenda-prefix-format                           '((agenda . "%i %-12c%?-12t% s")
                                                             (todo . "%i %-12 c")
                                                             (tags . "%i")
                                                             (search . "%i %-12 c")))

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

  (advice-add 'org-schedule :after #'org-agenda-refresh))

;; save all the agenda files after each capture
(add-hook 'org-agenda-finalize-hook 'my/save-all-agenda-buffers)

;; Auto save agenda files
;;(add-hook 'org-agenda-mode-hook
;;          (lambda ()
;;            (add-hook 'auto-save-hook 'org-save-all-org-buffers nil t)
;;            (auto-save-mode)))
;; Auto rebuild agenda buffer after 30 seconds


(provide 'init-org-agenda)
;;; init-org-agenda.el ends here
