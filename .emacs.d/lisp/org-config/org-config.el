;;; org-config.el --- Emacs org mode config  -*- lexical-binding: t no-byte-compile: t -*-
;;
;; Author: Luiz Tagliaferro <luiz@luiznux.com>
;; URL: https://luiznux.com
;; This file is free software :)
;;
;;; Commentary:
;;
;;  ██████╗ ██████╗  ██████╗     ███╗   ███╗ ██████╗ ██████╗ ███████╗
;; ██╔═══██╗██╔══██╗██╔════╝     ████╗ ████║██╔═══██╗██╔══██╗██╔════╝
;; ██║   ██║██████╔╝██║  ███╗    ██╔████╔██║██║   ██║██║  ██║█████╗
;; ██║   ██║██╔══██╗██║   ██║    ██║╚██╔╝██║██║   ██║██║  ██║██╔══╝
;; ╚██████╔╝██║  ██║╚██████╔╝    ██║ ╚═╝ ██║╚██████╔╝██████╔╝███████╗
;;  ╚═════╝ ╚═╝  ╚═╝ ╚═════╝     ╚═╝     ╚═╝ ╚═════╝ ╚═════╝ ╚══════╝
;;
;; Emacs org mode and its packages config
;;
;;; Code:

(require 'customizations)

(defun setup-org-packages ()
  "Setup and call org packages."

  (use-package org-bullets
    :config
    (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

  (use-package org-download
    :hook ('dired-mode-hook 'org-download-enable))

  ;; Presentation
  (use-package org-tree-slide
    :functions (org-display-inline-images org-remove-inline-images)
    :bind (:map org-mode-map
           ("s-<f7>" . org-tree-slide-mode)
           :map org-tree-slide-mode-map
           ("<left>" . org-tree-slide-move-previous-tree)
           ("<right>" . org-tree-slide-move-next-tree)
           ("S-SPC" . org-tree-slide-move-previous-tree)
           ("SPC" . org-tree-slide-move-next-tree))
    :hook ((org-tree-slide-play . (lambda ()
                                    (text-scale-increase 4)
                                    (org-display-inline-images)
                                    (read-only-mode 1)))
           (org-tree-slide-stop . (lambda ()
                                    (text-scale-increase 0)
                                    (org-remove-inline-images)
                                    (read-only-mode -1))))
    :config
    (org-tree-slide-simple-profile)
    (setq org-tree-slide-skip-outline-level 2))

  (use-package org-superstar
    :hook (org-mode . org-superstar-mode)
    :init
    (setq org-superstar-special-todo-items t)
    ;; Enable custom bullets for TODO items
    (setq org-superstar-todo-bullet-alist
          '(("CANCELLED" . ?✘)
            ("DONE" . ?✔))))

  (use-package org-fragtog
    :config (add-hook 'org-mode-hook 'org-fragtog-mode))

  ;; Table of contents
  (use-package toc-org
    :hook (org-mode . toc-org-mode))

  (use-package org-make-toc
    :after org)

  (use-package org-sticky-header
    :hook (org-mode . org-sticky-header-mode))

  (use-package org-autolist
    :hook (org-mode . (lambda () (org-autolist-mode))))

  (use-package org-wild-notifier
    :hook (after-init . org-wild-notifier-mode)
    :init
    (setq org-wild-notifier-keyword-whitelist    '("TODO" "WAITING" "WARNING" "DOING" "MEETING")
          org-wild-notifier-notification-title   "Agenda 📅"))

  (use-package org-fancy-priorities
    :defines org-fancy-priorities-list
    :hook (org-mode . org-fancy-priorities-mode)
    :config
    (setq org-fancy-priorities-list '("🅰" "🅱" "🅲" "🅳" "🅴")))

  (use-package org-pretty-tags
    :config
    (org-pretty-tags-global-mode))

  (use-package org-gcal
    :if  (file-exists-p "~/org/org-api.el")
    :defines luiznux-client-id luiznux-client-secret
    :config
    (load "~/org/org-api.el") ;; file with the keys
    (setq org-gcal-client-id  luiznux-client-id
          org-gcal-client-secret luiznux-client-secret
          org-gcal-file-alist '(("luiztagli10@gmail.com" .  "~/org/gcal.org"))))


  (use-package org-roam
    :custom
    (org-roam-directory (file-truename "~/org/roam/"))
    :bind (("C-c n l" . org-roam-buffer-toggle)
           ("C-c n f" . org-roam-node-find)
           ("C-c n g" . org-roam-graph)
           ("C-c n i" . org-roam-node-insert)
           ("C-c n c" . org-roam-capture)
           ;; Dailies
           ("C-c n j" . org-roam-dailies-capture-today))
    :init
    (setq org-roam-v2-ack t)
    (add-hook 'before-save-hook 'time-stamp)
    :config
    (org-roam-db-autosync-mode)
    ;; If using org-roam-protocol
    (require 'org-roam-protocol))

  (use-package org-web-tools)
  (use-package org-alert))

(use-package org
  :ensure nil
  :defines org-babel-clojure-backend
  :custom-face (org-ellipsis ((t (:foreground nil))))
  :bind (("C-c c" . 'org-capture)
         ("C-c a" . org-agenda))
  :hook
  (((org-babel-after-execute org-mode) . org-redisplay-inline-images) ; display image
   (org-mode . (lambda ()
                 "Beautify org symbols."
                 (setq prettify-symbols-alist centaur-prettify-org-symbols-alist)
                 (prettify-symbols-mode 1)))
   (org-indent-mode . (lambda()
                        (diminish 'org-indent-mode)
                        ;; WORKAROUND: Prevent text moving around while using brackets
                        ;; @see https://github.com/seagle0128/.emacs.d/issues/88
                        (make-variable-buffer-local 'show-paren-mode)
                        (setq show-paren-mode nil))))

  :init
  ;; load some org-modules
  (with-eval-after-load 'org
    (add-to-list 'org-modules 'org-habit t))

  (setq org-catch-invisible-edits         'smart
        org-log-done                      'time
        org-pretty-entities                nil
        org-hide-emphasis-markers          t
        org-startup-indented               t
        org-use-fast-todo-selection        t
        org-enforce-todo-dependencies      t
        org-cycle-separator-lines          2
        org-ellipsis                       "⤵"
        bidi-paragraph-direction           t
        org-image-actual-width             nil ;avoid wrong size of images

        ;; log time on rescheduling and changing deadlines
        org-log-reschedule                 'time
        org-log-redeadline                 'time
        org-agenda-show-log                t

        ;; on links `RET' follows the link
        org-return-follows-link            t
        org-reverse-note-order             t

        ;; turn on speed keys for headlines
        org-use-speed-commands             t

        org-directory                      "~/org"

        org-agenda-prefix-format           '((agenda . " %i %-12c%?-12t% s")
                                             (todo . " %i %-12 c")
                                             (tags . " %i")
                                             (search . " %i %-12 c"))

        org-blank-before-new-entry          '((heading . t) (plain-list-item . auto))

        ;; Set `org-agenda' custom tags
        org-tag-alist                      '(("work " . ?w)
                                             ("project " . ?p)
                                             ("agenda " . ?a)
                                             ("bday " . ?b)
                                             ("college" . ?c)
                                             ("capture" . ?s)
                                             ("week-days" . ?f)
                                             ("daily" .?d))

        ;; Set `org' priority custom faces
        org-priority-faces                 '((?A . (:foreground "#f32020"))
                                             (?B . (:foreground "#dd8844"))
                                             (?C . (:foreground "#6CCB6E")))


        ;; Add and customize org TODO keywords
        org-todo-keywords                  (quote ((sequence "TODO(t)" "DOING(o!)"  "|" "DONE(d!)")
                                                   (sequence "WARNING(i@/!)" "WAITING(w@/!)" "|" "CANCELLED(c@/!)")
                                                   (sequence "MEETING(m!)" "|" "DONE(d!)")))

        org-todo-keyword-faces             '(("TODO"         . (:foreground "#ff8080" :weight bold))
                                             ("WARNING"      . (:foreground "#f32020" :weight bold))
                                             ("WAITING"      . (:foreground "#ffb378" :weight bold))
                                             ("MEETING"      . (:foreground "#6CCB6E" :weight bold))
                                             ("DOING"        . (:foreground "#A020F0" :weight bold))
                                             ("CANCELLED"    . (:foreground "#ff6c6b" :weight bold))
                                             ("DONE"         . (:foreground "#1E90FF" :weight bold)))
        ;; config `org-capture'
        org-default-notes-file             "~/org/capture.org"
        org-capture-templates              '(("t" "TODO" entry
                                              (file "~/org/capture.org") "* TODO %^{Title}")
                                             ("e" "Event" entry
                                              (file "~/org/agenda .org.org") "* %^{Is it a todo?||TODO}%^{Title}\n%^t\n%?")
                                             ("w" "Work TODO" entry
                                              (file "~/org/work.org") "* TODO %^{Title}"))

        ;; `org-babel' config
        org-confirm-babel-evaluate         nil
        org-src-fontify-natively           t
        org-src-tab-acts-natively          t
        org-src-window-setup               'current-window
        ;; `cider' backend for org babel
        org-babel-clojure-backend          'cider)

  ;; cool message for scratch  ( ͡° ͜ʖ ͡°)
  (setq initial-major-mode 'org-mode
        initial-scratch-message "Eai seu *CORNO* 🐂 \n\n#+begin_src\n\n#+end_src")

  (use-package verb
    :config
    (define-key org-mode-map (kbd "C-c C-r") verb-command-map))

  ;; varlist for `org-babel' languages
  (defvar load-language-list '((emacs-lisp . t)
                               (clojure . t)
                               (lisp . t)
                               (eshell . t)
                               (shell . t)
                               (sql . t)
                               (C . t)
                               (java . t)
                               (python . t)
                               (sed . t)
                               (latex . t)
                               (js . t)
                               (css . t)
                               (verb . t)))

  (use-package ob-go
    :init (cl-pushnew '(go . t) load-language-list))

  (use-package ob-http
    :init(cl-pushnew '(http . t) load-language-list))

  (org-babel-do-load-languages 'org-babel-load-languages
                               load-language-list)

  (defun org-src--construct-edit-buffer-name (org-buffer-name lang)
    "Construct the buffer name for a source editing buffer."
    (concat org-buffer-name " (org src)"))

  (add-hook 'org-babel-after-execute-hook 'org-redisplay-inline-images)

  ;; easy templates special blocks in latex export
  (add-to-list 'org-structure-template-alist '("f" . "figure"))

  ;; Redo agenda after capturing.
  (add-hook 'org-capture-after-finalize-hook 'org-agenda-maybe-redo)

  :config
  (setup-org-packages))


(use-package org-habit
  :ensure nil
  :init
  (setq org-habit-completed-glyph           10003
        org-habit-today-glyph               10082
        org-habit-graph-column              60
        org-habit-preceding-days            2
        org-habit-following-days            2
        org-habit-show-all-today            nil
        org-habit-following-days            5))


(use-package org-agenda
  :ensure nil
  :commands org-current-level
  :functions (renewOrgBuffer
              org-agenda-maybe-redo
              org-agenda-files
              my-agenda-indent-string
              my/style-org-agenda
              org-agenda-format-date-aligned)
  :init
  (defun my-agenda-prefix ()
    (format "%s" (my-agenda-indent-string (org-current-level))))

  (defun my-agenda-indent-string (level)
    (if (= level 1)
        ""
      (let ((str ""))
        (while (> level 2)
          (setq level (1- level)
                str (concat str "   ╰→")))
        (concat str ""))))

  (defun my/style-org-agenda()
    (set-face-attribute 'org-agenda-date nil :height 1.1)
    (set-face-attribute 'org-agenda-date-today nil :height 1.1 :slant 'italic)
    (set-face-attribute 'org-agenda-date-weekend nil :height 1.1))

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
        org-agenda-time-grid                              '((daily today require-timed)
                                                            (800 1000 1200 1400 1600 1800 2000)
                                                            " ...... " "----------------")

        org-agenda-format-date                            (lambda (date) (concat  (make-string (window-width) 9472)
                                                                             "\n"
                                                                             (org-agenda-format-date-aligned date)))

        org-agenda-files                                  (quote ("~/org/agenda.org"
                                                                  "~/org/project.org"
                                                                  "~/org/habit.org"
                                                                  "~/org/birthdays.org"
                                                                  "~/org/college.org"
                                                                  "~/org/capture.org"
                                                                  "~/org/work.org"))

        ;;org-tags-match-list-sublevels 'indented


        org-agenda-custom-commands                        '(("x" "My Agenda :)"
                                                             (
                                                              (agenda ""     (
                                                                              (org-agenda-overriding-header "My Agenda 📅")
                                                                              (org-agenda-remove-tags t)
                                                                              (org-agenda-span '2)))

                                                              (tags-todo "work" ((org-agenda-overriding-header "Work Stuffs 🖥 \n")
                                                                                 (org-agenda-prefix-format "%e %(my-agenda-prefix)")
                                                                                 (org-agenda-sorting-strategy '(priority-down category-keep))
                                                                                 (org-use-property-inheritance '("PRIORITY"))
                                                                                 (org-tags-match-list-sublevels t)
                                                                                 (org-agenda-remove-tags t)
                                                                                 (org-agenda-skip-scheduled-if-done t)
                                                                                 (org-agenda-skip-deadline-if-done  t)
                                                                                 (org-agenda-todo-ignore-scheduled 'all)))

                                                              (tags-todo "college" ((org-agenda-overriding-header "College \n")
                                                                                    ;;(org-agenda-prefix-format " %e %(my-agenda-prefix) ")
                                                                                    (org-agenda-prefix-format " ")
                                                                                    (org-tags-match-list-sublevels t)
                                                                                    (org-agenda-sorting-strategy '(priority-down category-keep))
                                                                                    (org-use-property-inheritance '("PRIORITY"))
                                                                                    ;;(org-agenda-dim-blocked-tasks 'invisible)
                                                                                    (org-agenda-remove-tags t)
                                                                                    (org-enforce-todo-dependencies t)
                                                                                    (org-agenda-skip-scheduled-if-done t)
                                                                                    (org-agenda-skip-deadline-if-done  t)
                                                                                    ;; (org-agenda-skip-function '(org-agenda-skip-entry-if 'timestamp))
                                                                                    (org-agenda-todo-ignore-scheduled 'all)))

                                                              ;; (tags "project" (
                                                              ;;                  (org-agenda-overriding-header "My Projects  \n")
                                                              ;;                  (org-agenda-sorting-strategy '(priority-down))
                                                              ;;                  (org-agenda-remove-tags t)
                                                              ;;                  (org-agenda-prefix-format "%e %(my-agenda-prefix)")

                                                              ;;                  (org-agenda-skip-scheduled-if-done t)
                                                              ;;                  (org-agenda-skip-deadline-if-done  t)
                                                              ;;                  ;; (org-agenda-skip-function '(org-agenda-skip-entry-if 'timestamp))
                                                              ;;                  (org-agenda-todo-ignore-scheduled 'all)))
                                                              ))))

  ;;testing
  (setq org-agenda-ignore-properties      '(effort appt category)
        org-agenda-dim-blocked-tasks      nil
        org-agenda-use-tag-inheritance    nil)

  (add-hook 'today-visible-calendar-hook 'calendar-mark-today)
  (add-hook 'org-agenda-mode-hook 'my/style-org-agenda)

  ;; Diary
  ;;org-agenda-include-diary                           t
  ;;diary-file                                         "~/org/diary"
  ;;calendar-mark-diary-entries-flag                   t
  ;;calendar-view-diary-initially-flag                 t
  ;;calendar-mark-diary-entries-flag                   t
  ;;Remove default holidays
  ;;holiday-christian-holidays                         nil
  ;;holiday-oriental-holidays                          nil
  ;;holiday-bahai-holidays                             nil
  ;;holiday-islamic-holidays                           nil
  ;;holiday-hebrew-holidays                            nil
  ;; Calendar Hooks
  ;;(add-hook 'diary-display-hook 'fancy-diary-display)
  ;;(add-hook 'list-diary-entries-hook 'sort-diary-entries t)

  :config
  (defun mpereira/org-paste-clipboard-image ()
    "TODO: docstring."
    (interactive)
    (if (executable-find "pngpaste")
        (let ((image-file (concat temporary-file-directory
                                  (make-temp-name "org-image-paste-")
                                  ".png")))
          (call-process-shell-command (concat "pngpaste " image-file))
          (insert (concat  "#+CAPTION: " (read-string "Caption: ") "\n"))
          (insert (format "[[file:%s]]" image-file))
          (org-display-inline-images))
      (message "Requires pngpaste in PATH")))

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

  ;; save all the agenda files after each capture
  ;; (add-hook 'org-agenda-finalize-hook 'my/save-all-agenda-buffers)

  ;; Auto save agenda files
  ;;(add-hook 'org-agenda-mode-hook
  ;;          (lambda ()
  ;;            (add-hook 'auto-save-hook 'org-save-all-org-buffers nil t)
  ;;            (auto-save-mode)))
  ;; Auto rebuild agenda buffer after 30 seconds
  (run-with-timer 3 600 #'renewOrgBuffer))


(provide 'org-config)
;;; org-config.el ends here
