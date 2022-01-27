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

(require 'custom-config)
(require 'functions)
(require 'my-org-packages)

(use-package org
  :ensure nil
  :defines org-babel-clojure-backend
  :custom-face (org-ellipsis ((t (:foreground nil))))
  :bind (("C-c c RET" . 'org-capture)
         ("C-c l" . 'org-store-link))
  :hook
  (((org-babel-after-execute org-mode) . org-redisplay-inline-images) ; display image
   (org-mode . (lambda ()
                 "Beautify org symbols."
                 (setq prettify-symbols-alist custom-prettify-org-symbols-alist)
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
        org-pretty-entities                nil
        org-hide-emphasis-markers          t
        org-startup-indented               t
        org-use-fast-todo-selection        t
        org-enforce-todo-dependencies      t
        org-use-property-inheritance       t ; We like to inhert properties from their parents
        org-cycle-separator-lines          2
        org-ellipsis                       "⤵"
        bidi-paragraph-direction           t
        org-image-actual-width             nil ; avoid wrong size of images

        ;; log time on rescheduling and changing deadlines
        org-log-done                       'time
        org-log-reschedule                 'time
        org-log-redeadline                 'time
        org-log-repeat                     nil
        org-log-into-drawer                "LOG"
        org-agenda-show-log                t


        ;; on links `RET' follows the link
        org-return-follows-link            t
        org-reverse-note-order             t

        ;; turn on speed keys for headlines
        org-use-speed-commands             t

        org-directory                      "~/org"


        org-blank-before-new-entry         '((heading . t) (plain-list-item . auto))

        ;; Set `org-agenda' custom tags
        org-tag-alist                      '(("work " . ?w)
                                             ("project " . ?p)
                                             ("agenda " . ?a)
                                             ("bday " . ?b)
                                             ("task " . ?t)
                                             ("college" . ?c)
                                             ("capture" . ?s)
                                             ("week-days" . ?f)
                                             ("daily" .?d)
                                             ("ignore" .?i))

        ;; Set `org' priority custom faces
        org-priority-faces                 '((?A . (:foreground "#f32020"))
                                             (?B . (:foreground "#dd8844"))
                                             (?C . (:foreground "#6CCB6E")))


        ;; Add and customize org TODO keywords
        org-todo-keywords                  (quote ((sequence "TODO(t)" "DOING(o!)" "|" "DONE(d!)")
                                                   (sequence "WARNING(i@/!)" "WAITING(w@/!)" "|" "CANCELLED(c@/!)")
                                                   (sequence "MEETING(m!)" "|" "DONE(d!)")))



        org-todo-keyword-faces             '(("TODO"         . (:foreground "#6CCB6E" :weight bold))
                                             ("WARNING"      . (:foreground "#f32020" :weight bold))
                                             ("WAITING"      . (:foreground "#ffb378" :weight bold))
                                             ("MEETING"      . (:foreground "#6EC1D6" :weight bold))
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
        org-babel-clojure-backend          'cider

        ;; `org-refile' config. Targets include this file and any file
        ;; contributing to the agenda - up to 9 levels deep
        org-refile-targets                       `((nil . (:maxlevel . 9)))
        org-refile-target-verify-function       'bh/verify-refile-target
        org-refile-allow-creating-parent-nodes  'confirm
        org-refile-use-outline-path             'file
        org-outline-path-complete-in-steps      nil)

  ;; Strike through headlines for done tasks in Org
  (setq org-fontify-done-headline t)
  (custom-set-faces
   '(org-done ((t (:strike-through t))))
   '(org-headline-done
     ((((class color) (min-colors 16) (background dark))
       (:strike-through t)))))

  ;; cool message for scratch  ( ͡° ͜ʖ ͡°)
  (setq initial-major-mode 'org-mode
        initial-scratch-message "*Scratch* ta aqui pra isso :pepe-happy: :pepe-ok: \n\n#+begin_src\n\n#+end_src")

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

;;;###autoload
  (defun unpackaged/org-fix-blank-lines (&optional prefix)
    "Ensure that blank lines exist between headings and between headings and their contents.
With prefix, operate on whole buffer. Ensures that blank lines
exist after each headings's drawers."
    (interactive "P")
    (org-map-entries (lambda ()
                       (org-with-wide-buffer
                        ;; `org-map-entries' narrows the buffer, which prevents us from seeing
                        ;; newlines before the current heading, so we do this part widened.
                        (while (not (looking-back "\n\n" nil))
                          ;; Insert blank lines before heading.
                          (insert "\n")))
                       (let ((end (org-entry-end-position)))
                         ;; Insert blank lines before entry content
                         (forward-line)
                         (while (and (org-at-planning-p)
                                     (< (point) (point-max)))
                           ;; Skip planning lines
                           (forward-line))
                         (while (re-search-forward org-drawer-regexp end t)
                           ;; Skip drawers. You might think that `org-at-drawer-p' would suffice, but
                           ;; for some reason it doesn't work correctly when operating on hidden text.
                           ;; This works, taken from `org-agenda-get-some-entry-text'.
                           (re-search-forward "^[ \t]*:END:.*\n?" end t)
                           (goto-char (match-end 0)))
                         (unless (or (= (point) (point-max))
                                     (org-at-heading-p)
                                     (looking-at-p "\n"))
                           (insert "\n"))))
                     t (if prefix
                           nil
                         'tree)))

;;;; Refile settings
  ;; Exclude DONE state tasks from refile targets
  (defun bh/verify-refile-target ()
    "Exclude todo keywords with a done state from refile targets"
    (not (member (nth 2 (org-heading-components)) org-done-keywords)))

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
              org-agenda-files
              my/style-org-agenda
              org-agenda-maybe-redo
              my-agenda-indent-string
              org-agenda-format-date-aligned)
  :bind ("C-c a" . org-agenda)
  :init
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

  ;; Fancy style for my `org-agenda' buffer
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
        org-agenda-time-grid                               '((daily today require-timed)
                                                             (800 1000 1200 1400 1600 1800 2000)
                                                             " ...... " "----------------")

        org-agenda-format-date                             (lambda (date) (concat  (make-string (window-width) 9472)
                                                                              "\n"
                                                                              (org-agenda-format-date-aligned date)))

        org-agenda-files                                   '("~/org/agenda.org"
                                                             "~/org/project.org"
                                                             "~/org/habit.org"
                                                             "~/org/tasks.org"
                                                             "~/org/birthdays.org"
                                                             "~/org/college.org"
                                                             "~/org/capture.org"
                                                             "~/org/work.org")

        org-agenda-prefix-format                           '((agenda . " %i %-12c%?-12t% s")
                                                             (todo . " %i %-12 c")
                                                             (tags . " %i")
                                                             (search . " %i %-12 c"))

        org-agenda-custom-commands  '(
                                      ("x" "My Agenda :)"
                                       ((agenda
                                         ""
                                         ((org-agenda-overriding-header        "My Agenda 📅")
                                          (org-agenda-remove-tags              t)
                                          (org-agenda-span                     '2)))

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
                                        ))

                                      ("t" "My General Todos"
                                       ((agenda
                                         ""
                                         ((org-agenda-overriding-header        "My Agenda 📅")
                                          (org-agenda-remove-tags              t)
                                          (org-agenda-span                     '2)))
                                        (tags-todo
                                         "+CATEGORY=\"task \""
                                         ((org-agenda-overriding-header        "My Tasks  ")
                                          (org-agenda-prefix-format            "%e %(my-agenda-prefix)")
                                          (org-agenda-sorting-strategy         '(category-keep))
                                          (org-agenda-remove-tags              t)
                                          (org-tags-match-list-sublevels       t)
                                          (org-enforce-todo-dependencies       t)
                                          (org-agenda-skip-scheduled-if-done   t)
                                          (org-agenda-skip-deadline-if-done    t)
                                          (org-agenda-todo-ignore-scheduled    'all)))
                                        ))
                                      )
        )

  ;;testing
  (setq org-agenda-ignore-properties      '(effort appt category)
        org-agenda-dim-blocked-tasks      nil
        org-agenda-use-tag-inheritance    nil)

  ;; Set Visual changes on agenda buffer
  (add-hook 'today-visible-calendar-hook 'calendar-mark-today)
  (add-hook 'org-agenda-mode-hook 'my/style-org-agenda)

  ;; Remove the mouse face in whole agenda buffer
  (add-hook 'org-agenda-finalize-hook
            (lambda () (remove-text-properties
                   (point-min) (point-max) '(mouse-face t))))

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
  (run-with-timer 3 600 #'renewOrgBuffer))

;; save all the agenda files after each capture
(add-hook 'org-agenda-finalize-hook 'my/save-all-agenda-buffers)

;; Auto save agenda files
;;(add-hook 'org-agenda-mode-hook
;;          (lambda ()
;;            (add-hook 'auto-save-hook 'org-save-all-org-buffers nil t)
;;            (auto-save-mode)))
;; Auto rebuild agenda buffer after 30 seconds


(provide 'org-config)
;;; org-config.el ends here
