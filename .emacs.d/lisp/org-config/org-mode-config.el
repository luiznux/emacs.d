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

(require 'constants)
(require 'functions)
(require 'my-org-packages)

(use-package org
  :ensure nil
  :defines org-babel-clojure-backend
  :custom-face
  (org-ellipsis ((t (:foreground unspecified))))
  (org-done ((t (:strike-through t))))
  (org-headline-done ((((class color) (min-colors 16) (background dark))
                       (:strike-through t))))

  :bind (("C-c c RET" . 'org-capture)
         ("C-c L" . 'org-store-link))

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
  (setq org-directory                     "~/org"
        org-catch-invisible-edits         'smart
        org-pretty-entities                nil
        org-hide-emphasis-markers          t
        org-startup-indented               t
        org-use-fast-todo-selection        t
        org-enforce-todo-dependencies      t
        org-use-property-inheritance       t ; I like to inhert properties from their parents
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

        org-blank-before-new-entry         '((heading . t) (plain-list-item . auto))

        ;; Set `org-agenda' custom tags
        org-tag-alist                      '(("agenda" . ?a)
                                             ("bday" . ?b)
                                             ("capture" . ?c)
                                             ("contas" . ?b)
                                             ("daily" .?d)
                                             ("ignore" .?i)
                                             ("studie" . ?s)
                                             ("task" . ?t)
                                             ("project" . ?p)
                                             ("week-days" . ?f)
                                             ("work" . ?w))

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

        ;; config capture-templates, for more info see `org-capture'
        org-default-notes-file             "~/org/agenda/capture.org"

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

  (use-package verb
    :config
    (define-key org-mode-map (kbd "C-c C-r") verb-command-map))

  ;; varlist for `org-babel' languages
  (defconst load-language-alist
    '((emacs-lisp . t)
      (clojure    . t)
      (lisp       . t)
      (eshell     . t)
      (shell      . t)
      (sql        . t)
      (C          . t)
      (java       . t)
      (python     . t)
      (sed        . t)
      (latex      . t)
      (js         . t)
      (css        . t)
      (verb       . t))
    "Alist of org ob languages.")

  (use-package ob-go
    :init (cl-pushnew '(go . t) load-language-alist))

  (use-package ob-http
    :init(cl-pushnew '(http . t) load-language-alist))

  (org-babel-do-load-languages 'org-babel-load-languages
                               load-language-alist)

  (defun org-src--construct-edit-buffer-name (org-buffer-name lang)
    "Construct the buffer name for a source editing buffer."
    (concat org-buffer-name " (org src)"))

  (add-hook 'org-babel-after-execute-hook 'org-redisplay-inline-images)

  ;; easy templates special blocks in latex export
  (add-to-list 'org-structure-template-alist '("f" . "figure"))

  ;; Redo agenda after capturing.
  (add-hook 'org-capture-after-finalize-hook 'org-agenda-maybe-redo)

  :config
  (with-eval-after-load 'counsel
    (bind-key [remap org-set-tags-command] #'counsel-org-tag org-mode-map))

  ;; Load modules
  (setq org-modules '(org-habit org-id))

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

  ;; Refile settings
  ;; Exclude DONE state tasks from refile targets
  (defun bh/verify-refile-target ()
    "Exclude todo keywords with a done state from refile targets"
    (not (member (nth 2 (org-heading-components)) org-done-keywords)))

  (defun luiznux/org-paste-clipboard-image ()
    "Paste a clipboard image."
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

;;;
;;; Org Packages

  (use-package org-bullets
    :config
    (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

  (use-package org-download
    :hook ('dired-mode-hook 'org-download-enable))

  (use-package org-cliplink
    :bind("C-x p i" . org-cliplink))

  (when emacs/>=27p
    (use-package org-fragtog
      :config (add-hook 'org-mode-hook 'org-fragtog-mode))
    ;; Preview
    (use-package org-preview-html
      :diminish))

  ;; Table of contents
  (use-package toc-org
    :hook (org-mode . toc-org-mode))

  (use-package org-make-toc
    :after org)

  (use-package org-sticky-header
    :hook (org-mode . org-sticky-header-mode))

  (use-package org-autolist
    :hook (org-mode . (lambda () (org-autolist-mode))))

  (use-package org-pretty-tags
    :config
    (org-pretty-tags-global-mode))

  (use-package org-appear
    :hook (org-mode . org-appear-mode))

  (use-package valign
    :hook (org-mode . valign-mode)
    :init
    (setq valign-fancy-bar      1
          valign-max-table-size 5000))

  (use-package ftable)
  (use-package org-web-tools)
  (use-package org-alert)
  (use-package org-ql)

  (use-package org-treeusage
    :custom
    ;; Below two settings recreate the above image
    ((org-treescope-overlay-header nil)
     (org-treeusage-overlay-usecolorbands nil)))

  (use-package ox-pandoc
    :when (executable-find "pandoc")
    :after ox
    :init
    (add-to-list 'org-export-backends 'pandoc)
    (setq org-pandoc-options
          '((standalone . t)
            (mathjax . t)
            (variable . "revealjs-url=https://revealjs.com"))))

  (use-package org-superstar
    :hook (org-mode . org-superstar-mode)
    :init
    (setq org-superstar-special-todo-items t)
    ;; Enable custom bullets for TODO items
    (setq org-superstar-todo-bullet-alist
          '(("CANCELLED" . ?✘)
            ("DONE" . ?✔))))

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

  (use-package org-fancy-priorities
    :defines org-fancy-priorities-list
    :hook (org-mode . org-fancy-priorities-mode) (org-ql-search . org-fancy-priorities-mode) (org-agenda-mode . org-fancy-priorities-mode)
    :config
    (setq org-fancy-priorities-list '((?A . "🅰")
                                      (?B . "🅱")
                                      (?C . "🅲")
                                      (?D . "🅳"))))
  (use-package org-wild-notifier
    :hook (after-init . org-wild-notifier-mode)
    :init
    (setq org-wild-notifier-keyword-whitelist    '("TODO" "WAITING" "WARNING" "DOING" "MEETING")
          org-wild-notifier-notification-title   "Agenda 📅"))

  (use-package org-edna
    :hook (org-mode . org-edna-mode))

  (use-package org-gcal
    :if (file-exists-p "~/org/org-api.el")
    :defines luiznux-client-id luiznux-client-secret
    :init
    (load "~/org/org-api.el") ;; file with the keys
    (setq plstore-cache-passphrase-for-symmetric-encryption t
          org-gcal-client-id  luiznux-client-id
          org-gcal-client-secret luiznux-client-secret
          org-gcal-file-alist '(("luiztagli10@gmail.com" .  "~/org/gcal.org"))))

  (use-package brazilian-holidays
    :hook ((calendar-mode . brazilian-holidays-mode)
           (org-agenda-mode . brazilian-holidays-mode))
    :config
    (setq org-agenda-include-diary                           t
          diary-file                                         "~/org/diary"
          calendar-mark-diary-entries-flag                   t
          calendar-view-diary-initially-flag                 t
          calendar-mark-diary-entries-flag                   t
          brazilian-sp-holidays                              t)

    ;;Calendar Hooks
    (add-hook 'diary-display-hook 'fancy-diary-display)
    (add-hook 'list-diary-entries-hook 'sort-diary-entries t))

  (use-package org-roam
    :diminish
    :custom
    (org-roam-directory (file-truename "~/org/roam/"))

    (org-roam-capture-templates
     '(("d" "default" plain "%?"
        :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
        :unnarrowed t)

       ("c" "custom-luiznux" plain ""
        :if-new (file+head  "%<%Y%m%d%H%M%S>-${slug}.org"
                            "#+TITLE: ${title}\n#+AUTHOR: %(user-full-name)\n#+DATE: %u\n#+EMAIL: %(get-user-email)\n#+DESCRIPTION: %^{description}\n#+STARTUP: inlineimages\n\n\n")
        :unnarrowed t)))

    :bind ((("C-c n l" . org-roam-buffer-toggle)
            ("C-c n f" . org-roam-node-find)
            ("C-c n g" . org-roam-graph)
            ("C-c n i" . org-roam-node-insert)
            ("C-c n c" . org-roam-capture))

           ;; Dailies
           (("C-c n j" . org-roam-dailies-capture-today)
            ("C-c n I" . org-roam-node-insert-immediate)
            ("C-c n d" . org-roam-dailies-map)))

    :init
    (setq org-roam-node-display-template  (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag))
          org-roam-dailies-directory      "journal/"
          org-roam-v2-ack                 t)
    (add-hook 'before-save-hook 'time-stamp)
    :config
    ;; Bind this to C-c n I
    (defun org-roam-node-insert-immediate (arg &rest args)
      (interactive "P")
      (let ((args (cons arg args))
            (org-roam-capture-templates (list (append (car org-roam-capture-templates)
                                                      '(:immediate-finish t)))))
        (apply #'org-roam-node-insert args)))

    (require 'org-roam-dailies)  ;; Ensure the keymap is available
    (require 'org-roam-protocol) ;; If using org-roam-protocol
    (org-roam-db-autosync-mode))

  (when emacs/>=27p
    (use-package org-roam-ui
      :bind ("C-c n u" . org-roam-ui-mode)
      :init (when (featurep 'xwidget-internal)
              (setq org-roam-ui-browser-function #'xwidget-webkit-browse-url))
      :config
      (setq org-roam-ui-sync-theme t
            org-roam-ui-follow t
            org-roam-ui-update-on-save t
            org-roam-ui-open-on-start t))))


(provide 'org-mode-config)
;;; org-mode-config.el ends here
