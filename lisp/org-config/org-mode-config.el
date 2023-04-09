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
(require 'custom-config)
(require 'functions)
(require 'org-auto-update-state)

(require 'file-color-agenda)
(require 'org-env-config)

(use-package org
  :ensure nil
  :defines org-babel-clojure-backend
  :custom-face
  (org-ellipsis ((t (:foreground unspecified))))
  (org-done ((t (:strike-through t))))
  (org-headline-done ((((class color) (min-colors 16) (background dark))
                       (:strike-through t))))
  :pretty-hydra
  ;; See `org-structure-template-alist'
  ((:title (pretty-hydra-title "Org Template" 'fileicon "org" :face 'all-the-icons-green :height 1.1 :v-adjust 0.0)
    :color blue :quit-key ("q" "C-g"))
   ("Basic"
    (("a" (hot-expand "<a") "ascii")
     ("c" (hot-expand "<c") "center")
     ("C" (hot-expand "<C") "comment")
     ("e" (hot-expand "<e") "example")
     ("E" (hot-expand "<E") "export")
     ("h" (hot-expand "<h") "html")
     ("l" (hot-expand "<l") "latex")
     ("n" (hot-expand "<n") "note")
     ("o" (hot-expand "<q") "quote")
     ("v" (hot-expand "<v") "verse"))
    "Head"
    (("i" (hot-expand "<i") "index")
     ("A" (hot-expand "<A") "ASCII")
     ("I" (hot-expand "<I") "INCLUDE")
     ("H" (hot-expand "<H") "HTML")
     ("L" (hot-expand "<L") "LaTeX"))
    "Source"
    (("s" (hot-expand "<s") "src")
     ("m" (hot-expand "<s" "emacs-lisp") "emacs-lisp")
     ("y" (hot-expand "<s" "python :results output") "python")
     ("p" (hot-expand "<s" "perl") "perl")
     ("w" (hot-expand "<s" "powershell") "powershell")
     ("r" (hot-expand "<s" "ruby") "ruby")
     ("S" (hot-expand "<s" "sh") "sh")
     ("g" (hot-expand "<s" "go :imports '\(\"fmt\"\)") "golang"))
    "Misc"
    (("u" (hot-expand "<s" "plantuml :file CHANGE.png") "plantuml")
     ("Y" (hot-expand "<s" "ipython :session :exports both :results raw drawer\n$0") "ipython")
     ("P" (progn
            (insert "#+HEADERS: :results output :exports both :shebang \"#!/usr/bin/env perl\"\n")
            (hot-expand "<s" "perl")) "Perl tangled")
     ("<" self-insert-command "ins"))))

  :bind (("C-c c RET" . 'org-capture)
         ("C-c L" . 'org-store-link)
         :map org-mode-map
         ("C-c c <" . (lambda ()
                        "Insert org template."
                        (interactive)
                        (if (or (region-active-p) (looking-back "^\s*" 1))
                            (org-hydra/body)
                          (self-insert-command 1)))))

  :hook
  (((org-babel-after-execute org-mode)  . org-redisplay-inline-images) ; display image after execute.
   (org-capture-after-finalize . org-agenda-maybe-redo) ; redo agenda after capturing.
   (before-save . zp/org-set-last-modified) ; update `LAST_MODIFIED' property on save
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

  :config
  (setq org-directory                      emacs-org-directory
        org-catch-invisible-edits          'smart
        org-pretty-entities                nil
        org-hide-emphasis-markers          t
        org-startup-indented               t
        org-use-fast-todo-selection        t
        org-enforce-todo-dependencies      t
        org-fontify-done-headline          t ; Strike through headlines for done tasks in Org
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
        org-tag-alist                      '(("agenda"    .  ?a)
                                             ("bday"      .  ?b)
                                             ("capture"   .  ?c)
                                             ("contas"    .  ?b)
                                             ("daily"     .  ?d)
                                             ("ignore"    .  ?i)
                                             ("studie"    .  ?s)
                                             ("task"      .  ?t)
                                             ("project"   .  ?p)
                                             ("holiday"   .  ?h)
                                             ("week-days" .  ?f)
                                             ("work"      .  ?w))

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
        org-refile-targets                      `((nil . (:maxlevel . 9)))
        org-refile-target-verify-function       'bh/verify-refile-target
        org-refile-allow-creating-parent-nodes  'confirm
        org-refile-use-outline-path             'file
        org-outline-path-complete-in-steps      nil)

  ;; For hydra
  (defun hot-expand (str &optional mod)
    "Expand org template.

STR is a structure template string recognised by org like <s. MOD is a
string with additional parameters to add the begin line of the
structure element. HEADER string includes more parameters that are
prepended to the element after the #+HEADER: tag."
    (let (text)
      (when (region-active-p)
        (setq text (buffer-substring (region-beginning) (region-end)))
        (delete-region (region-beginning) (region-end)))
      (insert str)
      (if (fboundp 'org-try-structure-completion)
          (org-try-structure-completion) ; < org 9
        (progn
          ;; New template expansion since org 9
          (require 'org-tempo nil t)
          (org-tempo-complete-tag)))
      (when mod (insert mod) (forward-line))
      (when text (insert text))))

  ;; Use embedded webkit browser if possible
  (when (featurep 'xwidget-internal)
    (push '("\\.\\(x?html?\\|pdf\\)\\'"
            .
            (lambda (file _link)
              (custom-webkit-browse-url (concat "file://" file) t)))
          org-file-apps))

  ;; Add md/gfm backends
  (add-to-list 'org-export-backends 'md)
  (use-package ox-gfm
    :init (add-to-list 'org-export-backends 'gfm))

  (with-eval-after-load 'counsel
    (bind-key [remap org-set-tags-command] #'counsel-org-tag org-mode-map))

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

  (org-babel-do-load-languages
   'org-babel-load-languages load-language-alist)

  ;; easy templates special blocks in latex export
  (add-to-list 'org-structure-template-alist '("f" . "figure"))

  ;; Load modules
  (with-eval-after-load 'org
    (add-to-list 'org-modules 'org-habit t)
    (add-to-list 'org-modules 'org-id t))

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
;;;

  ;; Paste Url links to org mode
  (use-package org-cliplink
    :bind("C-x p i" . org-cliplink))

  ;; Rich text clipboard
  (use-package org-rich-yank
    :bind (:map org-mode-map
           ("C-M-y" . org-rich-yank)))

  (use-package org-download
    :hook (dired-mode . org-download-enable))

  ;; Table of contents
  (use-package toc-org
    :hook (org-mode . toc-org-mode))

  ;; Customize tables
  (use-package org-make-toc)

  ;; Format tables
  (use-package ftable)

  ;; Auto insert lists
  (use-package org-autolist
    :hook (org-mode . org-autolist-mode))

  ;; Make org markers visible
  (use-package org-appear
    :hook (org-mode . org-appear-mode))

  ;; Web page content to org mode
  (use-package org-web-tools)

  ;; See usage of each subtree
  (use-package org-treeusage
    :custom
    ;; Below two settings recreate the above image
    ((org-treescope-overlay-header nil)
     (org-treeusage-overlay-usecolorbands nil)))

  (when (display-graphic-p)
    (use-package org-sticky-header
      :hook (org-mode . org-sticky-header-mode))

    ;; Render tables
    (use-package valign
      :hook (org-mode . valign-mode)
      :init
      (setq valign-fancy-bar      1
            valign-max-table-size 5000))

    (use-package org-pretty-tags
      :config
      (org-pretty-tags-global-mode)))

  (when emacs/>=27p
    ;; Auto-toggle Org LaTeX fragments
    (use-package org-fragtog
      :diminish
      :hook (org-mode . org-fragtog-mode))

    ;; Preview
    (use-package org-preview-html
      :diminish
      :bind (:map org-mode-map
             ("C-c C-h" . org-preview-html-mode))
      :init (when (featurep 'xwidget-internal)
              (setq org-preview-html-viewer 'xwidget))))

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
    :if (and (display-graphic-p) (char-displayable-p ?◉))
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
    :hook ((org-mode . org-fancy-priorities-mode)
           (org-ql-search . org-fancy-priorities-mode)
           (org-agenda-mode . org-fancy-priorities-mode))
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

  (use-package org-gcal
    :if (file-exists-p "~/org/org-api.el")
    :defines luiznux-client-id luiznux-client-secret
    :init
    (load "~/org/org-api.el") ;; file with the keys
    (setq plstore-cache-passphrase-for-symmetric-encryption t
          org-gcal-client-id  luiznux-client-id
          org-gcal-client-secret luiznux-client-secret
          org-gcal-file-alist '(("luiztagli10@gmail.com" .  "~/org/gcal.org"))))

  (use-package org-roam
    :diminish
    :defines org-roam-graph-viewer
    :hook (before-save . time-stamp)
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
    (setq org-roam-directory (file-truename emacs-org-roam-directory)
          org-roam-node-display-template  (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag))
          org-roam-dailies-directory      "journal/"
          org-roam-v2-ack                 t
          org-roam-graph-viewer           (if (featurep 'xwidget-internal)
                                              #'xwidget-webkit-browse-url
                                            #'browse-url))

    (setq org-roam-capture-templates '(("d" "default" plain "%?"
                                        :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
                                        :unnarrowed t)

                                       ("c" "custom-luiznux" plain ""
                                        :if-new (file+head  "%<%Y%m%d%H%M%S>-${slug}.org"
                                                            "#+TITLE: ${title}\n#+AUTHOR: %(user-full-name)\n#+DATE: %u\n#+LAST_MODIFIED: %u\n#+EMAIL: %(get-user-email)\n#+DESCRIPTION: %^{description}\n#+STARTUP: inlineimages\n\n\n")
                                        :unnarrowed t)))
    :config
    (unless (file-exists-p emacs-org-roam-directory)
      (make-directory emacs-org-roam-directory))

    ;; Bind this to C-c n I
    (defun org-roam-node-insert-immediate (arg &rest args)
      (interactive "P")
      (let ((args (cons arg args))
            (org-roam-capture-templates (list (append (car org-roam-capture-templates)
                                                      '(:immediate-finish t)))))
        (apply #'org-roam-node-insert args)))

    (org-roam-db-autosync-enable))

  (when emacs/>=27p
    (use-package org-roam-ui
      :bind ("C-c n u" . org-roam-ui-mode)
      :init (when (featurep 'xwidget-internal)
              (setq org-roam-ui-browser-function #'xwidget-webkit-browse-url)))))


(provide 'org-mode-config)
;;; org-mode-config.el ends here
