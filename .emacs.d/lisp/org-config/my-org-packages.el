;;; my-org-packages.el --- Org mode packages downloaded -*- lexical-binding: t no-byte-compile: t -*-
;;
;; Author: Luiz Tagliaferro <luiz@luiznux.com>
;; URL: https://luiznux.com
;; This file is free software :)
;;
;;; Commentary:
;;
;; My org mode selected packages
;;
;;; Code:

(defun setup-org-packages ()
  "Setup and call org packages."

  (use-package org-bullets
    :config
    (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

  (use-package org-download
    :hook ('dired-mode-hook 'org-download-enable))

  (use-package org-cliplink
    :bind("C-x p i" . org-cliplink))

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
  (use-package org-super-agenda)
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

  (use-package org-gcal
    :if  (file-exists-p "~/org/org-api.el")
    :defines luiznux-client-id luiznux-client-secret
    :config
    (load "~/org/org-api.el") ;; file with the keys
    (setq org-gcal-client-id  luiznux-client-id
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
    (setq org-roam-dailies-directory  "journal/"
          org-roam-v2-ack              t)
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

  (use-package org-roam-ui
    :after org-roam
    ;; normally we'd recommend hooking orui after org-roam, but since org-roam
    ;; does not have a hookable mode anymore, you're advised to pick something
    ;; yourself if you don't care about startup time, use:
    ;; :hook (after-init . org-roam-ui-mode)
    :config
    (setq org-roam-ui-sync-theme t
          org-roam-ui-follow t
          org-roam-ui-update-on-save t
          org-roam-ui-open-on-start t)))

(provide 'my-org-packages)
;;; my-org-packages.el ends here
