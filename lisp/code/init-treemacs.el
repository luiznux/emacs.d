;;; init-treemacs.el --- Initialize treemacs configurations   -*- lexical-binding: t -*-
;;
;; Author: Luiz Tagliaferro <luiz@luiznux.com>
;; URL: https://luiznux.com
;; This file is free software :)
;;
;;; Commentary:
;;
;;
;; ████████╗██████╗ ███████╗███████╗███╗   ███╗ █████╗  ██████╗███████╗
;; ╚══██╔══╝██╔══██╗██╔════╝██╔════╝████╗ ████║██╔══██╗██╔════╝██╔════╝
;;    ██║   ██████╔╝█████╗  █████╗  ██╔████╔██║███████║██║     ███████╗
;;    ██║   ██╔══██╗██╔══╝  ██╔══╝  ██║╚██╔╝██║██╔══██║██║     ╚════██║
;;    ██║   ██║  ██║███████╗███████╗██║ ╚═╝ ██║██║  ██║╚██████╗███████║
;;    ╚═╝   ╚═╝  ╚═╝╚══════╝╚══════╝╚═╝     ╚═╝╚═╝  ╚═╝ ╚═════╝╚══════╝
;;
;;; Code:

(eval-when-compile
  (require 'custom-config))

;; A tree layout file explorer
(use-package treemacs
  :commands (treemacs-toggle-fixed-width
             treemacs-follow-mode
             treemacs-filewatch-mode
             treemacs-git-mode
             treemacs-fringe-indicator-mode)
  :custom-face
  (cfrs-border-color ((t (:inherit posframe-border))))
  :hook
  (treemacs-mode . (lambda ()
                     (treemacs-toggle-fixed-width)
                     (treemacs-fringe-indicator-mode 'always)))
  :bind (([f8]        . treemacs)
         ("M-0"       . treemacs-select-window)
         ("C-x t 1"   . treemacs-delete-other-windows)
         ("C-x t t"   . treemacs)
         ("C-x t B"   . treemacs-bookmark)
         ("C-x t C-t" . treemacs-find-file)
         ("C-x t M-t" . treemacs-find-tag)
         :map treemacs-mode-map
         ([mouse-1]   . treemacs-single-click-expand-action))
  :config
  (setq treemacs-collapse-dirs                 (if treemacs-python-executable 3 0)
        treemacs-missing-project-action        'remove
        treemacs-sorting                       'alphabetic-asc
        treemacs-follow-after-init             t
        treemacs-width                         30
        treemacs-no-png-images                 (not emacs-icon)
        ;;treemacs-text-scale                    -1
        treemacs-recenter-after-project-expand 'on-distance)

  (treemacs-follow-mode     t)
  (treemacs-filewatch-mode  t)
  (pcase (cons (not (null (executable-find "git")))
               (not (null (executable-find "python3"))))
    (`(t . t)
     (treemacs-git-mode 'deferred))
    (`(t . _)
     (treemacs-git-mode 'simple)))

  (use-package treemacs-evil
    :demand t
    :after (treemacs evil))

  (use-package treemacs-projectile
    :after projectile
    :bind (:map projectile-command-map
           ("h" . treemacs-projectile)))

  (use-package treemacs-nerd-icons
    :demand t
    :when (icons-displayable-p)
    :custom-face
    (treemacs-nerd-icons-root-face ((t (:inherit nerd-icons-green :height 1.3))))
    (treemacs-nerd-icons-file-face ((t (:inherit nerd-icons-dsilver))))
    :config (treemacs-load-theme "nerd-icons"))

  (use-package treemacs-magit
    :hook ((magit-post-commit
            git-commit-post-finish
            magit-post-stage
            magit-post-unstage)
           . treemacs-magit--schedule-update))

  (use-package treemacs-tab-bar
    :demand t
    :config (treemacs-set-scope-type 'Tabs)))


(provide 'init-treemacs)
;;; init-treemacs.el ends here
