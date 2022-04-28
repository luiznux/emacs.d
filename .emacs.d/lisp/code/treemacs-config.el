;;; treemacs-config.el --- Packages for treemacs config  -*- lexical-binding: t -*-
;;
;; Author: Luiz Tagliaferro <luiz@luiznux.com>
;; URL: https://luiznux.com
;; This file is free software :)
;;
;;; Commentary:
;;
;; ████████╗██████╗ ███████╗███████╗███╗   ███╗ █████╗  ██████╗███████╗
;; ╚══██╔══╝██╔══██╗██╔════╝██╔════╝████╗ ████║██╔══██╗██╔════╝██╔════╝
;;    ██║   ██████╔╝█████╗  █████╗  ██╔████╔██║███████║██║     ███████╗
;;    ██║   ██╔══██╗██╔══╝  ██╔══╝  ██║╚██╔╝██║██╔══██║██║     ╚════██║
;;    ██║   ██║  ██║███████╗███████╗██║ ╚═╝ ██║██║  ██║╚██████╗███████║
;;    ╚═╝   ╚═╝  ╚═╝╚══════╝╚══════╝╚═╝     ╚═╝╚═╝  ╚═╝ ╚═════╝╚══════╝
;;
;;; Code:

(require 'constants)

(when emacs/>=25.2p
  ;; A tree layout file explorer
  (use-package treemacs
    :defines treemacs-resize-icons
    :commands (treemacs-toggle-fixed-width
               treemacs--find-python3
               treemacs-follow-mode
               treemacs-filewatch-mode
               treemacs-fringe-indicator-mode
               treemacs-git-mode)
    :bind (([f8]        . treemacs)
           ("M-0"       . treemacs-select-window)
           ("C-x t 1"   . treemacs-delete-other-windows)
           ("C-x t t"   . treemacs)
           ("C-x t B"   . treemacs-bookmark)
           ("C-x t C-t" . treemacs-find-file)
           ("C-x t M-t" . treemacs-find-tag)
           :map treemacs-mode-map
           ([mouse-1]   . treemacs-single-click-expand-action))
    :init
    (setq treemacs-collapse-dirs                 (if (treemacs--find-python3) 3 0)
          treemacs-missing-project-action        'remove
          treemacs-sorting                       'alphabetic-asc
          treemacs-follow-after-init             t
          treemacs-width                         30
          ;;treemacs-text-scale                    -1
          treemacs-deferred-git-apply-delay      0.5
          treemacs-display-in-side-window        t
          treemacs-is-never-other-window         nil
          treemacs-no-png-images                 nil
          treemacs-no-delete-other-windows       t
          treemacs-read-string-input             'from-child-frame
          treemacs-recenter-after-file-follow    'always
          treemacs-recenter-after-project-jump   'always
          treemacs-recenter-after-project-expand 'on-distance
          treemacs-show-hidden-files             t
          treemacs-space-between-root-nodes      t
          treemacs-user-mode-line-format         nil
          treemacs-follow-mode                   t
          treemacs-filewatch-mode                t
          treemacs-fringe-indicator-mode         t)

    (pcase (cons (not (null (executable-find "git")))
                 (not (null (executable-find "python3"))))
      (`(t . t)
       (treemacs-git-mode 'deferred))
      (`(t . _)
       (treemacs-git-mode 'extended)))

    :config
    (add-hook 'treemacs-mode-hook (lambda() (display-line-numbers-mode -1)))
    (add-hook 'treemacs-mode-hook (lambda() (treemacs-toggle-fixed-width)))

    (use-package treemacs-evil
      :after evil)

    (use-package treemacs-projectile
      :after projectile
      :bind (:map projectile-command-map
             ("h" . treemacs-projectile)))

    (use-package treemacs-magit
      :after magit
      :commands treemacs-magit--schedule-update
      :hook ((magit-post-commit
              git-commit-post-finish
              magit-post-stage
              magit-post-unstage)
             . treemacs-magit--schedule-update))))


(provide 'treemacs-config)
;;; treemacs-config.el ends here
