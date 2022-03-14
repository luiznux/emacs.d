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

(use-package treemacs
  :defer t
  :defines treemacs-resize-icons
  :commands (treemacs-toggle-fixed-width
             treemacs--find-python3
             treemacs-follow-mode
             treemacs-filewatch-mode
             treemacs-fringe-indicator-mode
             treemacs-git-mode)
  :bind
  (:map global-map
   ("M-0"       . treemacs-select-window)
   ("C-x t 1"   . treemacs-delete-other-windows)
   ("C-x t t"   . treemacs)
   ("C-x t B"   . treemacs-bookmark)
   ("C-x t C-t" . treemacs-find-file)
   ("C-x t M-t" . treemacs-find-tag))
  :init
  (add-hook 'treemacs-mode-hook (lambda() (display-line-numbers-mode -1)))
  (add-hook 'treemacs-mode-hook (lambda() (treemacs-toggle-fixed-width)))

  (progn
    (setq treemacs-collapse-dirs                 (if (treemacs--find-python3) 3 0)
          treemacs-deferred-git-apply-delay      0.5
          treemacs-directory-name-transformer    #'identity
          treemacs-display-in-side-window        t
          treemacs-eldoc-display                 t
          treemacs-file-event-delay              5000
          treemacs-file-extension-regex          treemacs-last-period-regex-value
          treemacs-file-follow-delay             0.2
          treemacs-file-name-transformer         #'identity
          treemacs-follow-after-init             t
          treemacs-git-command-pipe              ""
          treemacs-goto-tag-strategy             'refetch-index
          treemacs-indentation                   2
          treemacs-indentation-string            " "
          treemacs-is-never-other-window         nil
          treemacs-max-git-entries               5000
          treemacs-missing-project-action        'ask
          treemacs-move-forward-on-expand        nil
          treemacs-no-png-images                 nil
          treemacs-no-delete-other-windows       t
          treemacs-project-follow-cleanup        nil
          treemacs-persist-file                  (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
          treemacs-position                      'left
          treemacs-read-string-input             'from-child-frame
          treemacs-recenter-distance             0.1
          treemacs-recenter-after-file-follow    nil
          treemacs-recenter-after-tag-follow     nil
          treemacs-recenter-after-project-jump   'always
          treemacs-recenter-after-project-expand 'on-distance
          treemacs-show-cursor                   nil
          treemacs-show-hidden-files             t
          treemacs-silent-filewatch              nil
          treemacs-silent-refresh                nil
          treemacs-sorting                       'alphabetic-asc
          treemacs-space-between-root-nodes      t
          treemacs-tag-follow-cleanup            t
          treemacs-tag-follow-delay              1.5
          treemacs-user-mode-line-format         nil
          treemacs-user-header-line-format       nil
          treemacs-workspace-switch-cleanup      nil
          treemacs-width                         34
          treemacs-follow-mode                   t
          treemacs-filewatch-mode                t
          treemacs-resize-icons                  18
          treemacs-text-scale                    -1
          treemacs-fringe-indicator-mode         t)

    (pcase (cons (not (null (executable-find "git")))
                 (not (null (treemacs--find-python3))))
      (`(t . t)
       (treemacs-git-mode 'deferred))
      (`(t . _)
       (treemacs-git-mode 'extended))))

  :config
  (use-package treemacs-evil
    :after treemacs evil)

  (use-package treemacs-projectile
    :after treemacs projectile)

  (use-package treemacs-icons-dired
    :after treemacs dired
    :init (treemacs-icons-dired-mode))

  (use-package treemacs-magit
    :after treemacs magit))


(provide 'treemacs-config)
;;; treemacs-config.el ends here
