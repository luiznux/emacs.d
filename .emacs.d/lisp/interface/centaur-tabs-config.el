;;; centaur-tabs-config.el --- packages for centaur-tabs config  -*- lexical-binding: t -*-
;;
;; Author: Luiz Tagliaferro <luiz@luiznux.com>
;; URL: https://luiznux.com
;; This file is free software :)
;;
;;; Commentary:
;;
;; ████████╗ █████╗ ██████╗ ███████╗
;; ╚══██╔══╝██╔══██╗██╔══██╗██╔════╝
;;    ██║   ███████║██████╔╝███████╗
;;    ██║   ██╔══██║██╔══██╗╚════██║
;;    ██║   ██║  ██║██████╔╝███████║
;;    ╚═╝   ╚═╝  ╚═╝╚═════╝ ╚══════╝
;;
;;; Code:

(use-package centaur-tabs
  :defines evil-normal-state-map

  :commands (centaur-tabs-group-by-projectile-project
             centaur-tabs-get-group-name
             centaur-tabs-headline-match
             centaur-tabs-change-fonts
             centaur-tabs-enable-buffer-reordering)

  :hook ((dashboard-mode . centaur-tabs-local-mode)
         (term-mode . centaur-tabs-local-mode)
         (vterm-mode . centaur-tabs-local-mode)
         (calendar-mode . centaur-tabs-local-mode)
         (org-agenda-mode . centaur-tabs-local-mode)
         (magit-mode . centaur-tabs-local-mode)
         (git-commit-mode . centaur-tabs-local-mode)
         (minibuffer-mode . centaur-tabs-local-mode)
         (org-capture-mode . centaur-tabs-local-mode)
         (which-key-mode . centaur-tabs-mode)
         (helpful-mode . centaur-tabs-local-mode))

  :bind (("C-<prior>" . centaur-tabs-backward)
         ("C-<next>" . centaur-tabs-forward)
         ("C-c t" . centaur-tabs-counsel-switch-group)
         (:map evil-normal-state-map
          ("g t" . centaur-tabs-forward)
          ("g T" . centaur-tabs-backward)))

  :init
  (setq centaur-tabs-style                    "chamfer"
        centaur-tabs-height                   32
        centaur-tabs-set-bar                  'under
        x-underline-at-descent-line           t
        centaur-tabs-set-modified-marker      t
        centaur-tabs-set-icons                t
        centaur-tabs-show-new-tab-button      t
        centaur-tabs-show-navigation-buttons  nil
        centaur-tabs-show-count               nil
        centaur-tabs-adjust-buffer-order      t)

  (centaur-tabs-headline-match)
  (centaur-tabs-group-by-projectile-project)
  (centaur-tabs-enable-buffer-reordering)
  (centaur-tabs-change-fonts (face-attribute 'default :font) 70)
  (centaur-tabs-mode t))


(provide 'centaur-tabs-config)
;;; centaur-tabs-config.el ends here
