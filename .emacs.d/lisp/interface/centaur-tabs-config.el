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

(require 'custom-config)


(use-package centaur-tabs
  :defines (evil-normal-state-map centaur-tabs-excluded-prefixes)

  :commands (centaur-tabs-group-by-projectile-project
             centaur-tabs-get-group-name
             centaur-tabs-headline-match
             centaur-tabs-change-fonts
             centaur-tabs-enable-buffer-reordering)

  :hook ((dashboard-mode
          calendar-mode
          process-menu-mode
          helpful-mode
          grep-mode
          vterm-mode term-mode
          magit-mode git-commit-mode
          minibuffer-mode which-key-mode
          org-agenda-mode org-capture-mode) . centaur-tabs-local-mode)

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
        centaur-tabs-set-icons                t
        centaur-tabs-show-new-tab-button      t
        centaur-tabs-set-modified-marker      t
        centaur-tabs-show-navigation-buttons  nil
        centaur-tabs-show-count               nil
        centaur-tabs-adjust-buffer-order      t
        x-underline-at-descent-line           t)

  (centaur-tabs-headline-match)
  (centaur-tabs-group-by-projectile-project)
  (centaur-tabs-enable-buffer-reordering)
  (centaur-tabs-change-fonts (face-attribute 'default :font) centaur-tabs-font-size)
  (centaur-tabs-mode t)

  :config
  (defvar excluded-prefixes-append-list '("*flycheck-posframe-buffer*" "*Shell Command Output*"
                                          "*Org Agenda*" "*dashboard*" "*Directory*" "*vterm*"))
  (cl-loop for prefix in excluded-prefixes-append-list
           do (add-to-list 'centaur-tabs-excluded-prefixes prefix t)))


(provide 'centaur-tabs-config)
;;; centaur-tabs-config.el ends here
