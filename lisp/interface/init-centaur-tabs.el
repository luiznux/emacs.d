;;; init-centaur-tabs.el --- Initialize centaur-tabs configurations   -*- lexical-binding: t -*-
;;
;; Author: Luiz Tagliaferro <luiz@luiznux.com>
;; URL: https://luiznux.com
;; This file is free software :)
;;
;;; Commentary:
;;
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
  :defines (evil-normal-state-map
            centaur-tabs-font-size
            centaur-tabs-excluded-prefixes)
  :commands (centaur-tabs-group-by-projectile-project
             centaur-tabs-get-group-name
             centaur-tabs-headline-match
             centaur-tabs-change-fonts
             centaur-tabs-enable-buffer-reordering)
  :hook ((after-init . centaur-tabs-mode)
         ((dashboard-mode
           calendar-mode
           process-menu-mode
           helpful-mode
           grep-mode
           vterm-mode term-mode
           magit-mode git-commit-mode
           minibuffer-mode which-key-mode
           org-agenda-mode org-capture-mode) . centaur-tabs-local-mode))
  :bind (("C-<prior>" . centaur-tabs-backward)
         ("C-<next>" . centaur-tabs-forward)
         ("C-c t" . centaur-tabs-counsel-switch-group)
         (:map evil-normal-state-map
          ("g t" . centaur-tabs-forward)
          ("g T" . centaur-tabs-backward)))
  :init
  (setq centaur-tabs-style                    "chamfer"
        centaur-tabs-icon-type                'nerd-icons
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
;;(centaur-tabs-group-by-projectile-project)
  (centaur-tabs-enable-buffer-reordering)
  (centaur-tabs-change-fonts (face-attribute 'default :font) centaur-tabs-font-size)

  :config
  (dolist (excluded-prefixes '( " *" "*Org Agenda*" "*Org Note*" "*Org Select*" "*Capture*" "*Calendar*" "*Flymake diagnostics"
                                "*flycheck-posframe-buffer*" "*Shell Command Output*" "*dashboard*" "*Directory*" "*vterm*"))
    (cl-pushnew excluded-prefixes centaur-tabs-excluded-prefixes)))


(provide 'init-centaur-tabs)
;;; init-centaur-tabs.el ends here
