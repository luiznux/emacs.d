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
  :vc (:url "https://github.com/ema2159/centaur-tabs"
       :rev "c72029609c7a6ae64cfcbf69331d6253319c0906")
  :defines (evil-normal-state-map
            centaur-tabs-font-size
            centaur-tabs-excluded-prefixes)
  :commands (centaur-tabs-group-by-projectile-project
             centaur-tabs-headline-match
             centaur-tabs-change-fonts
             centaur-tabs-adjust-buffer-order
             centaur-tabs-enable-buffer-reordering)
  :hook ((after-init . centaur-tabs-mode)
         ((dashboard-mode
           calendar-mode process-menu-mode
           helpful-mode grep-mode vterm-mode term-mode
           magit-mode git-commit-mode minibuffer-mode which-key-mode
           org-agenda-mode org-capture-mode) . centaur-tabs-local-mode))
  :bind (("C-<prior>"                 . centaur-tabs-backward)
         ("C-<next>"                  . centaur-tabs-forward)
         ("C-S-<prior>"               . centaur-tabs-move-current-tab-to-left)
         ("C-S-<next>"                . centaur-tabs-move-current-tab-to-right)
         ("<tab-line>C-<wheel-up>"    . centaur-tabs-move-current-tab-to-left)
         ("<tab-line>C-<wheel-down>"  . centaur-tabs-move-current-tab-to-right)
         (:map evil-normal-state-map
          ("g t" . centaur-tabs-forward)
          ("g T" . centaur-tabs-backward)))
  :init
  (setq centaur-tabs-style                    "box"
        centaur-tabs-icon-type                'nerd-icons
        centaur-tabs-height                   32
        centaur-tabs-set-bar                  'under
        centaur-tabs-adjust-buffer-order      'right
        centaur-tabs-set-icons                t
        centaur-tabs-show-new-tab-button      t
        centaur-tabs-set-modified-marker      t
        x-underline-at-descent-line           t
        centaur-tabs-show-navigation-buttons  nil
        centaur-tabs-show-count               nil)
  :config
  (centaur-tabs-headline-match)
  (centaur-tabs-enable-buffer-reordering)
  (centaur-tabs-group-by-projectile-project)
  (advice-add 'centaur-tabs-buffer-track-killed :override #'ignore)
  (centaur-tabs-change-fonts (face-attribute 'default :font) centaur-tabs-font-size)

  (dolist
      (excluded-prefixes
       '( " *" "*Org Agenda*" "*Org Note*" "*Org Select*" "*Capture*" "*Calendar*" "*Flymake diagnostics" "*Kill Ring*"
          "*flycheck-posframe-buffer*" "*Shell Command Output*" "*dashboard*" "*Directory*" "*vterm*" "*compilation*"
          "magit:" "magit-" "*vc*"))
    (add-to-list 'centaur-tabs-excluded-prefixes excluded-prefixes)))


(provide 'init-centaur-tabs)
;;; init-centaur-tabs.el ends here
