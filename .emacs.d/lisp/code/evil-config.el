;;; evil-config.el --- Package configuration for `evil'  -*- lexical-binding: t -*-
;;
;; Author: Luiz Tagliaferro <luiz@luiznux.com>
;; URL: https://luiznux.com
;; This file is free software :)
;;
;;; Commentary:
;;
;; Emacs Evil configuration --- Package configuration for Emacs evil
;;
;; ███████╗██╗   ██╗██╗██╗
;; ██╔════╝██║   ██║██║██║
;; █████╗  ██║   ██║██║██║
;; ██╔══╝  ╚██╗ ██╔╝██║██║
;; ███████╗ ╚████╔╝ ██║███████╗
;; ╚══════╝  ╚═══╝  ╚═╝╚══════╝
;;
;;; Code:

(use-package evil
  :demand t
  :commands 'evil-scroll-up
  :bind (:map evil-normal-state-map
         ("M-." . nil)
         ("C-u" . 'evil-scroll-up))
  :init
  (setq evil-want-integration t
        evil-want-keybinding  nil
        evil-undo-system      'undo-tree)

  :config
  (evil-mode 1)

  ;; dependency for evil-undo-system
  (use-package undo-tree
    :hook (after-init . global-undo-tree-mode)
    :init
    ;; Prevent undo tree files from polluting your git repo
    (setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo"))))

  (use-package evil-collection
    :demand t
    :after evil
    :init
    (setq evil-collection-setup-minibuffer nil)
    :config
    (evil-collection-init))

  (use-package evil-org
    :demand t
    :after evil org
    :hook ('org-mode-hook . (lambda () (evil-org-mode)))
    :config
    (with-no-warnings
      (require 'evil-org-agenda)
      (evil-org-agenda-set-keys)
      (evil-org-set-key-theme
       '(textobjects insert navigation additional shift todo heading)))))

(use-package evil-goggles
  :init
  (setq evil-goggles-pulse nil)
  :config
  (with-no-warnings
    (evil-goggles-mode)
    (evil-goggles-use-diff-faces)))

(use-package evil-multiedit
  :bind (:map evil-visual-state-map
         ("R" . 'evil-multiedit-match-all)))

(use-package evil-commentary
  :config
  (global-evil-surround-mode 1))

(use-package evil-surround
  :config
  (global-evil-surround-mode 1))

(use-package evil-matchit
  :config
  (global-evil-matchit-mode 1))

(use-package evil-leader
  :config
  (global-evil-leader-mode))


(provide 'evil-config)
;;; evil-config.el ends here
