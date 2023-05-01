;;; init-evil.el --- Initialize `evil-mode' configurations   -*- lexical-binding: t -*-
;;
;; Author: Luiz Tagliaferro <luiz@luiznux.com>
;; URL: https://luiznux.com
;; This file is free software :)
;;
;;; Commentary:
;;
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
  :hook (after-init . evil-mode)
  :init
  (setq evil-want-integration t
        evil-want-keybinding  nil
        evil-undo-system      'undo-tree))

(use-package evil-collection
  :after evil
  :hook (after-init . evil-collection-init)
  :custom (setq evil-collection-setup-minibuffer nil))

;; dependency for evil-undo-system
(use-package undo-tree
  :diminish
  :hook (after-init . global-undo-tree-mode)
  :init
  ;; Prevent undo tree files from polluting your git repo
  (setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo"))
        undo-tree-visualizer-timestamps   t
        undo-tree-visualizer-diff         t))

(use-package evil-org
  :after org
  :hook (org-mode . (lambda () (evil-org-mode)))
  :config
  (with-no-warnings
    (require 'evil-org-agenda)
    (evil-org-agenda-set-keys)))

(use-package evil-goggles
  :hook (after-init . evil-goggles-mode)
  :custom (evil-goggles-use-diff-faces)
  :init
  (setq evil-goggles-pulse nil))

(use-package evil-multiedit
  :bind (:map evil-visual-state-map
         ("R" . 'evil-multiedit-match-all)))

(use-package evil-commentary
  :config
  (evil-commentary-mode 1))

(use-package evil-surround
  :config
  (global-evil-surround-mode 1))

(use-package evil-matchit
  :config
  (global-evil-matchit-mode 1))


(provide 'init-evil)
;;; init-evil.el ends here
