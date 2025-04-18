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
  :bind (:map evil-normal-state-map
         ("M-." . nil))
  :hook (after-init . evil-mode)
  :init
  (setq evil-want-integration t
        evil-want-C-u-scroll  t
        evil-want-keybinding  nil
        evil-echo-state       nil
        evil-undo-system      'undo-redo))

(use-package evil-collection
  :after evil
  :hook (after-init . evil-collection-init)
  :custom (setq evil-collection-setup-minibuffer nil))

(use-package evil-org
  :commands (evil-org-mode)
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
  :commands evil-multiedit-default-keybinds
  :init
  (evil-multiedit-default-keybinds))

(use-package evil-commentary
  :commands evil-commentary-mode
  :config
  (evil-commentary-mode 1))

(use-package evil-surround
  :commands global-evil-surround-mode
  :config
  (global-evil-surround-mode 1))

(use-package evil-embrace
  :commands evil-embrace-enable-evil-surround-integration
  :after evil-surround
  :hook ((org-mode . embrace-org-mode-hook)
         (emacs-lisp-mode . embrace-emacs-lisp-mode-hook))
  :init
  (evil-embrace-enable-evil-surround-integration))

(use-package evil-matchit
  :commands global-evil-matchit-mode
  :config
  (global-evil-matchit-mode 1))


(provide 'init-evil)
;;; init-evil.el ends here
