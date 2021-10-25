;;; code-config.el --- Packages for code features  -*- lexical-binding: t -*-
;;
;; Author: Luiz Tagliaferro <luiz@luiznux.com>
;; URL: https://luiznux.com
;; This file is free software :)
;;
;;; Commentary:
;;
;; Code features packages configuration
;;
;;    ██████╗ ██████╗ ██████╗ ███████╗
;;   ██╔════╝██╔═══██╗██╔══██╗██╔════╝
;;   ██║     ██║   ██║██║  ██║█████╗
;;   ██║     ██║   ██║██║  ██║██╔══╝
;;   ╚██████╗╚██████╔╝██████╔╝███████╗
;;    ╚═════╝ ╚═════╝ ╚═════╝ ╚══════╝
;;
;;
;;; Code:

(require 'customizations)


(use-package yasnippet
  :hook (after-init-hook . yas-global-mode)
  :config
  (yas-global-mode 1)
  (use-package yasnippet-snippets))

(use-package origami
  :init
  (global-origami-mode))

;; Autoclose brackets, quotes.
(use-package elec-pair
  :init
  (electric-pair-mode 1))

  ;;; https://github.com/purcell/whitespace-cleanup-mode
(use-package whitespace-cleanup-mode
  :init
  (setq  global-whitespace-cleanup-mode nil))

  ;;; https://github.com/Malabarba/aggressive-indent-mode
(use-package aggressive-indent
  :init
  (global-aggressive-indent-mode 0)
  (add-hook 'emacs-lisp-mode-hook #'aggressive-indent-mode)
  (add-to-list 'aggressive-indent-excluded-modes 'html-mode))

(use-package anzu
  :bind
  ("C-r"   . anzu-query-replace-regexp)
  ("C-M-r" . anzu-query-replace-at-cursor-thing)
  :hook (after-init . global-anzu-mode)
  :init
  (setq anzu-replace-to-string-separator " => "
        anzu-deactivate-region           t
        anzu-mode-lighter                ""))

(use-package rainbow-delimiters
  :hook
  (prog-mode . rainbow-delimiters-mode))

(use-package autorevert
  :ensure nil
  :hook (after-init . global-auto-revert-mode))

(use-package quickrun
  :bind
  (("<f5>" . quickrun)
   ("M-<f5>" . quickrun-shell)
   ("C-c e" . quickrun)
   ("C-c C-e" . quickrun-shell)))

;; Cross-referencing commands
(use-package xref
  :ensure nil
  :init
  (when (and (boundp 'xref-search-program) (executable-find "ag"))
    (setq xref-search-program 'ripgrep))

  (if emacs/>=28p
      (setq xref-show-xrefs-function #'xref-show-definitions-completing-read
            xref-show-definitions-function #'xref-show-definitions-completing-read)
    ;; Select from xref candidates with Ivy
    (use-package ivy-xref
      :after ivy
      :init
      (when emacs/>=27p
        (setq xref-show-definitions-function #'ivy-xref-show-defs))
      (setq xref-show-xrefs-function #'ivy-xref-show-xrefs))))

(use-package dumb-jump
  :commands xref-show-definitions-completing-read
  :init
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate) ; use M-. to go to definition
  (setq  dumb-jump-prefer-searcher       'ag
         dumb-jump-force-searcher        'ag
         dumb-jump-selector              'ivy))

(use-package ggtags
  :init
  (add-hook 'c-mode-common-hook
            (lambda ()
              (when (derived-mode-p 'c-mode 'c++-mode 'java-mode)
                (ggtags-mode 1)))))

(use-package projectile
  :bind (:map projectile-mode-map
              ("C-c p" . projectile-command-map))
  :hook (after-init . projectile-mode)
  :init
  (setq projectile-sort-order 'recentf
        projectile-use-git-grep t)
  :config
  ;; Use the faster searcher to handle project files: ripgrep `rg'.
  (when (and (not (executable-find "fd"))
             (executable-find "rg"))
    (setq projectile-generic-command
          (let ((rg-cmd ""))
            (dolist (dir projectile-globally-ignored-directories)
              (setq rg-cmd (format "%s --glob '!%s'" rg-cmd dir)))
            (concat "rg -0 --files --color=never --hidden" rg-cmd)))))

(use-package minimap
  :custom
  (minimap-major-modes '(prog-mode))
  :custom-face
  '(minimap-font-face ((t (:height 32 :family "DejaVu Sans Mono"))))
  '(minimap-active-region-background ((t (:extend t :background "#232526"))))
  '(minimap-current-line-face ((t (:background "#344256"))))
  :init
  (setq minimap-window-location 'right
        minimap-update-delay 0.5
        minimap-highlight-line  t
        minimap-hide-scroll-bar nil
        minimap-display-semantic-overlays t))

(use-package undohist
  :hook
  (with-no-warnings
    after-init-hook . (undohist-initialize)))


(use-package ag)
(use-package format-all)
(use-package sudo-edit)


(provide 'code-config)
;;; code-config.el ends here
