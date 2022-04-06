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
;;; Code:

(require 'constants)
(require 'custom-config)


(use-package yasnippet
  :hook (after-init-hook . yas-global-mode)
  :config
  (yas-global-mode 1)
  (use-package yasnippet-snippets))

(use-package origami
  :hook (prog-mode . origami-mode)
  :init (setq origami-show-fold-header t)
  :config (face-spec-reset-face 'origami-fold-header-face))

;; Automatic parenthesis pairing
(use-package elec-pair
  :ensure nil
  :hook (after-init . electric-pair-mode)
  :init (setq electric-pair-inhibit-predicate 'electric-pair-conservative-inhibit))

  ;;; https://github.com/purcell/whitespace-cleanup-mode
(use-package whitespace-cleanup-mode
  :init
  (setq  global-whitespace-cleanup-mode nil))

  ;;; https://github.com/Malabarba/aggressive-indent-mode
(use-package aggressive-indent
  :init
  (global-aggressive-indent-mode 0)
  (add-hook 'emacs-lisp-mode-hook #'aggressive-indent-mode)
  (add-to-list 'aggressive-indent-excluded-modes 'html-mode)
  ;; Disable in some commands
  (add-to-list 'aggressive-indent-protected-commands #'delete-trailing-whitespace t))

;; Show number of matches in mode-line while searching
(use-package anzu
  :bind
  (([remap query-replace] . anzu-query-replace)
   ([remap query-replace-regexp] . anzu-query-replace-regexp)
   :map isearch-mode-map
   ([remap isearch-query-replace] . anzu-isearch-query-replace)
   ([remap isearch-query-replace-regexp] . anzu-isearch-query-replace-regexp))

  :hook (after-init . global-anzu-mode)
  :init
  (setq anzu-replace-to-string-separator " => "
        anzu-deactivate-region           t
        anzu-mode-lighter                ""))

;; Drag stuff (lines, words, region, etc...) around
(use-package drag-stuff
  :diminish
  :commands drag-stuff-define-keys
  :hook (after-init . drag-stuff-global-mode)
  :config
  (with-no-warnings
    (add-to-list 'drag-stuff-except-modes 'org-mode)
    (drag-stuff-define-keys)))

;; Smartly select region, rectangle, multi cursors
(use-package smart-region
  :hook (after-init . smart-region-on))

;; Edit multiple regions in the same way simultaneously
(use-package iedit
  :defines desktop-minor-mode-table
  :bind (("C-;" . iedit-mode)
         ("C-x r RET" . iedit-rectangle-mode)
         :map isearch-mode-map ("C-;" . iedit-mode-from-isearch)
         :map esc-map ("C-;" . iedit-execute-last-modification)
         :map help-map ("C-;" . iedit-mode-toggle-on-function))
  :config
  ;; Avoid restoring `iedit-mode'
  (with-eval-after-load 'desktop
    (add-to-list 'desktop-minor-mode-table
                 '(iedit-mode nil))))

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

;; Prettify Symbols
;; e.g. display “lambda” as “λ”
(use-package prog-mode
  :ensure nil
  :hook (prog-mode . prettify-symbols-mode)
  :init
  (setq-default prettify-symbols-alist custom-prettify-symbols-alist)
  (setq prettify-symbols-unprettify-at-point 'right-edge))

;; Only support with dynamic module
(when (functionp 'module-load)
  (use-package tree-sitter
    :ensure tree-sitter-langs
    :hook ((after-init . global-tree-sitter-mode)
           (tree-sitter-after-on . tree-sitter-hl-mode))))

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
