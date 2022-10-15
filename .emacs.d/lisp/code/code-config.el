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
(require 'functions)
(require 'custom-config)


(use-package yasnippet
  :hook (after-init-hook . yas-global-mode)
  :config
  (yas-global-mode 1)
  (use-package yasnippet-snippets))

(use-package hideshow
  :ensure nil
  :diminish hs-minor-mode
  :hook (prog-mode . hs-minor-mode)
  :config
  ;; More functions
  ;; @see https://karthinks.com/software/simple-folding-with-hideshow/
  (defun hs-cycle (&optional level)
    (interactive "p")
    (let (message-log-max
          (inhibit-message t))
      (if (= level 1)
          (pcase last-command
            ('hs-cycle
             (hs-hide-level 1)
             (setq this-command 'hs-cycle-children))
            ('hs-cycle-children
             (save-excursion (hs-show-block))
             (setq this-command 'hs-cycle-subtree))
            ('hs-cycle-subtree
             (hs-hide-block))
            (_
             (if (not (hs-already-hidden-p))
                 (hs-hide-block)
               (hs-hide-level 1)
               (setq this-command 'hs-cycle-children))))
        (hs-hide-level level)
        (setq this-command 'hs-hide-level))))

  (defun hs-toggle-all ()
    "Toggle hide/show all."
    (interactive)
    (pcase last-command
      ('hs-toggle-all
       (save-excursion (hs-show-all))
       (setq this-command 'hs-global-show))
      (_ (hs-hide-all))))

  ;; Display line counts
  (defun hs-display-code-line-counts (ov)
    "Display line counts when hiding codes."
    (when (eq 'code (overlay-get ov 'hs))
      (overlay-put ov 'display
                   (concat
                    " "
                    (propertize
                     (if (char-displayable-p ?⏷) "⏷" "...")
                     'face 'shadow)
                    (propertize
                     (format " (%d lines)"
                             (count-lines (overlay-start ov)
                                          (overlay-end ov)))
                     'face '(:inherit shadow :height 0.8))
                    " "))))
  (setq hs-set-up-overlay #'hs-display-code-line-counts))

;; Automatic parenthesis pairing
(use-package elec-pair
  :ensure nil
  :hook (after-init . electric-pair-mode)
  :init (setq electric-pair-inhibit-predicate 'electric-pair-conservative-inhibit))

;; https://github.com/purcell/whitespace-cleanup-mode
(use-package whitespace-cleanup-mode
  :init
  (setq  global-whitespace-cleanup-mode nil))

;; https://github.com/Malabarba/aggressive-indent-mode
(use-package aggressive-indent
  :diminish
  :hook(
        ;; HACK: Disable in big files due to the performance issues
        ;; https://github.com/Malabarba/aggressive-indent-mode/issues/73
        (find-file . (lambda ()
                       (when (too-long-file-p)
                         (aggressive-indent-mode -1)))))
  :init
  (add-hook 'emacs-lisp-mode-hook #'aggressive-indent-mode)
  (add-hook 'lsp-mode-hook #'aggressive-indent-mode)
  :config
  ;; Disable in some modes
  (dolist (mode '(gitconfig-mode python-mode rustic-mode asm-mode web-mode html-mode css-mode go-mode scala-mode))
    (push mode aggressive-indent-excluded-modes))

  ;; Disable in some commands
  (add-to-list 'aggressive-indent-protected-commands #'delete-trailing-whitespace t)

  ;; Be slightly less aggressive in C/C++/C#/Java/Go/Swift
  (add-to-list 'aggressive-indent-dont-indent-if
               '(and (derived-mode-p 'c-mode 'c++-mode 'csharp-mode
                                     'java-mode 'go-mode 'swift-mode)
                     (null (string-match "\\([;{}]\\|\\b\\(if\\|for\\|while\\)\\b\\)"
                                         (thing-at-point 'line))))))

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
  :autoload drag-stuff-define-keys
  :hook (after-init . drag-stuff-global-mode)
  :config
  (with-no-warnings
    (add-to-list 'drag-stuff-except-modes 'org-mode)
    (drag-stuff-define-keys)))

;; A comprehensive visual interface to diff & patch
(use-package ediff
  :ensure nil
  :hook(;; show org ediffs unfolded
        (ediff-prepare-buffer . outline-show-all)
        ;; restore window layout when done
        (ediff-quit . winner-undo))
  :config
  (setq ediff-window-setup-function 'ediff-setup-windows-plain
        ediff-split-window-function 'split-window-horizontally
        ediff-merge-split-window-function 'split-window-horizontally))

;; Smartly select region, rectangle, multi cursors
(use-package smart-region
  :hook (after-init . smart-region-on))

;; Visual `align-regexp'
(use-package ialign)

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

;; Increase selected region by semantic units
(use-package expand-region
  :bind ("C-=" . er/expand-region))

(use-package autorevert
  :ensure nil
  :hook (after-init . global-auto-revert-mode))

;; Pass a URL to a WWW browser
(use-package browse-url
  :ensure nil
  :defines dired-mode-map
  :bind (("C-c C-z ." . browse-url-at-point)
         ("C-c C-z b" . browse-url-of-buffer)
         ("C-c C-z r" . browse-url-of-region)
         ("C-c C-z u" . browse-url)
         ("C-c C-z e" . browse-url-emacs)
         ("C-c C-z v" . browse-url-of-file))
  :init
  (with-eval-after-load 'dired
    (bind-key "C-c C-z f" #'browse-url-of-file dired-mode-map)))

;; Click to browse URL or to send to e-mail address
(use-package goto-addr
  :ensure nil
  :hook ((text-mode . goto-address-mode)
         (prog-mode . goto-address-prog-mode)))

;; Code styles
(use-package editorconfig
  :diminish
  :hook (after-init . editorconfig-mode))

(use-package quickrun
  :bind
  (("<f5>"    . quickrun)
   ("M-<f5>"  . quickrun-shell)
   ("C-c e"   . quickrun)
   ("C-c C-e" . quickrun-shell)))

;; Prettify Symbols
;; e.g. display “lambda” as “λ”
(use-package prog-mode
  :ensure nil
  :hook (prog-mode . prettify-symbols-mode)
  :init
  (setq-default prettify-symbols-alist custom-prettify-symbols-alist)
  (setq prettify-symbols-unprettify-at-point 'right-edge))

;; Search tool
(use-package grep
  :ensure nil
  :autoload grep-apply-setting
  :config
  (cond
   ((executable-find "ugrep")
    (grep-apply-setting
     'grep-command "ugrep --color=auto -0In -e ")
    (grep-apply-setting
     'grep-template "ugrep --color=auto -0In -e <R> <D>")
    (grep-apply-setting
     'grep-find-command '("ugrep --color=auto -0Inr -e ''" . 30))
    (grep-apply-setting
     'grep-find-template "ugrep <C> -0Inr -e <R> <D>"))
   ((executable-find "rg")
    (grep-apply-setting
     'grep-command "rg --color=auto --null -nH --no-heading -e ")
    (grep-apply-setting
     'grep-template "rg --color=auto --null --no-heading -g '!*/' -e <R> <D>")
    (grep-apply-setting
     'grep-find-command '("rg --color=auto --null -nH --no-heading -e ''" . 38))
    (grep-apply-setting
     'grep-find-template "rg --color=auto --null -nH --no-heading -e <R> <D>"))))

;; Only support with dynamic module
(when (functionp 'module-load)
  (use-package tree-sitter
    :ensure tree-sitter-langs
    :diminish
    :hook ((after-init . global-tree-sitter-mode)
           (tree-sitter-after-on . tree-sitter-hl-mode))))

;; Cross-referencing commands
(use-package xref
  :ensure nil
  :config
  (with-no-warnings
    ;; Use faster search tool
    (when emacs/>=28p
      (add-to-list 'xref-search-program-alist
                   '(ugrep . "xargs -0 ugrep <C> --null -ns -e <R>"))
      (cond
       ((executable-find "ugrep")
        (setq xref-search-program 'ugrep))
       ((executable-find "rg")
        (setq xref-search-program 'ripgrep))))

    ;; Select from xref candidates with Ivy
    (if emacs/>=28p
        (setq xref-show-definitions-function #'xref-show-definitions-completing-read
              xref-show-xrefs-function #'xref-show-definitions-completing-read)
      (use-package ivy-xref
        :after ivy
        :init
        (when emacs/>=27p
          (setq xref-show-definitions-function #'ivy-xref-show-defs))
        (setq xref-show-xrefs-function #'ivy-xref-show-xrefs)))))

;; Jump to definition
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

;; Windows-scroll commands
(use-package pager
  :bind (([remap scroll-up-command] . pager-page-down)
         ([remap scroll-down-command] . pager-page-up)
         ([next]   . pager-page-down)
         ([prior]  . pager-page-up)
         ([M-up]   . pager-row-up)
         ([M-kp-8] . pager-row-up)
         ([M-down] . pager-row-down)
         ([M-kp-2] . pager-row-down)))

(use-package undohist
  :hook
  (with-no-warnings
    after-init-hook . (undohist-initialize)))

;; Preview when `goto-line'
(use-package goto-line-preview
  :bind ([remap goto-line] . goto-line-preview))

;; cucumber support
;; read https://github.com/michaelklishin/cucumber.el
(use-package feature-mode
  :init
  (add-to-list 'auto-mode-alist '("\.feature$" . feature-mode)))

(use-package ecukes)

(use-package nxml-mode
  :ensure nil
  :mode (("\\.xaml$" . xml-mode)))

;; New `conf-toml-mode' in Emacs 26
(unless (fboundp 'conf-toml-mode)
  (use-package toml-mode))

;; Open files as another user
(unless sys/win32p
  (use-package sudo-edit))

(when emacs/>=27p
  (use-package csv-mode))

(use-package ag)
(use-package terraform-mode)
(use-package vimrc-mode)
(use-package mermaid-mode)
(use-package plantuml-mode)
(use-package cask-mode)
(use-package cmake-mode)
(use-package format-all)
(use-package rmsbolt)                   ; A compiler output viewer


(provide 'code-config)
;;; code-config.el ends here
