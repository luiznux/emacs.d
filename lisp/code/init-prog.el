;;; init-prog.el --- Initialize programming configurations   -*- lexical-binding: t -*-
;;
;; Author: Luiz Tagliaferro <luiz@luiznux.com>
;; URL: https://luiznux.com
;; This file is free software :)
;;
;;; Commentary:
;;
;;
;;  ██████╗ ██████╗  ██████╗  ██████╗
;;  ██╔══██╗██╔══██╗██╔═══██╗██╔════╝
;;  ██████╔╝██████╔╝██║   ██║██║  ███╗
;;  ██╔═══╝ ██╔══██╗██║   ██║██║   ██║
;;  ██║     ██║  ██║╚██████╔╝╚██████╔╝
;;  ╚═╝     ╚═╝  ╚═╝ ╚═════╝  ╚═════╝
;;
;;; Code:

(require 'custom-config)
(require 'constants)
(require 'functions)

;; Prettify Symbols
;; e.g. display “lambda” as “λ”
(use-package prog-mode
  :ensure nil
  :hook (prog-mode . prettify-symbols-mode)
  :init
  (setq-default prettify-symbols-alist custom-prettify-symbols-alist)
  (setq prettify-symbols-unprettify-at-point 'right-edge))

;; Tree-sitter support
(when (emacs-treesit-available-p)
  (use-package treesit-auto
    :hook (after-init . global-treesit-auto-mode)
    :init (setq treesit-auto-install 'prompt)))

(use-package yasnippet
  :hook (after-init . yas-global-mode)
  :config
  (use-package yasnippet-snippets))

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
        (setq xref-show-xrefs-function #'ivy-xref-show-xrefs)))))

;; Jump to definition
(use-package dumb-jump
  :commands xref-show-definitions-completing-read
  :pretty-hydra
  ((:title (pretty-hydra-title "Dump Jump" 'faicon "nf-fa-anchor")
    :color blue :quit-key ("q" "C-g"))
   ("Jump"
    (("j" dumb-jump-go "Go")
     ("o" dumb-jump-go-other-window "Go other window")
     ("e" dumb-jump-go-prefer-external "Go external")
     ("x" dumb-jump-go-prefer-external-other-window "Go external other window"))
    "Other"
    (("i" dumb-jump-go-prompt "Prompt")
     ("l" dumb-jump-quick-look "Quick look")
     ("b" dumb-jump-back "Back"))))
  :bind ("C-M-j" . dumb-jump-hydra/body)
  :init
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate) ; use M-. to go to definition
  (setq  dumb-jump-prefer-searcher       'ag
         dumb-jump-force-searcher        'ag
         dumb-jump-selector              'ivy))

;; Code styles
(use-package editorconfig
  :diminish
  :hook (after-init . editorconfig-mode))

(use-package quickrun
  :bind
  (("C-<f5>"  . quickrun)
   ("C-c C-e" . quickrun-shell)))

(use-package csv-mode)

;; cucumber support
;; read https://github.com/michaelklishin/cucumber.el
(use-package feature-mode
  :init
  (add-to-list 'auto-mode-alist '("\.feature$" . feature-mode)))

(use-package ecukes)

(use-package nxml-mode
  :ensure nil
  :mode (("\\.xaml$" . xml-mode)))

(use-package ag)
(use-package terraform-mode)
(use-package vimrc-mode)
(use-package dart-mode)
(use-package mermaid-mode)
(use-package plantuml-mode)
(use-package cask-mode)
(use-package cmake-mode)
(use-package rmsbolt)                   ; A compiler output viewer


(provide 'init-prog)
;;; init-prog.el ends here
