;;; init.el --- Initialization file for Emacs  -*- lexical-binding: t  no-byte-compile: t -*-
;;
;; Author: Luiz Tagliaferro <luiz@luiznux.com>
;; URL: https://luiznux.com
;; This file is free software :)
;;
;;; Commentary:
;;
;;  ███████╗███╗   ███╗ █████╗  ██████╗███████╗
;;  ██╔════╝████╗ ████║██╔══██╗██╔════╝██╔════╝
;;  █████╗  ██╔████╔██║███████║██║     ███████╗
;;  ██╔══╝  ██║╚██╔╝██║██╔══██║██║     ╚════██║
;;  ███████╗██║ ╚═╝ ██║██║  ██║╚██████╗███████║
;;  ╚══════╝╚═╝     ╚═╝╚═╝  ╚═╝ ╚═════╝╚══════╝
;;
;;
;;; Code:

(when (version< emacs-version "26.1")
  (error "This requires Emacs 26.1 and above!"))

;; avoid anoing message
(setq byte-compile-warnings '(cl-functions))

;;
;; Speed up startup
;;

;; Defer garbage collection further back in the startup process
(setq gc-cons-threshold most-positive-fixnum)

;; Prevent flash of unstyled modeline at startup
(setq-default mode-line-format nil)

;; Don't pass case-insensitive to `auto-mode-alist'
(setq auto-mode-case-fold nil)

(unless (or (daemonp) noninteractive init-file-debug)
  ;; Suppress file handlers operations at startup
  ;; `file-name-handler-alist' is consulted on each call to `require' and `load'
  (let ((old-value file-name-handler-alist))
    (setq file-name-handler-alist nil)
    (set-default-toplevel-value 'file-name-handler-alist file-name-handler-alist)
    (add-hook 'emacs-startup-hook
              (lambda ()
                "Recover file name handlers."
                (setq file-name-handler-alist
                      (delete-dups (append file-name-handler-alist old-value))))
              101)))

;; Load path
;; Optimize: Force "lisp" at the head to reduce the startup time.
(defun update-load-path (&rest _)
  "Update `load-path'."
  (dolist (dir '("lisp"))
    (push (expand-file-name dir user-emacs-directory) load-path)))

(defun add-subdirs-to-load-path (&rest _)
  "Add subdirectories to `load-path'."
  (let ((default-directory (expand-file-name "lisp" user-emacs-directory)))
    (normal-top-level-add-subdirs-to-load-path)))

(advice-add #'package-initialize :after #'update-load-path)
(advice-add #'package-initialize :after #'add-subdirs-to-load-path)

(update-load-path)

(require 'packages)

;; Core packages
(require 'base)
(require 'hydra-config)
(require 'interface)
(require 'edit-config)
(require 'window-config)
(require 'ibuffer-config)
(require 'highlight-config)
(require 'kill-ring-config)
(require 'persp-config)
(require 'evil-config)
(require 'company-config)
(require 'bookmark-config)
(require 'ivy-config)

;; Visual
(require 'dashboard-config)
(require 'centaur-tabs-config)
(require 'treemacs-config)
(require 'dired-config)

;; Org and Agenda config
(require 'org-env-config)
(require 'org-mode-config)
(require 'org-auto-update-state)
(require 'org-agenda-config)
(require 'file-color-agenda)

;; Programming
(require 'git-config)
(require 'flycheck-config)
(require 'projectile-config)
(require 'lsp-config)
(require 'ctags-config)

(require 'prog-config)
(require 'elisp-config)
(require 'c-config)
(require 'clojure-config)
(require 'python-config)
(require 'go-config)
(require 'rust-config)
(require 'dart-config)
(require 'web-config)
(require 'docker-config)
(require 'player-config)
(require 'other-modes)
(require 'terminal-config)
(require 'markdown-config)
(require 'reader-config)


;;; init.el ends here
