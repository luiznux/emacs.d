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

;; Speed up startup
(setq auto-mode-case-fold nil)

(unless (or (daemonp) noninteractive init-file-debug)
  (let ((old-file-name-handler-alist file-name-handler-alist))
    (setq file-name-handler-alist nil)
    (add-hook 'emacs-startup-hook
              (lambda ()
                "Recover file name handlers."
                (setq file-name-handler-alist
                      (delete-dups (append file-name-handler-alist
                                           old-file-name-handler-alist)))))))

;; avoid anoing message
(setq byte-compile-warnings '(cl-functions))

;; Defer garbage collection further back in the startup process
(setq gc-cons-threshold most-positive-fixnum)
(add-hook 'emacs-startup-hook
          (lambda ()
            "Recover GC values after startup."
            (setq gc-cons-threshold 800000)))

;; Suppress flashing at startup
(setq-default inhibit-redisplay t
              inhibit-message t)
(add-hook 'window-setup-hook
          (lambda ()
            (setq-default inhibit-redisplay nil
                          inhibit-message nil)
            (redisplay)))

;; Load path
;; Optimize: Force "lisp"" and "elpa" at the head to reduce the startup time.
(defun update-load-path (&rest _)
  "Update `load-path'."
  (dolist (dir '("elpa" "lisp"))
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
(require 'basic)
(require 'interface)
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
(require 'centaur-tabs-config)
(require 'dashboard-config)
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

(require 'code-config)
(require 'elisp-config)
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
