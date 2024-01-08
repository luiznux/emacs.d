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

(when (version< emacs-version "28.1")
  (error "This requires Emacs 28.1 and above!"))

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
  (dolist (dir '("lisp" "site-lisp"))
    (push (expand-file-name dir user-emacs-directory) load-path)))

(defun add-subdirs-to-load-path (&rest _)
  "Add subdirectories to `load-path'."
  (dolist (dir '("lisp" "site-lisp"))
    (let ((default-directory (expand-file-name dir user-emacs-directory)))
      (normal-top-level-add-subdirs-to-load-path))))

(advice-add #'package-initialize :after #'update-load-path)
(advice-add #'package-initialize :after #'add-subdirs-to-load-path)

(update-load-path)

;; Requisites
(require 'constants)
(require 'custom-config)
(require 'functions)

(require 'packages)

;; Core packages
(require 'base)
(require 'init-hydra)
(require 'init-interface)
(require 'init-edit)
(require 'init-evil)
(require 'init-ivy)
(require 'init-company)
(require 'init-corfu)

(require 'init-bookmark)
(require 'init-dashboard)
(require 'init-dired)
(require 'init-highlight)
(require 'init-ibuffer)
(require 'init-kill-ring)
(require 'init-workspace)
(require 'init-window)
(require 'init-treemacs)
(require 'init-centaur-tabs)
(require 'init-terminal)
(require 'init-markdown)

;; Org and Agenda config
(require 'init-org)
(require 'init-org-agenda)

(require 'init-reader)
(require 'init-docker)
(require 'init-extra)

;; Programming
(require 'init-vcs)
(require 'init-flymake)
(require 'init-projectile)
(require 'init-lsp)
(require 'init-ctags)

(require 'init-prog)
(require 'init-elisp)
(require 'init-clojure)
(require 'init-c)
(require 'init-go)
(require 'init-rust)
(require 'init-python)
(require 'init-web)


;;; init.el ends here
