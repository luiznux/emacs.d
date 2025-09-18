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

(when (version< emacs-version "29.1")
  (error "This requires Emacs 29.1 and above!"))

;;
;; Speed up startup Process
;;

;; Optimize Garbage Collection for Startup
(setq gc-cons-threshold most-positive-fixnum)

;; Optimize `auto-mode-alist'
(setq auto-mode-case-fold nil)

(unless (or (daemonp) noninteractive init-file-debug)
  ;; Temporarily suppress file-handler processing to speed up startup
  ;; `file-name-handler-alist' is consulted on each call to `require' and `load'
  (let ((default-handlers file-name-handler-alist))
    (setq file-name-handler-alist nil)
    ;; Recover handlers after startup
    (add-hook 'emacs-startup-hook
              (lambda ()
                "Recover file name handlers."
                (setq file-name-handler-alist
                      (delete-dups (append file-name-handler-alist default-handlers))))
              101)))

;;
;; Configure Load Path
;;

;; Add "lisp" and "site-lisp" to the beginning of `load-path`
(defun update-load-path (&rest _)
  "Update the `load-path' to prioritize personal configurations."
  (dolist (dir '("lisp" "site-lisp"))
    (push (expand-file-name dir user-emacs-directory) load-path)))

;; Add subdirectories inside "site-lisp" and "lisp" to `load-path'
(defun add-subdirs-to-load-path (&rest _)
  "Recursively add subdirectories in `site-lisp` to `load-path`.

Avoid placing large files like EAF in `site-lisp` to prevent slow startup."
  (dolist (dir '("lisp" "site-lisp"))
    (let ((default-directory (expand-file-name dir user-emacs-directory)))
      (normal-top-level-add-subdirs-to-load-path))))

;; Ensure these functions are called after `package-initialize
(advice-add #'package-initialize :after #'update-load-path)
(advice-add #'package-initialize :after #'add-subdirs-to-load-path)

;; Initialize load paths explicitly
(update-load-path)

;; Requisites
(require 'init-constants)
(require 'init-custom)
(require 'init-functions)

;; Packages
(require 'init-packages)

;; Core packages
(require 'init-base)
(require 'init-keybinds)
(require 'init-hydra)
(require 'init-interface)
(require 'init-edit)
(require 'init-evil)
(require 'init-completion)

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
(require 'init-checker)
(require 'init-projectile)
(require 'init-lsp)

(require 'init-prog)
(require 'init-elisp)
(require 'init-clojure)
(require 'init-ctags)
(require 'init-c)
(require 'init-go)
(require 'init-rust)
(require 'init-python)
(require 'init-web)


;;; init.el ends here
