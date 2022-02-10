;;; basic.el --- Basic emasc config file  -*- lexical-binding: t -*-
;;
;; Author: Luiz Tagliaferro <luiz@luiznux.com>
;; URL: https://luiznux.com
;; This file is free software :)
;;
;;; Commentary:
;;
;; basics Emacs config.
;;
;;  ██████╗  █████╗ ███████╗██╗ ██████╗
;;  ██╔══██╗██╔══██╗██╔════╝██║██╔════╝
;;  ██████╔╝███████║███████╗██║██║
;;  ██╔══██╗██╔══██║╚════██║██║██║
;;  ██████╔╝██║  ██║███████║██║╚██████╗
;;  ╚═════╝ ╚═╝  ╚═╝╚══════╝╚═╝ ╚═════╝
;;
;;; Code:

(require 'constants)
(require 'custom-config)
(require 'functions)

(with-no-warnings
  ;; Garbage Collector Magic Hack
  (use-package gcmh
    :diminish
    :init
    (setq gcmh-idle-delay 5
          gcmh-high-cons-threshold #x1000000) ; 16MB
    (gcmh-mode 1)))

;; Environment
(when (or sys/mac-x-p sys/linux-x-p (daemonp))
  (use-package exec-path-from-shell
    :init
    (setq exec-path-from-shell-variables '("PATH" "MANPATH" "SSH_AUTH_SOCK" "SSH_AGENT_PID" "GPG_AGENT_INFO" "LANG" "LC_CTYPE")
          exec-path-from-shell-arguments '("-l"))
    (exec-path-from-shell-initialize)))

;; Increase how much is read from processes in a single chunk (default is 4kb)
(setq read-process-output-max #x10000)  ; 64kb

;; History
(use-package saveplace
  :ensure nil
  :hook (after-init . save-place-mode))

(use-package recentf
  :ensure nil
  :bind (("C-x C-r" . recentf-open-files))
  :hook (after-init . recentf-mode)
  :init (setq recentf-max-saved-items 300
              recentf-exclude
              '("\\.?cache" ".cask" "url" "COMMIT_EDITMSG\\'" "bookmarks"
                "\\.\\(?:gz\\|gif\\|svg\\|png\\|jpe?g\\|bmp\\|xpm\\)$"
                "\\.?ido\\.last$" "\\.revive$" "/G?TAGS$" "/.elfeed/"
                "^/tmp/" "^/var/folders/.+$" "^/ssh:" "/persp-confs/"
                "/org/*" ;; prevent  show recent org-agenda files
                (lambda (file) (file-in-directory-p file package-user-dir))))
  :config
  (push (expand-file-name recentf-save-file) recentf-exclude)
  (add-to-list 'recentf-filename-handlers #'abbreviate-file-name))

(use-package savehist
  :ensure nil
  :hook (after-init . savehist-mode)
  :init (setq enable-recursive-minibuffers t ; Allow commands in minibuffers
              history-length 1000
              savehist-additional-variables '(mark-ring
                                              global-mark-ring
                                              search-ring
                                              regexp-search-ring
                                              extended-command-history)
              savehist-autosave-interval 300))

(when emacs/>=27p
  (use-package so-long
    :ensure nil
    :hook (after-init . global-so-long-mode)
    :config (setq so-long-threshold 400)))

;; Fullscreen
(when (display-graphic-p)
  (bind-keys ("C-<f11>" . toggle-frame-fullscreen)
             ("S-s-<return>" . toggle-frame-fullscreen)
             ("M-S-<return>" . toggle-frame-fullscreen)))


(provide 'basic)
;;; basic.el ends here
