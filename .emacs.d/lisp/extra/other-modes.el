;;; other-modes.el --- Package configuration for other modes packages  -*- lexical-binding: t -*-
;;
;; Author: Luiz Tagliaferro <luiz@luiznux.com>
;; URL: https://luiznux.com
;; This file is free software :)
;;
;;; Commentary:
;;
;; Emacs other modes configuration.
;;
;;   ██████╗ ████████╗██╗  ██╗███████╗██████╗ ███████╗
;;  ██╔═══██╗╚══██╔══╝██║  ██║██╔════╝██╔══██╗██╔════╝
;;  ██║   ██║   ██║   ███████║█████╗  ██████╔╝███████╗
;;  ██║   ██║   ██║   ██╔══██║██╔══╝  ██╔══██╗╚════██║
;;  ╚██████╔╝   ██║   ██║  ██║███████╗██║  ██║███████║
;;   ╚═════╝    ╚═╝   ╚═╝  ╚═╝╚══════╝╚═╝  ╚═╝╚══════╝
;;
;;; Code:

(use-package terraform-mode)
(use-package vimrc-mode)
(use-package google-this)

(use-package google-translate
  :bind
  ("M-o t" . google-translate-at-point)
  ("M-o T" . google-translate-at-point-reverse)
  :custom
  (google-translate-default-source-language "en")
  (google-translate-default-target-language "ja"))

(use-package olivetti
  :diminish
  :bind ("<f7>" . olivetti-mode)
  :init (setq olivetti-body-width 0.62))

;; emacs stuffs
(use-package copyit)                    ; copy path, url, etc.
(use-package focus)                     ; Focus on the current region
(use-package keycast-mode)
(use-package memory-usage)
(use-package bug-hunter)
(use-package logview)
(use-package daemons)                 ; system services/daemons
(use-package tldr)

;; Process
(use-package proced
  :ensure nil
  :init
  (setq-default proced-format 'verbose)
  (setq proced-auto-update-flag t
        proced-auto-update-interval 3))

(use-package esup
  :config
  ;; Work around a bug where esup tries to step into the byte-compiled
  ;; version of `cl-lib', and fails horribly.
  (setq esup-depth 0)
  :pin melpa)

(use-package list-environment
  :hook (list-environment-mode . (lambda ()
                                   (setq tabulated-list-format
                                         (vconcat `(("" , 2))
                                                  tabulated-list-format))
                                   (tabulated-list-init-header)))
  :init
  (with-no-warnings
    (defun my-list-environment-entries ()
      "Generate environment variable entries list for tabulated-list."
      (mapcar (lambda (env)
                (let* ((kv (split-string env "="))
                       (key (car kv))
                       (val (mapconcat #'identity (cdr kv) "=")))
                  (list key (vector
                             (all-the-icons-octicon "key" :height 0.8 :v-adjust -0.05)
                             `(,key face font-lock-keyword-face)
                             `(,val face font-lock-string-face)))))
              process-environment))
    (advice-add #'list-environment-entries :override #'my-list-environment-entries)))

;; games and useless things
(use-package hackernews
  :commands (hackernews))
(use-package 2048-game
  :commands (2048-game))
(use-package speed-type
  :commands (speed-type-text))
(use-package snow)


(provide 'other-modes)
;;; other-modes.el ends here
