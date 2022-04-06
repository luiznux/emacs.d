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
(use-package jenkinsfile-mode)
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
  :config
  (setq-default olivetti-body-width 100))

;; emacs stuffs
(use-package esup
  :config
  ;; Work around a bug where esup tries to step into the byte-compiled
  ;; version of `cl-lib', and fails horribly.
  (setq esup-depth 0)
  :pin melpa)
(use-package memory-usage)
(use-package bug-hunter)
(use-package logview
  :defer t)

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
