;;; dart-config.el --- Dart Package configuration file  -*- lexical-binding: t -*-
;;
;; Author: Luiz Tagliaferro <luiz@luiznux.com>
;; URL: https://luiznux.com
;; This file is free software :)
;;
;;; Commentary:
;;
;; LSP packages for Emacs better coding
;;
;;  ██████╗  █████╗ ██████╗ ████████╗
;;  ██╔══██╗██╔══██╗██╔══██╗╚══██╔══╝
;;  ██║  ██║███████║██████╔╝   ██║
;;  ██║  ██║██╔══██║██╔══██╗   ██║
;;  ██████╔╝██║  ██║██║  ██║   ██║
;;  ╚═════╝ ╚═╝  ╚═╝╚═╝  ╚═╝   ╚═╝
;;
;;; Code:

;; Dart
(use-package dart-mode
  :defines (projectile-project-root-files-bottom-up)
  :config
  (with-eval-after-load 'projectile
    (add-to-list 'projectile-project-root-files-bottom-up "pubspec.yaml")
    (add-to-list 'projectile-project-root-files-bottom-up "BUILD")))


(provide 'dart-config)
;;; dart-config.el ends here.
