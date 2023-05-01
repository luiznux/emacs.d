;;; init-dart.el --- Initialize dart configurations   -*- lexical-binding: t -*-
;;
;; Author: Luiz Tagliaferro <luiz@luiznux.com>
;; URL: https://luiznux.com
;; This file is free software :)
;;
;;; Commentary:
;;
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


(provide 'init-dart)
;;; init-dart.el ends here.
