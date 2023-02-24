;;; c-config.el --- Package configuration C lang -*- lexical-binding: t -*-
;;
;; Author: Luiz Tagliaferro <luiz@luiznux.com>
;; URL: https://luiznux.com
;; This file is free software :)
;;
;;; Commentary:
;;
;; C Lang config
;;
;;    ██████╗
;;   ██╔════╝
;;   ██║
;;   ██║
;;   ╚██████╗
;;    ╚═════╝
;;
;;; Code:

(require 'functions)

;; C/C++ Mode
(use-package cc-mode
  :ensure nil
  :bind (:map c-mode-base-map
         ("<f12>" . compile))
  :init (setq-default c-basic-offset 4))

(use-package c-ts-mode
  :ensure nil
  :when (emacs-treesit-available-p)
  :init (setq c-ts-mode-indent-offset 4))

(use-package ggtags
  :init
  (add-hook 'c-mode-common-hook
            (lambda ()
              (when (derived-mode-p 'c-mode 'c++-mode 'java-mode)
                (ggtags-mode 1)))))


(provide 'c-config)
;;; c-config.el ends here
