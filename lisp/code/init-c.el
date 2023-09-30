;;; init-c.el --- Initialize C lang configurations   -*- lexical-binding: t -*-
;;
;; Author: Luiz Tagliaferro <luiz@luiznux.com>
;; URL: https://luiznux.com
;; This file is free software :)
;;
;;; Commentary:
;;
;;
;;    ██████╗
;;   ██╔════╝
;;   ██║
;;   ██║
;;   ╚██████╗
;;    ╚═════╝
;;
;;; Code:

(eval-when-compile
  (require 'custom-config))

;; C/C++ Mode
(use-package cc-mode
  :ensure nil
  :bind (:map c-mode-base-map
         ("<f12>" . compile))
  :init (setq-default c-basic-offset 4))

(when (emacs-treesit-available-p)
  (use-package c-ts-mode
    :init (setq c-ts-mode-indent-offset 4)))


(provide 'init-c)
;;; init-c.el ends here
