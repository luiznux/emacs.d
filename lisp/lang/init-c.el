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
  (require 'init-custom))

;; C/C++ Mode
(use-package cc-mode
  :ensure nil
  :bind (:map c-mode-base-map
         ("<f12>" . compile))
  :init (setq-default c-basic-offset 4))

(when (and (emacs-treesit-available-p) (eq emacs-parsing-system 'treesit))
  (use-package c-ts-mode
    :init (setq c-ts-mode-indent-offset 4)))


(provide 'init-c)
;;; init-c.el ends here
