;;; init-rust.el --- Initialize rust configurations   -*- lexical-binding: t -*-
;;
;; Author: Luiz Tagliaferro <luiz@luiznux.com>
;; URL: https://luiznux.com
;; This file is free software :)
;;
;;; Commentary:
;;
;;
;;  ██████╗ ██╗   ██╗███████╗████████╗
;;  ██╔══██╗██║   ██║██╔════╝╚══██╔══╝
;;  ██████╔╝██║   ██║███████╗   ██║
;;  ██╔══██╗██║   ██║╚════██║   ██║
;;  ██║  ██║╚██████╔╝███████║   ██║
;;  ╚═╝  ╚═╝ ╚═════╝ ╚══════╝   ╚═╝
;;
;;; Code:

(use-package rustic
  :init
  (setq rustic-lsp-client 'lsp-mode))

(use-package ron-mode
  :mode ("\\.ron" . ron-mode))

(use-package rust-playground)


(provide 'init-rust)
;;; init-rust.el ends here
