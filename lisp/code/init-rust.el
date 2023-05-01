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

(require 'constants)

(use-package rustic
  :init
  (setq rustic-lsp-client 'lsp-mode))

(use-package rust-playground)


(provide 'init-rust)
;;; init-rust.el ends here
