;;; rust-config.el --- Package configuration rust lang -*- lexical-binding: t -*-
;;
;; Author: Luiz Tagliaferro <luiz@luiznux.com>
;; URL: https://luiznux.com
;; This file is free software :)
;;
;;; Commentary:
;;
;; Rust Lang config
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

;;(when emacs/>=26p
;;  (use-package rustic
;;    :init
;;    (setq rustic-lsp-client 'lsp-mode)))

(use-package rust-playground)


(provide 'rust-config)
;;; rust-config.el ends here
