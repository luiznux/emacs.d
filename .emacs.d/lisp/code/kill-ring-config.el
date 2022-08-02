;;; kill-ring-config.el ---  Kill-ring configurations.  -*- lexical-binding: t -*-
;;
;; Author: Luiz Tagliaferro <luiz@luiznux.com>
;; URL: https://luiznux.com
;; This file is free software :)
;;
;;; Commentary:
;;
;;  ██╗  ██╗██╗██╗     ██╗         ██████╗ ██╗███╗   ██╗ ██████╗
;;  ██║ ██╔╝██║██║     ██║         ██╔══██╗██║████╗  ██║██╔════╝
;;  █████╔╝ ██║██║     ██║         ██████╔╝██║██╔██╗ ██║██║  ███╗
;;  ██╔═██╗ ██║██║     ██║         ██╔══██╗██║██║╚██╗██║██║   ██║
;;  ██║  ██╗██║███████╗███████╗    ██║  ██║██║██║ ╚████║╚██████╔╝
;;  ╚═╝  ╚═╝╚═╝╚══════╝╚══════╝    ╚═╝  ╚═╝╚═╝╚═╝  ╚═══╝ ╚═════╝
;;
;;; Code:

(setq kill-ring-max 200)

;; Save clipboard contents into kill-ring before replace them
(setq save-interprogram-paste-before-kill t)

;; Kill & Mark things easily
(use-package easy-kill
  :bind (([remap kill-ring-save] . easy-kill)
         ([remap mark-sexp] . easy-mark)))

;; Interactively insert and edit items from kill-ring
(use-package browse-kill-ring
  :bind ("C-c k" . browse-kill-ring)
  :hook (after-init . browse-kill-ring-default-keybindings))


(provide 'kill-ring-config)
;;; kill-ring-config.el ends here
