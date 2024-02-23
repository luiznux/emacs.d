;;; init-workspace.el --- Initialize workspace configurations  -*- lexical-binding: t -*-
;;
;; Author: Luiz Tagliaferro <luiz@luiznux.com>
;; URL: https://luiznux.com
;; This file is free software :)
;;
;;; Commentary:
;;
;;  ████████╗ █████╗ ██████╗ ███████╗
;;  ╚══██╔══╝██╔══██╗██╔══██╗██╔════╝
;;     ██║   ███████║██████╔╝███████╗
;;     ██║   ██╔══██║██╔══██╗╚════██║
;;     ██║   ██║  ██║██████╔╝███████║
;;     ╚═╝   ╚═╝  ╚═╝╚═════╝ ╚══════╝
;;
;;
;;; Code:

(use-package tabspaces
  :custom
  (tab-bar-show nil)
  (tabspaces-use-filtered-buffers-as-default t)
  (tabspaces-default-tab "Default")
  (tabspaces-remove-to-default t)
  (tabspaces-include-buffers '("*scratch*" "*Messages*" "*dashboard*" "*Org Agenda*"))
  ;; sessions
  (tabspaces-session t)
  (tabspaces-session-auto-restore t))

(provide 'init-workspace)
;;; init-workspace.el ends here
