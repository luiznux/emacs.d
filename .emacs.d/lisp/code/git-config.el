;;; git-config.el --- Packages for git config  -*- lexical-binding: t -*-
;;
;; Author: Luiz Tagliaferro <luiz@luiznux.com>
;; URL: https://luiznux.com
;; This file is free software :)
;;
;;; Commentary:
;;
;;  ██████╗ ██╗████████╗
;; ██╔════╝ ██║╚══██╔══╝
;; ██║  ███╗██║   ██║
;; ██║   ██║██║   ██║
;; ╚██████╔╝██║   ██║
;;  ╚═════╝ ╚═╝   ╚═╝
;;
;;; Code:

(use-package magit
  :defer t)

(use-package exec-path-from-shell
  :init
  (setq exec-path-from-shell-variables '("SSH_AUTH_SOCK" "SSH_AGENT_PID" ))
  :config
  (exec-path-from-shell-initialize))

(use-package diff-hl
  :init
  (global-diff-hl-mode)
  (add-hook 'magit-pre-refresh-hook 'diff-hl-magit-pre-refresh)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh))

(use-package grip-mode
  :init
  (setq grip-preview-use-webkit t))

(provide 'git-config)
;;; git-config.el ends here
