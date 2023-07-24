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
  :hook (after-init . tabspaces-mode) ;; use this only if you want the minor-mode loaded at startup.
  :commands (tabspaces-switch-or-create-workspace
             tabspaces-open-or-create-project-and-workspace)
  :custom
  (tab-bar-show nil)
  (tabspaces-use-filtered-buffers-as-default t)
  (tabspaces-default-tab "Default")
  (tabspaces-remove-to-default t)
  (tabspaces-include-buffers '("*scratch*"))
  ;; sessions
  (tabspaces-session t)
  (tabspaces-session-auto-restore t)
  :config
  (defun tabspaces-ivy-switch-buffer (buffer)
    "Display the local buffer BUFFER in the selected window.
This is the frame/tab-local equivilant to `switch-to-buffer'."
    (interactive
     (list
      (let ((blst (mapcar #'buffer-name (tabspaces-buffer-list))))
        (read-buffer
         "Switch to local buffer: " blst nil
         (lambda (b) (member (if (stringp b) b (car b)) blst))))))
    (ivy-switch-buffer buffer)))

(provide 'init-workspace)
;;; init-workspace.el ends here
