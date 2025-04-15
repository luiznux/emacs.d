;;; init-projectile.el --- Projectile package config -*- lexical-binding: t -*-
;;
;; Author: Luiz Tagliaferro <luiz@luiznux.com>
;; URL: https://luiznux.com
;; This file is free software :)
;;
;;; Commentary:
;;
;;  ██████╗ ██████╗  ██████╗      ██╗███████╗ ██████╗████████╗
;;  ██╔══██╗██╔══██╗██╔═══██╗     ██║██╔════╝██╔════╝╚══██╔══╝
;;  ██████╔╝██████╔╝██║   ██║     ██║█████╗  ██║        ██║
;;  ██╔═══╝ ██╔══██╗██║   ██║██   ██║██╔══╝  ██║        ██║
;;  ██║     ██║  ██║╚██████╔╝╚█████╔╝███████╗╚██████╗   ██║
;;  ╚═╝     ╚═╝  ╚═╝ ╚═════╝  ╚════╝ ╚══════╝ ╚═════╝   ╚═╝
;;
;;; Code:

(use-package projectile
  :autoload project-p emacs-project-root projectile-project-root
  :bind (:map projectile-mode-map
         ("C-c p"   . projectile-command-map))
  :hook (after-init . projectile-mode)
  :init
  (setq projectile-mode-line-prefix ""
        projectile-sort-order       'recentf
        projectile-use-git-grep     t)

  :config
  ;; Use the faster searcher to handle project files
  (when (and (not (executable-find "fd"))
             (executable-find "rg"))
    (setq projectile-generic-command
          (let ((rg-cmd ""))
            (dolist (dir projectile-globally-ignored-directories)
              (setq rg-cmd (format "%s --glob '!%s'" rg-cmd dir)))
            (concat "rg -0 --files --color=never --hidden" rg-cmd))))

  ;; Support Perforce project
  (let ((val (or (getenv "P4CONFIG") ".p4config")))
    (add-to-list 'projectile-project-root-files-bottom-up val))

  ;; Registry my org-files as project
  (with-no-warnings
    (projectile-register-project-type 'org '("config.org")
                                      :project-file "config.org"))

  (defun project-p (&optional dir)
    "Return t if DIR (defaults to `default-directory') is a valid project."
    (and (emacs-project-root dir)
         t))

  (defun emacs-project-root (&optional dir)
    "Return the project root of DIR (defaults to `default-directory').
Returns nil if not in a project."
    (let ((projectile-project-root
           (unless dir (bound-and-true-p projectile-project-root)))
          projectile-require-project-root)
      (projectile-project-root dir))))


(provide 'init-projectile)
;;; init-projectile.el ends here.
