;;; centaur-tabs-config.el --- packages for centaur-tabs config  -*- lexical-binding: t -*-
;;
;; Author: Luiz Tagliaferro <luiz@luiznux.com>
;; URL: https://luiznux.com
;; This file is free software :)
;;
;;; commentary:
;;
;; ████████╗ █████╗ ██████╗ ███████╗
;; ╚══██╔══╝██╔══██╗██╔══██╗██╔════╝
;;    ██║   ███████║██████╔╝███████╗
;;    ██║   ██╔══██║██╔══██╗╚════██║
;;    ██║   ██║  ██║██████╔╝███████║
;;    ╚═╝   ╚═╝  ╚═╝╚═════╝ ╚══════╝
;;
;;; code:

(use-package centaur-tabs
  :defines evil-normal-state-map
  :commands (centaur-tabs-group-by-projectile-project
             centaur-tabs-get-group-name
             centaur-tabs-headline-match
             centaur-tabs-change-fonts)
  :config
  (setq centaur-tabs-style                    "chamfer"
        centaur-tabs-height                   32
        centaur-tabs-set-icons                t
        centaur-tabs-set-bar                  'under
        x-underline-at-descent-line           t
        centaur-tabs-set-modified-marker      t
        centaur-tabs-show-navigation-buttons  t)

  (centaur-tabs-headline-match)
  (centaur-tabs-group-by-projectile-project)
  (centaur-tabs-mode t)
  (centaur-tabs-change-fonts "Source Code Pro" 102)

  (defun centaur-tabs-buffer-groups ()
    "`centaur-tabs-buffer-groups' control buffers' group rules.
 Group centaur-tabs with mode if buffer is derived
from `eshell-mode' `emacs-lisp-mode' `dired-mode' `org-mode' `magit-mode'.
 All buffer name start with * will group to \"Emacs\".
 Other buffer group by `centaur-tabs-get-group-name' with project name."
    (list
     (cond
      ((or (string-equal "*" (substring (buffer-name) 0 1))
           (memq major-mode '(magit-process-mode
                              magit-status-mode
                              magit-diff-mode
                              magit-log-mode
                              magit-file-mode
                              magit-blob-mode
                              magit-blame-mode
                              )))
       "Emacs")
      ((derived-mode-p 'prog-mode)
       "Editing")
      ((derived-mode-p 'emacs-lisp-mode)
       "Elisp")
      ((derived-mode-p 'dired-mode)
       "Dired")
      ((memq major-mode '(helpful-mode
                          help-mode))
       "Help")
      ((memq major-mode '(org-mode
                          org-agenda-clockreport-mode
                          org-src-mode
                          org-agenda-mode
                          org-beamer-mode
                          org-indent-mode
                          org-bullets-mode
                          org-cdlatex-mode
                          org-agenda-log-mode
                          diary-mode))
       "OrgMode")
      (t
       (centaur-tabs-get-group-name (current-buffer))))))
  :hook
  (dashboard-mode . centaur-tabs-local-mode)
  (term-mode . centaur-tabs-local-mode)
  (calendar-mode . centaur-tabs-local-mode)
  (org-agenda-mode . centaur-tabs-local-mode)
  (helpful-mode . centaur-tabs-local-mode)
  :bind
  ("C-<prior>" . centaur-tabs-backward)
  ("C-<next>" . centaur-tabs-forward)
  ("C-c t" . centaur-tabs-counsel-switch-group)
  (:map evil-normal-state-map
   ("g t" . centaur-tabs-forward)
   ("g T" . centaur-tabs-backward)))


(provide 'centaur-tabs-config)
;;; centaur-tabs-config.el ends here
