;;; init-dashboard.el --- Initialize dashboard configurations   -*- lexical-binding: t -*-
;;
;; Author: Luiz Tagliaferro <luiz@luiznux.com>
;; URL: https://luiznux.com
;; This file is free software :)
;;
;;; Commentary:
;;
;;
;; ██████╗  █████╗ ███████╗██╗  ██╗██████╗  ██████╗  █████╗ ██████╗ ██████╗
;; ██╔══██╗██╔══██╗██╔════╝██║  ██║██╔══██╗██╔═══██╗██╔══██╗██╔══██╗██╔══██╗
;; ██║  ██║███████║███████╗███████║██████╔╝██║   ██║███████║██████╔╝██║  ██║
;; ██║  ██║██╔══██║╚════██║██╔══██║██╔══██╗██║   ██║██╔══██║██╔══██╗██║  ██║
;; ██████╔╝██║  ██║███████║██║  ██║██████╔╝╚██████╔╝██║  ██║██║  ██║██████╔╝
;; ╚═════╝ ╚═╝  ╚═╝╚══════╝╚═╝  ╚═╝╚═════╝  ╚═════╝ ╚═╝  ╚═╝╚═╝  ╚═╝╚═════╝
;;
;;; Code:

(eval-when-compile
  (require 'init-custom))

(use-package dashboard
  :diminish dashboard-mode
  :functions (nerd-icons-faicon
              nerd-icons-mdicon)
  :custom-face
  (dashboard-heading ((t (:inherit (font-lock-string-face bold)))))
  (dashboard-items-face ((t (:weight bold))))
  (dashboard-no-items-face ((t (:weight normal))))
  :pretty-hydra
  ((:title (pretty-hydra-title "Dashboard" 'mdicon "nf-md-view_dashboard")
    :color pink :quit-key ("q" "C-g"))
   ("Navigator"
    (("U" update-packages "update" :exit t)
     ("H" browse-homepage "homepage" :exit t)
     ("R" restore-session "recover session" :exit t)
     ("S" find-custom-file "settings" :exit t)
     ("N" centaur-tabs--create-new-tab "create new tab" :exit t))
    "Section"
    (("}" dashboard-next-section "next")
     ("{" dashboard-previous-section "previous")
     ("r" dashboard-goto-recent-files "recent files")
     ("m" dashboard-goto-bookmarks "bookmarks")
     ("p" dashboard-goto-projects "projects"))
    "Item"
    (("RET" widget-button-press "open" :exit t)
     ("<tab>" widget-forward "next")
     ("C-i" widget-forward "next")
     ("<backtab>" widget-backward "previous")
     ("C-n" next-line "next line")
     ("C-p" previous-line "previous  line"))
    "Misc"
    (("<f2>" open-dashboard "open" :exit t)
     ("g" dashboard-refresh-buffer "refresh" :exit t)
     ("Q" quit-dashboard "quit" :exit t))))
  :bind (("<f2>" . open-dashboard)
         :map dashboard-mode-map
         ("S-<f2>" . dashboard-hydra/body)
         ("R"      . restore-session)
         ("S"      . find-custom-file)
         ("U"      . update-packages)
         ("N"      . centaur-tabs--create-new-tab)
         ("q"      . quit-dashboard))
  :init
  (setq dashboard-startup-banner       (or luiznux-logo 'logo)
        dashboard-show-shortcuts       nil
        dashboard-projects-backend     'project-el
        dashboard-display-icons-p      #'icons-displayable-p
        dashboard-icon-type            'nerd-icons
        dashboard-path-style           'truncate-middle
        dashboard-path-max-length      80
        dashboard-set-heading-icons    emacs-icon
        dashboard-set-file-icons       emacs-icon
        dashboard-items                '((recents   . 10) (projects  . 5))
        dashboard-heading-icons        '((recents   . "nf-oct-history") (projects  . "nf-oct-briefcase"))
        dashboard-navigator-buttons    `(;;line1
                                         ((,(when (icons-displayable-p)
                                              (nerd-icons-mdicon "nf-md-github" :height 1.4))
                                           "Github" "My Github"
                                           (lambda (&rest _) (browse-url "https://luiznux.com")))

                                          (,(when (icons-displayable-p)
                                              (nerd-icons-mdicon "nf-md-backup_restore" :height 1.5))
                                           "Restore" "Restore previous session"
                                           (lambda (&rest _) (restore-session)))

                                          (,(when (icons-displayable-p)
                                              (nerd-icons-mdicon "nf-md-tools" :height 1.3))
                                           "Config" "Open custom file"
                                           (lambda (&rest _) (find-file custom-file)))

                                          (,(when (icons-displayable-p)
                                              (nerd-icons-faicon "nf-fa-plus" :height 1.2))
                                           "New Tab" "New Tab"
                                           (lambda (&rest _) (centaur-tabs--create-new-tab)))))

        dashboard-startupify-list      '(dashboard-insert-banner
                                         dashboard-insert-newline
                                         dashboard-insert-banner-title
                                         dashboard-insert-newline
                                         dashboard-insert-navigator
                                         dashboard-insert-newline
                                         dashboard-insert-init-info
                                         dashboard-insert-items
                                         dashboard-insert-newline
                                         dashboard-insert-footer))
  (when open-agenda-with-dashboard
    (advice-add 'dashboard-mode :after #'open-agenda-on-right-buffer))
  (dashboard-setup-startup-hook)

  :config
  (with-no-warnings
    (defun restore-session ()
      "Restore the previous session."
      (interactive)
      (message "Restoring previous session...")
      (quit-window t)
      (cond
       ((bound-and-true-p tabspaces-mode)
        (tabspaces-restore-session))
       ((bound-and-true-p desktop-save-mode)
        (desktop-read)))
      (message "Restoring previous session...done"))

    (defun dashboard-goto-recent-files ()
      "Go to recent files."
      (interactive)
      (let ((func (local-key-binding "r")))
        (and func (funcall func))))

    (defvar dashboard-recover-layout-p nil
      "Wether recovers the layout.")

    (defun open-dashboard ()
      "Open the *dashboard* buffer and jump to the first widget."
      (interactive)
      ;; Check if need to recover layout
      (if (length> (window-list-1)
                   ;; exclude `treemacs' window
                   (if (and (fboundp 'treemacs-current-visibility)
                            (eq (treemacs-current-visibility) 'visible))
                       2
                     1))
          (setq dashboard-recover-layout-p t))

      ;; Display dashboard in maximized window
      (delete-other-windows)

      ;; Refresh dashboard buffer
      (dashboard-refresh-buffer)

      ;; Jump to the first section
      (dashboard-goto-recent-files))

    (defun quit-dashboard ()
      "Quit dashboard window."
      (interactive)
      (quit-window t)
      (and dashboard-recover-layout-p
           (setq dashboard-recover-layout-p nil)))

    (defun open-agenda-on-right-buffer ()
      "Open agenda in the right buffer."
      (interactive)
      (org-agenda t "x")
      (other-window (goto-char (point-min))))))


(provide 'init-dashboard)
;;; init-dashboard.el ends here
