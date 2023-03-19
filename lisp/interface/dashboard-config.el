;;; dashboard-config.el --- Packages for dashboard config  -*- lexical-binding: t -*-
;;
;; Author: Luiz Tagliaferro <luiz@luiznux.com>
;; URL: https://luiznux.com
;; This file is free software :)
;;
;;; Commentary:
;;
;; ██████╗  █████╗ ███████╗██╗  ██╗██████╗  ██████╗  █████╗ ██████╗ ██████╗
;; ██╔══██╗██╔══██╗██╔════╝██║  ██║██╔══██╗██╔═══██╗██╔══██╗██╔══██╗██╔══██╗
;; ██║  ██║███████║███████╗███████║██████╔╝██║   ██║███████║██████╔╝██║  ██║
;; ██║  ██║██╔══██║╚════██║██╔══██║██╔══██╗██║   ██║██╔══██║██╔══██╗██║  ██║
;; ██████╔╝██║  ██║███████║██║  ██║██████╔╝╚██████╔╝██║  ██║██║  ██║██████╔╝
;; ╚═════╝ ╚═╝  ╚═╝╚══════╝╚═╝  ╚═╝╚═════╝  ╚═════╝ ╚═╝  ╚═╝╚═╝  ╚═╝╚═════╝
;;
;;; Code:

(require 'custom-config)
(require 'functions)
(require 'all-the-icons)

(use-package dashboard
  :diminish dashboard-mode
  :functions (all-the-icons-faicon
              all-the-icons-material)
  :custom-face (dashboard-heading ((t (:inherit (font-lock-string-face bold)))))
  :pretty-hydra
  ((:title (pretty-hydra-title "Dashboard" 'material "dashboard" :height 1.2 :v-adjust -0.2)
    :color pink :quit-key ("q" "C-g"))
   ("Navigator"
    (("U" update-packages "update" :exit t)
     ("H" browse-homepage "homepage" :exit t)
     ("R" restore-previous-session "recover session" :exit t)
     ("L" restore-session "list sessions" :exit t)
     ("S" open-custom-file "settings" :exit t)
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
         ("R"      . restore-previous-session)
         ("L"      . restore-session)
         ("S"      . open-custom-file)
         ("U"      . update-packages)
         ("N"      . centaur-tabs--create-new-tab)
         ("q"      . quit-dashboard))
  :hook (dashboard-mode . (lambda () (setq-local frame-title-format nil)
                            (when open-agenda-with-dashboard
                              (open-agenda-on-right-buffer))))
  :init
  (setq dashboard-startup-banner       (or luiznux-logo 'logo)
        dashboard-show-shortcuts       nil
        dashboard-set-heading-icons    emacs-icon
        dashboard-set-file-icons       emacs-icon
        dashboard-set-init-info        t
        dashboard-items                '((recents   . 10) (projects  . 5))
        dashboard-set-navigator        t
        dashboard-navigator-buttons    `(;;line1
                                         ((,(when (icon-displayable-p)
                                              (all-the-icons-octicon "mark-github" :height 1.0 :v-adjust 0.0))
                                           "Github" "My Github"
                                           (lambda (&rest _) (browse-url "https://luiznux.com")))

                                          (,(when (icon-displayable-p)
                                              (all-the-icons-material "restore" :height 1.35 :v-adjust -0.24))
                                           "Restore" "Restore previous session"
                                           (lambda (&rest _) (restore-previous-session)))

                                          (,(when (icon-displayable-p)
                                              (all-the-icons-octicon "tools" :height 1.0 :v-adjust -0.1))
                                           "Config" "Open custom file"
                                           (lambda (&rest _) (find-file custom-file)))

                                          (,(when (icon-displayable-p)
                                              (all-the-icons-faicon "plus" :height 1.2 :v-adjust -0.1))
                                           "New Tab" "New Tab"
                                           (lambda (&rest _) (centaur-tabs--create-new-tab))))))
  (dashboard-setup-startup-hook)

  :config
  (with-no-warnings
    (defun restore-previous-session ()
      "Restore the previous session."
      (interactive)
      (when (bound-and-true-p persp-mode)
        (restore-session persp-auto-save-fname)))

    (defun restore-session (fname)
      "Restore the specified session."
      (interactive (list (read-file-name "Load perspectives from a file: "
                                         persp-save-dir)))
      (when (bound-and-true-p persp-mode)
        (message "Restoring session...")
        (quit-window t)
        (condition-case-unless-debug err
            (persp-load-state-from-file fname)
          (error "Error: Unable to restore session -- %s" err))
        (message "Restoring session...done")))

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


(provide 'dashboard-config)
;;; dashboard-config.el ends here
