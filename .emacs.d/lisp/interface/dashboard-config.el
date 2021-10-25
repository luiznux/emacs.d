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

(use-package dashboard
  :init
  (progn
    (setq recentf-exclude '("/org/*")) ;prevent  show recent org-agenda files
    (setq dashboard-items '((recents   . 8)
                            (projects  . 7))))
  :config
  (dashboard-setup-startup-hook)

  (setq dashboard-set-heading-icons  t
        dashboard-set-file-icons     t
        dashboard-set-navigator      t
        dashboard-startup-banner     'logo)

  (with-no-warnings
    (setq dashboard-navigator-buttons
          `((;;line1
             (,(all-the-icons-octicon "mark-github" :height 1.1 :v-adjust 0.0)
              "Github" "My github homepage"
              (lambda (&rest _) (browse-url "https://github.com/luiznux")))

             (,(all-the-icons-faicon "refresh" :height 1.1 :v-adjust 0.0)
              "" "Refresh Dashboard"
              (lambda (&rest _) (dashboard-refresh-buffer)))))))

  (add-hook 'dashboard-mode-hook (lambda () (org-agenda t "x")) (lambda () (ace-window))))


(provide 'dashboard-config)
;;; dashboard-config.el ends here
