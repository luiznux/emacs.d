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

(use-package dashboard
  :custom-face (dashboard-heading ((t (:inherit (font-lock-string-face bold)))))
  :init
  (setq dashboard-set-heading-icons    t
        dashboard-set-file-icons       t
        dashboard-set-navigator        t
        dashboard-set-init-info        t
        dashboard-startup-banner       'logo
        ;;        dashboard-page-separator       "\n\f\n"
        dashboard-items                '((recents   . 8) (projects  . 7))
        dashboard-navigator-buttons    `((;;line1
                                          (,(all-the-icons-octicon "mark-github" :height 1.1 :v-adjust 0.0)
                                           "Github" "My github homepage"
                                           (lambda (&rest _) (browse-url "https://github.com/luiznux")))

                                          (,(all-the-icons-faicon "plus" :height 0.9 :v-adjust 0.0)
                                           "New Tab" "New Tab"
                                           (lambda (&rest _) (centaur-tabs--create-new-tab)))

                                          (,(all-the-icons-faicon "refresh" :height 1.1 :v-adjust 0.0)
                                           "" "Refresh Dashboard"
                                           (lambda (&rest _) (dashboard-refresh-buffer))))))

  (add-hook 'dashboard-mode-hook  #'(lambda () (open-agenda-on-right-buffer)))

  :config
  (dashboard-setup-startup-hook))


(provide 'dashboard-config)
;;; dashboard-config.el ends here
