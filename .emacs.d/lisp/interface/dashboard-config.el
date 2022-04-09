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
  :hook (dashboard-mode . (lambda () (setq-local frame-title-format nil)))
  :init
  (setq dashboard-set-heading-icons    t
        dashboard-set-file-icons       t
        dashboard-set-navigator        t
        dashboard-set-init-info        t
        dashboard-startup-banner       (or luiznux-logo 'logo)
        ;;dashboard-page-separator       "\n\f\n"
        dashboard-items                '((recents   . 10) (projects  . 5))
        dashboard-navigator-buttons    `(;;line1
                                         ((,(all-the-icons-octicon "mark-github" :height 1.0 :v-adjust 0.0)
                                           "Github" "My Github"
                                           (lambda (&rest _) (browse-url "https://luiznux.com")))

                                          (,(all-the-icons-octicon "tools" :height 1.0 :v-adjust -0.1)
                                           "Config" "Open custom file"
                                           (lambda (&rest _) (find-file custom-file)))

                                          (,(all-the-icons-faicon "refresh" :height 1.2 :v-adjust -0.1)
                                           "" "Refresh Dashboard"
                                           (lambda (&rest _) (dashboard-refresh-buffer)))

                                          (,(all-the-icons-faicon "plus" :height 1.2 :v-adjust -0.1)
                                           "" "New Tab"
                                           (lambda (&rest _) (centaur-tabs--create-new-tab))))))

  (add-hook 'dashboard-mode-hook  #'(lambda () (open-agenda-on-right-buffer)))

  :config
  (dashboard-setup-startup-hook))


(provide 'dashboard-config)
;;; dashboard-config.el ends here
