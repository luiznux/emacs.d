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

(require 'consult)

(use-package tabspaces
  :functions (consult--source-workspace  my--consult-tabspaces)
  ;;:hook (after-init . tabspaces-mode)
  :init
  (setq tab-bar-show nil
        tabspaces-use-filtered-buffers-as-default t
        tabspaces-default-tab "Home"
        tabspaces-remove-to-default t
        tabspaces-include-buffers '("*scratch*" "*Messages*" "*dashboard*" "*Org Agenda*")
        tabspaces-session t
        tabspaces-session-auto-restore t
        tabspaces-session-file (concat user-emacs-directory "tabspaces/tabsession.el")
        tabspaces-session-project-session-store (concat user-emacs-directory "tabspaces/sessions/"))
  :config
  ;; Filter Buffers for Consult-Buffer
  (with-eval-after-load 'consult
    ;; hide full buffer list (still available with "b" prefix)
    (consult-customize consult--source-buffer :hidden t :default nil)
    ;; set consult-workspace buffer list
    (defvar consult--source-workspace
      (list :name     "Workspace Buffers"
            :narrow   ?w
            :history  'buffer-name-history
            :category 'buffer
            :state    #'consult--buffer-state
            :default  t
            :items    (lambda () (consult--buffer-query
                             :predicate #'tabspaces--local-buffer-p
                             :sort 'visibility
                             :as #'buffer-name)))
      "Set workspace buffer list for consult-buffer.")
    (add-to-list 'consult-buffer-sources 'consult--source-workspace)

    (defun my--consult-tabspaces ()
      "Deactivate isolated buffers when not using tabspaces."
      (require 'consult)
      (cond (tabspaces-mode
             ;; hide full buffer list (still available with "b")
             (consult-customize consult--source-buffer :hidden t :default nil)
             (add-to-list 'consult-buffer-sources 'consult--source-workspace))
            (t
             ;; reset consult-buffer to show all buffers
             (consult-customize consult--source-buffer :hidden nil :default t)
             (setq consult-buffer-sources (remove #'consult--source-workspace consult-buffer-sources)))))
    (add-hook 'tabspaces-mode-hook #'my--consult-tabspaces)))


(provide 'init-workspace)
;;; init-workspace.el ends here
