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
  :bind (("<f2>" . open-dashboard)
         :map dashboard-mode-map
         ("R" . restore-previous-session)
         ("L" . restore-session)
         ("q" . quit-dashboard))
  :hook (dashboard-mode . (lambda () (setq-local frame-title-format nil)))
  :init
  (setq dashboard-startup-banner       (or luiznux-logo 'logo)
        dashboard-show-shortcuts       nil
        dashboard-set-heading-icons    t
        dashboard-set-file-icons       t
        dashboard-set-init-info        t
        dashboard-items                '((recents   . 10) (projects  . 5))
        dashboard-set-navigator        t
        dashboard-navigator-buttons    `(;;line1
                                         ((,(all-the-icons-octicon "mark-github" :height 1.0 :v-adjust 0.0)
                                           "Github" "My Github"
                                           (lambda (&rest _) (browse-url "https://luiznux.com")))

                                          (,(all-the-icons-octicon "tools" :height 1.0 :v-adjust -0.1)
                                           "Config" "Open custom file"
                                           (lambda (&rest _) (find-file custom-file)))

                                          (,(all-the-icons-material "restore" :height 1.35 :v-adjust -0.24)
                                           "" "Restore previous session"
                                           (lambda (&rest _) (restore-previous-session)))

                                          (,(all-the-icons-faicon "refresh" :height 1.2 :v-adjust -0.1)
                                           "" "Refresh Dashboard"
                                           (lambda (&rest _) (dashboard-refresh-buffer)))

                                          (,(all-the-icons-faicon "plus" :height 1.2 :v-adjust -0.1)
                                           "" "New Tab"
                                           (lambda (&rest _) (centaur-tabs--create-new-tab))))))

  (dashboard-setup-startup-hook)
  (add-hook 'dashboard-mode-hook  #'(lambda () (open-agenda-on-right-buffer)))

  :config
  (with-no-warnings
  ;; WORKAROUND: fix differnct background color of the banner image.
  ;; @see https://github.com/emacs-dashboard/emacs-dashboard/issues/203
  (defun my-dashboard-insert-image-banner (banner)
    "Display an image BANNER."
    (when (file-exists-p banner)
      (let* ((title dashboard-banner-logo-title)
             (spec (create-image banner))
             (size (image-size spec))
             (width (car size))
             (left-margin (max 0 (floor (- dashboard-banner-length width) 2))))
        (goto-char (point-min))
        (insert "\n")
        (insert (make-string left-margin ?\ ))
        (insert-image spec)
        (insert "\n\n")
        (when title
          (dashboard-insert-center
           (format "%s\n\n" (propertize title 'face 'dashboard-banner-logo-title)))))))
  (advice-add #'dashboard-insert-image-banner :override #'my-dashboard-insert-image-banner)

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

  (defun open-agenda-on-right-buffer ()
    "Open agenda in the right buffer."
    (interactive)
    (org-agenda t "x")
    (other-window (goto-char (point-min))))))


(provide 'dashboard-config)
;;; dashboard-config.el ends here
