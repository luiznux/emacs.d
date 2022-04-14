;;; centaur-tabs-config.el --- packages for centaur-tabs config  -*- lexical-binding: t -*-
;;
;; Author: Luiz Tagliaferro <luiz@luiznux.com>
;; URL: https://luiznux.com
;; This file is free software :)
;;
;;; Commentary:
;;
;; ████████╗ █████╗ ██████╗ ███████╗
;; ╚══██╔══╝██╔══██╗██╔══██╗██╔════╝
;;    ██║   ███████║██████╔╝███████╗
;;    ██║   ██╔══██║██╔══██╗╚════██║
;;    ██║   ██║  ██║██████╔╝███████║
;;    ╚═╝   ╚═╝  ╚═╝╚═════╝ ╚══════╝
;;
;;; Code:

(use-package centaur-tabs
  :defines evil-normal-state-map

  :commands (centaur-tabs-group-by-projectile-project
             centaur-tabs-get-group-name
             centaur-tabs-headline-match
             centaur-tabs-change-fonts
             centaur-tabs-enable-buffer-reordering)

  :hook ((dashboard-mode . centaur-tabs-local-mode)
         (term-mode . centaur-tabs-local-mode)
         (vterm-mode . centaur-tabs-local-mode)
         (calendar-mode . centaur-tabs-local-mode)
         (org-agenda-mode . centaur-tabs-local-mode)
         (magit-mode . centaur-tabs-local-mode)
         (git-commit-mode . centaur-tabs-local-mode)
         (minibuffer-mode . centaur-tabs-local-mode)
         (org-capture-mode . centaur-tabs-local-mode)
         (which-key-mode . centaur-tabs-mode)
         (helpful-mode . centaur-tabs-local-mode))

  :bind (("C-<prior>" . centaur-tabs-backward)
         ("C-<next>" . centaur-tabs-forward)
         ("C-c t" . centaur-tabs-counsel-switch-group)
         (:map evil-normal-state-map
          ("g t" . centaur-tabs-forward)
          ("g T" . centaur-tabs-backward)))

  :init
  (setq centaur-tabs-style                    "chamfer"
        centaur-tabs-height                   32
        centaur-tabs-set-bar                  'under
        x-underline-at-descent-line           t
        centaur-tabs-set-modified-marker      t
        centaur-tabs-set-icons                t
        centaur-tabs-show-new-tab-button      t
        centaur-tabs-show-navigation-buttons  nil
        centaur-tabs-show-count               nil
        centaur-tabs-adjust-buffer-order      t)

  (centaur-tabs-headline-match)
  (centaur-tabs-group-by-projectile-project)
  (centaur-tabs-enable-buffer-reordering)
  (centaur-tabs-change-fonts (face-attribute 'default :font) 70)
  (centaur-tabs-mode t)

  ;;  (defun centaur-tabs-hide-tab (x)
  ;;    "Do no to show buffer X in tabs."
  ;;    (let ((name (format "%s" x)))
  ;;      (or
  ;;       ;; Current window is not dedicated window.
  ;;       (window-dedicated-p (selected-window))
  ;;
  ;;       ;; Buffer name not match below blacklist.
  ;;       (string-prefix-p "*epc" name)
  ;;       (string-prefix-p "*helm" name)
  ;;       (string-prefix-p "*Helm" name)
  ;;       (string-prefix-p "*Compile-Log*" name)
  ;;       (string-prefix-p "*lsp" name)
  ;;       (string-prefix-p "*company" name)
  ;;       (string-prefix-p "*Flycheck" name)
  ;;       (string-prefix-p "*tramp" name)
  ;;       (string-prefix-p " *Mini" name)
  ;;       (string-prefix-p "*help" name)
  ;;       (string-prefix-p "*Help" name)
  ;;       (string-prefix-p "*Help*" name)
  ;;       (string-prefix-p "*straight" name)
  ;;       (string-prefix-p " *temp" name)
  ;;       (string-prefix-p "*Ibuffer*" name)
  ;;       (string-prefix-p "*mybuf" name)
  ;;       (string-prefix-p "diary" name)
  ;;       (string-prefix-p "*Org Agenda*" name)
  ;;       (string-prefix-p "*dashboard*" name)
  ;;       (string-prefix-p "*eldoc-posframe-buffer*" name)
  ;;       (string-prefix-p "*flycheck-posframe-buffer*" name)
  ;;       (string-prefix-p "*which-key*" name)
  ;;       (string-prefix-p "*LSP Error List*" name)
  ;;       (string-prefix-p "*vterm*" name)
  ;;       (string-prefix-p "COMMIT_EDITMSG" name) ;; magit commit buffer
  ;;       (string-prefix-p "*Org Agenda*" name)
  ;;       (string-prefix-p "*Org Select*" name)
  ;;       (string-prefix-p "*Capture*" name)
  ;;
  ;;       ;; Is not magit buffer.
  ;;       (and (string-prefix-p "magit" name)
  ;;	        (not (file-name-extension name)))))))
  )
(provide 'centaur-tabs-config)
;;; centaur-tabs-config.el ends here
