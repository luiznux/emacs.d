;;; custom.el --- user customization file    -*- lexical-binding: t no-byte-compile: t -*-
;;
;;; Commentary:
;;
;;; Code:

;; Calendar
;; Set location , then press `S' can show the time of sunrise and sunset
(setq calendar-location-name   "Sao Paulo"
      calendar-latitude        23.33
      calendar-longitude       46.38)

(setq luiznux-server t)

(set-face-attribute 'default nil
                    :font "Source Code Pro Medium 9")

;; Sets ibuffer as default.
(defalias 'list-buffers 'ibuffer)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(alert-fade-time 10)
 '(auth-source-save-behavior nil)
 '(blink-cursor-mode nil)
 '(evil-undo-system 'undo-tree)
 '(fci-rule-color "#dedede")
 '(global-auto-revert-mode t)
 '(line-spacing 0.2)
 '(standard-indent 4))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(all-the-icons-ivy-rich-dir-face ((t (:inherit default))))
 '(centaur-tabs-selected ((t (:background "#282c34" :foreground "#bbc2cf" :overline nil :underline "#51afef" :weight semi-bold :height 99 :width normal :family "Source Code Pro"))))
 '(diff-hl-change ((t (:foreground "#51afef" :background nil))))
 '(diff-hl-delete ((t (:inherit diff-removed :background nil))))
 '(diff-hl-insert ((t (:inherit diff-added :background nil))))
 '(flycheck-posframe-background-face ((t (:inherit tooltip))))
 '(flycheck-posframe-border-face ((t (:inherit font-lock-comment-face))))
 '(flycheck-posframe-face ((t (:foreground "#98be65"))))
 '(flycheck-posframe-info-face ((t (:foreground "#98be65"))))
 '(git-timemachine-minibuffer-author-face ((t (:inherit success))))
 '(git-timemachine-minibuffer-detail-face ((t (:inherit warning))))
 '(ivy-minibuffer-match-face-1 ((t (:foreground "#7e7e87"))))
 '(lsp-headerline-breadcrumb-path-error-face ((t :underline (:style wave :color "#ff665c") :inherit lsp-headerline-breadcrumb-path-face)))
 '(lsp-headerline-breadcrumb-path-hint-face ((t :underline (:style wave :color "#7bc275") :inherit lsp-headerline-breadcrumb-path-face)))
 '(lsp-headerline-breadcrumb-path-info-face ((t :underline (:style wave :color "#7bc275") :inherit lsp-headerline-breadcrumb-path-face)))
 '(lsp-headerline-breadcrumb-path-warning-face ((t :underline (:style wave :color "#FCCE7B") :inherit lsp-headerline-breadcrumb-path-face)))
 '(lsp-headerline-breadcrumb-symbols-error-face ((t :inherit lsp-headerline-breadcrumb-symbols-face :underline (:style wave :color "#ff665c"))))
 '(lsp-headerline-breadcrumb-symbols-hint-face ((t :inherit lsp-headerline-breadcrumb-symbols-face :underline (:style wave :color "#7bc275"))))
 '(lsp-headerline-breadcrumb-symbols-info-face ((t :inherit lsp-headerline-breadcrumb-symbols-face :underline (:style wave :color "#7bc275"))))
 '(lsp-headerline-breadcrumb-symbols-warning-face ((t :inherit lsp-headerline-breadcrumb-symbols-face :underline (:style wave :color "#FCCE7B"))))
 '(lsp-ui-sideline-code-action ((t (:inherit warning))))
 '(macrostep-expansion-highlight-face ((t (:inherit tooltip :extend t))))
 '(org-agenda-date-today ((t (:foreground "lime green" :weight ultra-bold))))
 '(org-ellipsis ((t (:foreground nil))))
 '(org-scheduled ((t (:foreground "SlateBlue2"))))
 '(org-scheduled-previously ((t (:foreground "medium turquoise"))))
 '(org-scheduled-today ((t (:foreground "deep sky blue"))))
 '(org-super-agenda-header ((t (:inherit default :foreground "#a3f7ff" :weight bold))))
 '(parrot-set-parrot-type 'emacs)
 '(quote (minimap-font-face ((t (:height 32 :family "DejaVu Sans Mono")))))
 '(symbol-overlay-default-face ((t (:inherit (region bold)))))
 '(transient-posframe ((t (:inherit tooltip))))
 '(transient-posframe-border ((t (:background "#62686E")))))

;;; custom.el ends here
