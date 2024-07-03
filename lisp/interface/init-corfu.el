;;; init-corfu.el --- Initialize corfu configurations   -*- lexical-binding: t -*-
;;
;; Author: Luiz Tagliaferro <luiz@luiznux.com>
;; URL: https://luiznux.com
;; This file is free software :)
;;
;;; Commentary:
;;
;;   ██████╗ ██████╗ ██████╗ ███████╗██╗   ██╗
;;  ██╔════╝██╔═══██╗██╔══██╗██╔════╝██║   ██║
;;  ██║     ██║   ██║██████╔╝█████╗  ██║   ██║
;;  ██║     ██║   ██║██╔══██╗██╔══╝  ██║   ██║
;;  ╚██████╗╚██████╔╝██║  ██║██║     ╚██████╔╝
;;   ╚═════╝ ╚═════╝ ╚═╝  ╚═╝╚═╝      ╚═════╝
;;
;;
;;; Code:

(use-package corfu
  :custom
  (corfu-auto t)
  (corfu-auto-prefix 2)
  (corfu-preview-current nil)
  (corfu-auto-delay 0.2)
  (corfu-bar-width 0.5)
  (corfu-min-width 20)
  (corfu-popupinfo-delay '(0.4 . 0.2))
  :custom-face
  (corfu-border ((t (:inherit region :background unspecified))))
  :bind ("M-/" . completion-at-point)
  :hook ((after-init . global-corfu-mode)
         (global-corfu-mode . corfu-popupinfo-mode)))

(use-package corfu-prescient
  :hook (corfu-mode . corfu-prescient-mode)
  :init
  (setq prescient-filter-method '(literal fuzzy initialism)))

(unless (display-graphic-p)
  (use-package corfu-terminal
    :hook (global-corfu-mode . corfu-terminal-mode)))

(use-package nerd-icons-corfu
  :after corfu
  :init (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

;; Add extensions
(use-package cape
  :init
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-abbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-elisp-block)
  (add-to-list 'completion-at-point-functions #'cape-keyword)
  (add-to-list 'completion-at-point-functions #'cape-history)
  (add-to-list 'completion-at-point-functions #'cape-tex)
  (add-to-list 'completion-at-point-functions #'cape-dict)

  (advice-add 'eglot-completion-at-point :around #'cape-wrap-buster))


(provide 'init-corfu)
;;; init-corfu.el ends here
