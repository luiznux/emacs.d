;;; init-packages.el --- Package configuration file  -*- lexical-binding: t -*-
;;
;; Author: Luiz Tagliaferro <luiz@luiznux.com>
;; URL: https://luiznux.com
;; This file is free software :)
;;
;;; Commentary:
;;
;; Emacs Packages configuration --- Package configuration for Emacs
;;
;;
;; ██████╗  █████╗  ██████╗██╗  ██╗ █████╗  ██████╗ ███████╗███████╗
;; ██╔══██╗██╔══██╗██╔════╝██║ ██╔╝██╔══██╗██╔════╝ ██╔════╝██╔════╝
;; ██████╔╝███████║██║     █████╔╝ ███████║██║  ███╗█████╗  ███████╗
;; ██╔═══╝ ██╔══██║██║     ██╔═██╗ ██╔══██║██║   ██║██╔══╝  ╚════██║
;; ██║     ██║  ██║╚██████╗██║  ██╗██║  ██║╚██████╔╝███████╗███████║
;; ╚═╝     ╚═╝  ╚═╝ ╚═════╝╚═╝  ╚═╝╚═╝  ╚═╝ ╚═════╝ ╚══════╝╚══════╝
;;
;;; Code:

(eval-when-compile
  (require 'init-constants)
  (require 'init-custom)
  (require 'init-functions))

;; Load `custom-file'
(and (file-readable-p custom-file) (load custom-file))

;; HACK: DO NOT save package-selected-packages to `custom-file'.
;; @see https://github.com/jwiegley/use-package/issues/383#issuecomment-247801751
(defun my-package--save-selected-packages (&optional value)
  "Set `package-selected-packages' to VALUE but don't save to option `custom-file'."
  (when value
    (setq package-selected-packages value))
  (unless after-init-time
    (add-hook 'after-init-hook #'my-package--save-selected-packages)))
(advice-add 'package--save-selected-packages :override #'my-package--save-selected-packages)

;; Set ELPA packages
(set-package-archives luiznux-package-archives nil nil t)

;; Initialize packages
(unless (bound-and-true-p package--initialized) ; To avoid warnings in 27
  (setq package-enable-at-startup nil)          ; To prevent initializing twice
  (package-initialize))

;; More options
(setq package-install-upgrade-built-in t)

;; Setup `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Should set before loading `use-package'
(eval-when-compile
  (setq use-package-always-ensure t
        use-package-always-defer t
        use-package-expand-minimally t
        use-package-enable-imenu-support t))

(eval-when-compile
  (require 'use-package))

(use-package benchmark-init
  :ensure t
  :demand t
  :hook (after-init . benchmark-init/deactivate))

;; Load custom lisp scripts
(use-package site-lisp
  :init
  (site-lisp-initialise))

;; Don't display minor modes and required by `use-package'
(use-package diminish :ensure t)

;; Update GPG keyring for GNU ELPA
(use-package gnu-elpa-keyring-update)

;; Auto update packages
(unless (fboundp 'package-update-all)
  (use-package auto-package-update
    :init
    (setq auto-package-update-delete-old-versions t
          auto-package-update-hide-results        t)
    (defalias 'upgrade-packages #'auto-package-update-now)))


(provide 'init-packages)
;;; init-packages.el ends here
