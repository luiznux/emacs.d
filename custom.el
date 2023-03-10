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

;; Set custom variables
(setq user-full-name           "Luiz Tagliaferro"
      user-mail-address        "luiz@luiznux.com"
      luiznux-enviroment-type  'nil
      luiznux-package-archives 'melpa
      luiznux-server           t)

;;(setq doom-modeline-font-size value)
;;(setq centaur-tabs-font-size  value)
;;(setq open-agenda-with-dashboard t)
;;(setq fancy-modeline t)

(defun setup-fonts ()
  "Set default fonts."
  (cl-loop for font in '("SauceCodePro Nerd Font" "Source Code Pro"
                         "DejaVu Sans Mono" "Noto Sans")
           when (font-installed-p font)
           return (set-face-attribute 'default nil
                                      :font font
                                      :height 90
                                      :weight 'medium))

  ;; Specify font for all unicode characters
  (cl-loop for font in '("Symbola" "Symbol")
           when (font-installed-p font)
           return (set-fontset-font t 'unicode font nil 'prepend)))

(setup-fonts)
(add-hook 'window-setup-hook #'setup-fonts)
(add-hook 'server-after-make-frame-hook #'setup-fonts)

;; Sets ibuffer as default.
(defalias 'list-buffers 'ibuffer)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;;; custom.el ends here
