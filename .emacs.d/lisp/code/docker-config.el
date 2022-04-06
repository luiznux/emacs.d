;;; docker-config.el --- Package configuration docker -*- lexical-binding: t -*-
;;
;; Author: Luiz Tagliaferro <luiz@luiznux.com>
;; URL: https://luiznux.com
;; This file is free software :)
;;
;;; Commentary:
;;
;; Docker config
;;
;;  ██████╗  ██████╗  ██████╗██╗  ██╗███████╗██████╗
;;  ██╔══██╗██╔═══██╗██╔════╝██║ ██╔╝██╔════╝██╔══██╗
;;  ██║  ██║██║   ██║██║     █████╔╝ █████╗  ██████╔╝
;;  ██║  ██║██║   ██║██║     ██╔═██╗ ██╔══╝  ██╔══██╗
;;  ██████╔╝╚██████╔╝╚██████╗██║  ██╗███████╗██║  ██║
;;  ╚═════╝  ╚═════╝  ╚═════╝╚═╝  ╚═╝╚══════╝╚═╝  ╚═╝
;;
;;; Code:

(use-package docker
  :bind ("C-c d" . docker)
  :init
  (with-no-warnings
    (setq docker-image-run-arguments '("-i" "-t" "--rm")
          docker-container-shell-file-name "/bin/bash")))

(use-package docker-tramp)
(use-package dockerfile-mode)


(provide 'docker-config)
;;; docker-config.el ends here.
