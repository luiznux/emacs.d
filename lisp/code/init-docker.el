;;; init-docker.el --- Initialize docker configurations   -*- lexical-binding: t -*-
;;
;; Author: Luiz Tagliaferro <luiz@luiznux.com>
;; URL: https://luiznux.com
;; This file is free software :)
;;
;;; Commentary:
;;
;;
;;  ██████╗  ██████╗  ██████╗██╗  ██╗███████╗██████╗
;;  ██╔══██╗██╔═══██╗██╔════╝██║ ██╔╝██╔════╝██╔══██╗
;;  ██║  ██║██║   ██║██║     █████╔╝ █████╗  ██████╔╝
;;  ██║  ██║██║   ██║██║     ██╔═██╗ ██╔══╝  ██╔══██╗
;;  ██████╔╝╚██████╔╝╚██████╗██║  ██╗███████╗██║  ██║
;;  ╚═════╝  ╚═════╝  ╚═════╝╚═╝  ╚═╝╚══════╝╚═╝  ╚═╝
;;
;;; Code:

(eval-when-compile
  (require 'init-constants))

(use-package docker
  :bind ("C-c d" . docker)
  :init
  (with-no-warnings
    (setq docker-image-run-arguments '("-i" "-t" "--rm")
          docker-container-shell-file-name "/bin/bash")))

(use-package dockerfile-mode)


(provide 'init-docker)
;;; init-docker.el ends here.
