;; player-config.el --- Initialize player configurations.	-*- lexical-binding: t -*-
;;
;; Author: Luiz Tagliaferro <luiz@luiznux.com>
;; URL: https://luiznux.com
;; This file is free software :)
;;
;;; Commentary:
;;
;; Emacs music player packages
;;
;;  ██████╗ ██╗      █████╗ ██╗   ██╗███████╗██████╗
;;  ██╔══██╗██║     ██╔══██╗╚██╗ ██╔╝██╔════╝██╔══██╗
;;  ██████╔╝██║     ███████║ ╚████╔╝ █████╗  ██████╔╝
;;  ██╔═══╝ ██║     ██╔══██║  ╚██╔╝  ██╔══╝  ██╔══██╗
;;  ██║     ███████╗██║  ██║   ██║   ███████╗██║  ██║
;;  ╚═╝     ╚══════╝╚═╝  ╚═╝   ╚═╝   ╚══════╝╚═╝  ╚═╝
;;
;;
;;; Code:

;; Music player
(use-package bongo
  :bind ("C-<f9>" . bongo)
  :config
  (with-eval-after-load 'dired
    (with-no-warnings
      (defun bongo-add-dired-files ()
        "Add marked files to the Bongo library."
        (interactive)
        (bongo-buffer)
        (let (file (files nil))
          (dired-map-over-marks
           (setq file (dired-get-filename)
                 files (append files (list file)))
           nil t)
          (with-bongo-library-buffer
           (mapc 'bongo-insert-file files)))
        (bongo-switch-buffers))
      (bind-key "b" #'bongo-add-dired-files dired-mode-map))))

;; Music Player Daemon
;; Built-in client for mpd
(use-package mpc
  :ensure nil
  :bind ("s-<f9>" . mpc)
  :config
  (defun restart-mpd ()
    (interactive)
    (call-process "pkill" nil nil nil "mpd")
    (call-process "mpd"))

  (with-no-warnings
    (defun add-mpc-status-to-mode-line ()
      "Display current song in mode line."
      (add-to-list 'global-mode-string '("" mpc-current-song)))
    (advice-add #'mpc :after #'add-mpc-status-to-mode-line)))

;; Simple client for mpd
(when (executable-find "mpc")
  (use-package simple-mpc
    :bind ("M-<f9>" . simple-mpc)))


(provide 'player-config)
;;; player-config.el ends here
