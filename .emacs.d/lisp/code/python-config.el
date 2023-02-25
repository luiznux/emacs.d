;;; pyhton-config.el --- `python' packages for emacs  -*- lexical-binding: t -*-
;;
;; Author: Luiz Tagliaferro <luiz@luiznux.com>
;; URL: https://luiznux.com
;; This file is free software :)
;;
;;; Commentary:
;;
;; Packages for better python coding experience
;;
;; ██████╗ ██╗   ██╗████████╗██╗  ██╗ ██████╗ ███╗   ██╗
;; ██╔══██╗╚██╗ ██╔╝╚══██╔══╝██║  ██║██╔═══██╗████╗  ██║
;; ██████╔╝ ╚████╔╝    ██║   ███████║██║   ██║██╔██╗ ██║
;; ██╔═══╝   ╚██╔╝     ██║   ██╔══██║██║   ██║██║╚██╗██║
;; ██║        ██║      ██║   ██║  ██║╚██████╔╝██║ ╚████║
;; ╚═╝        ╚═╝      ╚═╝   ╚═╝  ╚═╝ ╚═════╝ ╚═╝  ╚═══╝
;;
;;; Code:

(use-package python
  :ensure nil
  :functions exec-path-from-shell-copy-env
  :hook (inferior-python-mode . (lambda ()
                                  (process-query-on-exit-flag
                                   (get-process "Python"))))
  :init
  ;; Disable readline based native completion
  (setq python-shell-completion-native-enable nil)
  :config
  ;; Default to Python 3. Prefer the versioned Python binaries since some
  ;; systems stupidly make the unversioned one point at Python 2.
  (when (and (executable-find "python3")
             (string= python-shell-interpreter "python"))
    (setq python-shell-interpreter "python3"))

  ;; Env vars
  (with-eval-after-load 'exec-path-from-shell
    (exec-path-from-shell-copy-env "PYTHONPATH"))

  ;; Live Coding in Python
  (use-package live-py-mode))


(provide 'python-config)
;;; python-config.el ends here
