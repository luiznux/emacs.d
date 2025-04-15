;;; init-python.el --- Initialize `python' configurations   -*- lexical-binding: t -*-
;;
;; Author: Luiz Tagliaferro <luiz@luiznux.com>
;; URL: https://luiznux.com
;; This file is free software :)
;;
;;; Commentary:
;;
;;
;; ██████╗ ██╗   ██╗████████╗██╗  ██╗ ██████╗ ███╗   ██╗
;; ██╔══██╗╚██╗ ██╔╝╚══██╔══╝██║  ██║██╔═══██╗████╗  ██║
;; ██████╔╝ ╚████╔╝    ██║   ███████║██║   ██║██╔██╗ ██║
;; ██╔═══╝   ╚██╔╝     ██║   ██╔══██║██║   ██║██║╚██╗██║
;; ██║        ██║      ██║   ██║  ██║╚██████╔╝██║ ╚████║
;; ╚═╝        ╚═╝      ╚═╝   ╚═╝  ╚═╝ ╚═════╝ ╚═╝  ╚═══╝
;;
;;; Code:

(require 'init-keybinds)

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

  ;; With basedpyright
  ;; See https://docs.basedpyright.com/latest/
  (use-package lsp-pyright
    :init
    (when (executable-find "python3")
      (setq lsp-pyright-python-executable-cmd "python3"))

    (when (executable-find "basedpyright")
      (setq lsp-pyright-langserver-command "basedpyright")))

  (use-package python-black
    :hook ((python-mode python-ts-mode) . python-black-on-save-mode))

  (use-package pyimport
    :commands +python/optimize-imports
    :init
    (defun +python/optimize-imports ()
      "organize imports"
      (interactive)
      (pyimport-remove-unused)
      (py-isort-bufer))

    (map! :map python-mode-map
          :localleader
          :prefix ("i" . "imports")
          :desc "Insert missing imports" "i" #'pyimport-insert-missing
          :desc "Remove unused imports"  "R" #'pyimport-remove-unused
          :desc "Optimize imports"       "o" #'+python/optimize-imports))

  (use-package py-isort
    :init
    (map! :map python-mode-map
          :localleader
          (:prefix ("i" . "imports")
           :desc "Sort imports"      "s" #'py-isort-buffer
           :desc "Sort region"       "r" #'py-isort-region)))

  ;; Live Coding in Python
  (use-package live-py-mode))


(provide 'init-python)
;;; init-python.el ends here
