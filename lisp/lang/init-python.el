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
  (setq python-shell-completion-native-enable nil
        python-indent-guess-indent-offset-verbose nil)
  :config
  ;; Default to Python 3. Prefer the versioned Python binaries since some
  ;; systems stupidly make the unversioned one point at Python 2.
  (when (and (executable-find "python3")
             (string= python-shell-interpreter "python"))
    (setq python-shell-interpreter "python3"))

  ;; Env vars
  (with-eval-after-load 'exec-path-from-shell
    (exec-path-from-shell-copy-env "PYTHONPATH"))

  (add-hook! 'python-mode-hook
    (defun +python-use-correct-flycheck-executables-h ()
      "Use the correct Python executables for Flycheck."
      (let ((executable python-shell-interpreter))
        (save-excursion
          (goto-char (point-min))
          (save-match-data
            (when (or (looking-at "#!/usr/bin/env \\(python[^ \n]+\\)")
                      (looking-at "#!\\([^ \n]+/python[^ \n]+\\)"))
              (setq executable (substring-no-properties (match-string 1))))))
        ;; Try to compile using the appropriate version of Python for
        ;; the file.
        (setq-local flycheck-python-pycompile-executable executable)
        ;; We might be running inside a virtualenv, in which case the
        ;; modules won't be available. But calling the executables
        ;; directly will work.
        (setq-local flycheck-python-pylint-executable "pylint")
        (setq-local flycheck-python-flake8-executable "flake8")))))

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

  (map! :after python
        :map python-mode-map
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
(use-package live-py-mode)

;; LSP
;; With basedpyright
;; See https://docs.basedpyright.com/latest/
(use-package lsp-pyright
  :init
  (when (executable-find "basedpyright")
    (setq lsp-pyright-langserver-command "basedpyright")))


(provide 'init-python)
;;; init-python.el ends here
