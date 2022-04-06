;;; go-config.el --- Package configuration go lang -*- lexical-binding: t -*-
;;
;; Author: Luiz Tagliaferro <luiz@luiznux.com>
;; URL: https://luiznux.com
;; This file is free software :)
;;
;;; Commentary:
;;
;; Go Lang config
;;
;;   ██████╗  ██████╗     ██╗      █████╗ ███╗   ██╗ ██████╗
;;  ██╔════╝ ██╔═══██╗    ██║     ██╔══██╗████╗  ██║██╔════╝
;;  ██║  ███╗██║   ██║    ██║     ███████║██╔██╗ ██║██║  ███╗
;;  ██║   ██║██║   ██║    ██║     ██╔══██║██║╚██╗██║██║   ██║
;;  ╚██████╔╝╚██████╔╝    ███████╗██║  ██║██║ ╚████║╚██████╔╝
;;   ╚═════╝  ╚═════╝     ╚══════╝╚═╝  ╚═╝╚═╝  ╚═══╝ ╚═════╝
;;
;;; Code:

(use-package go-mode
  :config
  (autoload 'go-mode "go-mode" nil t)
  (add-to-list 'auto-mode-alist '("\\.go\\'" . go-mode))

  ;; Misc
  (use-package go-dlv)
  (use-package go-fill-struct)
  (use-package go-impl)

  ;; Install: See https://github.com/golangci/golangci-lint#install
  (use-package flycheck-golangci-lint
    :if (executable-find "golangci-lint")
    :after flycheck
    :defines flycheck-disabled-checkers
    :hook (go-mode . (lambda ()
                       "Enable golangci-lint."
                       (setq flycheck-disabled-checkers '(go-gofmt
                                                          go-golint
                                                          go-vet
                                                          go-build
                                                          go-test
                                                          go-errcheck))
                       (flycheck-golangci-lint-setup))))

  (use-package go-gen-test
    :bind (:map go-mode-map
           ("C-c t g" . go-gen-test-dwim)))

  (use-package gotest
    :bind (:map go-mode-map
           ("C-c t f" . go-test-current-file)
           ("C-c t t" . go-test-current-test)
           ("C-c t j" . go-test-current-project)
           ("C-c t b" . go-test-current-benchmark)
           ("C-c t c" . go-test-current-coverage)
           ("C-c t x" . go-run))))

;; Local Golang playground for short snippets
(use-package go-playground
:diminish
:commands (go-playground-mode))


(provide 'go-config)
;;; go-config.el ends here
