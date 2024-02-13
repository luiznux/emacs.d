;;; init-clojure.el --- Initialize clojure configurations   -*- lexical-binding: t -*-
;;
;; Author: Luiz Tagliaferro <luiz@luiznux.com>
;; URL: https://luiznux.com
;; This file is free software :)
;;
;;; Commentary:
;;
;;
;;   ██████╗██╗      ██████╗      ██╗██╗   ██╗██████╗ ███████╗
;;  ██╔════╝██║     ██╔═══██╗     ██║██║   ██║██╔══██╗██╔════╝
;;  ██║     ██║     ██║   ██║     ██║██║   ██║██████╔╝█████╗
;;  ██║     ██║     ██║   ██║██   ██║██║   ██║██╔══██╗██╔══╝
;;  ╚██████╗███████╗╚██████╔╝╚█████╔╝╚██████╔╝██║  ██║███████╗
;;   ╚═════╝╚══════╝ ╚═════╝  ╚════╝  ╚═════╝ ╚═╝  ╚═╝╚══════╝
;;
;;; Code:

(use-package cider
  :commands cider-enable-flex-completion
  :hook ((clojure-mode . cider-mode)
         (cider-test-report-mode . jcf-soft-wrap))
  :config
  (setq cider-repl-pop-to-buffer-on-connect      'nil ;;display-only
        cider-completion-annotations-include-ns  'always
        cider-prompt-for-symbol                  t
        nrepl-hide-special-buffers               t
        cider-repl-use-content-types             t
        cider-repl-wrap-history                  t
        cider-repl-buffer-size-limit             100000
        cider-repl-history-size                  1000)
  (cider-enable-flex-completion))

(when (executable-find "lein")
  (use-package dizzee
    :config
    (dz-defservice jcf-lein-headless
                   "lein"
                   :cd "~/"
                   :args ("repl" ":headless"))

    (dz-defservice jcf-lein-datomic
                   "lein"
                   :args ("datomic"))))

(use-package clj-refactor
  :hook (clojure-mode . clj-refactor-mode))

(use-package eros
  :hook (clojure-mode . eros-mode))

(use-package clojure-snippets)
(use-package inf-clojure)
(use-package zprint-mode)


(provide 'init-clojure)
;;; init-clojure.el ends here
