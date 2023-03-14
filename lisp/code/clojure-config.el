;;; clojure-config.el --- Packages for clojure language coding  -*- lexical-binding: t -*-
;;
;; Author: Luiz Tagliaferro <luiz@luiznux.com>
;; URL: https://luiznux.com
;; This file is free software :)
;;
;;; Commentary:
;;
;; Code features for clojure language
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
  :hook ((cider-repl-mode . cider-company-enable-fuzzy-completion)
         (cider-mode      . cider-company-enable-fuzzy-completion)
         (cider-test-report-mode . jcf-soft-wrap))
  :config
  (setq cider-repl-pop-to-buffer-on-connect      'nil ;;display-only
        cider-completion-annotations-include-ns  'always
        cider-prompt-for-symbol                  t
        nrepl-hide-special-buffers               t
        cider-repl-use-content-types             t
        cider-repl-wrap-history                  t
        cider-repl-buffer-size-limit             100000
        cider-repl-history-size                  1000))

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


(provide 'clojure-config)
;;; clojure-config.el ends here
