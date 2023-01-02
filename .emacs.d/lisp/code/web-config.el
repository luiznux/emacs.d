;;; web-config.el --- Packages for frontend/web languages  -*- lexical-binding: t -*-
;;
;; Author: Luiz Tagliaferro <luiz@luiznux.com>
;; URL: https://luiznux.com
;; This file is free software :)
;;
;;; Commentary:
;;
;; JS, CSS and HTML packages for better coding
;;
;;  ██╗    ██╗███████╗██████╗
;;  ██║    ██║██╔════╝██╔══██╗
;;  ██║ █╗ ██║█████╗  ██████╔╝
;;  ██║███╗██║██╔══╝  ██╔══██╗
;;  ╚███╔███╔╝███████╗██████╔╝
;;   ╚══╝╚══╝ ╚══════╝╚═════╝
;;
;;; Code:

;; eww
(use-package eww
  :ensure nil
  :init
  ;; Install: npm install -g readability-cli
  (when (executable-find "readable")
    (setq eww-retrieve-command '("readable"))))

;; Webkit browser
(use-package xwidget
  :ensure nil
  :if (featurep 'xwidget-internal)
  :bind (("C-c C-z w" . xwidget-webkit-browse-url)
         :map xwidget-webkit-mode-map
         ("h"         . xwidget-hydra/body))
  :init
  ;; Link navigation
  (use-package xwwp-follow-link-ivy
    :after ivy
    :bind (("C-c C-z x" . xwwp)
           :map xwidget-webkit-mode-map
           ("v"         . xwwp-follow-link))
    :init (setq xwwp-follow-link-completion-system 'ivy)))

;; CSS
(use-package css-mode
  :ensure nil
  :init (setq css-indent-offset 2))

;; SCSS
(use-package scss-mode
  :init
  ;; Disable complilation on save
  (setq scss-compile-at-save nil))

;; LESS
(unless (fboundp 'less-css-mode)
  (use-package less-css-mode))

;; JSON
(unless (fboundp 'js-json-mode)
  (use-package json-mode))

;; JavaScript
(use-package js-mode
  :ensure nil
  :defines (js-indent-level flycheck-javascript-eslint-executable)
  :config
  (setq js-indent-level 2)

  (with-eval-after-load 'flycheck
    ;; https://github.com/mantoni/eslint_d.js
    ;; Install: npm -i -g eslint_d
    (when (executable-find "eslint_d")
      (setq flycheck-javascript-eslint-executable "eslint_d"))))

(use-package js2-mode
  :mode (("\\.js\\'" . js2-mode)
         ("\\.jsx\\'" . js2-jsx-mode))
  :interpreter (("node" . js2-mode)
                ("node" . js2-jsx-mode))
  :hook ((js2-mode . js2-imenu-extras-mode)
         (js2-mode . js2-highlight-unused-variables-mode))
  :config
  ;; Use default keybindings for lsp
  (unbind-key "M-." js2-mode-map)

  (with-eval-after-load 'flycheck
    (when (or (executable-find "eslint_d")
              (executable-find "eslint")
              (executable-find "jshint"))
      (setq js2-mode-show-strict-warnings nil))))

(use-package js2-refactor
  :diminish
  :hook (js2-mode . js2-refactor-mode)
  :config (js2r-add-keybindings-with-prefix "C-c C-m"))

;; Format HTML, CSS and JavaScript/JSON
;; Install: npm -g install prettier
(when (executable-find "prettier")
  (use-package prettier
    :diminish
    :hook ((js-mode js2-mode css-mode sgml-mode web-mode) . prettier-mode)
    :init (setq prettier-pre-warm 'none)))

(use-package skewer-mode
  :diminish
  :hook (((js-mode js2-mode) . skewer-mode)
         (css-mode           . skewer-css-mode)
         (web-mode           . skewer-html-mode)
         (html-mode          . skewer-html-mode))
  :init
  ;; diminish
  (with-eval-after-load 'skewer-css
    (diminish 'skewer-css-mode))
  (with-eval-after-load 'skewer-html
    (diminish 'skewer-html-mode)))

(use-package typescript-mode
  :mode ("\\.ts[x]\\'" . typescript-mode))

(use-package web-mode
  :mode "\\.\\(phtml\\|php\\|[gj]sp\\|as[cp]x\\|erb\\|djhtml\\|html?\\|hbs\\|ejs\\|jade\\|swig\\|tm?pl\\|vue\\)$"
  :config
  (setq web-mode-enable-current-column-highlight  t
        web-mode-markup-indent-offset             4
        web-mode-css-indent-offset                2
        web-mode-code-indent-offset               2
        web-mode-enable-auto-pairing              t))

(use-package auto-rename-tag
  :config
  (add-hook 'html-mode-hook #'auto-rename-tag-mode))

(use-package impatient-mode
  :config
  (defun impatien-start ()
    "Start http and impatient mode"
    (interactive)
    (httpd-start) (impatient-mode))

  (defun impatient-browse ()
    "Jump to browser and show impatients buffer files"
    (interactive)
    (browse-url  "http://localhost:8080/imp/")))

;; REST
(use-package restclient
  :mode ("\\.http\\'" . restclient-mode)
  :config
  (use-package restclient-test
    :diminish
    :hook (restclient-mode . restclient-test-mode))
  (with-eval-after-load 'company
    (use-package company-restclient
      :defines company-backends
      :init (add-to-list 'company-backends 'company-restclient))))

(use-package yaml-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode)))

;; Adds node_modules/.bin directory to `exec_path'
(use-package add-node-modules-path
  :hook ((web-mode js-mode js2-mode) . add-node-modules-path))

(use-package htmlize)


(provide 'web-config)
;;; web-config.el ends here
