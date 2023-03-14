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
  :pretty-hydra
  ((:title (pretty-hydra-title "Webkit" 'faicon "chrome" :face 'all-the-icons-blue)
    :color amaranth :quit-key ("q" "C-g"))
   ("Navigate"
    (("b" xwidget-webkit-back "back")
     ("f" xwidget-webkit-forward "forward")
     ("r" xwidget-webkit-reload "refresh")
     ("SPC" xwidget-webkit-scroll-up "scroll up")
     ("DEL" xwidget-webkit-scroll-down "scroll down")
     ("S-SPC" xwidget-webkit-scroll-down "scroll down"))
    "Zoom"
    (("+" xwidget-webkit-zoom-in "zoom in")
     ("=" xwidget-webkit-zoom-in "zoom in")
     ("-" xwidget-webkit-zoom-out "zoom out"))
    "Misc"
    (("g" xwidget-webkit-browse-url "browse url" :exit t)
     ("u" xwidget-webkit-current-url "show url" :exit t)
     ("v" xwwp-follow-link "follow link" :exit t)
     ("w" xwidget-webkit-current-url-message-kill "copy url" :exit t)
     ("?" describe-mode "help" :exit t)
     ("Q" quit-window "quit" :exit t))))
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

;; Format HTML, CSS and JavaScript/JSON
;; Install: npm -g install prettier
(when (executable-find "prettier")
  (use-package prettier
    :diminish
    :hook ((js-mode js2-mode css-mode sgml-mode web-mode) . prettier-mode)
    :init (setq prettier-pre-warm 'none)))

;; Live browser JavaScript, CSS, and HTML interaction
(use-package skewer-mode
  :diminish
  :hook (((js-mode js2-mode)   . skewer-mode)
         (css-mode             . skewer-css-mode)
         ((html-mode web-mode) . skewer-html-mode))
  :init
  ;; diminish
  (with-eval-after-load 'skewer-css
    (diminish 'skewer-css-mode))
  (with-eval-after-load 'skewer-html
    (diminish 'skewer-html-mode)))

(use-package typescript-mode
  :mode ("\\.ts[x]\\'" . typescript-mode))

;; Run Mocha or Jasmine tests
(use-package mocha
  :config (use-package mocha-snippets))

;; Major mode for editing web templates
(use-package web-mode
  :mode "\\.\\(phtml\\|php\\|[gj]sp\\|as[cp]x\\|erb\\|djhtml\\|html?\\|hbs\\|ejs\\|jade\\|swig\\|tm?pl\\|vue\\)$"
  :config
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2))

;; Adds node_modules/.bin directory to `exec_path'
(use-package add-node-modules-path
  :hook ((web-mode js-mode js2-mode) . add-node-modules-path))

(use-package haml-mode)

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

;; YAML mode
(use-package yaml-mode)


(provide 'web-config)
;;; web-config.el ends here
