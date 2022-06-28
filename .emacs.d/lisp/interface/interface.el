;;; interface.el --- visual beauties for Emacs  -*- lexical-binding: t -*-
;;
;; Author: Luiz Tagliaferro <luiz@luiznux.com>
;; URL: https://luiznux.com
;; This file is free software :)
;;
;;; Commentary:
;;
;; Visual beauties for coding more happier :)
;;
;;  ██████╗ ██╗   ██╗██╗
;; ██╔════╝ ██║   ██║██║
;; ██║  ███╗██║   ██║██║
;; ██║   ██║██║   ██║██║
;; ╚██████╔╝╚██████╔╝██║
;;  ╚═════╝  ╚═════╝ ╚═╝
;;
;;; Code:

(require 'constants)
(require 'custom-config)
(require 'functions)
(require 'my-custom-emojis)

;; Optimization
(setq idle-update-delay 1.0)

(setq fast-but-imprecise-scrolling t)
(setq redisplay-skip-fontification-on-input t)

;; Inhibit resizing frame
(setq frame-inhibit-implied-resize   t
      frame-resize-pixelwise         t)

(setq menu-bar-mode        nil
      tool-bar-mode        nil
      scroll-bar-mode      nil
      blink-cursor-mode    nil)

(use-package solaire-mode
  :hook (after-load-theme . solaire-global-mode))

(use-package doom-themes
  :custom-face
  (doom-modeline-buffer-file ((t (:inherit (mode-line bold)))))
  :custom (doom-themes-treemacs-theme "doom-colors")
  :init (load-theme 'doom-one t)
  :config
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config)

  ;; Enable customized theme
  ;; FIXME https://github.com/emacs-lsp/lsp-treemacs/issues/89
  (with-eval-after-load 'lsp-treemacs
    (doom-themes-treemacs-config)))

(use-package doom-modeline
  :hook (after-init . doom-modeline-mode)
  :init
  (setq doom-modeline-icon                        t
        doom-modeline-height                      20
        doom-modeline-major-mode-icon             t
        doom-modeline-buffer-state-icon           t
        doom-modeline-major-mode-color-icon       t
        doom-modeline-buffer-modification-icon    t
        doom-modeline-modal-icon                  t
        doom-modeline-lsp                         t
        doom-modeline-github                      t
        doom-modeline-persp-name                  t
        doom-modeline-persp-icon                  t
        doom-modeline-minor-modes                 t
        doom-modeline-buffer-encoding             nil
        doom-modeline-buffer-file-name-style      'auto
        doom-modeline-project-detection           'auto)

  ;; Prevent flash of unstyled modeline at startup
  (unless after-init-time
    (setq-default mode-line-format nil))

  :config
  ;; thanks to this stupid commit:
  ;; https://git.savannah.gnu.org/cgit/emacs.git/commit/?id=3f843b25dc96867043feebb1d928bde4a7a777a3
  ;; now I use this WORKAROUND:, mentioned in :
  ;; https://github.com/seagle0128/doom-modeline/issues/486
  ;; :pepe-sad:
  (if (facep 'mode-line-active) ;; for 29+
      (set-face-attribute 'mode-line-active nil :family (face-attribute 'default :font) :height doom-modeline-font-size)
    (set-face-attribute 'mode-line nil :family (face-attribute 'default :font) :height doom-modeline-font-size))
  (set-face-attribute 'mode-line-inactive nil :family (face-attribute 'default :font) :height doom-modeline-font-size))

(use-package hide-mode-line
  :hook (((completion-list-mode
           completion-in-region-mode
           pdf-annot-list-mode
           flycheck-error-list-mode
           eshell-mode
           shell-mode
           term-mode
           vterm-mode
           ido-mode
           lsp-treemacs-error-list-mode) . hide-mode-line-mode)))

(use-package nyan-mode
  :custom
  (nyan-cat-face-number 1)
  (nyan-animate-nyancat t)
  :hook
  (doom-modeline-mode . nyan-mode))

(use-package parrot
  :commands (parrot-set-parrot-type parrot-start-animation)
  :custom-face (parrot-set-parrot-type 'emacs)
  :hook (after-init . parrot-mode)
  :init
  (setq parrot-num-rotations 6)
  (add-hook 'evil-insert-state-entry-hook #'parrot-start-animation)
  (add-hook 'evil-visual-state-entry-hook #'parrot-start-animation)
  (add-hook 'evil-emacs-state-entry-hook  #'parrot-start-animation))

;; Display ugly ^L page breaks as tidy horizontal lines
(use-package page-break-lines
  :diminish
  :hook (after-init . global-page-break-lines-mode))

;; A minor-mode menu for mode-line
(use-package minions
  :hook (doom-modeline-mode . minions-mode))

;; Child frame
(use-package posframe
  :hook ((after-load-theme . posframe-delete-all)
         ((after-load-theme server-after-make-frame) . my-set-posframe-faces))
  :init
  (defface posframe-border
    `((t (:background ,(face-foreground 'shadow nil t))))
    "Face used by the `posframe' border."
    :group 'posframe)

  (defun my-set-posframe-faces ()
    "Set `posframe' faces."
    (custom-set-faces
     `(posframe-border ((t (:background ,(face-foreground 'shadow nil t)))))))

  (with-eval-after-load 'persp-mode
    (add-hook 'persp-load-buffer-functions
              (lambda (&rest _)
                (posframe-delete-all))))
  :config
  (with-no-warnings
    (defun my-posframe--prettify-frame (&rest _)
      (set-face-background 'fringe nil posframe--frame))
    (advice-add #'posframe--create-posframe :after #'my-posframe--prettify-frame)

    (defun posframe-poshandler-frame-center-near-bottom (info)
      (cons (/ (- (plist-get info :parent-frame-width)
                  (plist-get info :posframe-width))
               2)
            (/ (+ (plist-get info :parent-frame-height)
                  (* 2 (plist-get info :font-height)))
               2)))))

(use-package which-key
  :diminish
  :bind ("C-h M-m" . which-key-show-major-mode)
  :hook (after-init . which-key-mode)
  :init
  (setq which-key-max-description-length 30
        which-key-show-remaining-keys t))

;; Fast search tool `ripgrep'
(use-package rg
  :defines projectile-command-map
  :hook (after-init . rg-enable-default-bindings)
  :bind (:map rg-global-map
         ("c" . rg-dwim-current-dir)
         ("f" . rg-dwim-current-file)
         ("m" . rg-menu))
  :init (setq rg-group-result t
              rg-show-columns t)
  :config
  (cl-pushnew '("tmpl" . "*.tmpl") rg-custom-type-aliases)

  (with-eval-after-load 'projectile
    (bind-key "s R" #'rg-project projectile-command-map)))

(use-package emojify
  :hook (after-init . global-emojify-mode)
  :bind ("C-c m" . 'emojify-insert-emoji)
  :init
  (with-no-warnings
    (when (featurep 'emojify)
      (emojify-set-emoji-data)))

  (setq emojify-company-tooltips-p   t
        emojify-display-style        'image
        emojify-composed-text-p      nil
        emojify-user-emojis          my-custom-emojis)
  :config
  ;; Others emojis fonts
  (cl-loop for font in '("Noto Color Emoji" "Apple Color Emoji")
           when (font-installed-p font)
           return (if (>= emacs-major-version 28)
                      (set-fontset-font t 'emoji (font-spec :family font) nil 'prepend)
                    (set-fontset-font t 'symbol (font-spec :family font) nil 'prepend))))

;; Add icons for emacs
(use-package all-the-icons
  :custom (all-the-icons-scale-factor 1.1)
  :init (unless (or (font-installed-p "all-the-icons")
                    (daemonp))
          (emacs-install-fonts))
  :config
  ;; Support more icons
  (let ((extension-icon-alist
         '(("bat"  all-the-icons-alltheicon "terminal" :face all-the-icons-lsilver)
           ("cmd"  all-the-icons-alltheicon "terminal" :face all-the-icons-lsilver)
           ("conf" all-the-icons-octicon "settings"    :v-adjust 0.0 :face all-the-icons-yellow)
           ("eln"  all-the-icons-octicon "file-binary" :v-adjust 0.0 :face all-the-icons-dsilver)
           ("epub" all-the-icons-faicon "book"         :height 1.0 :v-adjust -0.1 :face all-the-icons-green)
           ("exe"  all-the-icons-octicon "file-binary" :v-adjust 0.0 :face all-the-icons-dsilver)
           ("make" all-the-icons-fileicon "gnu"        :face all-the-icons-dorange)
           ("rss"  all-the-icons-octicon "rss"         :height 1.1 :v-adjust 0.0 :face all-the-icons-lorange)
           ("toml" all-the-icons-octicon "settings"    :v-adjust 0.0 :face all-the-icons-yellow)
           ("tsx"  all-the-icons-fileicon "tsx"        :height 1.0 :v-adjust -0.1 :face all-the-icons-cyan-alt)
           ("xpm"  all-the-icons-octicon "file-media"  :v-adjust 0.0 :face all-the-icons-dgreen))))
    (dolist (icon extension-icon-alist)
      (add-to-list 'all-the-icons-extension-icon-alist icon)))

  (let ((regexp-icon-alist
         '(("\\.[bB][iI][nN]$"               all-the-icons-octicon "file-binary" :v-adjust 0.0 :face all-the-icons-yellow)
           ("^config$"                       all-the-icons-octicon "settings"    :v-adjust 0.0 :face all-the-icons-dorange)
           ("\\.\\(ba\\|z\\)shrc$"           all-the-icons-alltheicon "script"   :height 0.9 :face all-the-icons-dpink)
           ("\\.\\(bash\\|zsh\\)*_?profile$" all-the-icons-alltheicon "script"   :height 0.9 :face all-the-icons-dred)
           ("\\.\\(ba\\|z\\)sh_history$"     all-the-icons-alltheicon "script"   :height 0.9 :face all-the-icons-dsilver)
           ("\\.zshenv$"                     all-the-icons-alltheicon "script"   :height 0.9 :face all-the-icons-dred)
           ("Cask\\'"                        all-the-icons-fileicon "elisp"      :height 1.0 :v-adjust -0.2 :face all-the-icons-blue)
           ("NEWS$"                          all-the-icons-faicon "newspaper-o"  :height 0.9 :v-adjust -0.2)
           ("^Rakefile$"                     all-the-icons-alltheicon "ruby-alt" :face all-the-icons-red)
           ("^go.\\(sum\\|mod\\)$"           all-the-icons-fileicon "go"         :face all-the-icons-dpurple))))
    (dolist (icon regexp-icon-alist)
      (add-to-list 'all-the-icons-regexp-icon-alist icon)))

  (let ((mode-icon-alist
         '((xwidget-webkit-mode           all-the-icons-faicon "chrome"          :v-adjust -0.1 :face all-the-icons-blue)
           (bongo-playlist-mode           all-the-icons-material "queue_music"   :height 1.2 :face all-the-icons-green)
           (bongo-library-mode            all-the-icons-material "library_music" :height 1.1 :face all-the-icons-green)
           (gnus-group-mode               all-the-icons-fileicon "gnu"           :face all-the-icons-silver)
           (gnus-summary-mode             all-the-icons-octicon "inbox"          :height 1.0 :v-adjust 0.0 :face all-the-icons-orange)
           (gnus-article-mode             all-the-icons-octicon "mail"           :height 1.1 :v-adjust 0.0 :face all-the-icons-lblue)
           (message-mode                  all-the-icons-octicon "mail"           :height 1.1 :v-adjust 0.0 :face all-the-icons-lblue)
           (diff-mode                     all-the-icons-octicon "git-compare"    :v-adjust 0.0 :face all-the-icons-lred)
           (flycheck-error-list-mode      all-the-icons-octicon "checklist"      :height 1.1 :v-adjust 0.0 :face all-the-icons-lred)
           (elfeed-search-mode            all-the-icons-faicon "rss-square"      :v-adjust -0.1 :face all-the-icons-orange)
           (elfeed-show-mode              all-the-icons-octicon "rss"            :height 1.1 :v-adjust 0.0 :face all-the-icons-lorange)
           (newsticker-mode               all-the-icons-faicon "rss-square"      :v-adjust -0.1 :face all-the-icons-orange)
           (newsticker-treeview-mode      all-the-icons-faicon "rss-square"      :v-adjust -0.1 :face all-the-icons-orange)
           (newsticker-treeview-list-mode all-the-icons-octicon "rss"            :height 1.1 :v-adjust 0.0 :face all-the-icons-orange)
           (newsticker-treeview-item-mode all-the-icons-octicon "rss"            :height 1.1 :v-adjust 0.0 :face all-the-icons-lorange)
           (conf-mode                     all-the-icons-octicon "settings"       :v-adjust 0.0 :face all-the-icons-yellow)
           (conf-space-mode               all-the-icons-octicon "settings"       :v-adjust 0.0 :face all-the-icons-yellow)
           (gitconfig-mode                all-the-icons-octicon "settings"       :v-adjust 0.0 :face all-the-icons-dorange)
           (forge-topic-mode              all-the-icons-alltheicon "git"         :face all-the-icons-blue)
           (help-mode                     all-the-icons-faicon "info-circle"     :height 1.1 :v-adjust -0.1 :face all-the-icons-purple)
           (helpful-mode                  all-the-icons-faicon "info-circle"     :height 1.1 :v-adjust -0.1 :face all-the-icons-purple)
           (Info-mode                     all-the-icons-faicon "info-circle"     :height 1.1 :v-adjust -0.1)
           (cask-mode                     all-the-icons-fileicon "elisp"         :height 1.0 :v-adjust -0.2 :face all-the-icons-blue)
           (ein:notebooklist-mode         all-the-icons-faicon "book"            :face all-the-icons-lorange)
           (ein:notebook-mode             all-the-icons-fileicon "jupyter"       :height 1.2 :face all-the-icons-orange)
           (ein:notebook-multilang-mode   all-the-icons-fileicon "jupyter"       :height 1.2 :face all-the-icons-dorange)
           (nov-mode                      all-the-icons-faicon "book"            :height 1.0 :v-adjust -0.1 :face all-the-icons-green)
           (gfm-mode                      all-the-icons-octicon "markdown"       :face all-the-icons-lblue)
           (osx-dictionary-mode           all-the-icons-material "library_books" :face all-the-icons-lblue)
           (youdao-dictionary-mode        all-the-icons-material "library_books" :face all-the-icons-lblue)
           (fanyi-mode                    all-the-icons-material "library_books" :face all-the-icons-lblue))))
    (dolist (icon mode-icon-alist)
      (add-to-list 'all-the-icons-mode-icon-alist icon))))

;; Show line numbers
(use-package display-line-numbers
  :ensure nil
  :hook ((prog-mode yaml-mode conf-mode) . display-line-numbers-mode)
  :init (setq display-line-numbers-width-start t))

;; Suppress GUI features
(setq use-dialog-box                      nil
      initial-scratch-message             nil
      use-file-dialog                     t
      inhibit-startup-screen              t
      inhibit-startup-message             t
      inhibit-default-init                t
      inhibit-startup-echo-area-message   user-login-name)

;; Display dividers between windows
(setq window-divider-default-bottom-width  0
      window-divider-default-right-width   4
      window-divider-default-places        t)
(add-hook 'window-setup-hook #'window-divider-mode)

;; Mouse & Smooth Scroll
;; Scroll one line at a time (less "jumpy" than defaults)
(when (display-graphic-p)
  (setq mouse-wheel-scroll-amount              '(1 ((shift) . 1))
        mouse-wheel-scroll-amount-horizontal   1
        mouse-wheel-progressive-speed          nil))

(setq scroll-step                          1 ;; keyboard scroll one line at a time
      scroll-margin                        0 ;; keyboard scroll at the bottom of the screen
      scroll-conservatively                100000
      auto-window-vscroll                  nil
      mouse-wheel-follow-mouse             't ;; scroll window under mouse
      scroll-preserve-screen-position      t)

;; Good pixel line scrolling
(if (fboundp 'pixel-scroll-precision-mode)
    (pixel-scroll-precision-mode t)
  (when (and emacs/>=27p (not sys/macp))
    (use-package good-scroll
      :diminish
      :hook (after-init . good-scroll-mode)
      :bind (([remap next] . good-scroll-up-full-screen)
             ([remap prior] . good-scroll-down-full-screen)))))

(setq pixel-scroll-precision-large-scroll-height  40.0
      pixel-scroll-precision-interpolation-factor 9)

;; Smooth scrolling over images
(use-package iscroll
  :diminish
  :hook (image-mode . iscroll-mode))

;; Use fixed pitch where it's sensible
(use-package mixed-pitch
  :diminish)

(with-no-warnings
  (when sys/macp
    ;; Render thinner fonts
    (setq ns-use-thin-smoothing t)
    ;; Don't open a file in a new frame
    (setq ns-pop-up-frames nil)))

;; Don't use GTK+ tooltip
(when (boundp 'x-gtk-use-system-tooltips)
  (setq x-gtk-use-system-tooltips nil))

;; When `custom-prettify-symbols-alist' is `nil' use font supported ligatures
(when emacs/>=27p
  (use-package composite
    :ensure nil
    :unless custom-prettify-symbols-alist
    :init (defvar composition-ligature-table (make-char-table nil))
    :hook (((prog-mode conf-mode nxml-mode markdown-mode help-mode)
            . (lambda () (setq-local composition-function-table composition-ligature-table))))
    :config
    ;; support ligatures, some toned down to prevent hang
    (let ((alist
           '((33 . ".\\(?:\\(==\\|[!=]\\)[!=]?\\)")
             (35 . ".\\(?:\\(###?\\|_(\\|[(:=?[_{]\\)[#(:=?[_{]?\\)")
             (36 . ".\\(?:\\(>\\)>?\\)")
             (37 . ".\\(?:\\(%\\)%?\\)")
             (38 . ".\\(?:\\(&\\)&?\\)")
             (42 . ".\\(?:\\(\\*\\*\\|[*>]\\)[*>]?\\)")
             ;; (42 . ".\\(?:\\(\\*\\*\\|[*/>]\\).?\\)")
             (43 . ".\\(?:\\([>]\\)>?\\)")
             ;; (43 . ".\\(?:\\(\\+\\+\\|[+>]\\).?\\)")
             (45 . ".\\(?:\\(-[->]\\|<<\\|>>\\|[-<>|~]\\)[-<>|~]?\\)")
             ;; (46 . ".\\(?:\\(\\.[.<]\\|[-.=]\\)[-.<=]?\\)")
             (46 . ".\\(?:\\(\\.<\\|[-=]\\)[-<=]?\\)")
             (47 . ".\\(?:\\(//\\|==\\|[=>]\\)[/=>]?\\)")
             ;; (47 . ".\\(?:\\(//\\|==\\|[*/=>]\\).?\\)")
             (48 . ".\\(?:x[a-zA-Z]\\)")
             (58 . ".\\(?:\\(::\\|[:<=>]\\)[:<=>]?\\)")
             (59 . ".\\(?:\\(;\\);?\\)")
             (60 . ".\\(?:\\(!--\\|\\$>\\|\\*>\\|\\+>\\|-[-<>|]\\|/>\\|<[-<=]\\|=[<>|]\\|==>?\\||>\\||||?\\|~[>~]\\|[$*+/:<=>|~-]\\)[$*+/:<=>|~-]?\\)")
             (61 . ".\\(?:\\(!=\\|/=\\|:=\\|<<\\|=[=>]\\|>>\\|[=>]\\)[=<>]?\\)")
             (62 . ".\\(?:\\(->\\|=>\\|>[-=>]\\|[-:=>]\\)[-:=>]?\\)")
             (63 . ".\\(?:\\([.:=?]\\)[.:=?]?\\)")
             (91 . ".\\(?:\\(|\\)[]|]?\\)")
             ;; (92 . ".\\(?:\\([\\n]\\)[\\]?\\)")
             (94 . ".\\(?:\\(=\\)=?\\)")

             (119 . ".\\(?:\\(ww\\)w?\\)")
             (123 . ".\\(?:\\(|\\)[|}]?\\)")
             (124 . ".\\(?:\\(->\\|=>\\||[-=>]\\||||*>\\|[]=>|}-]\\).?\\)")
             (126 . ".\\(?:\\(~>\\|[-=>@~]\\)[-=>@~]?\\)"))))
      (dolist (char-regexp alist)
        (set-char-table-range composition-ligature-table (car char-regexp)
                              `([,(cdr char-regexp) 0 font-shape-gstring]))))
    (set-char-table-parent composition-ligature-table composition-function-table)))

(use-package font-utils)
(use-package fontawesome)
(use-package latex-preview-pane)
(use-package math-preview)


(provide 'interface)
;;; interface.el ends here
