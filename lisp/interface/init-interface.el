;;; init-interface.el --- Initialize visual beauties for Emacs   -*- lexical-binding: t -*-
;;
;; Author: Luiz Tagliaferro <luiz@luiznux.com>
;; URL: https://luiznux.com
;; This file is free software :)
;;
;;; Commentary:
;;
;; Visual beauties for coding more happier :)
;;
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

(setq-default cursor-in-non-selected-windows nil)

(setq fast-but-imprecise-scrolling t)
(setq redisplay-skip-fontification-on-input t)

;; Inhibit resizing frame
(setq frame-inhibit-implied-resize   t
      frame-resize-pixelwise         t)

;; Logo
(setq fancy-splash-image luiznux-logo)

(setq menu-bar-mode        nil
      tool-bar-mode        nil
      scroll-bar-mode      nil
      blink-cursor-mode    nil)

(when (and sys/mac-ns-p sys/mac-x-p)
  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
  (add-to-list 'default-frame-alist '(ns-appearance . dark))
  (add-hook 'after-load-theme-hook
            (lambda ()
              (let ((bg (frame-parameter nil 'background-mode)))
                (set-frame-parameter nil 'ns-appearance bg)
                (setcdr (assq 'ns-appearance default-frame-alist) bg)))))

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
  :bind (:map doom-modeline-mode-map
         ("C-<f6>" . doom-modeline-hydra/body))
  :pretty-hydra
  ((:title (pretty-hydra-title "Mode Line" 'fileicon "emacs" :face 'all-the-icons-purple :v-adjust -0.1)
    :color amaranth :quit-key ("q" "C-g"))
   ("Icon"
    (("i" (setq doom-modeline-icon (not doom-modeline-icon))
      "display icons" :toggle doom-modeline-icon)
     ("u" (setq doom-modeline-unicode-fallback (not doom-modeline-unicode-fallback))
      "unicode fallback" :toggle doom-modeline-unicode-fallback)
     ("m" (setq doom-modeline-major-mode-icon (not doom-modeline-major-mode-icon))
      "major mode" :toggle doom-modeline-major-mode-icon)
     ("c" (setq doom-modeline-major-mode-color-icon (not doom-modeline-major-mode-color-icon))
      "colorful major mode" :toggle doom-modeline-major-mode-color-icon)
     ("s" (setq doom-modeline-buffer-state-icon (not doom-modeline-buffer-state-icon))
      "buffer state" :toggle doom-modeline-buffer-state-icon)
     ("o" (setq doom-modeline-buffer-modification-icon (not doom-modeline-buffer-modification-icon))
      "modification" :toggle doom-modeline-buffer-modification-icon)
     ("x" (setq doom-modeline-time-icon (not doom-modeline-time-icon))
      "time" :toggle doom-modeline-time-icon)
     ("v" (setq doom-modeline-modal-icon (not doom-modeline-modal-icon))
      "modal" :toggle doom-modeline-modal-icon))
    "Segment"
    (("g h" (setq doom-modeline-hud (not doom-modeline-hud))
      "hud" :toggle doom-modeline-hud)
     ("g m" (setq doom-modeline-minor-modes (not doom-modeline-minor-modes))
      "minor modes" :toggle doom-modeline-minor-modes)
     ("g w" (setq doom-modeline-enable-word-count (not doom-modeline-enable-word-count))
      "word count" :toggle doom-modeline-enable-word-count)
     ("g e" (setq doom-modeline-buffer-encoding (not doom-modeline-buffer-encoding))
      "encoding" :toggle doom-modeline-buffer-encoding)
     ("g i" (setq doom-modeline-indent-info (not doom-modeline-indent-info))
      "indent" :toggle doom-modeline-indent-info)
     ("g c" (setq doom-modeline-display-misc-in-all-mode-lines (not doom-modeline-display-misc-in-all-mode-lines))
      "misc info" :toggle doom-modeline-display-misc-in-all-mode-lines)
     ("g l" (setq doom-modeline-lsp (not doom-modeline-lsp))
      "lsp" :toggle doom-modeline-lsp)
     ("g p" (setq doom-modeline-persp-name (not doom-modeline-persp-name))
      "perspective" :toggle doom-modeline-persp-name)
     ("g g" (setq doom-modeline-github (not doom-modeline-github))
      "github" :toggle doom-modeline-github)
     ("g n" (setq doom-modeline-gnus (not doom-modeline-gnus))
      "gnus" :toggle doom-modeline-gnus)
     ("g u" (setq doom-modeline-mu4e (not doom-modeline-mu4e))
      "mu4e" :toggle doom-modeline-mu4e)
     ("g r" (setq doom-modeline-irc (not doom-modeline-irc))
      "irc" :toggle doom-modeline-irc)
     ("g f" (setq doom-modeline-irc-buffers (not doom-modeline-irc-buffers))
      "irc buffers" :toggle doom-modeline-irc-buffers)
     ("g s" (progn
              (setq doom-modeline-checker-simple-format (not doom-modeline-checker-simple-format))
              (and (bound-and-true-p flycheck-mode) (flycheck-buffer)))
      "simple checker" :toggle doom-modeline-checker-simple-format)
     ("g t" (setq doom-modeline-time (not doom-modeline-time))
      "time" :toggle doom-modeline-time)
     ("g v" (setq doom-modeline-env-version (not doom-modeline-env-version))
      "version" :toggle doom-modeline-env-version))
    "Style"
    (("a" (setq doom-modeline-buffer-file-name-style 'auto)
      "auto"
      :toggle (eq doom-modeline-buffer-file-name-style 'auto))
     ("b" (setq doom-modeline-buffer-file-name-style 'buffer-name)
      "buffer name"
      :toggle (eq doom-modeline-buffer-file-name-style 'buffer-name))
     ("f" (setq doom-modeline-buffer-file-name-style 'file-name)
      "file name"
      :toggle (eq doom-modeline-buffer-file-name-style 'file-name))
     ("t u" (setq doom-modeline-buffer-file-name-style 'truncate-upto-project)
      "truncate upto project"
      :toggle (eq doom-modeline-buffer-file-name-style 'truncate-upto-project))
     ("t f" (setq doom-modeline-buffer-file-name-style 'truncate-from-project)
      "truncate from project"
      :toggle (eq doom-modeline-buffer-file-name-style 'truncate-from-project))
     ("t w" (setq doom-modeline-buffer-file-name-style 'truncate-with-project)
      "truncate with project"
      :toggle (eq doom-modeline-buffer-file-name-style 'truncate-with-project))
     ("t e" (setq doom-modeline-buffer-file-name-style 'truncate-except-project)
      "truncate except project"
      :toggle (eq doom-modeline-buffer-file-name-style 'truncate-except-project))
     ("t r" (setq doom-modeline-buffer-file-name-style 'truncate-upto-root)
      "truncate upto root"
      :toggle (eq doom-modeline-buffer-file-name-style 'truncate-upto-root))
     ("t a" (setq doom-modeline-buffer-file-name-style 'truncate-all)
      "truncate all"
      :toggle (eq doom-modeline-buffer-file-name-style 'truncate-all))
     ("t n" (setq doom-modeline-buffer-file-name-style 'truncate-nil)
      "truncate none"
      :toggle (eq doom-modeline-buffer-file-name-style 'truncate-nil))
     ("r f" (setq doom-modeline-buffer-file-name-style 'relative-from-project)
      "relative from project"
      :toggle (eq doom-modeline-buffer-file-name-style 'relative-from-project))
     ("r t" (setq doom-modeline-buffer-file-name-style 'relative-to-project)
      "relative to project"
      :toggle (eq doom-modeline-buffer-file-name-style 'relative-to-project)))
    "Project Detection"
    (("p a" (setq doom-modeline-project-detection 'auto)
      "auto"
      :toggle (eq doom-modeline-project-detection 'auto))
     ("p f" (setq doom-modeline-project-detection 'ffip)
      "ffip"
      :toggle (eq doom-modeline-project-detection 'ffip))
     ("p i" (setq doom-modeline-project-detection 'projectile)
      "projectile"
      :toggle (eq doom-modeline-project-detection 'projectile))
     ("p p" (setq doom-modeline-project-detection 'project)
      "project"
      :toggle (eq doom-modeline-project-detection 'project))
     ("p n" (setq doom-modeline-project-detection nil)
      "disable"
      :toggle (eq doom-modeline-project-detection nil)))
    "Misc"
    (("n" (progn
            (message "Fetching GitHub notifications...")
            (run-with-timer 300 nil #'doom-modeline--github-fetch-notifications)
            (browse-url "https://github.com/notifications"))
      "github notifications" :exit t)
     ("e" (cond ((bound-and-true-p flycheck-mode)
                 (flycheck-list-errors))
                ((bound-and-true-p flymake-mode)
                 (flymake-show-diagnostics-buffer)))
      "list errors" :exit t)
     ("w" (if (bound-and-true-p grip-mode)
              (grip-browse-preview)
            (message "Not in preview"))
      "browse preview" :exit t)
     ("z h" (counsel-set-variable 'doom-modeline-height) "set height" :exit t)
     ("z w" (counsel-set-variable 'doom-modeline-bar-width) "set bar width" :exit t)
     ("z g" (counsel-set-variable 'doom-modeline-github-interval) "set github interval" :exit t)
     ("z n" (counsel-set-variable 'doom-modeline-gnus-timer) "set gnus interval" :exit t))))

  :init
  (setq doom-modeline-icon                        emacs-icon
        doom-modeline-height                      20
        doom-modeline-vcs-max-length              20
        doom-modeline-window-width-limit          100
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
        doom-modeline-buffer-encoding             t
        doom-modeline-buffer-file-name-style      'auto
        doom-modeline-project-detection           'auto)

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
           eshell-mode shell-mode
           term-mode vterm-mode
           lsp-ui-imenu-mode
           pdf-annot-list-mode
           flycheck-error-list-mode
           lsp-treemacs-error-list-mode) . hide-mode-line-mode)))


(when fancy-modeline
  (use-package nyan-mode
    :custom
    (nyan-cat-face-number 1)
    (nyan-animate-nyancat t)
    :hook
    (doom-modeline-mode . nyan-mode))

  (use-package parrot
    :autoload parrot-start-animation
    :defines (parrot-set-parrot-type parrot-num-rotations)
    :hook (after-init . parrot-mode)
    :init
    (setq parrot-set-parrot-type 'emacs
          parrot-num-rotations   6)
    (add-hook 'evil-insert-state-entry-hook #'parrot-start-animation)
    (add-hook 'evil-visual-state-entry-hook #'parrot-start-animation)
    (add-hook 'evil-emacs-state-entry-hook  #'parrot-start-animation)))

;; Display ugly ^L page breaks as tidy horizontal lines
(use-package page-break-lines
  :diminish
  :hook (after-init . global-page-break-lines-mode))

;; A minor-mode menu for mode-line
(use-package minions
  :hook (doom-modeline-mode . minions-mode))

;; Child frame
(use-package posframe
  :hook (after-load-theme . posframe-delete-all)
  :init
  (defface posframe-border
    `((t (:inherit region)))
    "Face used by the `posframe' border."
    :group 'posframe)

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

;; Search tools
;; Writable `grep' buffer
(use-package wgrep
  :init
  (setq wgrep-auto-save-buffer t
        wgrep-change-readonly-file t))

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

(when emacs-icon
  (use-package emojify
    :hook ((org-agenda-mode . emojify-mode)
           (org-mode        . emojify-mode))
    :bind ("C-c m" . 'emojify-insert-emoji)
    :init
    (setq emojify-company-tooltips-p   t
          emojify-composed-text-p      nil
          emojify-display-style        'image
          emojify-user-emojis          my-custom-emojis)

    (with-no-warnings
      (when (featurep 'emojify)
        (emojify-set-emoji-data)))

    ;; Others emojis fonts
    (cl-loop for font in '("Noto Color Emoji" "Apple Color Emoji")
             when (font-installed-p font)
             return (if (>= emacs-major-version 28)
                        (set-fontset-font t 'emoji (font-spec :family font) nil 'prepend)
                      (set-fontset-font t 'symbol (font-spec :family font) nil 'prepend)))
    :config
    (use-package company-emojify
      :init
      (setq company-emojify-insert-unicode nil
            company-emojify-emoji-styles   '(github))
      (add-to-list 'company-backends 'company-emojify)))

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
             ("\\.org_archive$"                all-the-icons-fileicon "org"        :face all-the-icons-dsilver)
             ("Cask\\'"                        all-the-icons-fileicon "elisp"      :height 1.0 :v-adjust -0.2 :face all-the-icons-blue)
             ("NEWS$"                          all-the-icons-faicon "newspaper-o"  :height 0.9 :v-adjust -0.2)
             ("^Rakefile$"                     all-the-icons-alltheicon "ruby-alt" :face all-the-icons-red))))
      (dolist (icon regexp-icon-alist)
        (add-to-list 'all-the-icons-regexp-icon-alist icon)))

    (let ((mode-icon-alist
           '((xwidget-webkit-mode           all-the-icons-faicon "chrome"          :v-adjust -0.1 :face all-the-icons-blue)
             (bongo-playlist-mode           all-the-icons-material "queue_music"   :height 1.2 :face all-the-icons-green)
             (bongo-library-mode            all-the-icons-material "library_music" :height 1.1 :face all-the-icons-green)
             (simple-mpc-mode               all-the-icons-faicon "music"           :v-adjust -0.1 :face all-the-icons-green)
             (gnus-group-mode               all-the-icons-fileicon "gnu"           :face all-the-icons-silver)
             (gnus-summary-mode             all-the-icons-octicon "inbox"          :height 1.0 :v-adjust 0.0 :face all-the-icons-orange)
             (gnus-article-mode             all-the-icons-octicon "mail"           :height 1.1 :v-adjust 0.0 :face all-the-icons-lblue)
             (message-mode                  all-the-icons-octicon "mail"           :height 1.1 :v-adjust 0.0 :face all-the-icons-lblue)
             (diff-mode                     all-the-icons-octicon "git-compare"    :v-adjust 0.0 :face all-the-icons-lred)
             (flycheck-error-list-mode      all-the-icons-octicon "checklist"      :height 1.1 :v-adjust 0.0 :face all-the-icons-lred)
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
             (osx-dictionary-mode           all-the-icons-material "library_books" :face all-the-icons-lblue))))
      (dolist (icon mode-icon-alist)
        (add-to-list 'all-the-icons-mode-icon-alist icon))))

  ;; Make mail look pretty
  (use-package all-the-icons-gnus
    :config (all-the-icons-gnus-setup)))

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
(unless (daemonp)
  (advice-add #'display-startup-echo-area-message :override #'ignore))

(use-package time
  :ensure nil
  :init (setq display-time-24hr-format t
              display-time-day-and-date t))

;; Display dividers between windows
(setq window-divider-default-bottom-width  0
      window-divider-default-right-width   4
      window-divider-default-places        t)
(add-hook 'window-setup-hook #'window-divider-mode)

;; Mouse & Smooth Scroll
;; Scroll one line at a time (less "jumpy" than defaults)
(when (display-graphic-p)
  (setq mouse-wheel-scroll-amount              '(1 ((shift) . hscroll))
        mouse-wheel-scroll-amount-horizontal   2
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
(with-no-warnings
  (setq pixel-scroll-precision-large-scroll-height  40.0))

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

;; Ligatures support
(when (and emacs/>=28p (not custom-prettify-symbols-alist))
  (use-package composite
    :ensure nil
    :init (defvar composition-ligature-table (make-char-table nil))
    :hook (((prog-mode
             conf-mode nxml-mode markdown-mode help-mode
             shell-mode eshell-mode term-mode vterm-mode)
            . (lambda () (setq-local composition-function-table composition-ligature-table))))
    :config
    ;; support ligatures, some toned down to prevent hang
    (let ((alist
           '((33  . ".\\(?:\\(==\\|[!=]\\)[!=]?\\)")
             (35  . ".\\(?:\\(###?\\|_(\\|[(:=?[_{]\\)[#(:=?[_{]?\\)")
             (36  . ".\\(?:\\(>\\)>?\\)")
             (37  . ".\\(?:\\(%\\)%?\\)")
             (38  . ".\\(?:\\(&\\)&?\\)")
             (42  . ".\\(?:\\(\\*\\*\\|[*>]\\)[*>]?\\)")
             ;; (42 . ".\\(?:\\(\\*\\*\\|[*/>]\\).?\\)")
             (43  . ".\\(?:\\([>]\\)>?\\)")
             ;; (43 . ".\\(?:\\(\\+\\+\\|[+>]\\).?\\)")
             (45  . ".\\(?:\\(-[->]\\|<<\\|>>\\|[-<>|~]\\)[-<>|~]?\\)")
             ;; (46 . ".\\(?:\\(\\.[.<]\\|[-.=]\\)[-.<=]?\\)")
             (46  . ".\\(?:\\(\\.<\\|[-=]\\)[-<=]?\\)")
             (47  . ".\\(?:\\(//\\|==\\|[=>]\\)[/=>]?\\)")
             ;; (47 . ".\\(?:\\(//\\|==\\|[*/=>]\\).?\\)")
             (48  . ".\\(?:x[a-zA-Z]\\)")
             (58  . ".\\(?:\\(::\\|[:<=>]\\)[:<=>]?\\)")
             (59  . ".\\(?:\\(;\\);?\\)")
             (60  . ".\\(?:\\(!--\\|\\$>\\|\\*>\\|\\+>\\|-[-<>|]\\|/>\\|<[-<=]\\|=[<>|]\\|==>?\\||>\\||||?\\|~[>~]\\|[$*+/:<=>|~-]\\)[$*+/:<=>|~-]?\\)")
             (61  . ".\\(?:\\(!=\\|/=\\|:=\\|<<\\|=[=>]\\|>>\\|[=>]\\)[=<>]?\\)")
             (62  . ".\\(?:\\(->\\|=>\\|>[-=>]\\|[-:=>]\\)[-:=>]?\\)")
             (63  . ".\\(?:\\([.:=?]\\)[.:=?]?\\)")
             (91  . ".\\(?:\\(|\\)[]|]?\\)")
             ;; (92 . ".\\(?:\\([\\n]\\)[\\]?\\)")
             (94  . ".\\(?:\\(=\\)=?\\)")
             (95  . ".\\(?:\\(|_\\|[_]\\)_?\\)")
             (119 . ".\\(?:\\(ww\\)w?\\)")
             (123 . ".\\(?:\\(|\\)[|}]?\\)")
             (124 . ".\\(?:\\(->\\|=>\\||[-=>]\\||||*>\\|[]=>|}-]\\).?\\)")
             (126 . ".\\(?:\\(~>\\|[-=>@~]\\)[-=>@~]?\\)"))))
      (dolist (char-regexp alist)
        (set-char-table-range composition-ligature-table (car char-regexp)
                              `([,(cdr char-regexp) 0 font-shape-gstring]))))
    (set-char-table-parent composition-ligature-table composition-function-table)))

(use-package font-utils)
(use-package latex-preview-pane)
(use-package math-preview)


(provide 'init-interface)
;;; init-interface.el ends here