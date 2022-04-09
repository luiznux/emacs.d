;;; lsp-config.el --- LSP Package configuration file  -*- lexical-binding: t -*-
;;
;; Author: Luiz Tagliaferro <luiz@luiznux.com>
;; URL: https://luiznux.com
;; This file is free software :)
;;
;;; Commentary:
;;
;; LSP packages for Emacs better coding
;;
;; ██╗     ███████╗██████╗
;; ██║     ██╔════╝██╔══██╗
;; ██║     ███████╗██████╔╝
;; ██║     ╚════██║██╔═══╝
;; ███████╗███████║██║
;; ╚══════╝╚══════╝╚═╝
;;
;;; Code:

(require 'constants)


(use-package lsp-mode

  :defines (lsp-clients-python-library-directories
            lsp-rust-server
            lsp-bash-highlight-parsing-errors
            lsp-bash-explainshell-endpoint
            lsp-bash-glob-pattern)

  :commands (lsp
             lsp-deferred
             lsp-enable-which-key-integration
             lsp-format-buffer
             lsp-organize-imports
             lsp-install-server
             lsp-go-install-save-hooks)

  :custom-face
  (lsp-headerline-breadcrumb-path-error-face
   ((t :underline (:style wave :color ,(face-foreground 'error))
       :inherit lsp-headerline-breadcrumb-path-face)))
  (lsp-headerline-breadcrumb-path-warning-face
   ((t :underline (:style wave :color ,(face-foreground 'warning))
       :inherit lsp-headerline-breadcrumb-path-face)))
  (lsp-headerline-breadcrumb-path-info-face
   ((t :underline (:style wave :color ,(face-foreground 'success))
       :inherit lsp-headerline-breadcrumb-path-face)))
  (lsp-headerline-breadcrumb-path-hint-face
   ((t :underline (:style wave :color ,(face-foreground 'success))
       :inherit lsp-headerline-breadcrumb-path-face)))

  (lsp-headerline-breadcrumb-symbols-error-face
   ((t :inherit lsp-headerline-breadcrumb-symbols-face
       :underline (:style wave :color ,(face-foreground 'error)))))
  (lsp-headerline-breadcrumb-symbols-warning-face
   ((t :inherit lsp-headerline-breadcrumb-symbols-face
       :underline (:style wave :color ,(face-foreground 'warning)))))
  (lsp-headerline-breadcrumb-symbols-info-face
   ((t :inherit lsp-headerline-breadcrumb-symbols-face
       :underline (:style wave :color ,(face-foreground 'success)))))
  (lsp-headerline-breadcrumb-symbols-hint-face
   ((t :inherit lsp-headerline-breadcrumb-symbols-face
       :underline (:style wave :color ,(face-foreground 'success)))))

  :hook
  ((prog-mode . (lambda ()
                  (unless (derived-mode-p 'emacs-lisp-mode 'lisp-mode 'makefile-mode)
                    (lsp-deferred))))

   ((go-mode . lsp-deferred) (web-mode . lsp))

   ((clojure-mode . lsp) (clojurec-mode . lsp) (clojurescript-mode . lsp))

   (lsp-mode . (lambda ()
                 ;; Integrate `which-key'
                 (lsp-enable-which-key-integration)
                 (add-hook 'before-save-hook #'lsp-format-buffer t t)
                 (add-hook 'before-save-hook #'lsp-organize-imports t t))))

  :bind (:map lsp-mode-map
         ("C-c C-d" . lsp-describe-thing-at-point)
         ([remap xref-find-definitions] . lsp-find-definition)
         ([remap xref-find-references] . lsp-find-references))

  :custom
  ;; what to use when checking on-save. "check" is default, I prefer clippy
  (lsp-rust-analyzer-cargo-watch-command "clippy")
  (lsp-eldoc-render-all nil)
  (lsp-idle-delay 0.5)
  (lsp-rust-analyzer-server-display-inlay-hints t)

  :init
  ;; @see https://emacs-lsp.github.io/lsp-mode/page/performance
  (setq read-process-output-max (* 1024 1024)) ;; 1MB
  (setq lsp-keymap-prefix                  "C-c l"
        lsp-eldoc-enable-hover             t
        lsp-lens-enable                    t
        lsp-modeline-code-actions-enable   t)

  ;; For `lsp-clients'
  (setq lsp-clients-python-library-directories '("/usr/local/" "/usr/"))

  :config
  (dolist (m '(clojure-mode
               clojurec-mode
               clojurescript-mode
               clojurex-mode))
    (add-to-list 'lsp-language-id-configuration `(,m . "clojure")))

  (setq lsp-bash-highlight-parsing-errors t
        lsp-bash-explainshell-endpoint    t
        lsp-bash-glob-pattern             t)

  (with-no-warnings
    ;; Disable `lsp-mode' in `git-timemachine-mode'
    (defun my-lsp--init-if-visible (fn &rest args)
      (unless (bound-and-true-p git-timemachine-mode)
        (apply fn args)))
    (advice-add #'lsp--init-if-visible :around #'my-lsp--init-if-visible)

    ;; Enable `lsp-mode' in sh/bash/zsh
    (defun my-lsp-bash-check-sh-shell (&rest _)
      (and (eq major-mode 'sh-mode)
           (memq sh-shell '(sh bash zsh))))
    (advice-add #'lsp-bash-check-sh-shell :override #'my-lsp-bash-check-sh-shell))

  (defun lsp-update-server ()
    "Update LSP server."
    (interactive)
    ;; Equals to `C-u M-x lsp-install-server'
    (lsp-install-server t))

  (defun lsp-go-install-save-hooks ()
    "Install save hooks."
    (add-hook 'before-save-hook #'lsp-format-buffer t t)
    (add-hook 'before-save-hook #'lsp-organize-imports t t))
  (add-hook 'go-mode-hook #'lsp-go-install-save-hooks))

(use-package lsp-ui
  :commands lsp-ui-doc-hide

  :custom-face
  (lsp-ui-sideline-code-action ((t (:inherit warning))))

  :bind (("C-c u" . lsp-ui-imenu)
         :map lsp-ui-mode-map
         ("M-RET" . lsp-ui-sideline-apply-code-actions)
         ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
         ([remap xref-find-references] . lsp-ui-peek-find-references))

  :hook (lsp-mode . lsp-ui-mode)

  :init
  (setq lsp-ui-doc-enable                  t
        lsp-ui-doc-header                  t
        lsp-ui-peek-enable                 t
        lsp-ui-peek-show-directory         t
        lsp-ui-sideline-show-code-actions  t
        lsp-ui-sideline-ignore-duplicate   t
        lsp-ui-doc-show-with-mouse         t
        lsp-ui-doc-delay                   0.9
        lsp-ui-doc-position                'at-point
        lsp-ui-doc-border                  (face-foreground 'font-lock-comment-face nil t)
        lsp-ui-imenu-colors                `(,(face-foreground 'font-lock-keyword-face)
                                             ,(face-foreground 'font-lock-string-face)
                                             ,(face-foreground 'font-lock-constant-face)
                                             ,(face-foreground 'font-lock-variable-name-face)))

  :config
  (with-no-warnings
    (defun my-lsp-ui-doc--handle-hr-lines nil
      (let (bolp next before after)
        (goto-char 1)
        (while (setq next (next-single-property-change (or next 1) 'markdown-hr))
          (when (get-text-property next 'markdown-hr)
            (goto-char next)
            (setq bolp (bolp)
                  before (char-before))
            (delete-region (point) (save-excursion (forward-visible-line 1) (point)))
            (setq after (char-after (1+ (point))))
            (insert
             (concat
              (and bolp (not (equal before ?\n)) (propertize "\n" 'face '(:height 0.5)))
              (propertize "\n" 'face '(:height 0.5))
              (propertize " "
                          ;; :align-to is added with lsp-ui-doc--fix-hr-props
                          'display '(space :height (1))
                          'lsp-ui-doc--replace-hr t
                          'face `(:background ,(face-foreground 'font-lock-comment-face)))
              ;; :align-to is added here too
              (propertize " " 'display '(space :height (1)))
              (and (not (equal after ?\n)) (propertize " \n" 'face '(:height 0.5)))))))))
    (advice-add #'lsp-ui-doc--handle-hr-lines :override #'my-lsp-ui-doc--handle-hr-lines))

  ;; `C-g'to close doc
  (advice-add #'keyboard-quit :before #'lsp-ui-doc-hide)

  ;; Reset `lsp-ui-doc-background' after loading theme
  (add-hook 'after-load-theme-hook
            (lambda ()
              (setq lsp-ui-doc-border (face-foreground 'font-lock-comment-face nil t))
              (set-face-background 'lsp-ui-doc-background (face-background 'tooltip nil t)))))


(use-package lsp-ivy
  :after lsp-mode
  :bind (:map lsp-mode-map
         ([remap xref-find-apropos] . lsp-ivy-workspace-symbol)
         ("C-s-." . lsp-ivy-global-workspace-symbol))
  :config
  (with-no-warnings
    (defvar lsp-ivy-symbol-kind-icons
      `(,(all-the-icons-material "find_in_page" :height 0.9 :v-adjust -0.15) ; Unknown - 0
        ,(all-the-icons-faicon "file-o" :height 0.9 :v-adjust -0.02) ; File - 1
        ,(all-the-icons-material "view_module" :height 0.9 :v-adjust -0.15 :face 'all-the-icons-lblue) ; Module - 2
        ,(all-the-icons-material "view_module" :height 0.95 :v-adjust -0.15 :face 'all-the-icons-lblue) ; Namespace - 3
        ,(all-the-icons-octicon "package" :height 0.9 :v-adjust -0.15) ; Package - 4
        ,(all-the-icons-material "settings_input_component" :height 0.9 :v-adjust -0.15 :face 'all-the-icons-orange) ; Class - 5
        ,(all-the-icons-faicon "cube" :height 0.9 :v-adjust -0.02 :face 'all-the-icons-purple) ; Method - 6
        ,(all-the-icons-faicon "wrench" :height 0.8 :v-adjust -0.02) ; Property - 7
        ,(all-the-icons-octicon "tag" :height 0.95 :v-adjust 0 :face 'all-the-icons-lblue) ; Field - 8
        ,(all-the-icons-faicon "cube" :height 0.9 :v-adjust -0.02 :face 'all-the-icons-lpurple) ; Constructor - 9
        ,(all-the-icons-material "storage" :height 0.9 :v-adjust -0.15 :face 'all-the-icons-orange) ; Enum - 10
        ,(all-the-icons-material "share" :height 0.9 :v-adjust -0.15 :face 'all-the-icons-lblue) ; Interface - 11
        ,(all-the-icons-faicon "cube" :height 0.9 :v-adjust -0.02 :face 'all-the-icons-purple) ; Function - 12
        ,(all-the-icons-octicon "tag" :height 0.95 :v-adjust 0 :face 'all-the-icons-lblue) ; Variable - 13
        ,(all-the-icons-faicon "cube" :height 0.9 :v-adjust -0.02 :face 'all-the-icons-purple) ; Constant - 14
        ,(all-the-icons-faicon "text-width" :height 0.9 :v-adjust -0.02) ; String - 15
        ,(all-the-icons-material "format_list_numbered" :height 0.95 :v-adjust -0.15) ; Number - 16
        ,(all-the-icons-octicon "tag" :height 0.9 :v-adjust 0.0 :face 'all-the-icons-lblue) ; Boolean - 17
        ,(all-the-icons-material "view_array" :height 0.95 :v-adjust -0.15) ; Array - 18
        ,(all-the-icons-octicon "tag" :height 0.9 :v-adjust 0.0 :face 'all-the-icons-blue) ; Object - 19
        ,(all-the-icons-faicon "key" :height 0.9 :v-adjust -0.02) ; Key - 20
        ,(all-the-icons-octicon "tag" :height 0.9 :v-adjust 0.0) ; Null - 21
        ,(all-the-icons-material "format_align_right" :height 0.95 :v-adjust -0.15 :face 'all-the-icons-lblue) ; EnumMember - 22
        ,(all-the-icons-material "settings_input_component" :height 0.9 :v-adjust -0.15 :face 'all-the-icons-orange) ; Struct - 23
        ,(all-the-icons-octicon "zap" :height 0.9 :v-adjust 0 :face 'all-the-icons-orange) ; Event - 24
        ,(all-the-icons-material "control_point" :height 0.9 :v-adjust -0.15) ; Operator - 25
        ,(all-the-icons-faicon "arrows" :height 0.9 :v-adjust -0.02) ; TypeParameter - 26
        ))

    (lsp-defun my-lsp-ivy--format-symbol-match
      ((sym &as &SymbolInformation :kind :location (&Location :uri))
       project-root)
      "Convert the match returned by `lsp-mode` into a candidate string."
      (let* ((sanitized-kind (if (< kind (length lsp-ivy-symbol-kind-icons)) kind 0))
             (type (elt lsp-ivy-symbol-kind-icons sanitized-kind))
             (typestr (if lsp-ivy-show-symbol-kind (format "%s " type) ""))
             (pathstr (if lsp-ivy-show-symbol-filename
                          (propertize (format " · %s" (file-relative-name (lsp--uri-to-path uri) project-root))
                                      'face font-lock-comment-face)
                        "")))
        (concat typestr (lsp-render-symbol-information sym ".") pathstr)))
    (advice-add #'lsp-ivy--format-symbol-match :override #'my-lsp-ivy--format-symbol-match)))


;; Debug
(when emacs/>=26p
  (use-package dap-mode
    :defines dap-python-executable
    :diminish
    :bind (:map lsp-mode-map
           ("<f5>" . dap-debug))
    :hook ((after-init . dap-auto-configure-mode)

           (python-mode . (lambda () (require 'dap-python)))
           (ruby-mode . (lambda () (require 'dap-ruby)))
           (go-mode . (lambda () (require 'dap-go)))
           (java-mode . (lambda () (require 'dap-java)))
           ((c-mode c++-mode objc-mode swift-mode) . (lambda () (require 'dap-lldb)))
           (php-mode . (lambda () (require 'dap-php)))
           (elixir-mode . (lambda () (require 'dap-elixir)))
           ((js-mode js2-mode) . (lambda () (require 'dap-chrome))))
    :init
    (when (executable-find "python3")
      (setq dap-python-executable "python3"))))

(use-package lsp-dart
  :hook (dart-mode . lsp))

(use-package lsp-python-ms
  :init (setq lsp-python-ms-auto-install-server t)
  :hook (python-mode . (lambda ()
                         (require 'lsp-python-ms)
                         (lsp)))) ; or lsp-deferred

;; `lsp-mode' and `treemacs' integration
(when emacs/>=25.2p
  (use-package lsp-treemacs
    :after lsp-mode
    :bind (:map lsp-mode-map
           ("C-<f8>" . lsp-treemacs-errors-list)
           ("M-<f8>" . lsp-treemacs-symbols)
           ("s-<f8>" . lsp-treemacs-java-deps-list))
    :init (lsp-treemacs-sync-mode 1)
    :config
    (with-eval-after-load 'ace-window
      (when (boundp 'aw-ignored-buffers)
        (push 'lsp-treemacs-symbols-mode aw-ignored-buffers)
        (push 'lsp-treemacs-java-deps-mode aw-ignored-buffers))))

  (with-no-warnings
    (when (require 'all-the-icons nil t)
      (treemacs-create-theme "centaur-colors"
        :config
        (progn
          (treemacs-create-icon
           :icon (format "%s " (all-the-icons-octicon "repo" :height 1.0 :v-adjust -0.1 :face 'all-the-icons-blue))
           :extensions (root))
          (treemacs-create-icon
           :icon (format "%s " (all-the-icons-octicon "tag" :height 0.9 :v-adjust 0.0 :face 'all-the-icons-lblue))
           :extensions (boolean-data))
          (treemacs-create-icon
           :icon (format "%s " (all-the-icons-material "settings_input_component" :height 0.95 :v-adjust -0.15 :face 'all-the-icons-orange))
           :extensions (class))
          (treemacs-create-icon
           :icon (format "%s " (all-the-icons-material "palette" :height 0.95 :v-adjust -0.15))
           :extensions (color-palette))
          (treemacs-create-icon
           :icon (format "%s " (all-the-icons-faicon "square-o" :height 0.95 :v-adjust -0.15))
           :extensions (constant))
          (treemacs-create-icon
           :icon (format "%s " (all-the-icons-faicon "file-text-o" :height 0.95 :v-adjust -0.05))
           :extensions (document))
          (treemacs-create-icon
           :icon (format "%s " (all-the-icons-material "storage" :height 0.95 :v-adjust -0.15 :face 'all-the-icons-orange))
           :extensions (enumerator))
          (treemacs-create-icon
           :icon (format "%s " (all-the-icons-material "format_align_right" :height 0.95 :v-adjust -0.15 :face 'all-the-icons-lblue))
           :extensions (enumitem))
          (treemacs-create-icon
           :icon (format "%s " (all-the-icons-faicon "bolt" :height 0.95 :v-adjust -0.05 :face 'all-the-icons-orange))
           :extensions (event))
          (treemacs-create-icon
           :icon (format "%s " (all-the-icons-octicon "tag" :height 0.9 :v-adjust 0.0 :face 'all-the-icons-lblue))
           :extensions (field))
          (treemacs-create-icon
           :icon (format "%s " (all-the-icons-faicon "search" :height 0.95 :v-adjust -0.05))
           :extensions (indexer))
          (treemacs-create-icon
           :icon (format "%s " (all-the-icons-material "filter_center_focus" :height 0.95 :v-adjust -0.15))
           :extensions (intellisense-keyword))
          (treemacs-create-icon
           :icon (format "%s " (all-the-icons-material "share" :height 0.95 :v-adjust -0.15 :face 'all-the-icons-lblue))
           :extensions (interface))
          (treemacs-create-icon
           :icon (format "%s " (all-the-icons-octicon "tag" :height 0.9 :v-adjust 0.0 :face 'all-the-icons-lblue))
           :extensions (localvariable))
          (treemacs-create-icon
           :icon (format "%s " (all-the-icons-faicon "cube" :height 0.95 :v-adjust -0.05 :face 'all-the-icons-purple))
           :extensions (method))
          (treemacs-create-icon
           :icon (format "%s " (all-the-icons-material "view_module" :height 0.95 :v-adjust -0.15 :face 'all-the-icons-lblue))
           :extensions (namespace))
          (treemacs-create-icon
           :icon (format "%s " (all-the-icons-material "format_list_numbered" :height 0.95 :v-adjust -0.15))
           :extensions (numeric))
          (treemacs-create-icon
           :icon (format "%s " (all-the-icons-material "control_point" :height 0.95 :v-adjust -0.2))
           :extensions (operator))
          (treemacs-create-icon
           :icon (format "%s " (all-the-icons-faicon "wrench" :height 0.8 :v-adjust -0.05))
           :extensions (property))
          (treemacs-create-icon
           :icon (format "%s " (all-the-icons-material "format_align_center" :height 0.95 :v-adjust -0.15))
           :extensions (snippet))
          (treemacs-create-icon
           :icon (format "%s " (all-the-icons-faicon "text-width" :height 0.9 :v-adjust -0.05))
           :extensions (string))
          (treemacs-create-icon
           :icon (format "%s " (all-the-icons-material "settings_input_component" :height 0.9 :v-adjust -0.15 :face 'all-the-icons-orange))
           :extensions (structure))
          (treemacs-create-icon
           :icon (format "%s " (all-the-icons-material "format_align_center" :height 0.95 :v-adjust -0.15))
           :extensions (template))
          (treemacs-create-icon
           :icon (format "%s " (all-the-icons-octicon "chevron-right" :height 0.75 :v-adjust 0.1 :face 'font-lock-doc-face))
           :extensions (collapsed) :fallback "+")
          (treemacs-create-icon
           :icon (format "%s " (all-the-icons-octicon "chevron-down" :height 0.75 :v-adjust 0.1 :face 'font-lock-doc-face))
           :extensions (expanded) :fallback "-")
          (treemacs-create-icon
           :icon (format "%s " (all-the-icons-octicon "file-binary" :height 0.9  :v-adjust 0.0 :face 'font-lock-doc-face))
           :extensions (classfile))
          (treemacs-create-icon
           :icon (format "%s " (all-the-icons-faicon "folder-open" :height 0.9 :v-adjust -0.05 :face 'all-the-icons-blue))
           :extensions (default-folder-opened))
          (treemacs-create-icon
           :icon (format "%s " (all-the-icons-octicon "file-directory" :height 0.9 :v-adjust 0.0 :face 'all-the-icons-blue))
           :extensions (default-folder))
          (treemacs-create-icon
           :icon (format "%s " (all-the-icons-faicon "folder-open" :height 0.9 :v-adjust -0.05 :face 'all-the-icons-green))
           :extensions (default-root-folder-opened))
          (treemacs-create-icon
           :icon (format "%s " (all-the-icons-octicon "file-directory" :height 0.9 :v-adjust 0.0 :face 'all-the-icons-green))
           :extensions (default-root-folder))
          (treemacs-create-icon
           :icon (format "%s " (all-the-icons-octicon "file-binary" :height 0.9 :v-adjust 0.0 :face 'font-lock-doc-face))
           :extensions ("class"))
          (treemacs-create-icon
           :icon (format "%s " (all-the-icons-octicon "file-zip" :height 0.9 :v-adjust 0.0 :face 'font-lock-doc-face))
           :extensions (file-type-jar))
          (treemacs-create-icon
           :icon (format "%s " (all-the-icons-faicon "folder-open" :height 0.9 :v-adjust -0.05 :face 'font-lock-doc-face))
           :extensions (folder-open))
          (treemacs-create-icon
           :icon (format "%s " (all-the-icons-octicon "file-directory" :height 0.9 :v-adjust 0.0 :face 'font-lock-doc-face))
           :extensions (folder))
          (treemacs-create-icon
           :icon (format "%s " (all-the-icons-faicon "folder-open" :height 0.9 :v-adjust -0.05 :face 'all-the-icons-orange))
           :extensions (folder-type-component-opened))
          (treemacs-create-icon
           :icon (format "%s " (all-the-icons-octicon "file-directory" :height 0.9 :v-adjust 0.0 :face 'all-the-icons-orange))
           :extensions (folder-type-component))
          (treemacs-create-icon
           :icon (format "%s " (all-the-icons-faicon "folder-open" :height 0.9 :v-adjust -0.05 :face 'all-the-icons-green))
           :extensions (folder-type-library-opened))
          (treemacs-create-icon
           :icon (format "%s " (all-the-icons-octicon "file-directory" :height 0.9 :v-adjust 0.0 :face 'all-the-icons-green))
           :extensions (folder-type-library))
          (treemacs-create-icon
           :icon (format "%s " (all-the-icons-faicon "folder-open" :height 0.9 :v-adjust -0.05 :face 'all-the-icons-pink))
           :extensions (folder-type-maven-opened))
          (treemacs-create-icon
           :icon (format "%s " (all-the-icons-octicon "file-directory" :height 0.9 :v-adjust 0.0 :face 'all-the-icons-pink))
           :extensions (folder-type-maven))
          (treemacs-create-icon
           :icon (format "%s " (all-the-icons-faicon "folder-open" :height 0.9 :v-adjust -0.05 :face 'font-lock-type-face))
           :extensions (folder-type-package-opened))
          (treemacs-create-icon
           :icon (format "%s " (all-the-icons-octicon "file-directory" :height 0.9 :v-adjust 0.0 :face 'font-lock-type-face))
           :extensions (folder-type-package))
          (treemacs-create-icon
           :icon (format "%s " (all-the-icons-faicon "plus" :height 0.9 :v-adjust -0.05 :face 'font-lock-doc-face))
           :extensions (icon-create))
          (treemacs-create-icon
           :icon (format "%s " (all-the-icons-faicon "list" :height 0.9 :v-adjust -0.05 :face 'font-lock-doc-face))
           :extensions (icon-flat))
          (treemacs-create-icon
           :icon (format "%s " (all-the-icons-material "share" :height 0.95 :v-adjust -0.2 :face 'all-the-icons-lblue))
           :extensions (icon-hierarchical))
          (treemacs-create-icon
           :icon (format "%s " (all-the-icons-faicon "link" :height 0.9 :v-adjust -0.05 :face 'font-lock-doc-face))
           :extensions (icon-link))
          (treemacs-create-icon
           :icon (format "%s " (all-the-icons-faicon "refresh" :height 0.9 :v-adjust -0.05 :face 'font-lock-doc-face))
           :extensions (icon-refresh))
          (treemacs-create-icon
           :icon (format "%s " (all-the-icons-faicon "chain-broken" :height 0.9 :v-adjust -0.05 :face 'font-lock-doc-face))
           :extensions (icon-unlink))
          (treemacs-create-icon
           :icon (format "%s " (all-the-icons-alltheicon "java" :height 1.0 :v-adjust 0.0 :face 'all-the-icons-orange))
           :extensions (jar))
          (treemacs-create-icon
           :icon (format "%s " (all-the-icons-faicon "book" :height 0.95 :v-adjust -0.05 :face 'all-the-icons-green))
           :extensions (library))
          (treemacs-create-icon
           :icon (format "%s " (all-the-icons-faicon "folder-open" :face 'all-the-icons-lblue))
           :extensions (packagefolder-open))
          (treemacs-create-icon
           :icon (format "%s " (all-the-icons-octicon "file-directory" :height 0.9 :v-adjust 0.0 :face 'all-the-icons-lblue))
           :extensions (packagefolder))
          (treemacs-create-icon
           :icon (format "%s " (all-the-icons-faicon "archive" :height 0.9 :v-adjust -0.05 :face 'font-lock-doc-face))
           :extensions (package))
          (treemacs-create-icon
           :icon (format "%s " (all-the-icons-octicon "repo" :height 1.0 :v-adjust -0.1 :face 'all-the-icons-blue))
           :extensions (java-project))))

      (setq lsp-treemacs-theme "centaur-colors"))))

(use-package lsp-origami
  :init
  (add-hook 'lsp-after-open-hook #'lsp-origami-try-enable))

;; Java support
(when emacs/>=25.2p
  (use-package lsp-java
    :hook (java-mode . (lambda () (require 'lsp-java)))))

(use-package ccls
  :defines projectile-project-root-files-top-down-recurring
  :hook ((c-mode c++-mode objc-mode cuda-mode) . (lambda () (require 'ccls)))
  :config
  (with-eval-after-load 'projectile
    (setq projectile-project-root-files-top-down-recurring
          (append '("compile_commands.json" ".ccls")
                  projectile-project-root-files-top-down-recurring)))
  (with-no-warnings
    ;; FIXME: fail to call ccls.xref
    ;; @see https://github.com/emacs-lsp/emacs-ccls/issues/109
    (cl-defmethod my-lsp-execute-command
      ((_server (eql ccls)) (command (eql ccls.xref)) arguments)
      (when-let ((xrefs (lsp--locations-to-xref-items
                         (lsp--send-execute-command (symbol-name command) arguments))))
        (xref--show-xrefs xrefs nil)))
    (advice-add #'lsp-execute-command :override #'my-lsp-execute-command)))

;; C/C++ Mode
(use-package cc-mode
  :ensure nil
  :bind (:map c-mode-base-map
         ("C-c c" . compile))
  :hook (c-mode-common . (lambda () (c-set-style "stroustrup")))
  :init (setq-default c-basic-offset 4)
  :config
  (use-package modern-cpp-font-lock
    :diminish
    :init (modern-c++-font-lock-global-mode t)))


(provide 'lsp-config)
;;; lsp-config.el ends here
