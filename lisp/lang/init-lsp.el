;;; init-lsp.el --- Initialize LSP configurations   -*- lexical-binding: t -*-
;;
;; Author: Luiz Tagliaferro <luiz@luiznux.com>
;; URL: https://luiznux.com
;; This file is free software :)
;;
;;; Commentary:
;;
;;
;; ██╗     ███████╗██████╗
;; ██║     ██╔════╝██╔══██╗
;; ██║     ███████╗██████╔╝
;; ██║     ╚════██║██╔═══╝
;; ███████╗███████║██║
;; ╚══════╝╚══════╝╚═╝
;;
;;; Code:

(eval-when-compile
  (require 'init-custom))

;; Emacs client for the Language Server Protocol
;; https://github.com/emacs-lsp/lsp-mode#supported-languages
(use-package lsp-mode
  :diminish
  :defines (lsp-diagnostics-disabled-modes lsp-diagnostics-provider lsp-clients-python-library-directories)
  :autoload lsp-enable-which-key-integration
  :commands (lsp-format-buffer lsp-organize-imports)
  :preface
  ;; Performace tuning
  ;; @see https://emacs-lsp.github.io/lsp-mode/page/performance/
  (setq read-process-output-max (* 1024 1024)) ;; 1MB
  (setenv "LSP_USE_PLISTS" "true")

  :hook ((prog-mode . (lambda ()
                        (unless (derived-mode-p 'emacs-lisp-mode 'lisp-mode 'makefile-mode 'snippet-mode)
                          (lsp-deferred))))
         ((markdown-mode yaml-mode yaml-ts-mode) . lsp-deferred)
         (lsp-mode . (lambda ()
                       ;; Integrate `which-key'
                       (lsp-enable-which-key-integration)

                       ;; Format and organize imports
                       (when (and lsp-format-on-save
                                  (not (apply #'derived-mode-p lsp-format-on-save-ignore-modes)))
                         (add-hook 'before-save-hook #'lsp-format-buffer t t)
                         (add-hook 'before-save-hook #'lsp-organize-imports t t)))))

  :bind (:map lsp-mode-map
         ("C-c C-d" . lsp-describe-thing-at-point)
         ([remap xref-find-definitions] . lsp-find-definition)
         ([remap xref-find-references] . lsp-find-references))

  :custom
  ;; what to use when checking on-save. "check" is default, I prefer clippy
  (lsp-rust-analyzer-cargo-watch-command "clippy")
  (lsp-rust-analyzer-server-display-inlay-hints t)

  :init
  (setq lsp-keymap-prefix                  "C-c l"
        lsp-use-plists                     t
        lsp-eldoc-enable-hover             t
        lsp-lens-enable                    t
        lsp-modeline-code-actions-enable   t
        lsp-semantic-tokens-enable         t
        lsp-modeline-diagnostics-enable    nil
        lsp-enable-symbol-highlighting     nil
        lsp-keep-workspace-alive           nil
        lsp-completion-provider            :none
        lsp-progress-spinner-type          'progress-bar-filled

        ;; For diagnostics
        lsp-diagnostics-disabled-modes '(markdown-mode gfm-mode)

        ;; For `lsp-clients'
        lsp-clients-python-library-directories '("/usr/local/" "/usr/"))

  (defadvice! +lsp--respect-user-defined-checkers-a (fn &rest args)
    "Ensure user-defined `flycheck-checker' isn't overwritten by `lsp'."
    :around #'lsp-diagnostics-flycheck-enable
    (if flycheck-checker
        (let ((old-checker flycheck-checker))
          (apply fn args)
          (setq-local flycheck-checker old-checker))
      (apply fn args)))

  :config
  (use-package consult-lsp
    :bind (:map lsp-mode-map
           ("C-M-." . consult-lsp-symbols)))

  (dolist (m '(clojure-mode clojurec-mode clojurescript-mode clojurex-mode))
    (add-to-list 'lsp-language-id-configuration `(,m . "clojure")))

  ;; Use `cider' for completions and indentation
  (when (derived-mode-p 'clojure-mode 'clojurec-mode 'clojurescript-mode 'cider-mode)
    (setq-local  lsp-enable-indentation nil
                 lsp-completion-enable  nil))

  (with-no-warnings
    ;; Emacs LSP booster
    ;; @seee https://github.com/blahgeek/emacs-lsp-booster
    (when (executable-find "emacs-lsp-booster")
      (defun lsp-booster--advice-json-parse (old-fn &rest args)
        "Try to parse bytecode instead of json."
        (or
         (when (equal (following-char) ?#)
           (let ((bytecode (read (current-buffer))))
             (when (byte-code-function-p bytecode)
               (funcall bytecode))))
         (apply old-fn args)))
      (advice-add (if (progn (require 'json)
                             (fboundp 'json-parse-buffer))
                      'json-parse-buffer
                    'json-read)
                  :around
                  #'lsp-booster--advice-json-parse)

      (defun lsp-booster--advice-final-command (old-fn cmd &optional test?)
        "Prepend emacs-lsp-booster command to lsp CMD."
        (let ((orig-result (funcall old-fn cmd test?)))
          (if (and (not test?)                             ;; for check lsp-server-present?
                   (not (file-remote-p default-directory)) ;; see lsp-resolve-final-command, it would add extra shell wrapper
                   lsp-use-plists
                   (not (functionp 'json-rpc-connection))  ;; native json-rpc
                   (executable-find "emacs-lsp-booster"))
              (progn
                (message "Using emacs-lsp-booster for %s!" orig-result)
                (cons "emacs-lsp-booster" orig-result))
            orig-result)))
      (advice-add 'lsp-resolve-final-command :around #'lsp-booster--advice-final-command))

    ;; Disable `lsp-mode' in `git-timemachine-mode'
    (defun my-lsp--init-if-visible (fn &rest args)
      (unless (bound-and-true-p git-timemachine-mode)
        (apply fn args)))
    (advice-add #'lsp--init-if-visible :around #'my-lsp--init-if-visible)

    ;; Enable `lsp-mode' in sh/bash/zsh
    (defun my-lsp-bash-check-sh-shell (&rest _)
      (and (memq major-mode '(sh-mode bash-ts-mode))
           (memq sh-shell '(sh bash zsh))))
    (advice-add #'lsp-bash-check-sh-shell :override #'my-lsp-bash-check-sh-shell)
    (add-to-list 'lsp-language-id-configuration '(bash-ts-mode . "shellscript"))

    ;; Display icons
    (when (icons-displayable-p)
      (defun my-lsp-icons-get-symbol-kind (fn &rest args)
        (and (icons-displayable-p) (apply fn args)))
      (advice-add #'lsp-icons-get-by-symbol-kind :around #'my-lsp-icons-get-symbol-kind)

      ;; For `lsp-headerline'
      (defun my-lsp-icons-get-by-file-ext (fn &rest args)
        (and (icons-displayable-p) (apply fn args)))
      (advice-add #'lsp-icons-get-by-file-ext :around #'my-lsp-icons-get-by-file-ext)

      (defun my-lsp-icons-get-by-file-ext (file-ext &optional feature)
        (when (and file-ext
                   (lsp-icons--enabled-for-feature feature))
          (nerd-icons-icon-for-extension file-ext)))
      (advice-add #'lsp-icons-get-by-file-ext :override #'my-lsp-icons-get-by-file-ext)

      (defvar lsp-symbol-alist
        '((misc          nerd-icons-codicon "nf-cod-symbol_namespace" :face font-lock-warning-face)
          (document      nerd-icons-codicon "nf-cod-symbol_file" :face font-lock-string-face)
          (namespace     nerd-icons-codicon "nf-cod-symbol_namespace" :face font-lock-type-face)
          (string        nerd-icons-codicon "nf-cod-symbol_string" :face font-lock-doc-face)
          (boolean-data  nerd-icons-codicon "nf-cod-symbol_boolean" :face font-lock-builtin-face)
          (numeric       nerd-icons-codicon "nf-cod-symbol_numeric" :face font-lock-builtin-face)
          (method        nerd-icons-codicon "nf-cod-symbol_method" :face font-lock-function-name-face)
          (field         nerd-icons-codicon "nf-cod-symbol_field" :face font-lock-variable-name-face)
          (localvariable nerd-icons-codicon "nf-cod-symbol_variable" :face font-lock-variable-name-face)
          (class         nerd-icons-codicon "nf-cod-symbol_class" :face font-lock-type-face)
          (interface     nerd-icons-codicon "nf-cod-symbol_interface" :face font-lock-type-face)
          (property      nerd-icons-codicon "nf-cod-symbol_property" :face font-lock-variable-name-face)
          (indexer       nerd-icons-codicon "nf-cod-symbol_enum" :face font-lock-builtin-face)
          (enumerator    nerd-icons-codicon "nf-cod-symbol_enum" :face font-lock-builtin-face)
          (enumitem      nerd-icons-codicon "nf-cod-symbol_enum_member" :face font-lock-builtin-face)
          (constant      nerd-icons-codicon "nf-cod-symbol_constant" :face font-lock-constant-face)
          (structure     nerd-icons-codicon "nf-cod-symbol_structure" :face font-lock-variable-name-face)
          (event         nerd-icons-codicon "nf-cod-symbol_event" :face font-lock-warning-face)
          (operator      nerd-icons-codicon "nf-cod-symbol_operator" :face font-lock-comment-delimiter-face)
          (template      nerd-icons-codicon "nf-cod-symbol_snippet" :face font-lock-type-face)))

      (defun my-lsp-icons-get-by-symbol-kind (kind &optional feature)
        (when (and kind
                   (lsp-icons--enabled-for-feature feature))
          (let* ((icon (cdr (assoc (lsp-treemacs-symbol-kind->icon kind) lsp-symbol-alist)))
                 (args (cdr icon)))
            (apply (car icon) args))))
      (advice-add #'lsp-icons-get-by-symbol-kind :override #'my-lsp-icons-get-by-symbol-kind)

      (setq lsp-headerline-arrow (nerd-icons-octicon "nf-oct-chevron_right"
                                                     :face 'lsp-headerline-breadcrumb-separator-face)))))

(use-package lsp-ui
  :commands lsp-ui-doc-hide
  :custom-face
  (lsp-ui-sideline-code-action ((t (:inherit warning))))
  :pretty-hydra
  ((:title (pretty-hydra-title "LSP UI" 'faicon "nf-fa-rocket" :face 'nerd-icons-green)
    :color amaranth :quit-key ("q" "C-g"))
   ("Doc"
    (("d e" (progn
              (lsp-ui-doc-enable (not lsp-ui-doc-mode))
              (setq lsp-ui-doc-enable (not lsp-ui-doc-enable)))
      "enable" :toggle lsp-ui-doc-mode)
     ("d s" (setq lsp-ui-doc-include-signature (not lsp-ui-doc-include-signature))
      "signature" :toggle lsp-ui-doc-include-signature)
     ("d t" (setq lsp-ui-doc-position 'top)
      "top" :toggle (eq lsp-ui-doc-position 'top))
     ("d b" (setq lsp-ui-doc-position 'bottom)
      "bottom" :toggle (eq lsp-ui-doc-position 'bottom))
     ("d p" (setq lsp-ui-doc-position 'at-point)
      "at point" :toggle (eq lsp-ui-doc-position 'at-point))
     ("d h" (setq lsp-ui-doc-header (not lsp-ui-doc-header))
      "header" :toggle lsp-ui-doc-header)
     ("d f" (setq lsp-ui-doc-alignment 'frame)
      "align frame" :toggle (eq lsp-ui-doc-alignment 'frame))
     ("d w" (setq lsp-ui-doc-alignment 'window)
      "align window" :toggle (eq lsp-ui-doc-alignment 'window)))
    "Sideline"
    (("s e" (progn
              (lsp-ui-sideline-enable (not lsp-ui-sideline-mode))
              (setq lsp-ui-sideline-enable (not lsp-ui-sideline-enable)))
      "enable" :toggle lsp-ui-sideline-mode)
     ("s h" (setq lsp-ui-sideline-show-hover (not lsp-ui-sideline-show-hover))
      "hover" :toggle lsp-ui-sideline-show-hover)
     ("s d" (setq lsp-ui-sideline-show-diagnostics (not lsp-ui-sideline-show-diagnostics))
      "diagnostics" :toggle lsp-ui-sideline-show-diagnostics)
     ("s s" (setq lsp-ui-sideline-show-symbol (not lsp-ui-sideline-show-symbol))
      "symbol" :toggle lsp-ui-sideline-show-symbol)
     ("s c" (setq lsp-ui-sideline-show-code-actions (not lsp-ui-sideline-show-code-actions))
      "code actions" :toggle lsp-ui-sideline-show-code-actions)
     ("s i" (setq lsp-ui-sideline-ignore-duplicate (not lsp-ui-sideline-ignore-duplicate))
      "ignore duplicate" :toggle lsp-ui-sideline-ignore-duplicate))
    "Action"
    (("h" backward-char "←")
     ("j" next-line "↓")
     ("k" previous-line "↑")
     ("l" forward-char "→")
     ("C-a" mwim-beginning-of-code-or-line nil)
     ("C-e" mwim-end-of-code-or-line nil)
     ("C-b" backward-char nil)
     ("C-n" next-line nil)
     ("C-p" previous-line nil)
     ("C-f" forward-char nil)
     ("M-b" backward-word nil)
     ("M-f" forward-word nil)
     ("c" lsp-ui-sideline-apply-code-actions "apply code actions"))))

  :bind (("C-c u" . lsp-ui-imenu)
         :map lsp-ui-mode-map
         ("M-<f6>" . lsp-ui-hydra/body)
         ("M-RET"  . lsp-ui-sideline-apply-code-actions)
         ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
         ([remap xref-find-references]  . lsp-ui-peek-find-references)
         :map lsp-ui-peek-mode-map
         ("j"   . lsp-ui-peek--select-next)
         ("k"   . lsp-ui-peek--select-prev)
         ("C-k" . lsp-ui-peek--select-prev-file)
         ("C-j" . lsp-ui-peek--select-next-file))

  :hook (lsp-mode . lsp-ui-mode)

  :init
  (setq lsp-ui-doc-enable                  t
        lsp-ui-doc-header                  t
        lsp-ui-peek-enable                 t
        lsp-ui-peek-show-directory         t
        lsp-ui-sideline-ignore-duplicate   t
        lsp-ui-doc-show-with-mouse         (display-graphic-p)
        lsp-ui-doc-show-with-cursor        (not (display-graphic-p))
        lsp-ui-doc-delay                   0.9
        lsp-ui-imenu-auto-refresh          'after-save
        lsp-ui-doc-position                'at-point
        lsp-ui-imenu-colors                `(,(face-foreground 'font-lock-keyword-face)
                                             ,(face-foreground 'font-lock-string-face)
                                             ,(face-foreground 'font-lock-constant-face)
                                             ,(face-foreground 'font-lock-variable-name-face)))
  ;; Set correct color to borders
  (defun my-lsp-ui-doc-set-border ()
    "Set the border color of lsp doc."
    (setq lsp-ui-doc-border
          (if (facep 'posframe-border)
              (face-background 'posframe-border nil t)
            (face-background 'region nil t))))
  (my-lsp-ui-doc-set-border)
  (add-hook 'after-load-theme-hook #'my-lsp-ui-doc-set-border t)

  :config
  (with-no-warnings
    ;; Display peek in child frame if possible
    ;; @see https://github.com/emacs-lsp/lsp-ui/issues/441
    (defvar lsp-ui-peek--buffer nil)
    (defun lsp-ui-peek--peek-display (fn src1 src2)
      (if (childframe-workable-p)
          (-let* ((win-width (frame-width))
                  (lsp-ui-peek-list-width (/ (frame-width) 2))
                  (string (-some--> (-zip-fill "" src1 src2)
                            (--map (lsp-ui-peek--adjust win-width it) it)
                            (-map-indexed 'lsp-ui-peek--make-line it)
                            (-concat it (lsp-ui-peek--make-footer)))))
            (setq lsp-ui-peek--buffer (get-buffer-create " *lsp-peek--buffer*"))
            (posframe-show lsp-ui-peek--buffer
                           :string (mapconcat 'identity string "")
                           :min-width (frame-width)
                           :internal-border-color (face-background 'posframe-border nil t)
                           :internal-border-width 1
                           :poshandler #'posframe-poshandler-frame-center))
        (funcall fn src1 src2)))
    (defun lsp-ui-peek--peek-destroy (fn)
      (if (childframe-workable-p)
          (progn
            (when (bufferp lsp-ui-peek--buffer)
              (posframe-hide lsp-ui-peek--buffer))
            (setq lsp-ui-peek--last-xref nil))
        (funcall fn)))
    (advice-add #'lsp-ui-peek--peek-new :around #'lsp-ui-peek--peek-display)
    (advice-add #'lsp-ui-peek--peek-hide :around #'lsp-ui-peek--peek-destroy)

    ;; Handle docs
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
                          'face `(:background ,(face-foreground 'font-lock-comment-face nil t)))
              ;; :align-to is added here too
              (propertize " " 'display '(space :height (1)))
              (and (not (equal after ?\n)) (propertize " \n" 'face '(:height 0.5)))))))))
    (advice-add #'lsp-ui-doc--handle-hr-lines :override #'my-lsp-ui-doc--handle-hr-lines))

  ;; `C-g'to close doc
  (advice-add #'keyboard-quit :before #'lsp-ui-doc-hide))

;; Debug
(use-package dap-mode
  :defines dap-python-executable
  :functions dap-hydra/nil
  :diminish
  :bind (:map lsp-mode-map
         ("<f5>" . dap-debug)
         ("M-<f5>" . dap-hydra))
  :hook ((after-init     . dap-auto-configure-mode)
         (dap-stopped    . (lambda (_) (dap-hydra)))
         (dap-terminated . (lambda (_) (dap-hydra/nil)))

         ((python-mode python-ts-mode)            . (lambda () (require 'dap-python)))
         ((ruby-mode ruby-ts-mode)                . (lambda () (require 'dap-ruby)))
         ((go-mode go-ts-mode)                    . (lambda () (require 'dap-go)))
         ((java-mode java-ts-mode jdee-mode)      . (lambda () (require 'dap-java)))
         ((c-mode c-ts-mode c++-mode c++-ts-mode) . (lambda () (require 'dap-lldb)))
         ((objc-mode swift-mode)                  . (lambda () (require 'dap-lldb)))
         (php-mode                                . (lambda () (require 'dap-php)))
         (elixir-mode                             . (lambda () (require 'dap-elixir)))
         ((js-mode js2-mode js-ts-mode)           . (lambda () (require 'dap-chrome))))
  :init
  (setq dap-auto-configure-features '(sessions locals breakpoints expressions controls))
  (when (executable-find "python3")
    (setq dap-python-executable "python3")))

;; `lsp-mode' and `treemacs' integration
(use-package lsp-treemacs
  :after lsp-mode
  :bind (:map lsp-mode-map
         ("C-<f8>" . lsp-treemacs-errors-list)
         ("M-<f8>" . lsp-treemacs-symbols)
         ("s-<f8>" . lsp-treemacs-java-deps-list))
  :init (setq lsp-treemacs-sync-mode t)
  :config
  (with-eval-after-load 'ace-window
    (when (boundp 'aw-ignored-buffers)
      (push 'lsp-treemacs-symbols-mode aw-ignored-buffers)
      (push 'lsp-treemacs-java-deps-mode aw-ignored-buffers)))

  (with-no-warnings
    (when (icons-displayable-p)
      (treemacs-create-theme "lsp-nerd-icons"
        :config
        (progn
          (treemacs-create-icon
           :icon (format "%s " (nerd-icons-codicon "nf-cod-repo" :face 'nerd-icons-blue))
           :extensions (root))
          (treemacs-create-icon
           :icon (format "%s " (nerd-icons-codicon "nf-cod-symbol_boolean" :face 'nerd-icons-lblue))
           :extensions (boolean-data))
          (treemacs-create-icon
           :icon (format "%s " (nerd-icons-codicon "nf-cod-symbol_class" :face 'nerd-icons-orange))
           :extensions (class))
          (treemacs-create-icon
           :icon (format "%s " (nerd-icons-codicon "nf-cod-symbol_color"))
           :extensions (color-palette))
          (treemacs-create-icon
           :icon (format "%s " (nerd-icons-codicon "nf-cod-symbol_constant"))
           :extensions (constant))
          (treemacs-create-icon
           :icon (format "%s " (nerd-icons-codicon "nf-cod-symbol_file"))
           :extensions (document))
          (treemacs-create-icon
           :icon (format "%s " (nerd-icons-codicon "nf-cod-symbol_misc" :face 'nerd-icons-orange))
           :extensions (enumerator))
          (treemacs-create-icon
           :icon (format "%s " (nerd-icons-codicon "nf-cod-symbol_enum_member" :face 'nerd-icons-lblue))
           :extensions (enumitem))
          (treemacs-create-icon
           :icon (format "%s " (nerd-icons-codicon "nf-cod-symbol_event" :face 'nerd-icons-orange))
           :extensions (event))
          (treemacs-create-icon
           :icon (format "%s " (nerd-icons-codicon "nf-cod-symbol_field" :face 'nerd-icons-lblue))
           :extensions (field))
          (treemacs-create-icon
           :icon (format "%s " (nerd-icons-codicon "nf-cod-symbol_misc"))
           :extensions (indexer))
          (treemacs-create-icon
           :icon (format "%s " (nerd-icons-codicon "nf-cod-symbol_keyword"))
           :extensions (intellisense-keyword))
          (treemacs-create-icon
           :icon (format "%s " (nerd-icons-codicon "nf-cod-symbol_interface" :face 'nerd-icons-lblue))
           :extensions (interface))
          (treemacs-create-icon
           :icon (format "%s " (nerd-icons-codicon "nf-cod-symbol_variable" :face 'nerd-icons-lblue))
           :extensions (localvariable))
          (treemacs-create-icon
           :icon (format "%s " (nerd-icons-codicon "nf-cod-symbol_method" :face 'nerd-icons-purple))
           :extensions (method))
          (treemacs-create-icon
           :icon (format "%s " (nerd-icons-codicon "nf-cod-symbol_namespace" :face 'nerd-icons-lblue))
           :extensions (namespace))
          (treemacs-create-icon
           :icon (format "%s " (nerd-icons-codicon "nf-cod-symbol_numeric"))
           :extensions (numeric))
          (treemacs-create-icon
           :icon (format "%s " (nerd-icons-codicon "nf-cod-symbol_operator"))
           :extensions (operator))
          (treemacs-create-icon
           :icon (format "%s " (nerd-icons-codicon "nf-cod-symbol_property"))
           :extensions (property))
          (treemacs-create-icon
           :icon (format "%s " (nerd-icons-codicon "nf-cod-symbol_snippet"))
           :extensions (snippet))
          (treemacs-create-icon
           :icon (format "%s " (nerd-icons-codicon "nf-cod-symbol_string"))
           :extensions (string))
          (treemacs-create-icon
           :icon (format "%s " (nerd-icons-codicon "nf-cod-symbol_structure" :face 'nerd-icons-orange))
           :extensions (structure))
          (treemacs-create-icon
           :icon (format "%s " (nerd-icons-codicon "nf-cod-symbol_snippet"))
           :extensions (template))
          (treemacs-create-icon
           :icon (format "%s " (nerd-icons-codicon "nf-cod-chevron_right" :face 'nerd-icons-dsilver))
           :extensions (collapsed) :fallback "+")
          (treemacs-create-icon
           :icon (format "%s " (nerd-icons-codicon "nf-cod-chevron_down" :face 'nerd-icons-dsilver))
           :extensions (expanded) :fallback "-")
          (treemacs-create-icon
           :icon (format "%s " (nerd-icons-codicon "nf-cod-file_binary" :face 'nerd-icons-dsilver))
           :extensions (classfile))
          (treemacs-create-icon
           :icon (format "%s " (nerd-icons-codicon "nf-cod-folder_opened" :face 'nerd-icons-blue))
           :extensions (default-folder-opened))
          (treemacs-create-icon
           :icon (format "%s " (nerd-icons-codicon "nf-cod-folder" :face 'nerd-icons-blue))
           :extensions (default-folder))
          (treemacs-create-icon
           :icon (format "%s " (nerd-icons-codicon "nf-cod-folder_opened" :face 'nerd-icons-green))
           :extensions (default-root-folder-opened))
          (treemacs-create-icon
           :icon (format "%s " (nerd-icons-codicon "nf-cod-folder" :face 'nerd-icons-green))
           :extensions (default-root-folder))
          (treemacs-create-icon
           :icon (format "%s " (nerd-icons-codicon "nf-cod-file_binary" :face 'nerd-icons-dsilver))
           :extensions ("class"))
          (treemacs-create-icon
           :icon (format "%s " (nerd-icons-codicon "nf-cod-file_zip" :face 'nerd-icons-dsilver))
           :extensions (file-type-jar))
          (treemacs-create-icon
           :icon (format "%s " (nerd-icons-codicon "nf-cod-folder_opened" :face 'nerd-icons-dsilver))
           :extensions (folder-open))
          (treemacs-create-icon
           :icon (format "%s " (nerd-icons-codicon "nf-cod-folder" :face 'nerd-icons-dsilver))
           :extensions (folder))
          (treemacs-create-icon
           :icon (format "%s " (nerd-icons-codicon "nf-cod-folder_opened" :face 'nerd-icons-orange))
           :extensions (folder-type-component-opened))
          (treemacs-create-icon
           :icon (format "%s " (nerd-icons-codicon "nf-cod-folder" :face 'nerd-icons-orange))
           :extensions (folder-type-component))
          (treemacs-create-icon
           :icon (format "%s " (nerd-icons-codicon "nf-cod-folder_opened" :face 'nerd-icons-green))
           :extensions (folder-type-library-opened))
          (treemacs-create-icon
           :icon (format "%s " (nerd-icons-codicon "nf-cod-folder" :face 'nerd-icons-green))
           :extensions (folder-type-library))
          (treemacs-create-icon
           :icon (format "%s " (nerd-icons-codicon "nf-cod-folder_opened" :face 'nerd-icons-pink))
           :extensions (folder-type-maven-opened))
          (treemacs-create-icon
           :icon (format "%s " (nerd-icons-codicon "nf-cod-folder" :face 'nerd-icons-pink))
           :extensions (folder-type-maven))
          (treemacs-create-icon
           :icon (format "%s " (nerd-icons-codicon "nf-cod-folder_opened" :face 'nerd-icons-orange))
           :extensions (folder-type-package-opened))
          (treemacs-create-icon
           :icon (format "%s " (nerd-icons-codicon "nf-cod-folder" :face 'nerd-icons-orange))
           :extensions (folder-type-package))
          (treemacs-create-icon
           :icon (format "%s " (nerd-icons-codicon "nf-cod-add" :face 'nerd-icons-dsilver))
           :extensions (icon-create))
          (treemacs-create-icon
           :icon (format "%s " (nerd-icons-codicon "nf-cod-list_flat" :face 'nerd-icons-dsilver))
           :extensions (icon-flat))
          (treemacs-create-icon
           :icon (format "%s " (nerd-icons-codicon "nf-cod-symbol_class" :face 'nerd-icons-blue))
           :extensions (icon-hierarchical))
          (treemacs-create-icon
           :icon (format "%s " (nerd-icons-codicon "nf-cod-link" :face 'nerd-icons-dsilver))
           :extensions (icon-link))
          (treemacs-create-icon
           :icon (format "%s " (nerd-icons-codicon "nf-cod-refresh" :face 'nerd-icons-dsilver))
           :extensions (icon-refresh))
          (treemacs-create-icon
           :icon (format "%s " (nerd-icons-faicon "nf-fa-unlink" :face 'nerd-icons-dsilver))
           :extensions (icon-unlink))
          (treemacs-create-icon
           :icon (format "%s " (nerd-icons-devicon "nf-dev-java" :face 'nerd-icons-orange))
           :extensions (jar))
          (treemacs-create-icon
           :icon (format "%s " (nerd-icons-codicon "nf-cod-library" :face 'nerd-icons-green))
           :extensions (library))
          (treemacs-create-icon
           :icon (format "%s " (nerd-icons-codicon "nf-cod-folder_opened" :face 'nerd-icons-lblue))
           :extensions (packagefolder-open))
          (treemacs-create-icon
           :icon (format "%s " (nerd-icons-codicon "nf-cod-folder" :face 'nerd-icons-lblue))
           :extensions (packagefolder))
          (treemacs-create-icon
           :icon (format "%s " (nerd-icons-codicon "nf-cod-archive" :face 'nerd-icons-dsilver))
           :extensions (package))
          (treemacs-create-icon
           :icon (format "%s " (nerd-icons-codicon "nf-cod-repo" :face 'nerd-icons-blue))
           :extensions (java-project))))

      (setq lsp-treemacs-theme "lsp-nerd-icons"))))

;; C/C++/Objective-C
(use-package ccls
  :hook ((c-mode c++-mode objc-mode cuda-mode) . (lambda () (require 'ccls)))
  :config
  (with-no-warnings
    ;; FIXME: fail to call ccls.xref
    ;; @see https://github.com/emacs-lsp/emacs-ccls/issues/109
    (cl-defmethod my-lsp-execute-command
      ((_server (eql ccls)) (command (eql ccls.xref)) arguments)
      (when-let* ((xrefs (lsp--locations-to-xref-items
                          (lsp--send-execute-command (symbol-name command) arguments))))
        (xref--show-xrefs xrefs nil)))
    (advice-add #'lsp-execute-command :override #'my-lsp-execute-command)))

;; Java support
(use-package lsp-java
  :hook ((java-mode java-ts-mode jdee-mode) . (lambda () (require 'lsp-java))))

;; Sql fomater
(use-package sqlformat
  :init
  (when (executable-find "pgformatter")
    (setq sqlformat-command 'pgformatter)
    (setq sqlformat-args '("-s2" "-g"))))

;; Enable LSP in org babel
;; https://github.com/emacs-lsp/lsp-mode/issues/377
(cl-defmacro lsp-org-babel-enable (lang)
  "Support LANG in org source code block."
  (cl-check-type lang string)
  (let* ((edit-pre (intern (format "org-babel-edit-prep:%s" lang)))
         (intern-pre (intern (format "lsp--%s" (symbol-name edit-pre)))))
    `(progn
       (defun ,intern-pre (info)
         (setq buffer-file-name (or (->> info caddr (alist-get :file))
                                    "org-src-babel.tmp"))
         (when (fboundp 'lsp-deferred)
           ;; Avoid headerline conflicts
           (setq-local lsp-headerline-breadcrumb-enable nil)
           (lsp-deferred)))
       (put ',intern-pre 'function-documentation
            (format "Enable lsp-mode in the buffer of org source block (%s)."
                    (upcase ,lang)))

       (if (fboundp ',edit-pre)
           (advice-add ',edit-pre :after ',intern-pre)
         (progn
           (defun ,edit-pre (info)
             (,intern-pre info))
           (put ',edit-pre 'function-documentation
                (format "Prepare local buffer environment for org source block (%s)."
                        (upcase ,lang))))))))

(defconst org-babel-lang-list
  '("go" "python" "ipython" "ruby" "js" "css" "sass" "c" "rust" "java" "cpp" "c++" "shell")
  "The supported programming languages for interactive Babel.")
(dolist (lang org-babel-lang-list)
  (eval `(lsp-org-babel-enable ,lang)))


(provide 'init-lsp)
;;; init-lsp.el ends here
