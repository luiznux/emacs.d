;;; init-completion.el --- Initialize completion configurations.   -*- lexical-binding: t -*-
;;
;; Author: Luiz Tagliaferro <luiz@luiznux.com>
;; URL: https://luiznux.com
;; This file is free software :)
;;
;;; Commentary:
;;
;;   ██████╗ ██████╗ ███╗   ███╗██████╗ ██╗     ███████╗████████╗██╗ ██████╗ ███╗   ██╗
;;  ██╔════╝██╔═══██╗████╗ ████║██╔══██╗██║     ██╔════╝╚══██╔══╝██║██╔═══██╗████╗  ██║
;;  ██║     ██║   ██║██╔████╔██║██████╔╝██║     █████╗     ██║   ██║██║   ██║██╔██╗ ██║
;;  ██║     ██║   ██║██║╚██╔╝██║██╔═══╝ ██║     ██╔══╝     ██║   ██║██║   ██║██║╚██╗██║
;;  ╚██████╗╚██████╔╝██║ ╚═╝ ██║██║     ███████╗███████╗   ██║   ██║╚██████╔╝██║ ╚████║
;;   ╚═════╝ ╚═════╝ ╚═╝     ╚═╝╚═╝     ╚══════╝╚══════╝   ╚═╝   ╚═╝ ╚═════╝ ╚═╝  ╚═══╝
;;
;;
;;; Code:

;; A few more useful configurations...
(use-package emacs
  :commands crm-indicator
  :defines crm-separator
  :init
  ;; Add prompt indicator to `completing-read-multiple'.
  ;; We display [CRM<separator>], e.g., [CRM,] if the separator is a comma.
  (defun crm-indicator (args)
    (cons (format "[CRM%s] %s"
                  (replace-regexp-in-string
                   "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                   crm-separator)
                  (car args))
          (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator))

;; Optionally use the `orderless' completion style.
(use-package orderless
  :custom
  (completion-styles '(basic orderless))
  (orderless-component-separator #'orderless-escapable-split-on-space))

(use-package vertico
  :defines vertico-mouse-mode
  :custom-face
  (vertico-current ((t (:nherit hilight :background "#2257A0" :extend t))))
  :bind (:map vertico-map
         ("RET" . vertico-directory-enter)
         ("DEL" . vertico-directory-delete-char)
         ("M-q" . vertico-directory-delete-word)
         ("M-l" . minibuffer-complete)
         ("M-j" . vertico-next)
         ("M-J" . vertico-next-group)
         ("M-k" . vertico-previous)
         ("M-K" . vertico-previous-group)
         ("M-d" . vertico-scroll-up)
         ("M-u" . vertico-scroll-down)
         ("M-<" . vertico-first)
         ("M->" . vertico-last))
  :hook ((after-init . vertico-mode)
         (rfn-eshadow-update-overlay . vertico-directory-tidy))
  :init
  (setq vertico-mouse-mode t
        vertico-resize     'grow-only
        vertico-count      12))

(use-package nerd-icons-completion
  :commands icons-displayable-p
  :when (icons-displayable-p)
  :hook (vertico-mode . nerd-icons-completion-mode))

(use-package marginalia
  :hook (after-init . marginalia-mode))

(use-package consult
  :commands consult-narrow-help
  :bind (;; C-c bindings in `mode-specific-map'
         ("C-c M-x" . consult-mode-command)
         ("C-c h"   . consult-history)
         ("C-c m"   . consult-man)
         ("C-c i"   . consult-imenu)
         ("C-c I"   . consult-info)
         ("C-c r"   . consult-recent-file)

         ("C-c c e" . consult-colors-emacs)
         ("C-c c w" . consult-colors-web)
         ("C-c c k" . consult-kmacro)
         ("C-c c r" . consult-ripgrep)
         ("C-c c t" . consult-theme)
         ("C-c c F" . describe-face)

         ([remap Info-search]        . consult-info)
         ([remap isearch-forward]    . consult-line)
         ([remap recentf-open-files] . consult-recent-file)

         ;; C-x bindings in `ctl-x-map'
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("C-x b"   . consult-buffer)              ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
         ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer

         ;; Custom M-# bindings for fast register access
         ("M-#"     . consult-register-load)
         ("M-'"     . consult-register-store)      ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#"   . consult-register)

         ;; Other custom bindings
         ("M-y"     . consult-yank-pop)            ;; orig. yank-pop

         ;; M-g bindings in `goto-map'
         ("M-g e"   . consult-compile-error)
         ("M-g f"   . consult-flymake)             ;; Alternative: consult-flycheck
         ("M-g g"   . consult-goto-line)           ;; orig. goto-line
         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ("M-g o"   . consult-outline)             ;; Alternative: consult-org-heading
         ("M-g m"   . consult-mark)
         ("M-g k"   . consult-global-mark)
         ("M-g i"   . consult-imenu)
         ("M-g I"   . consult-imenu-multi)

         ;; M-s bindings in `search-map'
         ("M-s d"   . consult-find)
         ("M-s D"   . consult-locate)
         ("M-s g"   . consult-grep)
         ("M-s G"   . consult-git-grep)
         ("M-s r"   . consult-ripgrep)
         ("M-s l"   . consult-line)
         ("M-s L"   . consult-line-multi)
         ("M-s k"   . consult-keep-lines)
         ("M-s u"   . consult-focus-lines)

         ;; Isearch integration
         ("M-s e"   . consult-isearch-history)
         :map isearch-mode-map
         ("M-e"     . consult-isearch-history)       ;; orig. isearch-edit-string
         ("M-s e"   . consult-isearch-history)       ;; orig. isearch-edit-string
         ("M-s l"   . consult-line)                  ;; needed by consult-line to detect isearch
         ("M-s L"   . consult-line-multi)            ;; needed by consult-line to detect isearch

         ;; Minibuffer history
         :map minibuffer-local-map
         ("C-s" . (lambda ()
                    "Insert the selected region or current symbol at point."
                    (interactive)
                    (insert (with-current-buffer
                                (window-buffer (minibuffer-selected-window))
                              (or (and transient-mark-mode mark-active (/= (point) (mark))
                                       (buffer-substring-no-properties (point) (mark)))
		                          (thing-at-point 'symbol t)
                                  "")))))
         ("M-s" . consult-history)                 ;; orig. next-matching-history-element
         ("M-r" . consult-history))                ;; orig. previous-matching-history-element

  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook (completion-list-mode . consult-preview-at-point-mode)

  ;; The :init configuration is always executed (Not lazy)
  :init
  (with-no-warnings
    ;; Optionally configure the register formatting. This improves the register
    ;; preview for `consult-register', `consult-register-load',
    ;; `consult-register-store' and the Emacs built-ins.
    (setq register-preview-delay 0.5
          register-preview-function #'consult-register-format)

    ;; do not show recent files
    (setq consult-buffer-sources
          '(consult--source-hidden-buffer
            consult--source-modified-buffer
            consult--source-buffer
            consult--source-file-register
            consult--source-bookmark
            consult--source-project-buffer-hidden
            consult--source-project-recent-file-hidden))

    ;; Optionally tweak the register preview window.
    ;; This adds thin lines, sorting and hides the mode line of the window.
    (advice-add #'register-preview :override #'consult-register-window)

    ;; Use Consult to select xref locations with preview
    (with-eval-after-load 'xref
      (setq xref-show-xrefs-function #'consult-xref
            xref-show-definitions-function #'consult-xref))

    ;; More utils
    (defvar consult-colors-history nil
      "History for `consult-colors-emacs' and `consult-colors-web'.")
    ;; No longer preloaded in Emacs 28.
    (autoload 'list-colors-duplicates "facemenu")
    ;; No preloaded in consult.el
    (autoload 'consult--read "consult")

    (defun consult-colors-emacs (color)
      "Show a list of all supported colors for a particular frame.

You can insert the name (default), or insert or kill the hexadecimal or RGB
value of the selected COLOR."
      (interactive
       (list (consult--read (list-colors-duplicates (defined-colors))
                            :prompt "Emacs color: "
                            :require-match t
                            :category 'color
                            :history '(:input consult-colors-history)
                            )))
      (insert color))

    ;; Adapted from counsel.el to get web colors.
    (defun consult-colors--web-list nil
      "Return list of CSS colors for `counsult-colors-web'."
      (require 'shr-color)
      (sort (mapcar #'downcase (mapcar #'car shr-color-html-colors-alist)) #'string-lessp))

    (defun consult-colors-web (color)
      "Show a list of all CSS colors.\

You can insert the name (default), or insert or kill the hexadecimal or RGB
value of the selected COLOR."
      (interactive
       (list (consult--read (consult-colors--web-list)
                            :prompt "Color: "
                            :require-match t
                            :category 'color
                            :history '(:input consult-colors-history)
                            )))
      (insert color))

    ;; Start `consult-line' search with symbol at point
    (defun consult-line-symbol-at-point ()
      (interactive)
      (consult-line (thing-at-point 'symbol))))

  :config
  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  ;; (setq consult-preview-key '("S-<down>" "S-<up>"))
  (setq consult-preview-key '(:debounce 1.0 any))
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
   consult-goto-line
   consult-theme :preview-key '(:debounce 0.5 any))

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; "C-+"

  ;; Optionally make narrowing help available in the minibuffer.
  ;; You may want to use `embark-prefix-help-command' or which-key instead.
  (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help)

  ;; replece `project.el' for find regexp
  (advice-add #'project-find-regexp :override #'consult-ripgrep))

(use-package consult-project-extra)

(use-package consult-org-roam
  :after org-roam
  :diminish
  :defines consult-org-roam-grep-func
  :custom (consult-org-roam-grep-func #'consult-ripgrep)
  :bind
  ;; Define some convenient keybindings as an addition
  ("C-c n e" . consult-org-roam-file-find)
  ("C-c n b" . consult-org-roam-backlinks)
  ("C-c n B" . consult-org-roam-backlinks-recursive)
  ("C-c n L" . consult-org-roam-forward-links)
  ("C-c n r" . consult-org-roam-search)
  :init
  (setq consult-org-roam-mode t)
  :config
  (consult-customize
   consult-org-roam-forward-links :preview-key '(:debounce 0.5 any)))

(use-package consult-notes
  :after org-roam
  :commands (consult-notes-org-headings-mode consult-notes-org-roam-mode)
  :config
  (setq consult-notes-file-dir-sources '(("roam" ?k "~/org/roam")))
  (consult-notes-org-headings-mode)
  (consult-notes-org-roam-mode))

(use-package consult-flyspell
  :bind ("M-g s" . consult-flyspell))

(use-package consult-yasnippet
  :bind ("M-g y" . consult-yasnippet))

(use-package embark
  :commands embark-prefix-help-command
  :bind (("C-." . embark-act) ;; pick some comfortable binding
         ("C-;" . embark-dwim) ;; good alternative: M-.
         ("C-h B" . embark-bindings)
         ([remap describe-bindings] . embark-bindings)
         :map minibuffer-local-map
         ("M-." . my-embark-preview))
  :init
  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)
  :config
  (with-no-warnings
    ;; Manual preview for non-Consult commands using Embark
    (defun my-embark-preview ()
      "Previews candidate in vertico buffer, unless it's a consult command."
      (interactive)
      (unless (bound-and-true-p consult--preview-function)
        (save-selected-window
          (let ((embark-quit-after-action nil))
            (embark-dwim)))))

    ;; Hide the mode line of the Embark live/completions buffers
    (add-to-list 'display-buffer-alist
                 '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                   nil
                   (window-parameters (mode-line-format . none))))

    (with-eval-after-load 'which-key
      (defun embark-which-key-indicator ()
        "An embark indicator that displays keymaps using which-key.
The which-key help message will show the type and value of the
current target followed by an ellipsis if there are further
targets."
        (lambda (&optional keymap targets prefix)
          (if (null keymap)
              (which-key--hide-popup-ignore-command)
            (which-key--show-keymap
             (if (eq (plist-get (car targets) :type) 'embark-become)
                 "Become"
               (format "Act on %s '%s'%s"
                       (plist-get (car targets) :type)
                       (embark--truncate-target (plist-get (car targets) :target))
                       (if (cdr targets) "…" "")))
             (if prefix
                 (pcase (lookup-key keymap prefix 'accept-default)
                   ((and (pred keymapp) km) km)
                   (_ (key-binding prefix 'accept-default)))
               keymap)
             nil nil t (lambda (binding)
                         (not (string-suffix-p "-argument" (cdr binding))))))))

      (setq embark-indicators
            '(embark-which-key-indicator
              embark-highlight-indicator
              embark-isearch-highlight-indicator))

      (defun embark-hide-which-key-indicator (fn &rest args)
        "Hide the which-key indicator immediately when using the completing-read prompter."
        (which-key--hide-popup-ignore-command)
        (let ((embark-indicators
               (remq #'embark-which-key-indicator embark-indicators)))
          (apply fn args)))

      (advice-add #'embark-completing-read-prompter
                  :around #'embark-hide-which-key-indicator))))

(use-package embark-consult
  :bind (:map minibuffer-mode-map
         ("C-c C-o" . embark-export))
  :hook (embark-collect-mode . consult-preview-at-point-mode))


(provide 'init-completion)
;;; init-completion.el ends here.
