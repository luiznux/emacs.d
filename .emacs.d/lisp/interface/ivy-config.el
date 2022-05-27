;;; ivy-config.el --- Loads all `ivy' configs and packages  -*- lexical-binding: t -*-
;;
;; Author: Luiz Tagliaferro <luiz@luiznux.com>
;; URL: https://luiznux.com
;; This file is free software :)
;;
;;; Commentary:
;;
;; Ivy config.
;;
;;   ██╗██╗   ██╗██╗   ██╗
;;   ██║██║   ██║╚██╗ ██╔╝
;;   ██║██║   ██║ ╚████╔╝
;;   ██║╚██╗ ██╔╝  ╚██╔╝
;;   ██║ ╚████╔╝    ██║
;;   ╚═╝  ╚═══╝     ╚═╝
;;
;; More info : https://github.com/abo-abo/swiper
;;
;;; Code:

(require 'functions)

(use-package counsel
  :diminish ivy-mode counsel-mode
  :commands (ivy-immediate-done
             ivy-alt-done
             ivy-set-occur
             ivy-next-line
             ivy-previous-line)
  :preface
  (defun config-ivy-with-empty-ivy-extra-directories (f &rest args)
    (let ((ivy-extra-directories nil))
      (apply f args)))

  :custom-face
  (ivy-current-match ((t (:inherit ivy-current-match))))
  (ivy-minibuffer-match-face-1 ((t (:foreground "dimgray" :distant-foreground nil :background nil))))
  (ivy-minibuffer-match-face-2 ((t (:distant-foreground nil :background nil))))
  (ivy-minibuffer-match-face-3 ((t (:distant-foreground nil :background nil))))
  (ivy-minibuffer-match-face-4 ((t (:distant-foreground nil :background nil))))

  :bind (("M-x"   . 'counsel-M-x)
         ("C-s"   . swiper-isearch)
         ("C-S-s" . swiper-all)

         ("C-c C-r" . ivy-resume)
         ("C-c v p" . ivy-push-view)
         ("C-c v o" . ivy-pop-view)
         ("C-c v ." . ivy-switch-view)

         :map counsel-mode-map
         (([remap swiper] . counsel-grep-or-swiper)
          ([remap swiper-backward] . counsel-grep-or-swiper-backward)
          ([remap dired] . counsel-dired)
          ([remap set-variable] . counsel-set-variable)
          ([remap insert-char] . counsel-unicode-char)
          ([remap recentf-open-files] . counsel-recentf)
          ([remap org-capture] . counsel-org-capture)

          ("C-x j"   . counsel-mark-ring)
          ("C-x C-f" . counsel-find-file)

          ("C-x b" . ivy-switch-buffer)
          ("C-c U" . counsel-unicode-char)
          ("C-c i" . counsel-imenu)
          ("C-x f" . counsel-find-file)
          ("C-c y" . counsel-yank-pop)
          ("C-c r" . counsel-recentf)
          ("C-c v" . counsel-switch-buffer-other-window)
          ("C-c h" . counsel-command-history)
          ("C-c O" . counsel-find-file-extern)

          ("C-c c B" . counsel-bookmarked-directory)
          ("C-c c e" . counsel-colors-emacs)
          ("C-c c m" . counsel-minibuffer-history)

          ("C-c c p" . counsel-pt)
          ("C-c c r" . counsel-rg)
          ("C-c c s" . counsel-ag)
          ("C-c c z" . counsel-fzf)
          ("C-c c t" . counsel-load-theme)
          ("C-c c u" . counsel-unicode-char)
          ("C-c c w" . counsel-colors-web)
          ("C-c c v" . counsel-set-variable))

         ;; Evil mapping for ivy-minibuffer
         :map ivy-minibuffer-map
         (("M-j" . 'ivy-next-line)
          ("M-k" . 'ivy-previous-line)
          ("M-d" . ivy-scroll-up-command)
          ("M-u" . ivy-scroll-down-command)
          ("M-<" . ivy-beginning-of-buffer)
          ("M->" . ivy-end-of-buffer)
          ("C-w" . ivy-yank-word))

         :map swiper-map
         ("M-s" . swiper-isearch-toggle)
         ("M-%" . swiper-query-replace))

  :hook ((after-init . ivy-mode)
         (ivy-mode . counsel-mode))

  :init
  (setq enable-recursive-minibuffers   t ; Allow commands in minibuffers
        ivy-height                     13
        ivy-use-selectable-prompt      t
        ivy-use-virtual-buffers        t ; Enable bookmarks and recentf
        ivy-fixed-height-minibuffer    t
        ivy-count-format               "(%d/%d) "
        ivy-ignore-buffers             '("\\` " "\\`\\*tramp/" "\\`\\*xref" "\\`\\*helpful "
                                         "\\`\\*.+-posframe-buffer\\*" "\\` ?\\*company-.+\\*")
        ivy-on-del-error-function      #'ignore
        ivy-initial-inputs-alist       nil)

  (setq swiper-action-recenter t)

  (setq counsel-find-file-at-point     t
        counsel-preselect-current-file t
        counsel-yank-pop-separator     "\n────────\n")
  (add-hook 'counsel-grep-post-action-hook #'recenter)

  ;; Use the faster search tools
  (when (executable-find "rg")
    (setq counsel-grep-base-command "rg -S --no-heading --line-number --color never '%s' '%s'"))
  (when (executable-find "fd")
    (setq counsel-fzf-cmd
          "fd --type f --hidden --follow --exclude .git --color never '%s'"))

  ;; Be compatible with `gls'
  (when (and sys/macp (executable-find "gls"))
    (setq counsel-find-file-occur-use-find nil
          counsel-find-file-occur-cmd
          "gls -a | grep -i -E '%s' | tr '\\n' '\\0' | xargs -0 gls -d --group-directories-first"))

  ;; ignore files
  (setq counsel-find-file-ignore-regexp
        "\\(?:^[#.]\\)\\|\\(?:[#~]$\\)\\|\\(?:^Icon?\\)")

  (add-hook 'minibuffer-setup-hook
            (lambda () (setq-local show-trailing-whitespace nil)))
  (ivy-set-occur 'ivy-switch-buffer 'ivy-switch-buffer-occur)

  ;; Do not show extra directories when finding files.
  (setq ivy-extra-directories '("."))
  (advice-add #'counsel-find-file :around #'config-ivy-with-empty-ivy-extra-directories)

  :config
  ;; Use C-j for immediate termination with the current value, and RET
  ;; for continuing completion for that directory. This is the ido
  ;; behaviour.
  (define-key ivy-minibuffer-map (kbd "C-j") #'ivy-immediate-done)
  (define-key ivy-minibuffer-map (kbd "RET") #'ivy-alt-done)

  (with-no-warnings
    ;; persist views
    (with-eval-after-load 'savehist
      (add-to-list 'savehist-additional-variables 'ivy-views))

    ;; Highlight the selected item
    (defun my-ivy-format-function (cands)
      "Transform CANDS into a string for minibuffer."
      (if (display-graphic-p)
          (ivy-format-function-line cands)
        (ivy-format-function-arrow cands)))
    (setf (alist-get 't ivy-format-functions-alist) #'my-ivy-format-function)

    ;; Pre-fill search keywords
    ;; @see https://www.reddit.com/r/emacs/comments/b7g1px/withemacs_execute_commands_like_marty_mcfly/
    (defvar my-ivy-fly-commands
      '(query-replace-regexp
        flush-lines keep-lines ivy-read
        swiper swiper-backward swiper-all
        swiper-isearch swiper-isearch-backward
        lsp-ivy-workspace-symbol lsp-ivy-global-workspace-symbol
        counsel-grep-or-swiper counsel-grep-or-swiper-backward
        counsel-grep counsel-ack counsel-ag counsel-rg counsel-pt))

    (defvar my-ivy-fly-back-commands
      '(self-insert-command
        ivy-forward-char ivy-delete-char delete-forward-char kill-word kill-sexp
        end-of-line mwim-end-of-line mwim-end-of-code-or-line mwim-end-of-line-or-code
        yank ivy-yank-word ivy-yank-char ivy-yank-symbol counsel-yank-pop))

    (defvar-local my-ivy-fly--travel nil)
    (defun my-ivy-fly-back-to-present ()
      (cond ((and (memq last-command my-ivy-fly-commands)
                  (equal (this-command-keys-vector) (kbd "M-p")))
             ;; repeat one time to get straight to the first history item
             (setq unread-command-events
                   (append unread-command-events
                           (listify-key-sequence (kbd "M-p")))))
            ((or (memq this-command my-ivy-fly-back-commands)
                 (equal (this-command-keys-vector) (kbd "M-n")))
             (unless my-ivy-fly--travel
               (delete-region (point) (point-max))
               (when (memq this-command '(ivy-forward-char
                                          ivy-delete-char delete-forward-char
                                          kill-word kill-sexp
                                          end-of-line mwim-end-of-line
                                          mwim-end-of-code-or-line
                                          mwim-end-of-line-or-code))
                 (insert (ivy-cleanup-string ivy-text))
                 (when (memq this-command '(ivy-delete-char
                                            delete-forward-char
                                            kill-word kill-sexp))
                   (beginning-of-line)))
               (setq my-ivy-fly--travel t)))))

    (defun my-ivy-fly-time-travel ()
      (when (memq this-command my-ivy-fly-commands)
        (insert (propertize
                 (save-excursion
		           (set-buffer (window-buffer (minibuffer-selected-window)))
		           (ivy-thing-at-point))
                 'face 'shadow))
        (add-hook 'pre-command-hook 'my-ivy-fly-back-to-present nil t)
        (beginning-of-line)))

    (add-hook 'minibuffer-setup-hook #'my-ivy-fly-time-travel)
    (add-hook 'minibuffer-exit-hook
              (lambda ()
                (remove-hook 'pre-command-hook 'my-ivy-fly-back-to-present t)))

    ;;
    ;; Improve search experience of `swiper' and `counsel'
    ;;
    (defun my-ivy-switch-to-swiper (&rest _)
      "Switch to `swiper' with the current input."
      (swiper ivy-text))

    (defun my-ivy-switch-to-swiper-isearch (&rest _)
      "Switch to `swiper-isearch' with the current input."
      (swiper-isearch ivy-text))

    (defun my-ivy-switch-to-swiper-all (&rest _)
      "Switch to `swiper-all' with the current input."
      (swiper-all ivy-text))

    (defun my-ivy-switch-to-rg-dwim (&rest _)
      "Switch to `rg-dwim' with the current input."
      (ivy-quit-and-run (rg-dwim default-directory)))

    (defun my-ivy-switch-to-counsel-rg (&rest _)
      "Switch to `counsel-rg' with the current input."
      (counsel-rg ivy-text default-directory))

    (defun my-ivy-switch-to-counsel-git-grep (&rest _)
      "Switch to `counsel-git-grep' with the current input."
      (counsel-git-grep ivy-text default-directory))

    (defun my-ivy-switch-to-counsel-find-file (&rest _)
      "Switch to `counsel-find-file' with the current input."
      (counsel-find-file ivy-text))

    (defun my-ivy-switch-to-counsel-fzf (&rest _)
      "Switch to `counsel-fzf' with the current input."
      (counsel-fzf ivy-text default-directory))

    (defun my-ivy-switch-to-counsel-git (&rest _)
      "Switch to `counsel-git' with the current input."
      (counsel-git ivy-text))

    (defun my-ivy-switch-to-list-bookmarks (&rest _)
      "Switch to `list-bookmarks'."
      (ivy-quit-and-run (call-interactively #'list-bookmarks)))

    (defun my-ivy-switch-to-list-colors (&rest _)
      "Switch to `list-colors-display'."
      (ivy-quit-and-run (list-colors-display)))

    (defun my-ivy-switch-to-list-packages (&rest _)
      "Switch to `list-packages'."
      (ivy-quit-and-run (list-packages)))

    (defun my-ivy-switch-to-list-processes (&rest _)
      "Switch to `list-processes'."
      (ivy-quit-and-run (list-processes)))

    (defun my-ivy-copy-library-path (lib)
      "Copy the full path of LIB."
      (let ((path (find-library-name lib)))
        (kill-new path)
        (message "Copied path: \"%s\"." path)))

    ;; @see https://emacs-china.org/t/swiper-swiper-isearch/9007/12
    (defun my-swiper-toggle-counsel-rg ()
      "Toggle `counsel-rg' and `swiper'/`swiper-isearch' with the current input."
      (interactive)
      (ivy-quit-and-run
        (if (memq (ivy-state-caller ivy-last) '(swiper swiper-isearch))
            (my-ivy-switch-to-counsel-rg)
          (my-ivy-switch-to-swiper-isearch))))
    (bind-key "<C-return>" #'my-swiper-toggle-counsel-rg swiper-map)
    (bind-key "<C-return>" #'my-swiper-toggle-counsel-rg counsel-ag-map)

    (with-eval-after-load 'rg
      (defun my-swiper-toggle-rg-dwim ()
        "Toggle `rg-dwim' with the current input."
        (interactive)
        (ivy-quit-and-run
          (rg-dwim default-directory)))
      (bind-key "<M-return>" #'my-swiper-toggle-rg-dwim swiper-map)
      (bind-key "<M-return>" #'my-swiper-toggle-rg-dwim counsel-ag-map))

    (defun my-swiper-toggle-swiper-isearch ()
      "Toggle `swiper' and `swiper-isearch' with the current input."
      (interactive)
      (ivy-quit-and-run
        (if (eq (ivy-state-caller ivy-last) 'swiper-isearch)
            (swiper ivy-text)
          (swiper-isearch ivy-text))))
    (bind-key "<s-return>" #'my-swiper-toggle-swiper-isearch swiper-map)

    (defun my-counsel-find-file-toggle-fzf ()
      "Toggle `counsel-fzf' with the current `counsel-find-file' input."
      (interactive)
      (ivy-quit-and-run
        (counsel-fzf (or ivy-text "") default-directory)))
    (bind-key "<C-return>" #'my-counsel-find-file-toggle-fzf counsel-find-file-map)

    (defun my-counsel-toggle ()
      "Toggle `counsel' commands and original commands."
      (interactive)
      (pcase (ivy-state-caller ivy-last)
        ('counsel-bookmark (my-ivy-switch-to-list-bookmarks))
        ('counsel-colors-emacs (my-ivy-switch-to-list-colors))
        ('counsel-colors-web (my-ivy-switch-to-list-colors))
        ('counsel-list-processes (my-ivy-switch-to-list-processes))
        ('counsel-package (my-ivy-switch-to-list-packages))
        (_ (ignore))))
    (bind-key "<C-return>" #'my-counsel-toggle ivy-minibuffer-map)

    ;; More actions
    (ivy-add-actions
     #'swiper-isearch
     '(("r" my-ivy-switch-to-counsel-rg "rg")
       ("d" my-ivy-switch-to-rg-dwim "rg dwim")
       ("s" my-ivy-switch-to-swiper "swiper")
       ("a" my-ivy-switch-to-swiper-all "swiper all")))

    (ivy-add-actions
     #'swiper
     '(("r" my-ivy-switch-to-counsel-rg "rg")
       ("d" my-ivy-switch-to-rg-dwim "rg dwim")
       ("s" my-ivy-switch-to-swiper-isearch "swiper isearch")
       ("a" my-ivy-switch-to-swiper-all "swiper all")))

    (ivy-add-actions
     #'swiper-all
     '(("g" my-ivy-switch-to-counsel-git-grep "git grep")
       ("r" my-ivy-switch-to-counsel-rg "rg")
       ("d" my-ivy-switch-to-rg-dwim "rg dwim")
       ("s" my-swiper-toggle-swiper-isearch "swiper isearch")
       ("S" my-ivy-switch-to-swiper "swiper")))

    (ivy-add-actions
     #'counsel-rg
     '(("s" my-ivy-switch-to-swiper-isearch "swiper isearch")
       ("S" my-ivy-switch-to-swiper "swiper")
       ("a" my-ivy-switch-to-swiper-all "swiper all")
       ("d" my-ivy-switch-to-rg-dwim "rg dwim")))

    (ivy-add-actions
     #'counsel-git-grep
     '(("s" my-ivy-switch-to-swiper-isearch "swiper isearch")
       ("S" my-ivy-switch-to-swiper "swiper")
       ("r" my-ivy-switch-to-rg-dwim "rg")
       ("d" my-ivy-switch-to-rg-dwim "rg dwim")
       ("a" my-ivy-switch-to-swiper-all "swiper all")))

    (ivy-add-actions
     #'counsel-find-file
     '(("g" my-ivy-switch-to-counsel-git "git")
       ("z" my-ivy-switch-to-counsel-fzf "fzf")))

    (ivy-add-actions
     #'counsel-git
     '(("f" my-ivy-switch-to-counsel-find-file "find file")
       ("z" my-ivy-switch-to-counsel-fzf "fzf")))

    (ivy-add-actions
     'counsel-fzf
     '(("f" my-ivy-switch-to-counsel-find-file "find file")
       ("g" my-ivy-switch-to-counsel-git "git")))

    (ivy-add-actions
     'counsel-find-library
     '(("p" my-ivy-copy-library-path "copy path")))

    (ivy-add-actions
     'counsel-load-library
     '(("p" my-ivy-copy-library-path "copy path")))

    (ivy-add-actions
     #'counsel-bookmark
     '(("l" my-ivy-switch-to-list-bookmarks "list")))

    (ivy-add-actions
     #'counsel-colors-emacs
     '(("l" my-ivy-switch-to-list-colors "list")))

    (ivy-add-actions
     #'counsel-colors-web
     '(("l" my-ivy-switch-to-list-colors "list")))

    (ivy-add-actions
     #'counsel-package
     '(("l" my-ivy-switch-to-list-packages "list packages")))

    (ivy-add-actions
     #'counsel-list-processes
     '(("l" my-ivy-switch-to-list-processes "list"))))


  ;; Enhance M-x
  (use-package amx
    :init (setq amx-history-length 20))

  ;; Ivy integration for Projectile
  (use-package counsel-projectile
    :hook (counsel-mode . counsel-projectile-mode)
    :init (setq counsel-projectile-grep-initial-input '(ivy-thing-at-point)))

  ;; Integrate yasnippet
  (use-package ivy-yasnippet
    :bind ("C-c C-y" . ivy-yasnippet))

  ;; Quick launch apps
  (cond
   (sys/linux-x-p
    (bind-key "s-<f6>" #'counsel-linux-app counsel-mode-map))
   (sys/macp
    (use-package counsel-osx-app
      :bind (:map counsel-mode-map
             ("s-<f6>" . counsel-osx-app)))))

  ;; Tramp ivy interface
  (use-package counsel-tramp
    :bind (:map counsel-mode-map
           ("C-c c T" . counsel-tramp)))

  ;; Ivy
  (use-package ivy-dired-history
    :demand t
    :after (savehist dired)
    :bind (:map dired-mode-map
           ("," . dired))
    :init (add-to-list 'savehist-additional-variables 'ivy-dired-history-variable))

  ;; Better experience with icons
  ;; Enable it before`ivy-rich-mode' for better performance
  (use-package all-the-icons-ivy-rich
    :hook (ivy-mode . all-the-icons-ivy-rich-mode)
    :config
    (plist-put all-the-icons-ivy-rich-display-transformers-list
               'load-theme
               '(:columns
                 ((all-the-icons-ivy-rich-theme-icon)
                  (ivy-rich-candidate))
                 :delimiter "\t"))
    (all-the-icons-ivy-rich-reload))

  ;; More friendly display transformer for Ivy
  (use-package ivy-rich
    :hook ((counsel-projectile-mode . ivy-rich-mode) ; MUST after `counsel-projectile'
           (ivy-rich-mode . ivy-rich-project-root-cache-mode)
           (ivy-rich-mode . (lambda ()
                              "Use abbreviate in `ivy-rich-mode'."
                              (setq ivy-virtual-abbreviate
                                    (or (and ivy-rich-mode 'abbreviate) 'name)))))
    :init
    ;; For better performance
    (setq ivy-rich-parse-remote-buffer nil))

  ;; flx is used as the fuzzy-matching indexer backend for ivy.
  (use-package flx
    :after ivy))



(provide 'ivy-config)
;;; ivy-config.el ends here
