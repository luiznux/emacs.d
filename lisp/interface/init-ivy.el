;;; init-ivy.el --- Initialize `ivy' configurations   -*- lexical-binding: t -*-
;;
;; Author: Luiz Tagliaferro <luiz@luiznux.com>
;; URL: https://luiznux.com
;; This file is free software :)
;;
;;; Commentary:
;;
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
  (ivy-minibuffer-match-face-1 ((t (:foreground "dimgray" :distant-foreground unspecified :background unspecified))))
  (ivy-minibuffer-match-face-2 ((t (:distant-foreground unspecified :background unspecified))))
  (ivy-minibuffer-match-face-3 ((t (:distant-foreground unspecified :background unspecified))))
  (ivy-minibuffer-match-face-4 ((t (:distant-foreground unspecified :background unspecified))))

  :bind (("C-s"   . swiper-isearch)
         ("C-S-s" . swiper-all)

         ("C-c C-r" . ivy-resume)
         ("C-c v p" . ivy-push-view)
         ("C-c v o" . ivy-pop-view)
         ("C-c v ." . ivy-switch-view)

         :map counsel-mode-map
         ([remap swiper]             . counsel-grep-or-swiper)
         ([remap swiper-backward]    . counsel-grep-or-swiper-backward)
         ([remap dired]              . counsel-dired)
         ([remap set-variable]       . counsel-set-variable)
         ([remap insert-char]        . counsel-unicode-char)
         ([remap recentf-open-files] . counsel-recentf)
         ([remap org-capture]        . counsel-org-capture)

         ("C-x j" . counsel-mark-ring)
         ("C-h F" . counsel-faces)

         ("C-c B" . counsel-bookmarked-directory)
         ("C-c O" . counsel-find-file-extern)
         ("C-c P" . counsel-package)
         ("C-c R" . counsel-list-processes)
         ("C-c U" . counsel-unicode-char)

         ("C-c f" . counsel-projectile-rg)
         ("C-c g" . counsel-git)
         ("C-c h" . counsel-command-history)
         ("C-c i" . counsel-imenu)
         ("C-c j" . counsel-git-grep)
         ("C-c l" . counsel-git-log)
         ("C-c o" . counsel-outline)
         ("C-c r" . counsel-recentf)
         ("C-c v" . counsel-switch-buffer-other-window)
         ("C-c y" . counsel-yank-pop)
         ("C-c z" . counsel-fzf)

         ("C-c c B" . counsel-bookmarked-directory)
         ("C-c c F" . counsel-faces)
         ("C-c c L" . counsel-load-library)
         ("C-c c K" . counsel-ace-link)
         ("C-c c O" . counsel-find-file-extern)
         ("C-c c P" . counsel-package)
         ("C-c c R" . counsel-list-processes)

         ("C-c c a" . counsel-apropos)
         ("C-c c e" . counsel-colors-emacs)
         ("C-c c f" . counsel-find-library)
         ("C-c c g" . counsel-grep)
         ("C-c c h" . counsel-command-history)
         ("C-c c i" . counsel-git)
         ("C-c c j" . counsel-git-grep)
         ("C-c c l" . counsel-git-log)
         ("C-c c m" . counsel-minibuffer-history)
         ("C-c c o" . counsel-outline)

         ("C-c c p" . counsel-pt)
         ("C-c c r" . counsel-rg)
         ("C-c c s" . counsel-ag)
         ("C-c c z" . counsel-fzf)
         ("C-c c t" . counsel-load-theme)
         ("C-c c u" . counsel-unicode-char)
         ("C-c c w" . counsel-colors-web)
         ("C-c c v" . counsel-set-variable)

         ;; Evil mapping for ivy-minibuffer
         :map ivy-minibuffer-map
         ("M-j" . 'ivy-next-line)
         ("M-k" . 'ivy-previous-line)
         ("M-d" . ivy-scroll-up-command)
         ("M-u" . ivy-scroll-down-command)
         ("M-<" . ivy-beginning-of-buffer)
         ("M->" . ivy-end-of-buffer)
         ("C-w" . ivy-yank-word)
         ;; Use C-j for immediate termination with the current value, and RET
         ;; for continuing completion for that directory. This is the ido
         ;; behaviour.
         ("C-j" . ivy-immediate-done)
         ("RET" . ivy-alt-done)

         :map counsel-find-file-map
         ("C-h" . counsel-up-directory)

         :map swiper-map
         ("M-s" . swiper-isearch-toggle)
         ("M-%" . swiper-query-replace)

         :map isearch-mode-map
         ("M-s" . swiper-isearch-toggle))

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

  ;; Set use `ivy--regex-fuzzy' for some counsel modes, otherwise use
  ;; `ivy--regex-plus'
  (setq ivy-re-builders-alist
        '((counsel-M-x . ivy--regex-fuzzy)
          (counsel-find-file . ivy--regex-fuzzy)
          (t . ivy--regex-plus)))

  ;; Set minibuffer height for different commands
  (setq ivy-height-alist '((counsel-evil-registers . 5)
                           (counsel-yank-pop       . 8)
                           (counsel-git-log        . 4)
                           (swiper                 . 13)
                           (counsel-projectile-ag  . 13)
                           (counsel-projectile-rg  . 13)))

  (setq swiper-action-recenter t)

  (setq counsel-find-file-at-point     t
        counsel-preselect-current-file t
        counsel-yank-pop-separator     "\n────────\n")
  (add-hook 'counsel-grep-post-action-hook #'recenter)

  ;; Use the faster search tools
  (cond
   ((executable-find "ugrep")
    (setq counsel-grep-base-command "ugrep --color=never -n -e '%s' '%s'"))
   ((executable-find "rg")
    (setq counsel-grep-base-command "rg -S --no-heading --line-number --color never '%s' '%s'")))

  (when (executable-find "fd")
    (setq counsel-fzf-cmd
          "fd --type f --hidden --follow --exclude .git --color never '%s'"))

  ;; Be compatible with `gls'
  (when (and sys/macp (executable-find "gls"))
    (setq counsel-find-file-occur-use-find nil
          counsel-find-file-occur-cmd
          "gls -a | grep -i -E '%s' | tr '\\n' '\\0' | xargs -0 gls -d --group-directories-first"))

  :config
  ;; ignore Dotfiles and Lockfiles files
  (setq counsel-find-file-ignore-regexp
        "\\(?:^[#.]\\)\\|\\(?:[#~]$\\)\\|\\(?:^Icon?\\)")

  ;; Do not show extra directories when finding files.
  (setq ivy-extra-directories '("."))
  (advice-add #'counsel-find-file :around #'config-ivy-with-empty-ivy-extra-directories)

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
    (defconst my-ivy-fly-commands
      '(query-replace-regexp
        flush-lines keep-lines ivy-read
        swiper swiper-backward swiper-all
        swiper-isearch swiper-isearch-backward
        lsp-ivy-workspace-symbol lsp-ivy-global-workspace-symbol
        counsel-grep-or-swiper counsel-grep-or-swiper-backward
        counsel-grep counsel-ack counsel-ag counsel-rg counsel-pt))

    (defconst my-ivy-fly-back-commands
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
      (ivy-quit-and-run (swiper ivy-text)))

    (defun my-ivy-switch-to-swiper-isearch (&rest _)
      "Switch to `swiper-isearch' with the current input."
      (ivy-quit-and-run (swiper-isearch ivy-text)))

    (defun my-ivy-switch-to-swiper-all (&rest _)
      "Switch to `swiper-all' with the current input."
      (ivy-quit-and-run (swiper-all ivy-text)))

    (defun my-ivy-switch-to-rg-dwim (&rest _)
      "Switch to `rg-dwim' with the current input."
      (interactive)
      (ivy-exit-with-action #'rg-dwim))

    (defun my-ivy-switch-to-counsel-rg (&rest _)
      "Switch to `counsel-rg' with the current input."
      (ivy-quit-and-run (counsel-rg ivy-text default-directory)))

    (defun my-ivy-switch-to-counsel-git-grep (&rest _)
      "Switch to `counsel-git-grep' with the current input."
      (ivy-quit-and-run (counsel-git-grep ivy-text default-directory)))

    (defun my-ivy-switch-to-counsel-find-file (&rest _)
      "Switch to `counsel-find-file' with the current input."
      (ivy-quit-and-run (counsel-find-file ivy-text)))

    (defun my-ivy-switch-to-counsel-fzf (&rest _)
      "Switch to `counsel-fzf' with the current input."
      (ivy-quit-and-run (counsel-fzf ivy-text default-directory)))

    (defun my-ivy-switch-to-counsel-git (&rest _)
      "Switch to `counsel-git' with the current input."
      (ivy-quit-and-run (counsel-git ivy-text)))

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
      (if (memq (ivy-state-caller ivy-last) '(swiper swiper-isearch))
          (my-ivy-switch-to-counsel-rg)
        (my-ivy-switch-to-swiper-isearch)))
    (bind-key "<C-return>" #'my-swiper-toggle-counsel-rg swiper-map)
    (bind-key "<C-return>" #'my-swiper-toggle-counsel-rg counsel-ag-map)

    (with-eval-after-load 'rg
      (bind-key "<M-return>" #'my-ivy-switch-to-rg-dwim swiper-map)
      (bind-key "<M-return>" #'my-ivy-switch-to-rg-dwim counsel-ag-map))

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

  ;; Avy integration
  (use-package ivy-avy
    :bind (:map ivy-minibuffer-map
           ("C-'" . ivy-avy)))

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
           ("C-c c T" . counsel-tramp))))

;; Use Ivy to open recent directories
(use-package ivy-dired-history
  :demand t
  :after dired
  :defines (savehist-additional-variables desktop-globals-to-save)
  :bind (:map dired-mode-map
         ("," . dired))
  :init
  (with-eval-after-load 'savehist
    (add-to-list 'savehist-additional-variables 'ivy-dired-history-variable))
  (with-eval-after-load 'desktop
    (add-to-list 'desktop-globals-to-save 'ivy-dired-history-variable)))

;; `projectile' integration
(use-package counsel-projectile
  :hook (counsel-mode . counsel-projectile-mode)
  :init
  (setq counsel-projectile-grep-initial-input '(ivy-thing-at-point))
  (when (executable-find "ugrep")
    (setq counsel-projectile-grep-base-command "ugrep --color=never -rnEI %s")))

;; Better experience with icons
;; Enable it before`ivy-rich-mode' for better performance
(use-package all-the-icons-ivy-rich
  :hook (ivy-mode . all-the-icons-ivy-rich-mode)
  :init (setq all-the-icons-ivy-rich-icon emacs-icon)
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
  :after ivy)


(provide 'init-ivy)
;;; init-ivy.el ends here