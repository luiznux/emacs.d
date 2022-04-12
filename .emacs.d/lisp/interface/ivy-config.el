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

(use-package counsel
  :defines wiper-action-recenter
  :commands (ivy-immediate-done
             ivy-alt-done
             ivy-set-occur
             ivy-next-line
             ivy-previous-line)
  :preface
  (defun config-ivy-with-empty-ivy-extra-directories (f &rest args)
    (let ((ivy-extra-directories nil))
      (apply f args)))

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
          ("C-c c t" . counsel-load-theme)
          ("C-c c u" . counsel-unicode-char)
          ("C-c c w" . counsel-colors-web)
          ("C-c c v" . counsel-set-variable)
          ("C-c c z" . counsel-fzf))

         ;; Evil mapping for ivy-minibuffer
         :map ivy-minibuffer-map
         (("M-j" . 'ivy-next-line)
          ("M-k" . 'ivy-previous-line)
          ("M-d" . ivy-scroll-up-command)
          ("M-u" . ivy-scroll-down-command)
          ("M-<" . ivy-beginning-of-buffer)
          ("M->" . ivy-end-of-buffer)
          ("C-w" . ivy-yank-word)))

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
                                         "\\`\\*.+-posframe-buffer\\*")
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
                   (beginning-of-line)))))))

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
      (ivy-quit-and-run
        (rg-dwim default-directory)))

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
    (bind-key "<C-return>" #'my-counsel-find-file-toggle-fzf counsel-find-file-map)))

;; flx is used as the fuzzy-matching indexer backend for ivy.
(use-package flx
  :after ivy)

;; Enhance M-x
(use-package amx
  :init (setq amx-history-length 20))

(use-package ivy-xref
  :init
  (when (boundp 'xref-show-definitions-function)
    (setq xref-show-definitions-function #'ivy-xref-show-defs))
  (setq xref-show-xrefs-function #'ivy-xref-show-xrefs))

(use-package prescient
  :commands prescient-persist-mode
  :init (prescient-persist-mode 1))

(use-package ivy-prescient
  :commands ivy-prescient-re-builder
  :custom-face
  (ivy-minibuffer-match-face-1 ((t (:foreground ,(face-foreground 'font-lock-doc-face nil t)))))
  :init
  (defun ivy-prescient-non-fuzzy (str)
    "Generate an Ivy-formatted non-fuzzy regexp list for the given STR.
This is for use in `ivy-re-builders-alist'."
    (let ((prescient-filter-method '(literal regexp)))
      (ivy-prescient-re-builder str)))

  (setq ivy-prescient-retain-classic-highlighting t
        ivy-re-builders-alist
        '((counsel-ag . ivy-prescient-non-fuzzy)
          (counsel-rg . ivy-prescient-non-fuzzy)
          (counsel-pt . ivy-prescient-non-fuzzy)
          (counsel-grep . ivy-prescient-non-fuzzy)
          (counsel-fzf . ivy-prescient-non-fuzzy)
          (counsel-imenu . ivy-prescient-non-fuzzy)
          (counsel-yank-pop . ivy-prescient-non-fuzzy)
          (swiper . ivy-prescient-non-fuzzy)
          (swiper-isearch . ivy-prescient-non-fuzzy)
          (swiper-all . ivy-prescient-non-fuzzy)
          (lsp-ivy-workspace-symbol . ivy-prescient-non-fuzzy)
          (lsp-ivy-global-workspace-symbol . ivy-prescient-non-fuzzy)
          (insert-char . ivy-prescient-non-fuzzy)
          (counsel-unicode-char . ivy-prescient-non-fuzzy)
          (t . ivy-prescient-re-builder))
        ivy-prescient-sort-commands
        '(:not swiper swiper-isearch ivy-switch-buffer
          lsp-ivy-workspace-symbol ivy-resume ivy--restore-session
          counsel-grep counsel-git-grep counsel-rg counsel-ag
          counsel-ack counsel-fzf counsel-pt counsel-imenu
          counsel-org-capture counsel-outline counsel-org-goto
          counsel-load-theme counsel-yank-pop
          counsel-recentf counsel-buffer-or-recentf))

  (ivy-prescient-mode 1))

;; Ivy integration for Projectile
(use-package counsel-projectile
  :hook (counsel-mode . counsel-projectile-mode)
  :init (setq counsel-projectile-grep-initial-input '(ivy-thing-at-point)))

;; Integrate yasnippet
(use-package ivy-yasnippet
  :bind ("C-c C-y" . ivy-yasnippet))

(use-package all-the-icons-ivy
  :init (add-hook 'after-init-hook 'all-the-icons-ivy-setup)
  :config
  (setq all-the-icons-ivy-file-commands
        '(counsel-find-file counsel-file-jump counsel-recentf counsel-projectile-find-file counsel-projectile-find-dir)))

;; Better experience with icons
;; Enable it before`ivy-rich-mode' for better performance
(use-package all-the-icons-ivy-rich
  :hook (ivy-mode . all-the-icons-ivy-rich-mode)
  :config
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

;;(use-package ivy-rich
;;  :commands ivy-format-function-line
;;  :custom
;;  (sivy-rich-path-style 'abbrev)
;;  (ivy-rich-switch-buffer-align-virtual-buffer nil)
;;  (ivy-rich-project-root-cache-mode t)
;;  :init
;;  (setq ivy-rich-parse-remote-buffer nil)
;;  :config
;;  (defun ivy-rich-switch-buffer-icon (candidate)
;;    (with-current-buffer
;;    (get-buffer candidate)
;;                                                        (let ((icon (all-the-icons-icon-for-mode major-mode)))
;;        (if (symbolp icon)
;;    (all-the-icons-icon-for-mode 'fundamental-mode)
;;                                                            icon))))
;;
;;                                                    (setq ivy-rich-display-transformers-list
;;        '(ivy-switch-buffer
;;          (:columns
;;           ((ivy-rich-switch-buffer-icon (:width 2))
;;            (ivy-rich-candidate (:width 30))
;;            (ivy-rich-switch-buffer-size (:width 7))
;;            (ivy-rich-switch-buffer-indicators (:width 4 :face error :align right))
;;            (ivy-rich-switch-buffer-major-mode (:width 12 :face warning))
;;            (ivy-rich-switch-buffer-project (:width 15 :face success))
;;            (ivy-rich-switch-buffer-path (:width (lambda (x) (ivy-rich-switch-buffer-shorten-path x (ivy-rich-minibuffer-width 0.3))))))
;;           :predicate
;;           (lambda (cand) (get-buffer cand)))))
;;
;;                                                    (defcustom ivy-filthy-rich-padding ?\s
;;                                                      "The padding of `ivy-filthy-rich-delimiter'.
;;It is used when there are extra space.
;;The length of the pad has to be one.
;;If not, `ivy-filth-rich' will fallback to using space to pad.
;;Currently only support character, because `make-string' only accept that."
;;                                                      :type 'character
;;                                                      :group 'ivy-filthy-rich)
;;
;;                                                    (defcustom ivy-filthy-rich-pad-side 'right
;;                                                      "The side which padding is pad to.
;;Either left or right.
;;Left means align right,
;;right means align left."
;;                                                      :type 'symbol
;;                                                      :group 'ivy-filthy-rich)
;;
;;                                                    (defcustom ivy-filthy-rich-max-length 0
;;                                                      "The max length of one entry (one line on ivy buffer).
;;If it is zero, the max-length is (1- (frame-width))"
;;                                                      :type 'number
;;                                                      :group 'ivy-filthy-rich)
;;                                                    (setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line)
;;                                                    (ivy-rich-mode))




(provide 'ivy-config)
;;; ivy-config.el ends here
