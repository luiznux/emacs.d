;;; highlight-config.el --- visual beauties for Emacs  -*- lexical-binding: t -*-
;;
;; Author: Luiz Tagliaferro <luiz@luiznux.com>
;; URL: https://luiznux.com
;; This file is free software :)
;;
;;; Commentary:
;;
;; Highlight some text for better view  :ok-hand:
;;
;; ██╗  ██╗██╗ ██████╗ ██╗  ██╗██╗     ██╗ ██████╗ ██╗  ██╗████████╗
;; ██║  ██║██║██╔════╝ ██║  ██║██║     ██║██╔════╝ ██║  ██║╚══██╔══╝
;; ███████║██║██║  ███╗███████║██║     ██║██║  ███╗███████║   ██║
;; ██╔══██║██║██║   ██║██╔══██║██║     ██║██║   ██║██╔══██║   ██║
;; ██║  ██║██║╚██████╔╝██║  ██║███████╗██║╚██████╔╝██║  ██║   ██║
;; ╚═╝  ╚═╝╚═╝ ╚═════╝ ╚═╝  ╚═╝╚══════╝╚═╝ ╚═════╝ ╚═╝  ╚═╝   ╚═╝
;;
;;; Code:

;; Highlight the current line
(use-package hl-line
  :ensure nil
  :hook ((after-init . global-hl-line-mode)
         (( eshell-mode shell-mode term-mode vterm-mode) .
          (lambda () (setq-local global-hl-line-mode nil)))))

(use-package paren
  :ensure nil
  :hook (after-init . show-paren-mode)
  :init
  (setq show-paren-when-point-inside-paren t
        show-paren-when-point-in-periphery t
        show-paren-highlight-openparen     t
        show-paren-style                   'paren))

;; Highlight indentions
(use-package highlight-indent-guides
  :diminish
  :hook ((prog-mode yaml-mode) . highlight-indent-guides-mode)
  :init (setq highlight-indent-guides-method 'character
              highlight-indent-guides-responsive 'top
              highlight-indent-guides-suppress-auto-error t)
  :config
  (with-no-warnings
    ;; Disable in `macrostep' expanding
    (with-eval-after-load 'macrostep
      (advice-add #'macrostep-expand
                  :after (lambda (&rest _)
                           (when highlight-indent-guides-mode
                             (highlight-indent-guides-mode -1))))
      (advice-add #'macrostep-collapse
                  :after (lambda (&rest _)
                           (when (derived-mode-p 'prog-mode 'yaml-mode)
                             (highlight-indent-guides-mode 1)))))

    ;; Don't display indentations in `swiper'
    ;; https://github.com/DarthFennec/highlight-indent-guides/issues/40
    (with-eval-after-load 'ivy
      (defun my-ivy-cleanup-indentation (str)
        "Clean up indentation highlighting in ivy minibuffer."
        (let ((pos 0)
              (next 0)
              (limit (length str))
              (prop 'highlight-indent-guides-prop))
          (while (and pos next)
            (setq next (text-property-not-all pos limit prop nil str))
            (when next
              (setq pos (text-property-any next limit prop nil str))
              (ignore-errors
                (remove-text-properties next pos '(display nil face nil) str))))))
      (advice-add #'ivy-cleanup-string :after #'my-ivy-cleanup-indentation))))

(use-package symbol-overlay
  :diminish
  :functions (turn-off-symbol-overlay turn-on-symbol-overlay)
  :custom-face (symbol-overlay-default-face ((t (:inherit (region bold)))))
  :bind (("M-i"  . symbol-overlay-put)
         ("M-n"  . symbol-overlay-jump-next)
         ("M-p"  . symbol-overlay-jump-prev)
         ("M-N"  . symbol-overlay-switch-forward)
         ("M-P"  . symbol-overlay-switch-backward)
         ("M-C"  . symbol-overlay-remove-all)
         ([M-f3] . symbol-overlay-remove-all))
  :hook (((prog-mode yaml-mode) . symbol-overlay-mode)
         (iedit-mode            . turn-off-symbol-overlay)
         (iedit-mode-end        . turn-on-symbol-overlay))
  :init (setq symbol-overlay-idle-time 0.1)
  (with-eval-after-load 'all-the-icons
    (setq symbol-overlay-faces
          '((:inherit (all-the-icons-blue bold) :inverse-video t)
            (:inherit (all-the-icons-pink bold) :inverse-video t)
            (:inherit (all-the-icons-yellow bold) :inverse-video t)
            (:inherit (all-the-icons-purple bold) :inverse-video t)
            (:inherit (all-the-icons-red bold) :inverse-video t)
            (:inherit (all-the-icons-orange bold) :inverse-video t)
            (:inherit (all-the-icons-green bold) :inverse-video t)
            (:inherit (all-the-icons-cyan bold) :inverse-video t))))
  :config
  ;; Disable symbol highlighting while selecting
  (defun turn-off-symbol-overlay (&rest _)
    "Turn off symbol highlighting."
    (interactive)
    (symbol-overlay-mode -1))
  (advice-add #'set-mark :after #'turn-off-symbol-overlay)

  (defun turn-on-symbol-overlay (&rest _)
    "Turn on symbol highlighting."
    (interactive)
    (when (derived-mode-p 'prog-mode 'yaml-mode)
      (symbol-overlay-mode 1)))
  (advice-add #'deactivate-mark :after #'turn-on-symbol-overlay))

(use-package rainbow-mode
  :diminish
  :hook ((html-mode emacs-lisp-mode) . rainbow-mode)
  :config
  (with-no-warnings
    ;; HACK: Use overlay instead of text properties to override `hl-line' faces.
    ;; @see https://emacs.stackexchange.com/questions/36420
    (defun my-rainbow-colorize-match (color &optional match)
      (let* ((match (or match 0))
             (ov (make-overlay (match-beginning match) (match-end match))))
        (overlay-put ov 'ovrainbow t)
        (overlay-put ov 'face `((:foreground ,(if (> 0.5 (rainbow-x-color-luminance color))
                                                  "white" "black"))
                                (:background ,color)))))
    (advice-add #'rainbow-colorize-match :override #'my-rainbow-colorize-match)

    (defun my-rainbow-clear-overlays ()
      "Clear all rainbow overlays."
      (remove-overlays (point-min) (point-max) 'ovrainbow t))
    (advice-add #'rainbow-turn-off :after #'my-rainbow-clear-overlays)))

(use-package diff-hl
  :bind (:map diff-hl-command-map
         ("SPC" . diff-hl-mark-hunk))

  :hook ((after-init . global-diff-hl-mode)
         (dired-mode . diff-hl-dired-mode)
         ((after-load-theme server-after-make-frame) . my-set-diff-hl-faces))

  :init (setq diff-hl-draw-borders nil)

  :config
  ;; Highlight on-the-fly
  (diff-hl-flydiff-mode 1)

  ;; Set fringe style
  (setq-default fringes-outside-margins t)

  (defun my-set-diff-hl-faces ()
    "Set `diff-hl' faces."
    (custom-set-faces
     `(diff-hl-change ((t (:foreground ,(face-background 'highlight) :background nil))))
     '(diff-hl-insert ((t (:inherit diff-added :background nil))))
     '(diff-hl-delete ((t (:inherit diff-removed :background nil))))))

  (with-no-warnings
    (defun my-diff-hl-fringe-bmp-function (_type _pos)
      "Fringe bitmap function for use as `diff-hl-fringe-bmp-function'."
      (define-fringe-bitmap 'my-diff-hl-bmp
        (vector (if sys/macp #b11100000 #b11111100))
        1 8
        '(center t)))
    (setq diff-hl-fringe-bmp-function #'my-diff-hl-fringe-bmp-function)

    (unless (display-graphic-p)
      ;; Fall back to the display margin since the fringe is unavailable in tty
      (diff-hl-margin-mode 1)
      ;; Avoid restoring `diff-hl-margin-mode'
      (with-eval-after-load 'desktop
        (add-to-list 'desktop-minor-mode-table
                     '(diff-hl-margin-mode nil))))

    ;; Integration with magit
    (with-eval-after-load 'magit
      (add-hook 'magit-pre-refresh-hook #'diff-hl-magit-pre-refresh)
      (add-hook 'magit-post-refresh-hook #'diff-hl-magit-post-refresh))))

;; Highlight some operations
(use-package volatile-highlights
  :diminish
  :hook (after-init . volatile-highlights-mode)
  :config
  (with-no-warnings
    (when (fboundp 'pulse-momentary-highlight-region)
      (defun my-vhl-pulse (beg end &optional _buf face)
        "Pulse the changes."
        (pulse-momentary-highlight-region beg end face))
      (advice-add #'vhl/.make-hl :override #'my-vhl-pulse))))

;; Highlight TODO and similar keywords in comments and strings
(use-package hl-todo
  :custom-face
  (hl-todo ((t (:inherit default :height 0.9 :width condensed :weight bold :underline nil :inverse-video t))))
  :bind (:map hl-todo-mode-map
         ([C-f3]    . hl-todo-occur)
         ("C-c t p" . hl-todo-previous)
         ("C-c t n" . hl-todo-next)
         ("C-c t o" . hl-todo-occur)
         ("C-c t i" . hl-todo-insert))
  :hook (after-init . global-hl-todo-mode)
  :init (setq hl-todo-require-punctuation t
              hl-todo-highlight-punctuation ":")
  :config
  (dolist (keyword '("BUG" "DEFECT" "ISSUE"))
    (cl-pushnew `(,keyword . ,(face-foreground 'error)) hl-todo-keyword-faces))
  (dolist (keyword '("WORKAROUND" "HACK" "TRICK"))
    (cl-pushnew `(,keyword . ,(face-foreground 'warning)) hl-todo-keyword-faces)))

(use-package highlight-symbol
  :bind (:map prog-mode-map
         ("M-o h" . highlight-symbol)
         ("M-p" . highlight-symbol-prev)
         ("M-n" . highlight-symbol-next)))




(provide 'highlight-config)
;;; highlight-config.el ends here
