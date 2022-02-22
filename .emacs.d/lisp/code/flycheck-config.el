;;; flycheck-config.el --- Package configuration for `flycheck-mode' and `flyspell-mode'  -*- lexical-binding: t -*-
;;
;; Author: Luiz Tagliaferro <luiz@luiznux.com>
;; URL: https://luiznux.com
;; This file is free software :)
;;
;;; Commentary:
;;
;; ███████╗██╗     ██╗   ██╗
;; ██╔════╝██║     ╚██╗ ██╔╝
;; █████╗  ██║      ╚████╔╝
;; ██╔══╝  ██║       ╚██╔╝
;; ██║     ███████╗   ██║
;; ╚═╝     ╚══════╝   ╚═╝
;;
;;; Code:

(use-package flycheck
  :diminish
  :defines flycheck-posframe-border-width
  :commands flycheck-redefine-standard-error-levels
  :hook (after-init . global-flycheck-mode)
  :init (setq flycheck-global-modes
              '(not text-mode outline-mode fundamental-mode lisp-interaction-mode
                    org-mode diff-mode shell-mode eshell-mode term-mode vterm-mode)
              flycheck-emacs-lisp-load-path 'inherit
              flycheck-indication-mode 'right-fringe
              ;; Only check while saving and opening files
              flycheck-check-syntax-automatically '(save mode-enabled))
  :config
  ;; Prettify indication styles
  (when (fboundp 'define-fringe-bitmap)
    (define-fringe-bitmap 'flycheck-fringe-bitmap-arrow
      [16 48 112 240 112 48 16] nil nil 'center))
  (flycheck-redefine-standard-error-levels "⏵" 'flycheck-fringe-bitmap-arrow)

  ;; Display Flycheck errors
  (use-package flycheck-posframe
    :custom-face
    (flycheck-posframe-face ((t (:foreground ,(face-foreground 'success)))))
    (flycheck-posframe-info-face ((t (:foreground ,(face-foreground 'success)))))
    (flycheck-posframe-background-face ((t (:inherit tooltip))))
    (flycheck-posframe-border-face ((t (:inherit font-lock-comment-face))))
    :hook (flycheck-mode . flycheck-posframe-mode)
    :init
    (setq flycheck-posframe-border-width 1)
    (add-hook 'flycheck-posframe-inhibit-functions
              (lambda (&rest _) (bound-and-true-p company-backend)))
    :config
    (with-no-warnings
      ;; FIXME: Add paddings to the child frame.
      ;; @see https://github.com/alexmurray/flycheck-posframe/issues/28
      (defun my-flycheck-posframe-show-posframe (errors)
        "Display ERRORS, using posframe.el library."
        (posframe-hide flycheck-posframe-buffer)
        (when (and errors
                   (not (run-hook-with-args-until-success 'flycheck-posframe-inhibit-functions)))
          (let ((poshandler (intern (format "posframe-poshandler-%s" flycheck-posframe-position)))
                (str (flycheck-posframe-format-errors errors)))
            (unless (functionp poshandler)
              (setq poshandler nil))
            (flycheck-posframe-check-position)
            (posframe-show
             flycheck-posframe-buffer
             :string (concat (propertize "\n" 'face '(:height 0.3))
                             str
                             (propertize "\n\n" 'face '(:height 0.3)))
             :background-color (face-background 'flycheck-posframe-background-face nil t)
             :position (point)
             :left-fringe 8
             :right-fringe 8
             :max-width (round (* (frame-width) 0.62))
             :max-height (round (* (frame-height) 0.62))
             :internal-border-width flycheck-posframe-border-width
             :internal-border-color (face-foreground 'flycheck-posframe-border-face nil t)
             :poshandler poshandler
             :hidehandler #'flycheck-posframe-hidehandler))))
      (advice-add #'flycheck-posframe-show-posframe :override #'my-flycheck-posframe-show-posframe)))
  (use-package flycheck-popup-tip
    :hook (flycheck-mode . flycheck-popup-tip-mode)))

(use-package flyspell
  :ensure nil
  :if (executable-find "aspell")
  :commands ispell-init-process
  :hook
  ((markdown-mode git-commit-mode magit-mode-hook) . flyspell-mode)
  (before-save-hook . flyspell-buffer)
  (flyspell-mode . (lambda ()
                     (dolist (key '("C-;" "C-," "C-."))
                       (unbind-key key flyspell-mode-map))))
  :custom
  (flyspell-issue-message-flag  nil)
  (ispell-program-name "aspell")
  (ispell-dictionary  "en_US")
  :custom-face
  (flyspell-incorrect ((t (:underline (:color "#f1fa8c" :style wave)))))
  (flyspell-duplicate ((t (:underline (:color "#50fa7b" :style wave)))))
  :preface
  (defun message-off-advice (oldfun &rest args)
    "Quiet down messages in adviced OLDFUN."
    (let ((message-off (make-symbol "message-off")))
      (unwind-protect
          (progn
            (advice-add #'message :around #'ignore (list 'name message-off))
            (apply oldfun args))
        (advice-remove #'message message-off))))
  :config
  (defun bk/spell-buffer-pt-BR ()
    "Spell check in portuguese."
    (interactive)
    (ispell-change-dictionary "pt_BR")
    (flyspell-buffer))

  (defun bk/spell-buffer-en ()
    "Spell check in english."
    (interactive)
    (ispell-change-dictionary "en_US")
    (flyspell-buffer))

  (advice-add #'ispell-init-process :around #'message-off-advice))

(use-package flyspell-correct
  :after flyspell
  :bind ("C-c f" . flyspell-correct-at-point))

(use-package flyspell-correct-popup
  :after flyspell-correct
  :bind(:map popup-menu-keymap
        ("M-j" . popup-next)
        ("M-h" . popup-previous))
  :init
  (setq flyspell-correct-interface #'flyspell-correct-popup)
  (add-hook 'flyspell-mode-hook #'flyspell-popup-auto-correct-mode))

(use-package langtool
  :config
  (setq langtool-language-tool-jar
        "/home/wand/.emacs.d/var/LanguageTool-4.5/languagetool-commandline.jar"))


(provide 'flycheck-config)
;;; flycheck-config.el ends here
