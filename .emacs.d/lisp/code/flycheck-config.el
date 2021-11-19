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
  ((yaml-mode markdown-mode git-commit-mode) . flyspell-mode)
  (prog-mode . flyspell-prog-mode)
  (before-save-hook . flyspell-buffer)
  ;; (flyspell-mode . (lambda ()
  ;;                    (dolist (key '("C-;" "C-," "C-."))
  ;;                      (unbind-key key flyspell-mode-map))))
  :custom
  (flyspell-issue-message-flag nil)
  (ispell-program-name "aspell")
  (ispell-extra-args '("--sug-mode=ultra" "--lang=en_US" "--run-together"))
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
  (advice-add #'ispell-init-process :around #'message-off-advice))

(use-package flyspell-correct-ivy
  :bind ("C-M-:" . flyspell-correct-at-point)
  :config
  (when (eq system-type 'darwin)
    (progn
      (global-set-key (kbd "C-M-;") 'flyspell-correct-at-point)))
  (setq flyspell-correct-interface #'flyspell-correct-ivy))

(use-package flyspell-popup
  :init
                                        ;  (define-key flyspell-mode-map (kbd "C-;") #'flyspell-popup-correct)
  (add-hook 'flyspell-mode-hook #'flyspell-popup-auto-correct-mode))


(provide 'flycheck-config)
;;; flycheck-config.el ends here
