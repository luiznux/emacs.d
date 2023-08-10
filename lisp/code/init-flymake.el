;;; init-flymake.el --- Initialize `flymake-mode' and `flyspell-mode' configurations   -*- lexical-binding: t -*-
;;
;; Author: Luiz Tagliaferro <luiz@luiznux.com>
;; URL: https://luiznux.com
;; This file is free software :)
;;
;;; Commentary:
;;
;;
;; ███████╗██╗     ██╗   ██╗
;; ██╔════╝██║     ╚██╗ ██╔╝
;; █████╗  ██║      ╚████╔╝
;; ██╔══╝  ██║       ╚██╔╝
;; ██║     ███████╗   ██║
;; ╚═╝     ╚══════╝   ╚═╝
;;
;;; Code:

(use-package flymake
  :hook (prog-mode . flymake-mode)
  :init (setq flymake-fringe-indicator-position 'right-fringe)
  :config (setq elisp-flymake-byte-compile-load-path
                (append elisp-flymake-byte-compile-load-path load-path)))

(use-package flymake-diagnostic-at-point
  :commands flymake-diagnostic-at-point-mode
  :hook (flymake-mode . flymake-diagnostic-at-point-mode)
  :config
  (when (and (childframe-workable-p)
             (require 'posframe nil t))
    (defvar flymake-posframe-buffer " *flymake-posframe-buffer*"
      "Name of the flymake posframe buffer.")
    (defun flymake-diagnostic-at-point-display-posframe (text)
      "Display the flymake diagnostic TEXT inside a child frame."
      (posframe-show
       flymake-posframe-buffer
       :string (propertize
                (concat flymake-diagnostic-at-point-error-prefix text)
                'face (if-let ((type (get-char-property (point) 'flymake-diagnostic)))
                          (pcase (flymake--diag-type type)
                            (:error 'error)
                            (:warning 'warning)
                            (:note 'success)
                            (_ 'default))
                        'default))
	   :left-fringe 4
	   :right-fringe 4
       :max-width (round (* (frame-width) 0.62))
       :max-height (round (* (frame-height) 0.62))
       :internal-border-width 1
       :internal-border-color (face-background 'posframe-border nil t)
       :background-color (face-background 'tooltip nil t))
      (unwind-protect
          (push (read-event) unread-command-events)
        (progn
          (posframe-hide flymake-posframe-buffer)
          (other-frame 0))))
    (setq flymake-diagnostic-at-point-display-diagnostic-function
          #'flymake-diagnostic-at-point-display-posframe)))


(use-package flyspell
  :ensure nil
  :if (executable-find "aspell")
  :commands ispell-init-process
  :hook
  (flyspell-mode . (lambda ()
                     (dolist (key '("C-;" "C-," "C-."))
                       (unbind-key key flyspell-mode-map))))
  :custom-face
  (flyspell-incorrect ((t (:underline (:color "#f1fa8c" :style wave)))))
  (flyspell-duplicate ((t (:underline (:color "#50fa7b" :style wave)))))
  :init
  (setq flyspell-issue-message-flag  nil
        ispell-program-name          "aspell"
        ispell-extra-args            '("--sug-mode=ultra")
        ispell-dictionary            "english")
  (add-hook 'git-commit-setup-hook 'git-commit-turn-on-flyspell)
  :config
  (defun spell-buffer-pt-BR ()
    "Spell check in portuguese."
    (interactive)
    (ispell-change-dictionary "brasileiro")
    (flyspell-buffer))

  (defun spell-buffer-en-US ()
    "Spell check in english."
    (interactive)
    (ispell-change-dictionary "english")
    (flyspell-buffer)))

(use-package flyspell-correct
  :after flyspell
  :bind ("C-," . flyspell-correct-at-point))

(use-package flyspell-correct-popup
  :after flyspell-correct
  :bind(:map popup-menu-keymap
        ("M-j" . popup-next)
        ("M-k" . popup-previous))
  :init
  (setq flyspell-correct-interface #'flyspell-correct-popup))


(provide 'init-flymake)
;;; init-flymake.el ends here
