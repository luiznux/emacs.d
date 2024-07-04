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
  :diminish
  :functions my-elisp-flymake-byte-compile
  :hook (prog-mode . flymake-mode)
  :bind (:map flymake-mode-map
         ("C-c ! l" . flymake-show-buffer-diagnostics)
         ("C-c ! n" . flymake-goto-next-error)
         ("C-c ! p" . flymake-goto-prev-error)
         ("C-c ! c" . flymake-start))
  :init (setq flymake-no-changes-timeout nil
              flymake-fringe-indicator-position 'right-fringe)
  :config
  ;; Check elisp with `load-path'
  (defun my-elisp-flymake-byte-compile (fn &rest args)
    "Wrapper for `elisp-flymake-byte-compile'."
    (let ((elisp-flymake-byte-compile-load-path
           (append elisp-flymake-byte-compile-load-path load-path)))
      (apply fn args)))
  (advice-add 'elisp-flymake-byte-compile :around #'my-elisp-flymake-byte-compile))

(use-package flymake-diagnostic-at-point
  :commands (childframe-workable-p
             flymake-diagnostic-at-point-mode
             flymake-diagnostic-at-point-display-posframe
             posframe-show posframe-hide)
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

;; Use flycheck checkers with flymake, to extend its coverage
(use-package flymake-flycheck
  :commands flymake-flycheck-all-chained-diagnostic-functions
  :init
  (with-eval-after-load 'flycheck
    (setq-default flycheck-disabled-checkers
                  (append (default-value 'flycheck-disabled-checkers)
                          '(emacs-lisp emacs-lisp-checkdoc emacs-lisp-package))))

  (defun sanityinc/enable-flymake-flycheck ()
    (setq-local flymake-diagnostic-functions
                (append flymake-diagnostic-functions
                        (flymake-flycheck-all-chained-diagnostic-functions))))
  (add-hook 'flymake-mode-hook 'sanityinc/enable-flymake-flycheck))

(use-package flyspell
  :ensure nil
  :if (executable-find "aspell")
  :commands ispell-init-process
  :hook
  ((git-commit-mode . flyspell-mode)
   (flyspell-mode   . (lambda ()
                        (dolist (key '("C-;" "C-," "C-."))
                          (unbind-key key flyspell-mode-map)))))
  :custom-face
  (flyspell-incorrect ((t (:underline (:color "#f1fa8c" :style wave)))))
  (flyspell-duplicate ((t (:underline (:color "#50fa7b" :style wave)))))
  :init
  (setq flyspell-issue-message-flag  nil
        ispell-program-name          "aspell"
        ispell-extra-args            '("--sug-mode=ultra")
        ispell-dictionary            "english")
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
  :commands flyspell-correct-popup
  :after flyspell-correct
  :bind(:map popup-menu-keymap
        ("M-j" . popup-next)
        ("M-k" . popup-previous))
  :init
  (setq flyspell-correct-interface #'flyspell-correct-popup))


(provide 'init-flymake)
;;; init-flymake.el ends here
