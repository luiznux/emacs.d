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
  ;; suppress warning
  (remove-hook 'flymake-diagnostic-functions 'flymake-proc-legacy-flymake)

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

;; spell-checker
;; requires libenchant, see https://github.com/minad/jinx
(use-package jinx
  :hook ((outline-mode git-commit-mode) . jinx-mode)
  :bind ("C-," . jinx-correct)
  :init
  (setq jinx-languages "en_US pt_BR"))


(provide 'init-flymake)
;;; init-flymake.el ends here
