;;; terminal-config.el --- Packages for running a terminal emulator inside emacs  -*- lexical-binding:  t no-byte-compile: t -*-
;;
;; Author: Luiz Tagliaferro <luiz@luiznux.com>
;; URL: https://luiznux.com
;; This file is free software :)
;;
;;; Commentary:
;;
;; Using Vterm on Emacs is one of the best way.
;;
;; ████████╗███████╗██████╗ ███╗   ███╗██╗███╗   ██╗ █████╗ ██╗
;; ╚══██╔══╝██╔════╝██╔══██╗████╗ ████║██║████╗  ██║██╔══██╗██║
;;    ██║   █████╗  ██████╔╝██╔████╔██║██║██╔██╗ ██║███████║██║
;;    ██║   ██╔══╝  ██╔══██╗██║╚██╔╝██║██║██║╚██╗██║██╔══██║██║
;;    ██║   ███████╗██║  ██║██║ ╚═╝ ██║██║██║ ╚████║██║  ██║███████╗
;;    ╚═╝   ╚══════╝╚═╝  ╚═╝╚═╝     ╚═╝╚═╝╚═╝  ╚═══╝╚═╝  ╚═╝╚══════╝
;;
;;; Code:

;; Better terminal emulator
;; @see https://github.com/akermu/emacs-libvterm#installation
(when (and module-file-suffix           ; dynamic module
           (executable-find "cmake")
           (executable-find "libtool")
           (executable-find "make"))
  (use-package vterm
    :bind (:map vterm-mode-map
           ([f9] . (lambda ()
                     (interactive)
                     (and (fboundp 'shell-pop-toggle)
                          (shell-pop-toggle)))))
    :init (setq vterm-always-compile-module t)))

;; Shell Pop: leverage `popper'
(with-no-warnings
  (defvar shell-pop--frame nil)
  (defun shell-pop-posframe-hidehandler (_)
    "Hidehandler used by `shell-pop-posframe-toggle'."
    (not (eq (selected-frame) posframe--frame)))

  (defun shell-pop--shell (&optional arg)
    "Get shell buffer."
    (cond ((fboundp 'vterm) (vterm arg))
          (sys/win32p (eshell arg))
          (t (shell))))

  (defun shell-pop-posframe-toggle ()
    "Toggle shell in child frame."
    (interactive)
    (let* ((buffer (shell-pop--shell))
           (window (get-buffer-window buffer)))
      ;; Hide window: for `popper'
      (when (window-live-p window)
        (delete-window window))

      (if (and (frame-live-p shell-pop--frame)
               (frame-visible-p shell-pop--frame))
          (progn
            ;; Hide child frame and refocus in parent frame
            (make-frame-invisible shell-pop--frame)
            (select-frame-set-input-focus (frame-parent shell-pop--frame))
            (setq shell-pop--frame nil))
        (let ((width  (max 80 (floor (* (frame-width) 0.5))))
              (height (floor (* (frame-height) 0.5))))
          ;; Shell pop in child frame
          (setq shell-pop--frame
                (posframe-show
                 buffer
                 :poshandler #'posframe-poshandler-frame-center
                 :hidehandler #'shell-pop-posframe-hidehandler
                 :left-fringe 8
                 :right-fringe 8
                 :width width
                 :height height
                 :min-width width
                 :min-height height
                 :internal-border-width 3
                 :internal-border-color (face-foreground 'font-lock-comment-face nil t)
                 :background-color (face-background 'tooltip nil t)
                 :override-parameters '((cursor-type . t))
                 :accept-focus t))

          ;; Focus in child frame
          (select-frame-set-input-focus shell-pop--frame)

          (with-current-buffer buffer
            (setq-local cursor-type 'box) ; blink cursor
            (goto-char (point-max))
            (when (fboundp 'vterm-reset-cursor-point)
              (vterm-reset-cursor-point)))))))
  (bind-key "C-`" #'shell-pop-posframe-toggle)

  (defvar shell-pop--window nil)
  (defun shell-pop-toggle ()
    "Toggle shell."
    (interactive)
    (unless (and (frame-live-p shell-pop--frame)
                 (frame-visible-p shell-pop--frame))
      (if (window-live-p shell-pop--window)
          (progn
            (delete-window shell-pop--window)
            (setq shell-pop--window nil))
        (setq shell-pop--window
              (get-buffer-window (shell-pop--shell))))))
  (bind-key [f9] #'shell-pop-toggle))


(provide 'terminal-config)
;;; terminal-config.el ends here
