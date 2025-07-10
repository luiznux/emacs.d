;;; init-checker.el --- Initialize `flycheck-mode' and `flyspell-mode' configurations   -*- lexical-binding: t -*-
;;
;; Author: Luiz Tagliaferro <luiz@luiznux.com>
;; URL: https://luiznux.com
;; This file is free software :)
;;
;;; Commentary:
;;
;;  ██████╗██╗  ██╗███████╗ ██████╗██╗  ██╗
;; ██╔════╝██║  ██║██╔════╝██╔════╝██║ ██╔╝
;; ██║     ███████║█████╗  ██║     █████╔╝
;; ██║     ██╔══██║██╔══╝  ██║     ██╔═██╗
;; ╚██████╗██║  ██║███████╗╚██████╗██║  ██╗
;;  ╚═════╝╚═╝  ╚═╝╚══════╝ ╚═════╝╚═╝  ╚═╝
;;
;;; Code:
(require 'init-functions)

(use-package flycheck
  :diminish
  :autoload +syntax-init-popups-h
  :commands flycheck-list-errors flycheck-buffer
  :defines projectile-project-root lsp-ui-sideline-enable
  :hook (after-init . global-flycheck-mode)
  :init
  (setq flycheck-buffer-switch-check-intermediate-buffers t
        flycheck-emacs-lisp-load-path 'inherit
        flycheck-indication-mode 'left-fringe
        flycheck-idle-change-delay 1.0
        flycheck-display-errors-delay 0.5)
  :config
  ;; Rerunning checks on every newline is a mote excessive.
  (delq 'new-line flycheck-check-syntax-automatically)

  (eval '(setf (flycheck-checker-get 'emacs-lisp 'predicate)
               (lambda ()
                 (and
                  ;; Do not check buffers that ask not to be byte-compiled.
                  (not (bound-and-true-p no-byte-compile))
                  ;; Disable the emacs-lisp checker in non-project (likely
                  ;; untrusted) buffers to mitigate potential code execution
                  ;; vulnerability during macro expansion. See CVE-2024-53920.
                  (project-p))))
        t)

  (map! :map flycheck-error-list-mode-map
        :n "C-n"    #'flycheck-error-list-next-error
        :n "C-p"    #'flycheck-error-list-previous-error
        :n "j"      #'flycheck-error-list-next-error
        :n "k"      #'flycheck-error-list-previous-error
        :n "RET"    #'flycheck-error-list-goto-error
        :n [return] #'flycheck-error-list-goto-error)

  (defun +syntax-init-popups-h ()
    "Activate `flycheck-posframe-mode' if available and in GUI Emacs.
Activate `flycheck-popup-tip-mode' otherwise.
Do nothing if `lsp-ui-mode' is active and `lsp-ui-sideline-enable' is non-nil."
    (unless (and (bound-and-true-p lsp-ui-mode)
                 lsp-ui-sideline-enable)
      (if (and (fboundp 'flycheck-posframe-mode)
               (display-graphic-p))
          (flycheck-posframe-mode +1)
        (flycheck-popup-tip-mode +1))))

  (if (display-graphic-p)
      (use-package flycheck-posframe
        :commands (+syntax--flycheck-posframe-hide-h
                   flycheck-posframe-check-position
                   posframe-hide)
        :hook (flycheck-mode . +syntax-init-popups-h)
        :config
        (setq flycheck-posframe-warning-prefix "⚠ "
              flycheck-posframe-info-prefix    "ⓘ "
              flycheck-posframe-error-prefix   "⮾ ")

        ;; HACK: Hide the flycheck posframe immediately on the next keypress/user
        ;; action, otherwise it lingers until the next time the user is idle.
        (defun +syntax--flycheck-posframe-hide-h ()
          (unless (flycheck-posframe-check-position)
            (posframe-hide flycheck-posframe-buffer))
          (remove-hook 'post-command-hook #'+syntax--flycheck-posframe-hide-h))

        (defun +syntax--flycheck-posframe-advice (fn &rest args)
          "Advice to hide the posframe after the next user input."
          (let ((original-posframe-show (symbol-function 'posframe-show)))
            (fset 'posframe-show
                  (lambda (&rest args)
                    (add-hook 'post-command-hook #'+syntax--flycheck-posframe-hide-h)
                    (apply original-posframe-show args)))
            (apply fn args)))

        (advice-add 'flycheck-posframe-show-posframe :around '+syntax--flycheck-posframe-advice)
        (after! evil
          ;; Don't display popups while in insert or replace mode, as it can affect
          ;; the cursor's position or cause disruptive input delays.
          (add-hook! 'flycheck-posframe-inhibit-functions
                     #'evil-insert-state-p
                     #'evil-replace-state-p)))

    (use-package flycheck-popup-tip
      :commands flycheck-popup-tip-show-popup flycheck-popup-tip-delete-popup
      :hook (flycheck-mode . +syntax-init-popups-h)
      :config
      (setq flycheck-popup-tip-error-prefix "⚠ ")
      ;; HACK: Only display the flycheck popup if we're in normal mode (for evil
      ;;   users) or if no selection or completion is active. This popup can
      ;;   interfere with the active evil mode, clear active regions, and other
      ;;   funny business (see #7242).
      (defadvice! +syntax--disable-flycheck-popup-tip-maybe-a (&rest _)
        :before-while #'flycheck-popup-tip-show-popup
        (if (and (bound-and-true-p evil-local-mode)
                 (not (evil-emacs-state-p)))
            (evil-normal-state-p)
          (and (not (region-active-p))
               (not (ignore-errors (>= corfu--index 0))))))))

(use-package consult-flycheck
  :bind("M-g f"   . consult-flycheck)))


;; spell-checker
;; requires libenchant, see https://github.com/minad/jinx
(use-package jinx
  :hook ((outline-mode git-commit-mode) . jinx-mode)
  :bind ("C-," . jinx-correct)
  :init
  (setq jinx-languages "en_US pt_BR"))


(provide 'init-checker)
;;; init-checker.el ends here
