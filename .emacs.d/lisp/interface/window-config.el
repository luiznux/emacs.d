;;; window-config.el --- window config for Emacs  -*- lexical-binding: t -*-
;;
;; Author: Luiz Tagliaferro <luiz@luiznux.com>
;; URL: https://luiznux.com
;; This file is free software :)
;;
;;; Commentary:
;;
;; ██╗    ██╗██╗███╗   ██╗
;; ██║    ██║██║████╗  ██║
;; ██║ █╗ ██║██║██╔██╗ ██║
;; ██║███╗██║██║██║╚██╗██║
;; ╚███╔███╔╝██║██║ ╚████║
;;  ╚══╝╚══╝ ╚═╝╚═╝  ╚═══╝
;;
;;; Code:

(require 'constants)

;; Restore old window configurations
(use-package winner
  :ensure nil
  :commands (winner-undo winner-redo)
  :hook (after-init . winner-mode)
  :init (setq winner-boring-buffers '("*Completions*"
                                      "*Compile-Log*"
                                      "*inferior-lisp*"
                                      "*Fuzzy Completions*"
                                      "*Apropos*"
                                      "*Help*"
                                      "*cvs*"
                                      "*Buffer List*"
                                      "*Ibuffer*"
                                      "*esh command on file*")))

;; Enforce rules for popups
;;(use-package shackle
;;  :hook (after-init . shackle-mode)
;;  :init
;;  (setq shackle-default-size 0.4
;;        shackle-default-alignment 'below
;;        shackle-default-rule nil
;;        shackle-select-reused-windows t
;;        shackle-rules
;;        '((("*Help*" "*Apropos*") :select t :size 0.3 :align 'below :autoclose t)
;;          (compilation-mode :select t :size 0.3 :align 'below :autoclose t)
;;          (comint-mode :select t :size 0.4 :align 'below :autoclose t)
;;          ("*Completions*" :size 0.3 :align 'below :autoclose t)
;;          ("*Pp Eval Output*" :size 15 :align 'below :autoclose t)
;;          ("*Backtrace*" :select t :size 15 :align 'below)
;;          (("*Warnings*" "*Messages*") :size 0.3 :align 'below :autoclose t)
;;          ("^\\*.*Shell Command.*\\*$" :regexp t :size 0.3 :align 'below :autoclose t)
;;          ("\\*[Wo]*Man.*\\*" :regexp t :select t :align 'below :autoclose t)
;;          ("*Calendar*" :select t :size 0.3 :align 'below)
;;          (("*shell*" "*eshell*" "*ielm*") :popup t :size 0.3 :align 'below)
;;          ("^\\*vc-.*\\*$" :regexp t :size 0.3 :align 'below :autoclose t)
;;          ("*gud-debug*" :select t :size 0.4 :align 'below :autoclose t)
;;          ("\\*ivy-occur .*\\*" :regexp t :select t :size 0.3 :align 'below)
;;          (" *undo-tree*" :select t)
;;          ("*quickrun*" :select t :size 15 :align 'below)
;;          ("*tldr*" :size 0.4 :align 'below :autoclose t)
;;          ("*Finder*" :select t :size 0.3 :align 'below :autoclose t)
;;          ("^\\*macro expansion\\**" :regexp t :size 0.4 :align 'below)
;;          ("^\\*elfeed-entry" :regexp t :size 0.7 :align 'below :autoclose t)
;;          (" *Install vterm* " :size 0.35 :same t :align 'below)
;;          (("*Paradox Report*" "*package update results*") :size 0.2 :align 'below :autoclose t)
;;          ("*Package-Lint*" :size 0.4 :align 'below :autoclose t)
;;          ("*How Do You*" :select t :size 0.5 :align 'below :autoclose t)
;;
;;          ((" *Agenda Commands*" " *Org todo*" "*Org Dashboard*" "*Org Select*")
;;           :select t :size 0.1 :align 'below :autoclose t)
;;          (("\\*Capture\\*" "^CAPTURE-.*\\.org*") :regexp t :select t :size 0.3 :align 'below :autoclose t)
;;
;;          ("\\*cider-repl .*\\*" :regexp t :select t :size 0.4 :align 'below)
;;
;;          ("*ert*" :size 15 :align 'below :autoclose t)
;;          (overseer-buffer-mode :size 15 :align 'below :autoclose t)
;;
;;          (" *Flycheck checkers*" :select t :size 0.3 :align 'below :autoclose t)
;;          ((flycheck-error-list-mode flymake-diagnostics-buffer-mode)
;;           :select t :size 0.25 :align 'below :autoclose t)
;;
;;          (("*lsp-help*" "*lsp session*") :size 0.3 :align 'below :autoclose t)
;;          ("*DAP Templates*" :select t :size 0.4 :align 'below :autoclose t)
;;          (dap-server-log-mode :size 15 :align 'below :autoclose t)
;;          ("*rustfmt*" :select t :size 0.3 :align 'below :autoclose t)
;;          ((rustic-compilation-mode rustic-cargo-clippy-mode rustic-cargo-outdated-mode rustic-cargo-test-mode)
;;           :select t :size 0.3 :align 'below :autoclose t)
;;
;;          (profiler-report-mode :select t :size 0.5 :align 'below)
;;          ("*ELP Profiling Restuls*" :select t :size 0.5 :align 'below)
;;
;;          ((inferior-python-mode inf-ruby-mode swift-repl-mode) :size 0.4 :align 'below)
;;          ("*prolog*" :size 0.4 :align 'below)
;;
;;          (("*Gofmt Errors*" "*Go Test*") :select t :size 0.3 :align 'below :autoclose t)
;;          (godoc-mode :select t :size 0.4 :align 'below :autoclose t)
;;
;;          ((grep-mode occur-mode rg-mode deadgrep-mode ag-mode pt-mode) :select t :size 0.4 :align 'below)
;;          (Buffer-menu-mode :select t :size 0.5 :align 'below :autoclose t)
;;          (gnus-article-mode :select t :size 0.7 :align 'below :autoclose t)
;;          (helpful-mode :select t :size 0.3 :align 'below :autoclose t)
;;          (devdocs-mode :select t :size 0.4 :align 'below :autoclose t)
;;          ((process-menu-mode list-environment-mode cargo-process-mode) :select t :size 0.3 :align 'below)
;;          (("*docker-containers*" "*docker-images*" "*docker-networks*" "*docker-volumes*")
;;           :size 0.4 :align 'below :autoclose t)
;;          (bookmark-bmenu-mode :select t :size 0.4 :align 'below)
;;          (tabulated-list-mode :size 0.4 :align 'below :autclose t)))
;;
;;  :config
;;  (with-no-warnings
;;    (defvar shackle--popup-window-list nil
;;      "All popup windows.")
;;    (defvar-local shackle--current-popup-window nil
;;      "Current popup window.")
;;    (put 'shackle--current-popup-window 'permanent-local t)
;;
;;    (defun shackle-last-popup-buffer ()
;;      "View last popup buffer."
;;      (interactive)
;;      (ignore-errors
;;        (display-buffer shackle-last-buffer)))
;;    (bind-key "C-h z" #'shackle-last-popup-buffer)
;;
;;    ;; Add keyword: `autoclose'
;;    (defun shackle-display-buffer-hack (fn buffer alist plist)
;;      (let ((window (funcall fn buffer alist plist)))
;;        (setq shackle--current-popup-window window)
;;
;;        (when (plist-get plist :autoclose)
;;          (push (cons window buffer) shackle--popup-window-list))
;;        window))
;;    (advice-add #'shackle-display-buffer :around #'shackle-display-buffer-hack)
;;
;;    (defun shackle-close-popup-window-hack (&rest _)
;;      "Close current popup window via `C-g'."
;;      (setq shackle--popup-window-list
;;            (cl-loop for (window . buffer) in shackle--popup-window-list
;;                     if (and (window-live-p window)
;;                             (equal (window-buffer window) buffer))
;;                     collect (cons window buffer)))
;;      ;; `C-g' can deactivate region
;;      (when (and (called-interactively-p 'interactive)
;;                 (not (region-active-p)))
;;        (if (one-window-p)
;;            (let ((window (selected-window)))
;;              (when (equal (buffer-local-value 'shackle--current-popup-window
;;                                               (window-buffer window))
;;                           window)
;;                (winner-undo)))
;;          (let* ((window (caar shackle--popup-window-list))
;;                 (buffer (cdar shackle--popup-window-list))
;;                 (process (get-buffer-process buffer)))
;;            (when (and (window-live-p window)
;;                       (equal (window-buffer window) buffer))
;;              (when (process-live-p process)
;;                (kill-process process))
;;              (delete-window window)
;;              (pop shackle--popup-window-list))))))
;;    (advice-add #'keyboard-quit :before #'shackle-close-popup-window-hack)
;;
;;    ;; Compatible with org
;;    (advice-add #'org-switch-to-buffer-other-window
;;                :override #'switch-to-buffer-other-window)))

;; TODO: testing popper
(when emacs/>=26p
  (use-package popper
    :defines popper-echo-dispatch-actions
    :commands popper-group-by-projectile
    :bind (:map popper-mode-map
           ("C-h z" . popper-toggle-latest)
           ("C-<tab>"   . popper-cycle)
           ("C-M-<tab>" . popper-toggle-type))
    :hook (after-init . popper-mode)
    :init
    (setq popper-reference-buffers
          '("\\*Messages\\*"
            "Output\\*$" "\\*Pp Eval Output\\*$"
            "\\*Compile-Log\\*"
            "\\*Completions\\*"
            "\\*Warnings\\*"
            "\\*Async Shell Command\\*"
            "\\*Apropos\\*"
            "\\*Backtrace\\*"
            "\\*Calendar\\*"
            "\\*Finder\\*"
            "\\*Embark Actions\\*"

            bookmark-bmenu-mode
            compilation-mode

            help-mode helpful-mode
            tabulated-list-mode
            Buffer-menu-mode

            gnus-article-mode devdocs-mode
            grep-mode occur-mode rg-mode deadgrep-mode ag-mode pt-mode
            ivy-occur-mode ivy-occur-grep-mode
            process-menu-mode list-environment-mode cargo-process-mode

            "^\\*eshell.*\\*.*$" eshell-mode
            "^\\*shell.*\\*.*$"  shell-mode
            "^\\*terminal.*\\*.*$" term-mode
            "^\\*vterm.*\\*.*$"  vterm-mode

            "\\*DAP Templates\\*$" dap-server-log-mode
            "\\*ELP Profiling Restuls\\*" profiler-report-mode
            "\\*Flycheck errors\\*$" " \\*Flycheck checker\\*$"
            "\\*Paradox Report\\*$" "\\*package update results\\*$" "\\*Package-Lint\\*$"
            "\\*[Wo]*Man.*\\*$"
            "\\*ert\\*$" overseer-buffer-mode
            "\\*gud-debug\\*$"
            "\\*lsp-help\\*$" "\\*lsp session\\*$"
            "\\*quickrun\\*$"
            "\\*tldr\\*$"
            "\\*vc-.*\\*$"
            "^\\*elfeed-entry\\*$"
            "^\\*macro expansion\\**"

            "\\*Org Select\\*" "\\*Capture\\*" "^CAPTURE-.*\\.org*" "\\*Agenda Commands\\*"
            "\\*Org Dashboard\\*" "\\*Org links\\*" "\\*Org Note\\*"

            "\\*cider-repl .*\\*"
            "\\*Gofmt Errors\\*$" "\\*Go Test\\*$" godoc-mode
            "\\*docker-containers\\*" "\\*docker-images\\*" "\\*docker-networks\\*" "\\*docker-volumes\\*"
            "\\*prolog\\*" inferior-python-mode inf-ruby-mode swift-repl-mode
            "\\*rustfmt\\*$" rustic-compilation-mode rustic-cargo-clippy-mode
            rustic-cargo-outdated-mode rustic-cargo-test-moed))

    (with-eval-after-load 'projectile
      (setq popper-group-function #'popper-group-by-projectile))

    (when (display-grayscale-p)
      (setq popper-mode-line
            '(:eval
              (format " %s " (all-the-icons-octicon "pin" :height 0.9 :v-adjust 0.0 :face 'mode-line-emphasis)))))

    (setq popper-echo-dispatch-actions t)

    :config
    (popper-echo-mode 1)

    (with-no-warnings
      (defun my-popper-fit-window-height (win)
        "Determine the height of popup window WIN by fitting it to the buffer's content."
        (fit-window-to-buffer
         win
         (floor (frame-height) 3)
         (floor (frame-height) 3)))
      (setq popper-window-height #'my-popper-fit-window-height)

      (defun popper-close-window-hack (&rest _)
        "Close popper window via `C-g'."
        ;; `C-g' can deactivate region
        (when (and (called-interactively-p 'interactive)
                   (not (region-active-p))
                   popper-open-popup-alist)
          (let ((window (caar popper-open-popup-alist)))
            (when (window-live-p window)
              (delete-window window)))))
      (advice-add #'keyboard-quit :before #'popper-close-window-hack))))

(use-package switch-window)


(provide 'window-config)
;;; window-config.el ends here
