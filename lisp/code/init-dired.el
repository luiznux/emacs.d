;; init-dired.el --- Initialize dired configurations   -*- lexical-binding: t -*-
;;
;; Author: Luiz Tagliaferro <luiz@luiznux.com>
;; URL: https://luiznux.com
;; This file is free software :)
;;
;; Directory configurations.
;; This code is not made by myself and the source is:
;; https://github.com/seagle0128/.emacs.d/blob/master/lisp/init-dired.el
;;
;;; Commentary:
;;
;;
;; ██████╗ ██╗██████╗ ███████╗██████╗
;; ██╔══██╗██║██╔══██╗██╔════╝██╔══██╗
;; ██║  ██║██║██████╔╝█████╗  ██║  ██║
;; ██║  ██║██║██╔══██╗██╔══╝  ██║  ██║
;; ██████╔╝██║██║  ██║███████╗██████╔╝
;; ╚═════╝ ╚═╝╚═╝  ╚═╝╚══════╝╚═════╝
;;
;;; Code:

(eval-when-compile
  (require 'init-constants))

;; Directory operations
(use-package dired
  :ensure nil
  :bind (:map dired-mode-map
         ("C-c C-p" . wdired-change-to-wdired-mode))
  :config
  (setq dired-auto-revert-buffer #'dired-buffer-stale-p ; don't prompt to revert, just do it
        ;; Ask whether destination dirs should get created when copying/removing files.
        dired-create-destination-dirs 'ask
        ;; Guess a default target directory
        dired-dwim-target t
        ;; Always delete and copy recursively
        dired-recursive-deletes 'always
        dired-recursive-copies 'top
        ;; Show directory first
        dired-listing-switches "-alh -v --group-directories-first")

  (when sys/macp
    (if (executable-find "gls")
        (progn
          ;; Use GNU ls as `gls' from `coreutils' if available.
          (setq insert-directory-program "gls")
          ;; Using `insert-directory-program'
          (setq ls-lisp-use-insert-directory-program t))
      (progn
        ;; Suppress the warning: `ls does not support --dired'.
        (setq dired-use-ls-dired nil)
        (setq dired-listing-switches "-alh"))))

  ;; Quick sort dired buffers via hydra
  (use-package dired-quick-sort
    :bind (:map dired-mode-map
           ("S" . hydra-dired-quick-sort/body)))

  ;; Show git info in dired
  (use-package dired-git-info
    :bind (:map dired-mode-map
           (")" . dired-git-info-mode)))

  ;; Allow rsync from dired buffers
  (use-package dired-rsync
    :bind (:map dired-mode-map
           ("C-c C-r" . dired-rsync)))

  ;; Colorful dired
  (use-package diredfl
    :hook (dired-mode . diredfl-mode))

  ;; Shows icons
  (use-package nerd-icons-dired
    :diminish
    :when (icons-displayable-p)
    :custom-face
    (nerd-icons-dired-dir-face ((t (:inherit nerd-icons-dsilver :foreground unspecified))))
    :hook (dired-mode . nerd-icons-dired-mode)
    :config
    ;; WORKAROUND: display transparent background of icons
    ;; @see https://github.com/rainstormstudio/nerd-icons-dired/issues/1#issuecomment-2628680359
    (defun my-nerd-icons-dired--add-overlay (pos string)
      "Add overlay to display STRING at POS."
      (let ((ov (make-overlay (1- pos) pos)))
        (overlay-put ov 'nerd-icons-dired-overlay t)
        (overlay-put ov 'after-string
                     (propertize "_" 'display string))))
    (advice-add #'nerd-icons-dired--add-overlay :override #'my-nerd-icons-dired--add-overlay))

  ;; Extra Dired functionality
  (use-package dired-aux :ensure nil)
  (use-package dired-x
    :ensure nil
    :demand t
    :config
    (let ((cmd (cond (sys/mac-x-p "open")
                     (sys/linux-x-p "xdg-open")
                     (t ""))))
      (setq dired-guess-shell-alist-user
            `(("\\.pdf\\'" ,cmd)
              ("\\.docx\\'" ,cmd)
              ("\\.\\(?:djvu\\|eps\\)\\'" ,cmd)
              ("\\.\\(?:jpg\\|jpeg\\|png\\|gif\\|xpm\\)\\'" ,cmd)
              ("\\.\\(?:xcf\\)\\'" ,cmd)
              ("\\.csv\\'" ,cmd)
              ("\\.tex\\'" ,cmd)
              ("\\.\\(?:mp4\\|mkv\\|avi\\|flv\\|rm\\|rmvb\\|ogv\\)\\(?:\\.part\\)?\\'" ,cmd)
              ("\\.\\(?:mp3\\|flac\\)\\'" ,cmd)
              ("\\.html?\\'" ,cmd)
              ("\\.md\\'" ,cmd))))

    (setq dired-omit-files
          (concat dired-omit-files
                  "\\|^.DS_Store$\\|^.projectile$\\|^.git*\\|^.svn$\\|^.vscode$\\|\\.js\\.meta$\\|\\.meta$\\|\\.elc$\\|^.emacs.*"))))

;; `find-dired' alternative using `fd'
(when (executable-find "fd")
  (use-package fd-dired))


(provide 'init-dired)
;;; init-dired.el ends here
