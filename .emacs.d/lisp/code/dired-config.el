;; dired-config.el --- Initialize dired configurations.	-*- lexical-binding: t -*-
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
;; ██████╗ ██╗██████╗ ███████╗██████╗
;; ██╔══██╗██║██╔══██╗██╔════╝██╔══██╗
;; ██║  ██║██║██████╔╝█████╗  ██║  ██║
;; ██║  ██║██║██╔══██╗██╔══╝  ██║  ██║
;; ██████╔╝██║██║  ██║███████╗██████╔╝
;; ╚═════╝ ╚═╝╚═╝  ╚═╝╚══════╝╚═════╝
;;
;;; Code:

(require 'constants)
(require 'functions)

;; Directory operations
(use-package dired
  :ensure nil
  :bind (:map dired-mode-map
         ("C-c C-p" . wdired-change-to-wdired-mode))
  :config
  ;; Always delete and copy recursively
  (setq dired-recursive-deletes 'always
        dired-recursive-copies 'always)

  (when sys/macp
    ;; Suppress the warning: `ls does not support --dired'.
    (setq dired-use-ls-dired nil)

    (when (executable-find "gls")
      ;; Use GNU ls as `gls' from `coreutils' if available.
      (setq insert-directory-program "gls")))

  (when (or (and sys/macp (executable-find "gls"))
            (and (not sys/macp) (executable-find "ls")))
    ;; Using `insert-directory-program'
    (setq ls-lisp-use-insert-directory-program t)

    ;; Show directory first
    (setq dired-listing-switches "-alh --group-directories-first")

    ;; Quick sort dired buffers via hydra
    (use-package dired-quick-sort
      :bind (:map dired-mode-map
             ("S" . hydra-dired-quick-sort/body))))

  ;; Show git info in dired
  (use-package dired-git-info
    :bind (:map dired-mode-map
           (")" . dired-git-info-mode)))

  ;; Allow rsync from dired buffers
  (use-package dired-rsync
    :bind (:map dired-mode-map
           ("C-c C-r" . dired-rsync)))

  ;; Colourful dired
  (use-package diredfl
    :functions diredfl-global-mode
    :init (diredfl-global-mode 1))

  ;; Shows icons
  (use-package all-the-icons-dired
    :diminish
    :hook (dired-mode . all-the-icons-dired-mode)
    :init (setq all-the-icons-dired-monochrome nil)
    :config
    (with-no-warnings
      (defun my-all-the-icons-dired--refresh ()
        "Display the icons of files in a dired buffer."
        (all-the-icons-dired--remove-all-overlays)
        ;; NOTE: don't display icons it too many items
        (if (<= (count-lines (point-min) (point-max)) 1000)
            (save-excursion
              (goto-char (point-min))
              (while (not (eobp))
                (when (dired-move-to-filename nil)
                  (let ((case-fold-search t))
                    (when-let* ((file (dired-get-filename 'relative 'noerror))
                                (icon (if (file-directory-p file)
                                          (all-the-icons-icon-for-dir
                                           file
                                           :face 'all-the-icons-dired-dir-face
                                           :height 0.9
                                           :v-adjust all-the-icons-dired-v-adjust)
                                        (apply #'all-the-icons-icon-for-file
                                               file
                                               (append
                                                '(:height 0.9)
                                                `(:v-adjust ,all-the-icons-dired-v-adjust)
                                                (when all-the-icons-dired-monochrome
                                                  `(:face ,(face-at-point))))))))
                      (if (member file '("." ".."))
                          (all-the-icons-dired--add-overlay (dired-mode-to-filename) "   \t")
                        (all-the-icons-dired--add-overlay (dired-move-to-filename) (concat " " icon "\t"))))))
                (forward-line 1)))
          (message "Not display icons because of too many items.")))
      (advice-add #'all-the-icons-dired--refresh :override #'my-all-the-icons-dired--refresh)))

  ;; Extra Dired functionality
  (use-package dired-aux :ensure nil)
  (use-package dired-x
    :ensure nil
    :demand
    :config
    (let ((cmd (cond (sys/mac-x-p "open")
                     (sys/linux-x-p "xdg-open")
                     (sys/win32p "start")
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


(provide 'dired-config)
;;; dired-config.el ends here
