;;; pdf-config.el --- packages for pdf reading config  -*- lexical-binding: t -*-
;;
;; Author: Luiz Tagliaferro <luiz@luiznux.com>
;; URL: https://luiznux.com
;; This file is free software :)
;;
;;; commentary:
;;
;; ██████╗ ██████╗ ███████╗
;; ██╔══██╗██╔══██╗██╔════╝
;; ██████╔╝██║  ██║█████╗
;; ██╔═══╝ ██║  ██║██╔══╝
;; ██║     ██████╔╝██║
;; ╚═╝     ╚═════╝ ╚═╝
;;
;;; code:

(use-package pdf-view
  :ensure pdf-tools
  :defines pdf-annot-activate-created-annotations
  :functions (my-pdf-view-set-midnight-colors my-pdf-view-set-dark-theme)
  :hook (pdf-view-mode . pdf-view-midnight-minor-mode)
  :mode ("\\.[pP][dD][fF]\\'" . pdf-view-mode)
  :magic ("%PDF" . pdf-view-mode)
  :bind (:map pdf-view-mode-map
         ("C-s" . isearch-forward))
  :init
  (setq pdf-annot-activate-created-annotations t)

  ;; Set dark theme
  (defun my-pdf-view-set-midnight-colors ()
    "Set pdf-view midnight colors."
    (setq pdf-view-midnight-colors
          `(,(face-foreground 'default) . ,(face-background 'default))))
  (my-pdf-view-set-midnight-colors)

  (defun my-pdf-view-set-dark-theme ()
    "Set pdf-view midnight theme as color theme."
    (my-pdf-view-set-midnight-colors)
    (dolist (buf (buffer-list))
      (with-current-buffer buf
        (when (eq major-mode 'pdf-view-mode)
          (pdf-view-midnight-minor-mode (if pdf-view-midnight-minor-mode 1 -1))))))
  (add-hook 'after-load-theme-hook #'my-pdf-view-set-dark-theme)
  :config
  ;; Build pdfinfo if needed, locking until it's complete
  (with-no-warnings
    ;; Build pdfinfo if needed, locking until it's complete
    (defun my-pdf-tools-install ()
      (unless (file-executable-p pdf-info-epdfinfo-program)
        (let ((wconf (current-window-configuration)))
          (pdf-tools-install t)
          (message "Building epdfinfo. Please wait for a moment...")
          (while compilation-in-progress
            ;; Block until `pdf-tools-install' is done
            (sleep-for 1))
          (when (file-executable-p pdf-info-epdfinfo-program)
            (set-window-configuration wconf)))))
    (advice-add #'pdf-view-decrypt-document :before #'my-pdf-tools-install)

    ;; Highlight matches
    (defun my-pdf-isearch-hl-matches (current matches &optional occur-hack-p)
      "Highlighting edges CURRENT and MATCHES."
      (cl-destructuring-bind (fg1 bg1 fg2 bg2)
          (pdf-isearch-current-colors)
        (let* ((width (car (pdf-view-image-size)))
               (page (pdf-view-current-page))
               (window (selected-window))
               (buffer (current-buffer))
               (tick (cl-incf pdf-isearch--hl-matches-tick))
               (pdf-info-asynchronous
                (lambda (status data)
                  (when (and (null status)
                             (eq tick pdf-isearch--hl-matches-tick)
                             (buffer-live-p buffer)
                             (window-live-p window)
                             (eq (window-buffer window)
                                 buffer))
                    (with-selected-window window
                      (when (and (derived-mode-p 'pdf-view-mode)
                                 (or isearch-mode
                                     occur-hack-p)
                                 (eq page (pdf-view-current-page)))
                        (pdf-view-display-image
                         (pdf-view-create-image data :width width))))))))
          (pdf-info-renderpage-text-regions
           page width t nil
           `(,fg1 ,bg1 ,@(pdf-util-scale-pixel-to-relative
                          current))
           `(,fg2 ,bg2 ,@(pdf-util-scale-pixel-to-relative
                          (apply 'append
                                 (remove current matches))))))))
    (advice-add #'pdf-isearch-hl-matches :override #'my-pdf-isearch-hl-matches))

  ;; Recover last viewed position
  (use-package saveplace-pdf-view
    :config
    (with-no-warnings
      (advice-add 'save-place-find-file-hook :around #'saveplace-pdf-view-find-file-advice)
      (advice-add 'save-place-to-alist :around #'saveplace-pdf-view-to-alist-advice))))


(provide 'pdf-config)
;;; pdf-config.el ends here
