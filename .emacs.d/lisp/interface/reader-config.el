;;; reader-config.el --- packages for reading -*- lexical-binding: t -*-
;;
;; Author: Luiz Tagliaferro <luiz@luiznux.com>
;; URL: https://luiznux.com
;; This file is free software :)
;;
;;; commentary:
;;
;;  ██████╗ ███████╗ █████╗ ██████╗ ███████╗██████╗
;;  ██╔══██╗██╔════╝██╔══██╗██╔══██╗██╔════╝██╔══██╗
;;  ██████╔╝█████╗  ███████║██║  ██║█████╗  ██████╔╝
;;  ██╔══██╗██╔══╝  ██╔══██║██║  ██║██╔══╝  ██╔══██╗
;;  ██║  ██║███████╗██║  ██║██████╔╝███████╗██║  ██║
;;  ╚═╝  ╚═╝╚══════╝╚═╝  ╚═╝╚═════╝ ╚══════╝╚═╝  ╚═╝
;;
;;; code:

(use-package pdf-view
  :ensure pdf-tools
  :diminish (pdf-view-midnight-minor-mode)
  :defines pdf-annot-activate-created-annotations
  :functions (my-pdf-view-set-midnight-colors my-pdf-view-set-dark-theme)
  :hook (pdf-view-mode . pdf-view-midnight-minor-mode)
  :mode ("\\.[pP][dD][fF]\\'" . pdf-view-mode)
  :magic ("%PDF" . pdf-view-mode)
  :bind (:map pdf-view-mode-map
         ("C-s" . isearch-forward))
  :init
  (setq pdf-view-use-scaling t
        pdf-view-use-imagemagick nil
        pdf-annot-activate-created-annotations t)

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
  ;; Activate the package
  (pdf-tools-install t nil t nil)

  (with-no-warnings
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
    (advice-add #'pdf-isearch-hl-matches :override #'my-pdf-isearch-hl-matches)

    (defun my-pdf-annot-show-annotation (a &optional highlight-p window)
      "Make annotation A visible."
      (save-selected-window
        (when window (select-window window 'norecord))
        (pdf-util-assert-pdf-window)
        (let ((page (pdf-annot-get a 'page))
              (size (pdf-view-image-size)))
          (unless (= page (pdf-view-current-page))
            (pdf-view-goto-page page))
          (let ((edges (pdf-annot-get-display-edges a)))
            (when highlight-p
              (pdf-view-display-image
               (pdf-view-create-image
                (pdf-cache-renderpage-highlight
                 page (car size)
                 `("white" "steel blue" 0.35 ,@edges))
                :map (pdf-view-apply-hotspot-functions
                      window page size)
                :width (car size))))
            (pdf-util-scroll-to-edges
             (pdf-util-scale-relative-to-pixel (car edges)))))))
    (advice-add #'pdf-annot-show-annotation :override #'my-pdf-annot-show-annotation))

  ;; Recover last viewed position
  (use-package saveplace-pdf-view
    :config
    (with-no-warnings
      (advice-add 'save-place-find-file-hook :around #'saveplace-pdf-view-find-file-advice)
      (advice-add 'save-place-to-alist :around #'saveplace-pdf-view-to-alist-advice))))

;; Epub reader
(use-package nov
  :mode ("\\.epub\\'" . nov-mode)
  :hook (nov-mode . my-nov-setup)
  :init
  (defun my-nov-setup ()
    "Setup `nov-mode' for better reading experience."
    (visual-line-mode 1)
    (face-remap-add-relative 'variable-pitch :family "Times New Roman" :height 1.5))
  :config
  (with-no-warnings
    ;; WORKAROUND: errors while opening `nov' files with Unicode characters
    ;; @see https://github.com/wasamasa/nov.el/issues/63
    (defun my-nov-content-unique-identifier (content)
      "Return the the unique identifier for CONTENT."
      (let* ((name (nov-content-unique-identifier-name content))
             (selector (format "package>metadata>identifier[id='%s']"
                               (regexp-quote name)))
             (id (car (esxml-node-children (esxml-query selector content)))))
        (and id (intern id))))
    (advice-add #'nov-content-unique-identifier :override #'my-nov-content-unique-identifier)))


(provide 'reader-config)
;;; reader-config.el ends here
