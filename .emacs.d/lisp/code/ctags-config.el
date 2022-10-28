;;; ctags-config.el --- Packages for treemacs config  -*- lexical-binding: t -*-
;;
;; This code is not made by myself and the source is:
;; https://github.com/seagle0128/.emacs.d/blob/master/lisp/init-ctags.el
;;
;;; Commentary:
;;
;;  ██████╗████████╗ █████╗  ██████╗ ███████╗
;; ██╔════╝╚══██╔══╝██╔══██╗██╔════╝ ██╔════╝
;; ██║        ██║   ███████║██║  ███╗███████╗
;; ██║        ██║   ██╔══██║██║   ██║╚════██║
;; ╚██████╗   ██║   ██║  ██║╚██████╔╝███████║
;;  ╚═════╝   ╚═╝   ╚═╝  ╚═╝ ╚═════╝ ╚══════╝
;;
;;; Code:

(require 'constants)
(require 'custom-config)

;; Ctags IDE on the True Editor
;; @see https://github.com/universal-ctags/citre#quick-start
(use-package citre
  :diminish
  :commands citre-jump-back
  :functions (projectile-project-root xref-go-back)
  :bind (:map prog-mode-map
         ("C-x c j" . citre-jump+)
         ("C-x c k" . citre-jump-back+)
         ("C-x c p" . citre-peek)
         ("C-x c a" . citre-ace-peek)
         ("C-x c u" . citre-update-this-tags-file))
  :init
  (setq citre-auto-enable-citre-mode-modes '(prog-mode)
        citre-default-create-tags-file-location 'global-cache
        citre-use-project-root-when-creating-tags t
        citre-prompt-language-for-ctags-command t)

  (with-eval-after-load 'projectile
    (setq citre-project-root-function #'projectile-project-root))

  (defun citre-jump+ ()
    "Jump to the definition of the symbol at point.
Fallback to `xref-find-definitions'."
    (interactive)
    (condition-case _
        (citre-jump)
      (error (call-interactively #'xref-find-definitions))))

  (defun citre-jump-back+ ()
    "Go back to the position before last `citre-jump'.
Fallback to `xref-go-back'."
    (interactive)
    (condition-case _
        (citre-jump-back)

      (error (if (fboundp #'xref-go-back)
                 (call-interactively #'xref-go-back)
               (call-interactively #'xref-pop-marker-stack)))))
  :config
  (with-eval-after-load 'cc-mode (require 'citre-lang-c))
  (with-eval-after-load 'dired (require 'citre-lang-fileref))

  (with-no-warnings
    ;; Use Citre xref backend as a fallback
    (define-advice xref--create-fetcher (:around (fn &rest args) fallback)
      (let ((fetcher (apply fn args))
            (citre-fetcher
             (let ((xref-backend-functions '(citre-xref-backend t)))
               (ignore xref-backend-functions)
               (apply fn args))))
        (lambda ()
          (or (with-demoted-errors "%s, fallback to citre"
                (funcall fetcher))
              (funcall citre-fetcher)))))

    ;; Combine completions from Citre and lsp
    (defun lsp-citre-capf-function ()
      "A capf backend that tries lsp first, then Citre."
      (let ((lsp-result (cond
                         ((bound-and-true-p lsp-mode)
                          (and (fboundp #'lsp-completion-at-point)
                               (lsp-completion-at-point)))
                         ((bound-and-true-p eglot--managed-mode)
                          (and (fboundp #'eglot-completion-at-point)
                               (eglot-completion-at-point))))))
        (if (and lsp-result
                 (try-completion
                  (buffer-substring (nth 0 lsp-result)
                                    (nth 1 lsp-result))
                  (nth 2 lsp-result)))
            lsp-result
          (citre-completion-at-point))))

    (defun enable-lsp-citre-capf-backend ()
      "Enable the lsp + Citre capf backend in current buffer."
      (add-hook 'completion-at-point-functions #'lsp-citre-capf-function nil t))

    (add-hook 'citre-mode-hook #'enable-lsp-citre-capf-backend)))

(provide 'ctags-config)
;;; ctags-config.el ends here
