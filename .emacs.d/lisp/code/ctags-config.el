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

;; Ctags IDE on the True Editor
;; @see https://github.com/universal-ctags/citre#quick-start
(when emacs/>=26p
  (use-package citre
    :defines citre-auto-enable-citre-mode-modes
    :diminish
    :bind (("C-x c j" . citre-jump+)
           ("C-x c k" . citre-jump-back)
           ("C-x c p" . citre-peek)
           ("C-x c a" . citre-ace-peek)
           ("C-x c u" . citre-update-this-tags-file))
    :init
    (require 'citre-config)
    (setq citre-auto-enable-citre-mode-modes '(prog-mode))

    (defun citre-jump+ ()
      "Jump to the definition of the symbol at point.
Fallback to `xref-find-definitions'."
      (interactive)
      (condition-case _
          (citre-jump)
        (error (call-interactively #'xref-find-definitions))))
    :config
    (with-no-warnings
      (with-eval-after-load 'projectile
        (setq citre-project-root-function #'projectile-project-root))

      ;; Integrate with `lsp-mode' and `eglot'
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

      (defun lsp-citre-capf-function ()
        "A capf backend that tries lsp first, then Citre."
        (let ((lsp-result (pcase centaur-lsp
                            ('lsp-mode
                             (and (fboundp #'lsp-completion-at-point)
                                  (lsp-completion-at-point)))
                            ('eglot
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

      (add-hook 'citre-mode-hook #'enable-lsp-citre-capf-backend))))

(provide 'ctags-config)
;;; ctags-config.el ends here
