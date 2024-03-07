;;; init-vcs.el --- Initialize version control system configurations   -*- lexical-binding: t -*-
;;
;; Author: Luiz Tagliaferro <luiz@luiznux.com>
;; URL: https://luiznux.com
;; This file is free software :)
;;
;;; Commentary:
;;
;;
;;  ██████╗ ██╗████████╗
;; ██╔════╝ ██║╚══██╔══╝
;; ██║  ███╗██║   ██║
;; ██║   ██║██║   ██║
;; ╚██████╔╝██║   ██║
;;  ╚═════╝ ╚═╝   ╚═╝
;;
;;; Code:

(eval-when-compile
  (require 'init-constants))

;; Git
;; See `magit-define-global-key-bindings'
(use-package magit
  :init (setq magit-diff-refine-hunk t)
  :config
  (when (fboundp 'transient-append-suffix)
    ;; Add switch: --tags
    (transient-append-suffix 'magit-fetch
      "-p" '("-t" "Fetch all tags" ("-t" "--tags"))))

  ;; Access Git forges from Magit
  (use-package forge
    :demand t
    :custom-face
    (forge-topic-label ((t (:inherit variable-pitch :height 0.9 :width condensed :weight regular :underline nil))))
    :init
    (setq forge-topic-list-columns
          '(("#" 5 forge-topic-list-sort-by-number (:right-align t) number nil)
            ("Title" 60 t nil title  nil)
            ("State" 6 t nil state nil)
            ("Updated" 10 t nil updated nil)))

    ;; avoid conflics with `evil-collection-forge-setup'
    (setq forge-add-default-bindings nil))

  ;; Show TODOs in magit
  (use-package magit-todos
    :defines magit-todos-nice
    :commands magit-todos-mode magit-todos--scan-with-git-grep
    :init
    (setq magit-todos-nice (if (executable-find "nice") t nil))
    (setq magit-todos-scanner #'magit-todos--scan-with-git-grep)
    (let ((inhibit-message t))
      (magit-todos-mode 1))
    :config
    (with-eval-after-load 'magit-status
      (transient-append-suffix 'magit-status-jump '(0 0 -1)
        '("t " "Todos" magit-todos-jump-to-todos)))))

(use-package transient-posframe
  :diminish
  :custom-face
  (transient-posframe ((t (:inherit tooltip))))
  (transient-posframe-border ((t (:inherit posframe-border :background unspecified))))
  :hook (after-init . transient-posframe-mode)
  :init
  (setq transient-posframe-border-width 3
        transient-posframe-min-height nil
        transient-posframe-min-width 80
        transient-posframe-poshandler 'posframe-poshandler-frame-center
        transient-posframe-parameters '((left-fringe . 8)
                                        (right-fringe . 8)))
  :config
  (with-no-warnings
    ;; FIXME:https://github.com/yanghaoxie/transient-posframe/issues/5#issuecomment-1974871665
    (defun my-transient-posframe--show-buffer (buffer _alist)
      "Show BUFFER in posframe and we do not use _ALIST at this period."
      (when (posframe-workable-p)
        (let* ((posframe
	            (posframe-show buffer
                               :height (with-current-buffer buffer (1- (count-screen-lines (point-min) (point-max))))
			                   :font transient-posframe-font
			                   :position (point)
			                   :poshandler transient-posframe-poshandler
			                   :background-color (face-attribute 'transient-posframe :background nil t)
			                   :foreground-color (face-attribute 'transient-posframe :foreground nil t)
			                   :min-width transient-posframe-min-width
			                   :min-height transient-posframe-min-height
			                   :internal-border-width transient-posframe-border-width
			                   :internal-border-color (face-attribute 'transient-posframe-border :background nil t)
			                   :override-parameters transient-posframe-parameters)))
          (frame-selected-window posframe))))
    (advice-add #'transient-posframe--show-buffer :override #'my-transient-posframe--show-buffer)

    (defun my-transient-posframe--hide ()
      "Hide transient posframe."
      (posframe-hide transient--buffer-name))
    (advice-add #'transient-posframe--delete :override #'my-transient-posframe--hide)))

;; Walk through git revisions of a file
(use-package git-timemachine
  :custom-face
  (git-timemachine-minibuffer-author-face ((t (:inherit success :foreground unspecified))))
  (git-timemachine-minibuffer-detail-face ((t (:inherit warning :foreground unspecified))))
  :bind (:map vc-prefix-map
         ("t" . git-timemachine))
  :hook ((git-timemachine-mode . (lambda ()
                                   "Improve `git-timemachine' buffers."
                                   ;; Display different colors in mode-line
                                   (face-remap-add-relative 'mode-line 'custom-state)
                                   (if (facep 'mode-line-active)
                                       (face-remap-add-relative 'mode-line-active 'custom-state)
                                     (face-remap-add-relative 'mode-line 'custom-state))

                                   ;; Highlight symbols in elisp
                                   (and (derived-mode-p 'emacs-lisp-mode)
                                        (fboundp 'highlight-defined-mode)
                                        (highlight-defined-mode t))

                                   ;; Display line numbers
                                   (and (derived-mode-p 'prog-mode 'yaml-mode)
                                        (fboundp 'display-line-numbers-mode)
                                        (display-line-numbers-mode t))))
         (before-revert . (lambda ()
                            (when (bound-and-true-p git-timemachine-mode)
                              (user-error "Cannot revert the timemachine buffer"))))))

;; Pop up last commit information of current line
(use-package git-messenger
  :bind (:map vc-prefix-map
         ("p" . git-messenger:popup-message)
         :map git-messenger-map
         ("m" . git-messenger:copy-message))
  :init (setq git-messenger:show-detail t
              git-messenger:use-magit-popup t)
  :config
  (with-no-warnings
    (with-eval-after-load 'hydra
      (defhydra git-messenger-hydra (:color blue)
        ("s" git-messenger:popup-show "show")
        ("c" git-messenger:copy-commit-id "copy hash")
        ("m" git-messenger:copy-message "copy message")
        ("," (catch 'git-messenger-loop (git-messenger:show-parent)) "go parent")
        ("q" git-messenger:popup-close "quit")))

    (defun my-git-messenger:format-detail (vcs commit-id author message)
      (if (eq vcs 'git)
          (let ((date (git-messenger:commit-date commit-id))
                (colon (propertize ":" 'face 'font-lock-comment-face)))
            (concat
             (format "%s%s %s \n%s%s %s\n%s  %s %s \n"
                     (propertize "Commit" 'face 'font-lock-keyword-face) colon
                     (propertize (substring commit-id 0 8) 'face 'font-lock-comment-face)
                     (propertize "Author" 'face 'font-lock-keyword-face) colon
                     (propertize author 'face 'font-lock-string-face)
                     (propertize "Date" 'face 'font-lock-keyword-face) colon
                     (propertize date 'face 'font-lock-string-face))
             (propertize (make-string 38 ?─) 'face 'font-lock-comment-face)
             message
             (propertize "\nPress q to quit" 'face '(:inherit (font-lock-comment-face italic)))))
        (git-messenger:format-detail vcs commit-id author message)))

    (defun my-git-messenger:popup-message ()
      "Popup message with `posframe', `pos-tip', `lv' or `message', and dispatch actions with `hydra'."
      (interactive)
      (let* ((hydra-hint-display-type 'message)
             (vcs (git-messenger:find-vcs))
             (file (buffer-file-name (buffer-base-buffer)))
             (line (line-number-at-pos))
             (commit-info (git-messenger:commit-info-at-line vcs file line))
             (commit-id (car commit-info))
             (author (cdr commit-info))
             (msg (git-messenger:commit-message vcs commit-id))
             (popuped-message (if (git-messenger:show-detail-p commit-id)
                                  (my-git-messenger:format-detail vcs commit-id author msg)
                                (cl-case vcs
                                  (git msg)
                                  (svn (if (string= commit-id "-")
                                           msg
                                         (git-messenger:svn-message msg)))
                                  (hg msg)))))
        (setq git-messenger:vcs vcs
              git-messenger:last-message msg
              git-messenger:last-commit-id commit-id)
        (run-hook-with-args 'git-messenger:before-popup-hook popuped-message)
        (git-messenger-hydra/body)
        (cond ((and (fboundp 'posframe-workable-p) (posframe-workable-p))
               (let ((buffer-name "*git-messenger*"))
                 (posframe-show buffer-name
                                :string (concat (propertize "\n" 'face '(:height 0.3))
                                                popuped-message
                                                "\n"
                                                (propertize "\n" 'face '(:height 0.3)))
                                :left-fringe 8
                                :right-fringe 8
                                :max-width (round (* (frame-width) 0.62))
                                :max-height (round (* (frame-height) 0.62))
                                :internal-border-width 1
                                :internal-border-color (face-background 'posframe-border nil t)
                                :background-color (face-background 'tooltip nil t))
                 (unwind-protect
                     (push (read-event) unread-command-events)
                   (posframe-hide buffer-name))))
              ((and (fboundp 'pos-tip-show) (display-graphic-p))
               (pos-tip-show popuped-message))
              ((fboundp 'lv-message)
               (lv-message popuped-message)
               (unwind-protect
                   (push (read-event) unread-command-events)
                 (lv-delete-window)))
              (t (message "%s" popuped-message)))
        (run-hook-with-args 'git-messenger:after-popup-hook popuped-message)))
    (advice-add #'git-messenger:popup-close :override #'ignore)
    (advice-add #'git-messenger:popup-message :override #'my-git-messenger:popup-message)))

(use-package blamer
  :defer 20
  :custom
  (blamer-idle-time 1.0)
  (blamer-min-offset 50)
  :custom-face
  (blamer-face ((t :foreground "#7a88cf"
                   :background unspecified
                   :height      80
                   :italic      t)))
  :init
  (setq blamer--overlay-popup-position  'smart
        blamer-datetime-formatter       "[%s] "
        blamer-author-formatter         "✎ %s"
        blamer-commit-formatter         "● %s"
        blamer-prettify-time-p          t))

;; Resolve diff3 conflicts
(use-package smerge-mode
  :ensure nil
  :diminish
  :pretty-hydra
  ((:title (pretty-hydra-title "Smerge" 'octicon "nf-oct-diff")
    :color pink :quit-key ("q" "C-g"))
   ("Move"
    (("n" smerge-next "next")
     ("p" smerge-prev "previous"))
    "Keep"
    (("b" smerge-keep-base "base")
     ("u" smerge-keep-upper "upper")
     ("l" smerge-keep-lower "lower")
     ("a" smerge-keep-all "all")
     ("RET" smerge-keep-current "current")
     ("C-m" smerge-keep-current "current"))
    "Diff"
    (("<" smerge-diff-base-upper "upper/base")
     ("=" smerge-diff-upper-lower "upper/lower")
     (">" smerge-diff-base-lower "upper/lower")
     ("R" smerge-refine "refine")
     ("E" smerge-ediff "ediff"))
    "Other"
    (("C" smerge-combine-with-next "combine")
     ("r" smerge-resolve "resolve")
     ("k" smerge-kill-current "kill")
     ("ZZ" (lambda ()
             (interactive)
             (save-buffer)
             (bury-buffer))
      "Save and bury buffer" :exit t))))
  :bind (:map smerge-mode-map
         ("C-c m" . smerge-mode-hydra/body))
  :hook ((find-file . (lambda ()
                        (save-excursion
                          (goto-char (point-min))
                          (when (re-search-forward "^<<<<<<< " nil t)
                            (smerge-mode 1)))))
         (magit-diff-visit-file . (lambda ()
                                    (when smerge-mode
                                      (smerge-mode-hydra/body))))))

;; Open github/gitlab/bitbucket page
(use-package browse-at-remote
  :bind (:map vc-prefix-map
         ("B" . browse-at-remote)))

(use-package grip-mode
  :init
  (setq grip-preview-use-webkit t))

;; Git configuration modes
(use-package git-modes)


(provide 'init-vcs)
;;; init-vcs.el ends here
