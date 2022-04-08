;;; functions.el --- Custom functions file -*- lexical-binding: t  t no-byte-compile: t-*-
;;
;; Author: Luiz Tagliaferro <luiz@luiznux.com>
;; URL: https://luiznux.com
;; This file is free software :)
;;
;;; Commentary:
;;
;; Custom functions
;;
;;  ███████╗██╗   ██╗███╗   ██╗ ██████╗███████╗
;;  ██╔════╝██║   ██║████╗  ██║██╔════╝██╔════╝
;;  █████╗  ██║   ██║██╔██╗ ██║██║     ███████╗
;;  ██╔══╝  ██║   ██║██║╚██╗██║██║     ╚════██║
;;  ██║     ╚██████╔╝██║ ╚████║╚██████╗███████║
;;  ╚═╝      ╚═════╝ ╚═╝  ╚═══╝ ╚═════╝╚══════╝
;;
;;; Code:

(require 'cl-lib)

(require 'constants)
(require 'custom-config)


;; Emacs config

(defvar line-breaker)
(defvar user-email)

(defun various-emacs-config ()
  "Various config for Emacs."

  (setq line-breaker page-delimiter)

  (setq password-cache-expiry      nil
        load-prefer-newer          t
        system-time-locale         "C"
        user-full-name             "Luiz Tagliaferro"
        user-mail-address          "luiz@luiznux.com")

  (setq history-length 100)
  (put 'minibuffer-history 'history-length 50)
  (put 'evil-ex-history 'history-length 50)
  (put 'kill-ring 'history-length 25)

  (global-hl-line-mode))

(defun open-agenda-on-right-buffer ()
  "Open agenda in the right buffer."
  (interactive)
  (org-agenda t "x")
  (other-window (goto-char (point-min))))

;; define function to shutdown emacs server instance
(defun server-shutdown ()
  "Save buffers, Quit, and Shutdown (kill) server."
  (interactive)
  (save-some-buffers)
  (kill-emacs))


;; UI

;; Font
(defun font-installed-p (font-name)
  "Check if font with FONT-NAME is available."
  (find-font (font-spec :name font-name)))

(require 'display-line-numbers)
(defcustom display-line-numbers-exempt-modes
  '(vterm-mode eshell-mode shell-mode term-mode ansi-term-mode org-agenda-mode)
  "Major modes on which to disable line numbers."
  :group 'display-line-numbers
  :type 'list
  :version "green")

(defun display-line-numbers--turn-on ()
  "Turn on line numbers except for certain major modes.
Exempt major modes are defined in `display-line-numbers-exempt-modes'."
  (unless (or (minibufferp)
              (member major-mode display-line-numbers-exempt-modes))
    (display-line-numbers-mode)))


;; File

(defun reload-init-file ()
  "Reload Emacs configurations."
  (interactive)
  (load user-init-file))
(global-set-key (kbd "C-c C-l") #'reload-init-file)

(defun rename-this-file (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (unless filename
      (error "Buffer '%s' is not visiting a file!" name))
    (progn
      (when (file-exists-p filename)
        (rename-file filename new-name 1))
      (set-visited-file-name new-name)
      (rename-buffer new-name))))

(defun delete-this-file ()
  "Delete the current file, and kill the buffer."
  (interactive)
  (unless (buffer-file-name)
    (error "No file is currently being edited"))
  (when (yes-or-no-p (format "Really delete '%s'?"
                             (file-name-nondirectory buffer-file-name)))
    (delete-file (buffer-file-name))
    (kill-this-buffer)))
(global-set-key (kbd "C-x K") #'delete-this-file)

(defun copy-file-name ()
  "Copy the current buffer file name to the clipboard."
  (interactive)
  (if-let ((filename (if (equal major-mode 'dired-mode)
                         default-directory
                       (buffer-file-name))))
      (progn
        (kill-new filename)
        (message "Copied '%s'" filename))
    (warn "Current buffer is not attached to a file!")))

(defun put-current-path-to-clipboard ()
  "Put the current path to clipboard."
  (interactive)
  (let ((file-path buffer-file-name)
        (dir-path default-directory))
    (cond (file-path
           (kill-new (expand-file-name file-path))
           (message "This file path is on the clipboard!"))
          (dir-path
           (kill-new (expand-file-name dir-path))
           (message "This directory path is on the clipboard!"))
          (t
           (error-message-string "Fail to get path name.")))))


;; Buffer

;; Source https://www.simplify.ba/articles/2016/01/25/display-buffer-alist/
(defun sasa/display-buffer (buffer &optional alist)
  "Select window for BUFFER (need to use word ALIST on the first line).
Returns thirth visible window if there are three visible windows, nil otherwise.
Minibuffer is ignored."
  (let ((wnr (if (active-minibuffer-window) 3 2)))
    (when (= (+ wnr 1) (length (window-list)))
      (let ((window (nth wnr (window-list))))
        (set-window-buffer window buffer)
        window))))
(defun sasa/call-help-temp-buffers ()
  "Call the other `sasa/display-buffer' func with args."

  (defvar sasa/help-temp-buffers '("^\\*Flycheck errors\\*$"
                                   "^\\*Completions\\*$"
                                   "^\\*Help\\*$"
                                   ;; Other buffers names...
                                   "^\\*Ido Completions\\*$"
                                   "^\\*Colors\\*$"
                                   "^\\*Async Shell Command\\*$"))
  (while sasa/help-temp-buffers
    (add-to-list 'display-buffer-alist
                 `(,(car sasa/help-temp-buffers)
                   (display-buffer-reuse-window
                    sasa/display-buffer
                    display-buffer-in-side-window)
                   (reusable-frames     . visible)
                   (side                . bottom)
                   (window-height       . 0.2)))
    (setq sasa/help-temp-buffers (cdr sasa/help-temp-buffers))))

(defun revert-this-buffer ()
  "Revert the current buffer."
  (interactive)
  (unless (minibuffer-window-active-p (selected-window))
    (revert-buffer t t)
    (message "Reverted this buffer")))


;; Edit

;; Remove useless whitespace before saving a file
(defun delete-trailing-whitespace-except-current-line ()
  "An alternative to `delete-trailing-whitespace'.
The original function deletes trailing whitespace of the current line."
  (interactive)
  (let ((begin (line-beginning-position))
        (end (line-end-position)))
    (save-excursion
      (when (< (point-min) (1- begin))
        (save-restriction
          (narrow-to-region (point-min) (1- begin))
          (delete-trailing-whitespace)
          (widen)))
      (when (> (point-max) (+ end 2))
        (save-restriction
          (narrow-to-region (+ end 2) (point-max))
          (delete-trailing-whitespace)
          (widen))))))
(defun smart-delete-trailing-whitespace ()
  "Invoke `delete-trailing-whitespace-except-current-line'
on selected major modes only."
  (unless (member major-mode '(diff-mode))
    (delete-trailing-whitespace-except-current-line)))
(add-hook 'before-save-hook #'smart-delete-trailing-whitespace)

(defun do-not-show-trailing-whitespace ()
  "Not show some whitespaces in some modes."
  (dolist (hook '(special-mode-hook
                  term-mode-hook
                  comint-mode-hook
                  vterm-mode-hook
                  compilation-mode-hook
                  minibuffer-inactive-mode-hook
                  minibuffer-setup-hook))

    (add-hook hook (lambda () (setq show-trailing-whitespace nil)))))

(defun set-default-indentation ()
  "Configures the default indentation (4 spaces)."
  (setq-default indent-tabs-mode nil)
  (setq-default tab-width 4)
  (setq indent-line-function 'insert-tab)
  (global-set-key (kbd "RET") 'newline-and-indent))

(defun beginning-of-line++ ()
  "Go to first character on a line."
  (interactive)
  (if (bolp)
	  (back-to-indentation)
	(beginning-of-line)))
(global-set-key (kbd "C-a") 'beginning-of-line++)


;; Config

(defun witch-sys? ()
  "Defime hooks for some sys."
  (with-no-warnings
    (cond
     (sys/win32p
      ;; make PC keyboard's Win key or other to type Super or Hyper
      ;; (setq w32-pass-lwindow-to-system nil)
      (setq w32-lwindow-modifier 'super     ; Left Windows key
            w32-apps-modifier 'hyper)       ; Menu/App key
      (w32-register-hot-key [s-t])
      (encode-mode))

     ;; Compatible with Emacs Mac port
     (sys/mac-port-p
      ;; Keybonds
      (global-set-key [(hyper a)] 'mark-whole-buffer)
      (global-set-key [(hyper v)] 'yank)
      (global-set-key [(hyper c)] 'kill-ring-save)
      (global-set-key [(hyper s)] 'save-buffer)
      (global-set-key [(hyper l)] 'goto-line)
      (global-set-key [(hyper w)]
                      (lambda () (interactive) (delete-window)))
      (global-set-key [(hyper z)] 'undo)

      ;; mac switch meta key
      (defun mac-switch-meta nil
        "switch meta between Option and Command"
        (interactive)
        (if (eq mac-option-modifier nil)
            (progn
	          (setq mac-option-modifier 'meta)
	          (setq mac-command-modifier 'hyper)
	          )
          (progn
            (setq mac-option-modifier nil)
            (setq mac-command-modifier 'meta))))

      ;; alert style for macos
      (setq alert-default-style 'osx-notifier)

      (defun mac-toggle-max-window ()
        "This function toggles the frame-parameter fullscreen,
     so that I can maximise Emacs from within rather than relying
     on the external MacOS controls. "
        (interactive)
        (set-frame-parameter
         nil
         'fullscreen
         (if (frame-parameter nil 'fullscreen)
             nil
           'fullboth))))

     (sys/gnu-linux
      (setq alert-default-style 'libnotify)))))

(defun recompile-elpa ()
  "Recompile packages in elpa directory.  Useful if you switch Emacs versions."
  (interactive)
  (if (fboundp 'async-byte-recompile-directory)
      (async-byte-recompile-directory package-user-dir)
    (byte-recompile-directory package-user-dir 0 t)))

(defun enable-ido-mode ()
  "Enables 'ido-mode'."
  (setq-default ido-enable-flex-matching t)
  (setq-default ido-everyehere t)
  (ido-mode 1))

(defun read-path-variable-from-zshrc ()
  "Read the path variable from zshrc."
  (let ((path (shell-command-to-string ". ~/.zshrc; echo -n $PATH")))
    (setenv "PATH" path)
    (setq exec-path
          (append
           (split-string-and-unquote path ":")
           exec-path))))

(defun get-user-email()
  "Get the user email adress."
  (setq user-email user-mail-address))



(witch-sys?)
(various-emacs-config)
(set-default-indentation)
(enable-ido-mode)
(read-path-variable-from-zshrc)
(sasa/call-help-temp-buffers)
(do-not-show-trailing-whitespace)


(provide 'functions)
;;; functions.el ends here
