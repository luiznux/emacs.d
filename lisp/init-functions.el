;;; init-functions.el --- Define functions.	-*- lexical-binding: t -*-
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
(require 'color)
(require 'org)

;; Suppress warnings
(eval-when-compile
  (require 'init-constants)
  (require 'init-custom))

(defvar line-breaker)
(defvar user-email)

(declare-function browse-url-interactive-arg "browse-url")
(declare-function nerd-icons-install-fonts "ext:nerd-icons")
(declare-function xwidget-webkit-current-session "xwidget")
(declare-function xwidget-buffer "xwidget")
(declare-function ivy-read "ivy")


;; UI

(defvar after-load-theme-hook nil
  "Hook run after a color theme is loaded using `load-theme'.")
(defun run-after-load-theme-hook (&rest _)
  "Run `after-load-theme-hook'."
  (run-hooks 'after-load-theme-hook))
(advice-add #'load-theme :after #'run-after-load-theme-hook)

(defun childframe-workable-p ()
  "Whether childframe is workable."
  (not (or noninteractive
           emacs-basic-display
           (not (display-graphic-p)))))


;; Font

(defun font-installed-p (font-name)
  "Check if font with FONT-NAME is available."
  (find-font (font-spec :name font-name)))

(defun emacs-install-fonts ()
  "Install necessary fonts."
  (interactive)
  (nerd-icons-install-fonts))

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

;; WORKAROUND: fix blank screen issue on macOS.
(defun fix-fullscreen-cocoa ()
  "Address blank screen issue with child-frame in fullscreen.
This issue has been addressed in 28."
  (and sys/mac-cocoa-p
       (not emacs/>=28p)
       (bound-and-true-p ns-use-native-fullscreen)
       (setq ns-use-native-fullscreen nil)))


;; File

(defun reload-init-file ()
  "Reload Emacs configurations."
  (interactive)
  (load user-init-file))
(global-set-key (kbd "C-c C-l") #'reload-init-file)

;; Browse the homepage
(defun browse-homepage ()
  "Browse the Github page of Centaur Emacs."
  (interactive)
  (browse-url luiznux-homepage))

;; Open custom file
(defun find-custom-file()
  "Open option `custom-file'."
  (interactive)
  (unless (file-exists-p custom-file)
    (user-error "The custom file doesn't exist"))
  (find-file custom-file))

(defun open-folder-in-system-file-manager()
  "Open the current folder into OS file manager.
Create a process thats jump into System's default file manager.
Only works in buffers that are related with files"
  (interactive)
  (let ((name "os file manager")
        (process-connection-type nil)
        (command (if (not sys/linuxp) "open" "xdg-open"))
        (dir (url-file-directory buffer-file-name)))

    (if (eq dir "")
        (error "Buffer '%s' is not a file!" (buffer-name))
      (start-process name nil command dir))))

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
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (if filename
        (progn
          (kill-new filename)
          (message "Copied '%s'" filename))
      (warn "Current buffer is not attached to a file!"))))

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
  "Invoke `delete-trailing-whitespace-except-current-line';
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
(add-hook 'after-init-hook #'do-not-show-trailing-whitespace)


;; Update

(defun update-packages ()
  "Refresh package contents and update all packages."
  (interactive)
  (message "Updating packages...")
  (package-upgrade-all)
  (message "Updating packages...done"))
(defalias 'emacs-update-packages #'update-packages)


;; Org

;; Refile settings
;; Exclude DONE state tasks from refile targets
(defun bh/verify-refile-target ()
  "Exclude todo keywords with a done state from refile targets."
  (not (member (nth 2 (org-heading-components)) org-done-keywords)))

(defun luiznux/org-paste-clipboard-image ()
  "Paste a clipboard image."
  (interactive)
  (if (executable-find "pngpaste")
      (let ((image-file (concat temporary-file-directory
                                (make-temp-name "org-image-paste-")
                                ".png")))
        (call-process-shell-command (concat "pngpaste " image-file))
        (insert (concat  "#+CAPTION: " (read-string "Caption: ") "\n"))
        (insert (format "[[file:%s]]" image-file))
        (org-display-inline-images))
    (message "Requires pngpaste in PATH")))

(defun unpackaged/org-fix-blank-lines (&optional prefix)
  "Ensure that blank lines exist between headings.
and between headings and their contents.
With PREFIX, operate on whole buffer. Ensures that blank lines
exist after each headings's drawers."
  (interactive "P")
  (org-map-entries (lambda ()
                     (org-with-wide-buffer
                      ;; `org-map-entries' narrows the buffer, which prevents us from seeing
                      ;; newlines before the current heading, so we do this part widened.
                      (while (not (looking-back "\n\n" nil))
                        ;; Insert blank lines before heading.
                        (insert "\n")))
                     (let ((end (org-entry-end-position)))
                       ;; Insert blank lines before entry content
                       (forward-line)
                       (while (and (org-at-planning-p)
                                   (< (point) (point-max)))
                         ;; Skip planning lines
                         (forward-line))
                       (while (re-search-forward org-drawer-regexp end t)
                         ;; Skip drawers. You might think that `org-at-drawer-p' would suffice, but
                         ;; for some reason it doesn't work correctly when operating on hidden text.
                         ;; This works, taken from `org-agenda-get-some-entry-text'.
                         (re-search-forward "^[ \t]*:END:.*\n?" end t)
                         (goto-char (match-end 0)))
                       (unless (or (= (point) (point-max))
                                   (org-at-heading-p)
                                   (looking-at-p "\n"))
                         (insert "\n"))))
                   t (if prefix
                         nil
                       'tree)))


;; Misc

(defun create-scratch-buffer ()
  "Create a scratch buffer."
  (interactive)
  (switch-to-buffer (get-buffer-create "*scratch*"))
  (lisp-interaction-mode))

(defun byte-compile-elpa ()
  "Compile packages in elpa directory.  Useful if you switch Emacs versions."
  (interactive)
  (if (fboundp 'async-byte-recompile-directory)
      (async-byte-recompile-directory package-user-dir)
    (byte-recompile-directory package-user-dir 0 t)))

(defun byte-compile-site-lisp ()
  "Compile packages in site-lisp directory."
  (interactive)
  (let ((dir (locate-user-emacs-file "site-lisp")))
    (if (fboundp 'async-byte-recompile-directory)
        (async-byte-recompile-directory dir)
      (byte-recompile-directory dir 0 t))))

(defun native-compile-elpa ()
  "Native-compile packages in elpa directory."
  (interactive)
  (if (fboundp 'native-compile-async)
      (native-compile-async package-user-dir t)))

(defun native-compile-site-lisp ()
  "Native compile packages in site-lisp directory."
  (interactive)
  (let ((dir (locate-user-emacs-file "site-lisp")))
    (if (fboundp 'native-compile-async)
        (native-compile-async dir t))))

(defun icons-displayable-p ()
  "Return non-nil if icons are displayable."
  (and emacs-icon
       (or (featurep 'nerd-icons)
           (require 'nerd-icons nil t))))

(defun emacs-treesit-available-p ()
  "Check whether tree-sitter is available.
Native tree-sitter is introduced since 29.1."
  (and (fboundp 'treesit-available-p)
       (treesit-available-p)))

(defun too-long-file-p ()
  "Check whether the file is too long."
  (or (> (buffer-size) 100000)
      (and (fboundp 'buffer-line-statistics)
           (> (car (buffer-line-statistics)) 10000))))

(define-minor-mode emacs-read-mode
  "Minor Mode for better reading experience."
  :init-value nil
  :group luiznux
  (if emacs-read-mode
      (progn
        (and (fboundp 'olivetti-mode) (olivetti-mode 1))
        (and (fboundp 'mixed-pitch-mode) (mixed-pitch-mode 1))
        (text-scale-set +1))
    (progn
      (and (fboundp 'olivetti-mode) (olivetti-mode -1))
      (and (fboundp 'mixed-pitch-mode) (mixed-pitch-mode -1))
      (text-scale-set 0))))

;; define function to shutdown emacs server instance
(defun server-shutdown ()
  "Save buffers, Quit, and Shutdown (kill) server."
  (interactive)
  (save-some-buffers)
  (kill-emacs))

(defun get-user-email()
  "Get the user email adress."
  (setq user-email user-mail-address))

(defun enable-ido-mode ()
  "Enables `ido-mode'."
  (setq-default ido-enable-flex-matching t)
  (setq-default ido-everyehere t)
  (ido-mode 1))
;;(add-hook 'after-init-hook #'enable-ido-mode)

(defun read-path-variable-from-zshrc ()
  "Read the path variable from zshrc."
  (interactive)
  (let ((path (shell-command-to-string ". ~/.zshrc; echo -n $PATH")))
    (setenv "PATH" path)
    (setq exec-path
          (append
           (split-string-and-unquote path ":")
           exec-path))))

(defun custom-set-variable (variable value &optional no-save)
  "Set the VARIABLE to VALUE, and return VALUE.

  Save to option `custom-file' if NO-SAVE is nil."
  (customize-set-variable variable value)
  (when (and (not no-save)
             (file-writable-p custom-file))
    (with-temp-buffer
      (insert-file-contents custom-file)
      (goto-char (point-min))
      (while (re-search-forward
              (format "^[\t ]*[;]*[\t ]*(setq %s .*)" variable)
              nil t)
        (replace-match (format "(setq %s '%s)" variable value) nil nil))
      (write-region nil nil custom-file)
      (message "Saved %s (%s) to %s" variable value custom-file))))

;; Package repository (ELPA)
(defun set-package-archives (archives &optional refresh async no-save)
  "Set the package ARCHIVES (ELPA).

REFRESH is non-nil, will refresh archive contents.
ASYNC specifies whether to perform the downloads in the background.
Save to option `custom-file' if NO-SAVE is nil."
  (interactive
   (list
    (intern
     (ivy-read "Select package archives: "
               (mapcar #'car luiznux-package-archives-alist)
               :preselect (symbol-name luiznux-package-archives)))))
  ;; Set option
  (custom-set-variable 'luiznux-package-archives archives no-save)

  ;; Refresh if need
  (and refresh (package-refresh-contents async))

  (message "Set package archives to `%s'" archives))
(defalias 'luiznux-set-package-archives #'set-package-archives)

;; Browse URL
(defun custom-webkit-browse-url (url &optional pop-buffer new-session)
  "Browse URL with xwidget-webkit' and switch or pop to the buffer.

POP-BUFFER specifies whether to pop to the buffer.
NEW-SESSION specifies whether to create a new xwidget-webkit session."
  (interactive (progn
                 (require 'browse-url)
                 (browse-url-interactive-arg "xwidget-webkit URL: ")))
  (or (featurep 'xwidget-internal)
      (user-error "Your Emacs was not compiled with xwidgets support"))

  (xwidget-webkit-browse-url url new-session)
  (let ((buf (xwidget-buffer (xwidget-webkit-current-session))))
    (when (buffer-live-p buf)
      (and (eq buf (current-buffer)) (quit-window))
      (if pop-buffer
          (pop-to-buffer buf)
        (switch-to-buffer buf)))))

;; CSV
(defun csv-highlight (&optional separator)
  "Set colors for columns on `csv-mode' using SEPARATOR."
  (interactive (list (when current-prefix-arg (read-char "Separator: "))))
  (font-lock-mode 1)
  (let* ((separator (or separator ?\, ?\;))
         (n (count-matches (string separator) (line-beginning-position) (line-end-position)))
         (colors (cl-loop for i from 0 to 1.0 by (/ 2.0 n)
                          collect (apply #'color-rgb-to-hex
                                         (color-hsl-to-rgb i 0.3 0.5)))))
    (cl-loop for i from 2 to n by 2
             for c in colors
             for r = (format "^\\([^%c\n]+%c\\)\\{%d\\}" separator separator i)
             do (font-lock-add-keywords nil `((,r (1 '(face (:foreground ,c)))))))))




(provide 'init-functions)
;;; init-functions.el ends here
