;;; basic.el --- Basic emasc config file  -*- lexical-binding: t -*-
;;
;; Author: Luiz Tagliaferro <luiz@luiznux.com>
;; URL: https://luiznux.com
;; This file is free software :)
;;
;;; Commentary:
;;
;; basics Emacs config.
;;
;;  ██████╗  █████╗ ███████╗██╗ ██████╗
;;  ██╔══██╗██╔══██╗██╔════╝██║██╔════╝
;;  ██████╔╝███████║███████╗██║██║
;;  ██╔══██╗██╔══██║╚════██║██║██║
;;  ██████╔╝██║  ██║███████║██║╚██████╗
;;  ╚═════╝ ╚═╝  ╚═╝╚══════╝╚═╝ ╚═════╝
;;
;;; Code:

(require 'constants)
(require 'custom-config)
(require 'functions)

(with-no-warnings
  ;; Garbage Collector Magic Hack
  (use-package gcmh
    :diminish
    :init
    (setq gcmh-idle-delay 5
          gcmh-high-cons-threshold #x1000000) ; 16MB
    (gcmh-mode 1)))

;; Environment
(when (or sys/mac-x-p sys/linux-x-p (daemonp))
  (use-package exec-path-from-shell
    :init
    (setq exec-path-from-shell-variables '("PATH" "MANPATH" "SSH_AUTH_SOCK" "SSH_AGENT_PID" "GPG_AGENT_INFO" "LANG")
          exec-path-from-shell-arguments '("-l"))
    (exec-path-from-shell-initialize)))

;; Increase how much is read from processes in a single chunk (default is 4kb)
(setq read-process-output-max #x10000)  ; 64kb

;; History
(use-package saveplace
  :ensure nil
  :hook (after-init . save-place-mode))

(use-package recentf
  :ensure nil
  :bind (("C-x C-r" . recentf-open-files))
  :hook (after-init . recentf-mode)
  :init (setq recentf-max-saved-items 300
              recentf-exclude
              '("\\.?cache" ".cask" "url" "COMMIT_EDITMSG\\'" "bookmarks"
                "\\.\\(?:gz\\|gif\\|svg\\|png\\|jpe?g\\|bmp\\|xpm\\)$"
                "\\.?ido\\.last$" "\\.revive$" "/G?TAGS$" "/.elfeed/"
                "^/tmp/" "^/var/folders/.+$" "^/ssh:" "/persp-confs/"
                "/org/*" ;; prevent  show recent org-agenda files
                (lambda (file) (file-in-directory-p file package-user-dir))))
  :config
  (push (expand-file-name recentf-save-file) recentf-exclude)
  (add-to-list 'recentf-filename-handlers #'abbreviate-file-name))

(use-package savehist
  :ensure nil
  :hook (after-init . savehist-mode)
  :init (setq enable-recursive-minibuffers t ; Allow commands in minibuffers
              history-length 1000
              savehist-additional-variables '(mark-ring
                                              global-mark-ring
                                              search-ring
                                              regexp-search-ring
                                              extended-command-history)
              savehist-autosave-interval 300))

(use-package simple
  :ensure nil
  :init
  ;; Prettify the process list
  (with-no-warnings
    (define-derived-mode process-menu-mode tabulated-list-mode "Process Menu"
      "Major mode for listing the processes called by Emacs."
      (setq tabulated-list-format `[("" ,(if (icons-displayable-p) 2 0))
                                    ("Process" 25 t)
			                        ("PID"      7 t)
			                        ("Status"   7 t)
                                    ;; 25 is the length of the long standard buffer
                                    ;; name "*Async Shell Command*<10>" (bug#30016)
			                        ("Buffer"  25 t)
			                        ("TTY"     12 t)
			                        ("Thread"  12 t)
			                        ("Command"  0 t)])
      (make-local-variable 'process-menu-query-only)
      (setq tabulated-list-sort-key (cons "Process" nil))
      (add-hook 'tabulated-list-revert-hook 'list-processes--refresh nil t))

    (defun list-processes--refresh ()
      "Recompute the list of processes for the Process List buffer.
Also, delete any process that is exited or signaled."
      (setq tabulated-list-entries nil)
      (dolist (p (process-list))
        (cond ((memq (process-status p) '(exit signal closed))
	           (delete-process p))
	          ((or (not process-menu-query-only)
	               (process-query-on-exit-flag p))
	           (let* ((icon
                       (or
                        (and (icons-displayable-p)
                             (all-the-icons-octicon "zap"
                                                    :height 1.0 :v-adjust -0.05
                                                    :face 'all-the-icons-lblue))
                        ""))
                      (buf (process-buffer p))
		              (type (process-type p))
		              (pid  (if (process-id p) (format "%d" (process-id p)) "--"))
		              (name (process-name p))
                      (status (process-status p))
		              (status `(,(symbol-name status)
                                face ,(if (memq status '(stop exit closed failed))
                                          'error
                                        'success)))
		              (buf-label (if (buffer-live-p buf)
				                     `(,(buffer-name buf)
				                       face link
				                       help-echo ,(format-message
					                               "Visit buffer `%s'"
					                               (buffer-name buf))
				                       follow-link t
				                       process-buffer ,buf
				                       action process-menu-visit-buffer)
			                       "--"))
		              (tty `(,(or (process-tty-name p) "--")
                             face font-lock-doc-face))
		              (thread
                       `(,(cond
                           ((or
                             (null (process-thread p))
                             (not (fboundp 'thread-name))) "--")
                           ((eq (process-thread p) main-thread) "Main")
		                   ((thread-name (process-thread p)))
		                   (t "--"))
                         face font-lock-doc-face))
		              (cmd
		               `(,(if (memq type '(network serial pipe))
		                      (let ((contact (process-contact p t t)))
			                    (if (eq type 'network)
			                        (format "(%s %s)"
				                            (if (plist-get contact :type)
					                            "datagram"
				                              "network")
				                            (if (plist-get contact :server)
					                            (format
                                                 "server on %s"
					                             (if (plist-get contact :host)
                                                     (format "%s:%s"
						                                     (plist-get contact :host)
                                                             (plist-get
                                                              contact :service))
					                               (plist-get contact :local)))
				                              (format "connection to %s:%s"
					                                  (plist-get contact :host)
					                                  (plist-get contact :service))))
			                      (format "(serial port %s%s)"
				                          (or (plist-get contact :port) "?")
				                          (let ((speed (plist-get contact :speed)))
				                            (if speed
					                            (format " at %s b/s" speed)
				                              "")))))
		                    (mapconcat 'identity (process-command p) " "))
                         face completions-annotations)))
	             (push (list p (vector icon name pid status buf-label tty thread cmd))
		               tabulated-list-entries)))))
      (tabulated-list-init-header))))

(when emacs/>=27p
  (use-package so-long
    :ensure nil
    :hook (after-init . global-so-long-mode)
    :config (setq so-long-threshold 400)))

;; Fullscreen
(when (display-graphic-p)
  (bind-keys ("C-<f11>" . toggle-frame-fullscreen)
             ("S-s-<return>" . toggle-frame-fullscreen)
             ("M-S-<return>" . toggle-frame-fullscreen)))


(provide 'basic)
;;; basic.el ends here
