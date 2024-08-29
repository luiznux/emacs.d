;;; init-custom.el --- Custom configuration file  -*- lexical-binding: t -*-
;;
;; Author: Luiz Tagliaferro <luiz@luiznux.com>
;; URL: https://luiznux.com
;; This file is free software :)
;;
;;; Commentary:
;;
;; Custom configs
;;
;;   ██████╗██╗   ██╗███████╗████████╗ ██████╗ ███╗   ███╗
;;  ██╔════╝██║   ██║██╔════╝╚══██╔══╝██╔═══██╗████╗ ████║
;;  ██║     ██║   ██║███████╗   ██║   ██║   ██║██╔████╔██║
;;  ██║     ██║   ██║╚════██║   ██║   ██║   ██║██║╚██╔╝██║
;;  ╚██████╗╚██████╔╝███████║   ██║   ╚██████╔╝██║ ╚═╝ ██║
;;   ╚═════╝ ╚═════╝ ╚══════╝   ╚═╝    ╚═════╝ ╚═╝     ╚═╝
;;
;;; Code:

(eval-when-compile
  (require 'package))

(defgroup luiznux nil
  "Luiznux Emacs customization."
  :group 'convenience
  :link '(url-link :tag "Homepage" "https://github.com/luiznux/emacs.d"))

(defcustom luiznux-logo (expand-file-name "logo.png" user-emacs-directory)
  "Set My Custom logo,  nil means official logo."
  :group 'luiznux
  :type 'string)

(defcustom luiznux-server t
  "Enable `server-mode' or not."
  :group 'luiznux
  :type 'boolean)

(defcustom luiznux-enviroment-type nil
  "Set the type of the current enviroment."
  :group 'luiznux
  :type '(choice (const :tag "Work Enviroment" work)
                 (const :tag "Personal Enviroment" personal)
                 (const :tag "None" nil)))

;; Emacs Lisp Package Archive (ELPA)
(defcustom luiznux-package-archives-alist
  '((default  . (("gnu"           . "https://elpa.gnu.org/packages/")
                 ("nongnu"        . "https://elpa.nongnu.org/nongnu/")
                 ("melpa"         . "https://melpa.org/packages/")
                 ("melpa-stable"  . "https://stable.melpa.org/packages/")))

    (melpa    . (("gnu"           . "https://elpa.gnu.org/packages/")
                 ("nongnu"        . "https://elpa.nongnu.org/nongnu/")
                 ("melpa"         . "https://melpa.org/packages/"))))
  "A list of the package archives."
  :group 'luiznux
  :type '(alist :key-type (symbol :tag "Archive group name")
                :value-type (alist :key-type (string :tag "Archive name")
                                   :value-type (string :tag "URL or directory name"))))

(defcustom luiznux-package-archives 'default
  "Set package archives from which to fetch."
  :group 'luiznux
  :set (lambda (symbol value)
         (set symbol value)
         (setq package-archives
               (or (alist-get value luiznux-package-archives-alist)
                   (error "Unknown package archives: `%s'" value))))
  :type `(choice ,@(mapcar
                    (lambda (item)
                      (let ((name (car item)))
                        (list 'const
                              :tag (capitalize (symbol-name name))
                              name)))
                    luiznux-package-archives-alist)))

(defcustom lsp-format-on-save nil
  "Auto format buffers on save."
  :group 'luiznux
  :type 'boolean)

(defcustom lsp-format-on-save-ignore-modes
  '(c-mode c++-mode python-mode markdown-mode)
  "The modes that don't auto format and organize imports while saving the buffers.
`prog-mode' means ignoring all derived modes."
  :group 'luiznux
  :type '(repeat (symbol :tag "Major-Mode")))

(defcustom centaur-tabs-font-size
  (if (display-graphic-p)
      (- (face-attribute 'default :height) 27)
    30)
  "Font size choice for `centaur-tabs'.
The default value in inherit from the `face-attribute' minus 27"
  :group 'luiznux
  :type 'int)

(defcustom doom-modeline-font-size
  (if (display-graphic-p)
      (- (face-attribute 'default :height) 26)
    20)
  "Font size choice for `doom-modeline'.
The default value in inherit from the `face-attribute' minus 26"
  :group 'luiznux
  :type 'int)

(defcustom fancy-modeline nil
  "Enable fancy stuffs in mode line or not.
Like `nyan-mode' and `parrot-mode'"
  :group 'luiznux
  :type 'boolean)

(defcustom open-agenda-with-dashboard nil
  "Enable open `org-agenda' on right of `dashboard'."
  :group 'luiznux
  :type 'boolean)

(defcustom emacs-icon t
  "Display icons or not."
  :group 'luiznux
  :type 'boolean)

(defcustom emacs-emojify (or (display-graphic-p) (daemonp))
  "Enable `emojify' or not."
  :group 'luiznux
  :type 'boolean)

(defcustom emacs-org-directory (expand-file-name "~/org")
  "Set org directory."
  :group 'luiznux
  :type 'string)

(defcustom emacs-org-roam-directory (expand-file-name "~/org/roam")
  "Set org roam directory."
  :group 'luiznux
  :type 'string)

(defcustom emacs-xwidget-internal nil
  "Enable using `xwidget-webkit' or not."
  :group 'luiznux
  :type 'boolean)

(defcustom emacs-parsing-system 'tree-sitter
  "Set parsing system program."
  :group 'luiznux
  :type '(choice (const :tag "built-in treesit package" treesit)
                 (const :tag "3rd party tree-sitter package" tree-sitter)
                 (const :tag "no parsing system" nil)))

;; source: https://github.com/seagle0128/.emacs.d
(defcustom custom-prettify-symbols-alist
  '(("lambda" . ?λ)
    ("<-"     . ?←)
    ("->"     . ?→)
    ("->>"    . ?↠)
    ("=>"     . ?⇒)
    ("map"    . ?↦)
    ("/="     . ?≠)
    ("!="     . ?≠)
    ("=="     . ?≡)
    ("<="     . ?≤)
    (">="     . ?≥)
    ("=<<"    . (?= (Br . Bl) ?≪))
    (">>="    . (?≫ (Br . Bl) ?=))
    ("<=<"    . ?↢)
    (">=>"    . ?↣)
    ("&&"     . ?∧)
    ("||"     . ?∨)
    ("not"    . ?¬))
  "A list of symbol prettifications.
Nil to use font supports ligatures."
  :group 'luiznux
  :type '(alist :key-type string :value-type (choice character sexp)))

(defcustom custom-prettify-org-symbols-alist
  '(("[ ]"             . ?)
    ("[-]"             . ?)
    ("[X]"             . ?)

    ("#+ARCHIVE:"      . ?📦)
    ("#+archive:"      . ?📦)

    ("#+AUTHOR:"       . ?👤)
    ("#+author:"       . ?👤)

    ("#+DESCRIPTION:"  . "")
    ("#+description:"  . "")

    ("#+EMAIL:"         . ?📧)
    ("#+email:"         . ?📧)
    ("#+CREATOR:"       . ?💁)
    ("#+DATE:"          . ?📆)
    ("#+LAST_MODIFIED:" . "")

    ("#+OPTIONS:"      . "")
    ("#+SETUPFILE:"    . ?⚒)
    ("#+TAGS:"         . "")
    ("#+TITLE:"        . ?📓)
    ("#+title:"        . ?📓)
    ("#+STARTUP:"      . "⏻")
    ("#+FILETAGS:"     . "")
    ("#+filetags:"     . "")
    ("#+CATEGORY:"     . "")

    (":WILD_NOTIFIER_NOTIFY_BEFORE:" . "")

    (":PROPERTIES:"    . "")
    (":END:"           . "🔚")
    (":STYLE:"         . "")
    (":LOG:"           . "")
    (":ID:"            . "")
    (":LINK:"          . "")

    ("SCHEDULED:"      . "")
    ("DEADLINE:"       . ?❗)
    ("CLOSED:"         . ?✅)

    ("#+BEGIN_SRC"     . "")
    ("#+begin_src"     . "")
    ("#+END_SRC"       . "")
    ("#+end_src"       . "")

    ("#+BEGIN_QUOTE"   . ?»)
    ("#+END_QUOTE"     . ?«)
    ("#+begin_quote"   . ?»)
    ("#+end_quote"     . ?«)

    ("#+HEADERS"       . ?)
    ("#+RESULTS:"      . ?💻))
  "A list of symbol prettifications for `org-mode'."
  :group 'luiznux
  :type '(alist :key-type string :value-type (choice character sexp)))

;; Load `custom-file'
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))


(provide 'init-custom)
;;; init-custom.el ends here
