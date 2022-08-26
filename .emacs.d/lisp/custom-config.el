;;; custom-config.el --- Custom configuration file  -*- lexical-binding: t -*-
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

(defcustom lsp-format-on-save-ignore-modes
  '(c-mode c++-mode python-mode markdown-mode)
  "The modes that don't auto format and organize imports while saving the buffers.
`prog-mode' means ignoring all derived modes."
  :group 'luiznux
  :type '(repeat (symbol :tag "Major-Mode")))

(defcustom centaur-tabs-font-size
  (- (face-attribute 'default :height) 20)
  "Font size choice for `centaur-tabs'.
The default value in inherit from the `face-attribute' minus 20"
  :group 'luiznux
  :type 'int)

(defcustom doom-modeline-font-size
  (- (face-attribute 'default :height) 15)
  "Font size choice for `doom-modeline'.
The default value in inherit from the `face-attribute' minus 20"
  :group 'luiznux
  :type 'int)

(defcustom fancy-modeline t
  "Enable fancy stuffs in mode line or not.
Like `nyan-mode' and `parrot-mode'"
  :group 'luiznux
  :type 'boolean)

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

    ("#+EMAIL:"        . ?📧)
    ("#+email:"        . ?📧)
    ("#+CREATOR:"      . ?💁)
    ("#+DATE:"         . ?📆)

    ("#+OPTIONS:"      . ?⛭)
    ("#+SETUPFILE:"    . ?⚒)
    ("#+TAGS:"         . "")
    ("#+TITLE:"        . ?📓)
    ("#+title:"        . ?📓)
    ("#+STARTUP:"      . "⏻")
    ("#+FILETAGS:"     . "")
    ("#+CATEGORY:"     . "")

    (":WILD_NOTIFIER_NOTIFY_BEFORE:" . "")

    (":PROPERTIES:"    . "")
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

    ("#+HEADERS"       . ?☰)
    ("#+RESULTS:"      . ?💻))
  "A list of symbol prettifications for `org-mode'."
  :group 'luiznux
  :type '(alist :key-type string :value-type (choice character sexp)))

;; Load `custom-file'
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

(provide 'custom-config)
;;; custom-config.el ends here
