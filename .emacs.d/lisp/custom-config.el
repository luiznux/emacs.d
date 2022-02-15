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
  "Centaur Emacs customization."
  :group 'convenience
  :link '(url-link :tag "Homepage" "https://github.com/luiznux/emacs.d"))

(defcustom luiznux-logo (expand-file-name
                         (if (display-graphic-p) "logo.png")
                         user-emacs-directory)
  "Set My Custom logo,  nil means official logo."
  :group 'luiznux
  :type 'string)

(defcustom luiznux-server t
  "Enable `server-mode' or not."
  :group 'luiznux
  :type 'boolean)

;; source: https://github.com/seagle0128/.emacs.d
(defcustom custom-prettify-symbols-alist
  '(("lambda" . ?λ)
    ("<-" . ?←)
    ("->" . ?→)
    ("->>" . ?↠)
    ("=>" . ?⇒)
    ("map" . ?↦)
    ("/=" . ?≠)
    ("!=" . ?≠)
    ("==" . ?≡)
    ("<=" . ?≤)
    (">=" . ?≥)
    ("=<<" . (?= (Br . Bl) ?≪))
    (">>=" . (?≫ (Br . Bl) ?=))
    ("<=<" . ?↢)
    (">=>" . ?↣)
    ("&&" . ?∧)
    ("||" . ?∨)
    ("not" . ?¬))
  "Alist of symbol prettifications.
Nil to use font supports ligatures."
  :group 'luiznux
  :type '(alist :key-type string :value-type (choice character sexp)))

(defcustom custom-prettify-org-symbols-alist
  '(("[ ]" . ?☐)
    ("[X]" . ?☑)
    ("[-]" . ?⛝)

    ("#+ARCHIVE:" . ?📦)
    ("#+archive:" . ?📦)

    ("#+AUTHOR:" . ?👤)
    ("#+author:" . ?👤)

    ("#+DESCRIPTION:" . ?⸙)
    ("#+description:" . ?⸙)

    ("#+EMAIL:" . ?📧)
    ("#+email:" . ?📧)

    ("#+CREATOR:" . ?💁)
    ("#+DATE:" . ?📆)

    ("#+OPTIONS:" . ?⛭)
    ("#+SETUPFILE:" . ?⛮)
    ("#+TAGS:" . ?🏷)
    ("#+TITLE:" . ?📓)
    ("#+title:" . ?📓)
    ("#+STARTUP:" . ?⏻)
    ("#+FILETAGS:" . ?)
    ("#+CATEGORY:" . ?)
    (":PROPERTIES:" . ?⚙)
    (":LOG:" . ?)
    (":WILD_NOTIFIER_NOTIFY_BEFORE:" . ?)
    (":ID:" . ?)

    ("#+BEGIN_SRC" . ?✎)
    ("#+END_SRC" . ?□)
    ("#+begin_src" . ?✎)
    ("#+end_src" . ?□)

    ("#+BEGIN_QUOTE" . ?»)
    ("#+END_QUOTE" . ?«)
    ("#+begin_quote" . ?»)
    ("#+end_quote" . ?«)

    ("#+HEADERS" . ?☰)
    ("#+RESULTS:" . ?💻))
  "Alist of symbol prettifications for `org-mode'."
  :group 'luiznux
  :type '(alist :key-type string :value-type (choice character sexp)))

;; Load `custom-file'
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

(provide 'custom-config)
;;; custom-config.el ends here
