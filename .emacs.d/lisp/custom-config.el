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

;; source: https://github.com/seagle0128/.emacs.d
(defcustom centaur-prettify-symbols-alist
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
  :group 'centaur
  :type '(alist :key-type string :value-type (choice character sexp)))

(defcustom centaur-prettify-org-symbols-alist
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
    ("#+STARTUP:" . ?⏻)
    ("#+FILETAGS:" . ?📘)
    ("#+CATEGORY:" . ?)
    (":PROPERTIES:" . ?⚙ )
    (":WILD_NOTIFIER_NOTIFY_BEFORE:" . ?)

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
  :group 'centaur
  :type '(alist :key-type string :value-type (choice character sexp)))

;; Load `custom-file'
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

(provide 'custom-config)
