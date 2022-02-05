;;; file-color-agenda.el --- Package for fancy agenda UI  -*- lexical-binding: t -*-
;;
;; Author: Luiz Tagliaferro <luiz@luiznux.com>
;; URL: https://luiznux.com
;; This file is free software :)
;;
;;; Commentary:
;;
;; This package simply apply a background and foreground color
;; to a specific headers(that are defined above).
;;
;; Also, it is not made by myself(I only done some mods for work as want),
;; source: https://llazarek.github.io/2018/07/improving-the-agenda.html
;;
;;; Code:

(declare-function reduce "reduce")

;; Helper definitions
(setq ll/org/agenda-todo-words
      '("work  " "coll 🎓 " "project  " "agenda  " "habit 🍩 " "bday  " "cap  " "task  " "conta 💸 "
        "Work Stuffs 🖥 " "College" "My Projects  " "My Tasks  " "➔" "╰→"))

(defun find-in-line (needle &optional beginning count)
  "Find the position of the start of NEEDLE in the current line.
If BEGINNING is non-nil, find the beginning of NEEDLE in the
current line.  If COUNT is non-nil, find the COUNT'th occurrence
from the left."

  (save-excursion
    (beginning-of-line)

    (let ((found (re-search-forward needle (point-at-eol) t count)))
      (if beginning
          (match-beginning 0)
        found))))

(defun ll/org/agenda/find-todo-word-end ()
  "Find TODO keyword on the end."

  (reduce (lambda (a b) (or a b))
          (mapcar #'find-in-line ll/org/agenda-todo-words)))


(defun ll/org/agenda/color-headers-with (tag col col2)
  "Color agenda lines matching TAG with color COL(foreground) and COL2(background)."
  (interactive)
  (goto-char (point-min))
  (while (re-search-forward tag nil t)
                                        ;(Unless (find-in-line "\\[#[A-Z]\\]")
    (let ((todo-end (or (ll/org/agenda/find-todo-word-end)
                        (point-at-bol)))
          (tags-beginning (or (find-in-line tag t)
                              (point-at-eol))))
      (add-text-properties todo-end
                           tags-beginning
                           `(face (:foreground ,col :background ,col2 :weight bold))))))

(defun ll/org/colorize-headings ()
  "Color all headings with :pers: colors."

  (ll/org/agenda/color-headers-with "work  " "#2d2d2d" "#FA74B2")
  (ll/org/agenda/color-headers-with "coll 🎓 " "#2d2d2d" "#c792ea")
  (ll/org/agenda/color-headers-with "project  " "#2d2d2d" "#839ce4")
  (ll/org/agenda/color-headers-with "agenda  " "#2d2d2d" "#da8548")
  (ll/org/agenda/color-headers-with "habit 🍩 " "#c8b6ff" "#655bc2")
  (ll/org/agenda/color-headers-with "bday  " "#2d2d2d" "#89ddff")
  (ll/org/agenda/color-headers-with "cap  " "#2d2d2d" "#c3e88d")
  (ll/org/agenda/color-headers-with "task  " "#2d2d2d" "#EBCB8B")
  (ll/org/agenda/color-headers-with "conta 💸" "#242D35" "#ae3d46")
  (ll/org/agenda/color-headers-with "Work Stuffs 🖥 " "#2d2d2d" "#6d8dad")
  (ll/org/agenda/color-headers-with "College " "#2d2d2d" "#c792ea")
  (ll/org/agenda/color-headers-with "My Projects  " "#2d2d2d" "#839ce4")
  (ll/org/agenda/color-headers-with "My Tasks  " "#2d2d2d" "#EBCB8B")
  (ll/org/agenda/color-headers-with "➔" "#b58900" nil)
  (ll/org/agenda/color-headers-with "╰→" "#a9a1e1" nil))


(add-hook 'org-agenda-finalize-hook #'ll/org/colorize-headings)

;; Local Variables:
;; byte-compile-warnings: (not free-vars noruntime)
;; End:

(provide 'file-color-agenda)
;;; file-color-agenda.el ends here
