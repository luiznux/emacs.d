;;; init-org-env.el --- Initialize org enviroment vars   -*- lexical-binding: t -*-
;;
;; Author: Luiz Tagliaferro <luiz@luiznux.com>
;; URL: https://luiznux.com
;; This file is free software :)
;;
;;; Commentary:
;;
;;
;;  ███████╗███╗   ██╗██╗   ██╗
;;  ██╔════╝████╗  ██║██║   ██║
;;  █████╗  ██╔██╗ ██║██║   ██║
;;  ██╔══╝  ██║╚██╗██║╚██╗ ██╔╝
;;  ███████╗██║ ╚████║ ╚████╔╝
;;  ╚══════╝╚═╝  ╚═══╝  ╚═══╝
;;
;; My org configs based on the current enviroment, controlled by the
;; variable `luiznux-enviroment-type'
;;
;;; Code:


;; Constants for agenda stuffs

(defconst luiznux-personal-agenda-path
  (concat emacs-org-directory "/personal/"))

(defconst luiznux-general-agenda-path
  (concat emacs-org-directory "/agenda/"))

(defconst luiznux-work-agenda-file
  (concat emacs-org-directory "/work.org"))

(defconst luiznux-birthday-file
  (concat luiznux-general-agenda-path "birthdays.org"))

(defconst luiznux-capture-file
  (concat luiznux-general-agenda-path "capture.org"))

(defconst luiznux-tasks-file
  (concat luiznux-personal-agenda-path "tasks.org"))


;; Functions for templates

(defun custom-template-schedule ()
  "Set schedule or deadline based on user answer."
  (let ((schedule (y-or-n-p "Schedule it ?"))
        (deadline (y-or-n-p "Insert Deadline ?")))
    (when deadline (call-interactively 'org-deadline))
    (when schedule (call-interactively 'org-schedule))))


;; Custom templates and agenda views based on enviroment.

(pcase luiznux-enviroment-type

  ;; Work type enviroment
  ('work
   (setq org-agenda-files            (list emacs-org-directory)
         org-agenda-custom-commands  '(("x" "My Agenda :)"
                                        ((tags-todo
                                          "work"
                                          ((org-agenda-overriding-header        "Work Stuffs  ")
                                           (org-agenda-prefix-format            "%e %(my-agenda-prefix)")
                                           (org-agenda-sorting-strategy         '(category-keep))
                                           (org-tags-match-list-sublevels       t)
                                           (org-enforce-todo-dependencies       t)
                                           (org-agenda-skip-scheduled-if-done   t)
                                           (org-agenda-skip-deadline-if-done    t)
                                           (org-agenda-todo-ignore-scheduled   'all))))))

         org-capture-templates       '(("w" ; key
                                        "          Create Work task" ; description
                                        entry ; type
                                        (file "~/org/work.org") ; target
                                        "* TODO %^{Title} %^g\nSCHEDULED: %^t\n#+description: %^{Description  }\n%?" ; template
                                        :empty-lines-before 2 ; properties
                                        :empty-lines-after  2
                                        :jump-to-captured   t)

                                       ("c"
                                        "          Caputere a new code task"
                                        entry
                                        (file "~/org/capture.org")
                                        "* TODO %^{Title} \nSCHEDULED: %^t\n#+description: %^{Description  }\n:LINK:  %A\n%?"
                                        :empty-lines-before 2
                                        :empty-lines-after  2
                                        :created            t))))

  ;; Personal type of enviroment
  ('personal
   (setq org-agenda-files            (list luiznux-personal-agenda-path luiznux-general-agenda-path)
         org-agenda-custom-commands  '(("x" "My Agenda :)"
                                        ((agenda
                                          ""
                                          ((org-agenda-overriding-header        "My Agenda 📅")
                                           (org-agenda-span                     '3)))

                                         (tags-todo
                                          "+CATEGORY=\"studie \""
                                          ((org-agenda-overriding-header        "Studies  ")
                                           (org-agenda-prefix-format            "%e %(my-agenda-prefix)")
                                           (org-agenda-sorting-strategy         '(category-keep))
                                           (org-tags-match-list-sublevels       t)
                                           (org-enforce-todo-dependencies       t)
                                           (org-agenda-skip-scheduled-if-done   t)
                                           (org-agenda-skip-deadline-if-done    t)
                                           (org-agenda-todo-ignore-scheduled    'all)))

                                         (tags-todo
                                          "+CATEGORY=\"project \""
                                          ((org-agenda-overriding-header        "My Projects  ")
                                           (org-agenda-prefix-format            "%e %(my-agenda-prefix)")
                                           (org-agenda-sorting-strategy         '(category-keep))
                                           (org-tags-match-list-sublevels       t)
                                           (org-enforce-todo-dependencies       t)
                                           (org-agenda-skip-scheduled-if-done   t)
                                           (org-agenda-skip-deadline-if-done    t)
                                           (org-agenda-todo-ignore-scheduled    'all)))))

                                       ("t" "My General Todos"
                                        ((tags-todo
                                          "+CATEGORY=\"task \""
                                          ((org-agenda-overriding-header        "My Tasks  ")
                                           (org-agenda-prefix-format            "%e %(my-agenda-prefix)")
                                           (org-agenda-sorting-strategy         '(category-keep))
                                           (org-tags-match-list-sublevels       t)
                                           (org-enforce-todo-dependencies       t)
                                           (org-agenda-skip-scheduled-if-done   t)
                                           (org-agenda-skip-deadline-if-done    t)
                                           (org-agenda-todo-ignore-scheduled    'all)))))

                                       ("b" "Birthdays Agenda"
                                        ((agenda
                                          ""
                                          ((org-agenda-overriding-header        "Birthdays  ")
                                           (org-agenda-files                    (list luiznux-birthday-file))
                                           (org-agenda-show-all-dates           nil)
                                           (org-agenda-span                     '182) ;; half of a year
                                           (org-agenda-prefix-format            '((agenda . "")))
                                           (org-agenda-sorting-strategy         '(category-keep)))))))

         org-capture-templates       '(("a"
                                        "          Add an event on the agenda calendar"
                                        entry
                                        (file+headline "~/org/agenda/agenda.org" "My TODOs 🍩")
                                        "** %^{Is a todo?|TODO|MEETING|WARNING|} %^{Title}\n#+description: %^{Description  }\n%?"
                                        :prepare-finalize custom-template-schedule
                                        :empty-lines-before 2
                                        :empty-lines-after  2
                                        :jump-to-captured   t)

                                       ("c"
                                        "          Caputere a new code task"
                                        entry
                                        (file luiznux-capture-file)
                                        "* TODO %^{Title} \nSCHEDULED: %^t\n#+description: %^{Description  }\n:LINK:  %A\n%?"
                                        :empty-lines-before 2
                                        :empty-lines-after  2
                                        :created            t)

                                       ("t"
                                        "          Create a new general task"
                                        entry
                                        (file luiznux-tasks-file)
                                        "* TODO %^{Title}\n#+description: %^{Description  }\n%?"
                                        :empty-lines-before 2
                                        :empty-lines-after  2
                                        :jump-to-captured   t
                                        :created            t))))

  ;; nil is the default value
  ('nil
   (setq org-agenda-files             (list emacs-org-directory )
         org-agenda-custom-commands   '(("x" "My Agenda :)"
                                         ((agenda
                                           ""
                                           ((org-agenda-overriding-header        "My Agenda 📅")
                                            (org-agenda-remove-tags              t)
                                            (org-agenda-span                     '2)))))))))


(provide 'init-org-env)
;;; init-org-env.el ends here
