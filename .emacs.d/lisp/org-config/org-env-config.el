;;; org-env-config.el --- Emacs org agenda config  -*- lexical-binding: t no-byte-compile: t -*-
;;
;; Author: Luiz Tagliaferro <luiz@luiznux.com>
;; URL: https://luiznux.com
;; This file is free software :)
;;
;;; Commentary:
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

(require 'org)
(require 'org-agenda)
(require 'org-capture)
(require 'custom-config)


(defconst luiznux-personal-agenda
  (concat org-directory "/personal/"))

(defconst luiznux-work-agenda
  (concat org-directory "/work/work.org"))

(defconst luiznux-general-agenda
  (concat org-directory "/agenda/"))

(defconst luiznux-birthday-agenda
  (concat luiznux-general-agenda "birthdays.org"))


(pcase luiznux-enviroment-type

  ;; Work type enviroment
  ('work
   (setq org-agenda-files            (list luiznux-work-agenda luiznux-general-agenda)
         org-agenda-custom-commands  '(("x" "My Agenda :)"
                                        ((agenda
                                          ""
                                          ((org-agenda-overriding-header        "My Agenda 📅")
                                           (org-agenda-span                     '2)))

                                         (tags-todo
                                          "work"
                                          ((org-agenda-overriding-header        "Work Stuffs  ")
                                           (org-agenda-prefix-format            "%e %(my-agenda-prefix)")
                                           (org-agenda-sorting-strategy         '(category-keep))
                                           (org-tags-match-list-sublevels       t)
                                           (org-enforce-todo-dependencies       t)
                                           (org-agenda-skip-scheduled-if-done   t)
                                           (org-agenda-skip-deadline-if-done    t)
                                           (org-agenda-todo-ignore-scheduled   'all))))
                                        )

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
                                           (org-agenda-todo-ignore-scheduled    'all))))
                                        )

                                       ("b" "Birthdays Agenda"
                                        ((agenda
                                          ""
                                          ((org-agenda-overriding-header        "Birthdays  ")
                                           (org-agenda-files                    (list luiznux-birthday-agenda))
                                           (org-agenda-show-all-dates           nil)
                                           (org-agenda-span                     '182) ;; half of a year
                                           (org-agenda-prefix-format            '((agenda . "")))
                                           (org-agenda-sorting-strategy         '(category-keep)))))
                                        )
                                       )

         org-capture-templates       '(("w"               ; key
                                        "          Create Work task"  ; description
                                        entry               ; type
                                        (file "~/org/work/work.org") ; target
                                        "* TODO %^{Title} %^g\nSCHEDULED: %^t\n#+description: %^{Description 🖋 }\n%?" ; template
                                        :empty-lines-before 2 ; properties
                                        :empty-lines-after  2
                                        :jump-to-captured   t)

                                       ("a"
                                        "          Add an event on the agenda calendar"
                                        entry
                                        (file+headline "~/org/agenda/agenda.org" "My TODOs 🍩")
                                        "** %^{Is it a todo?|TODO|MEETING|WARNING} %^{Title}\nSCHEDULED: %^t\n#+description: %^{Description 🖋 }\n%?"
                                        :empty-lines-before 2
                                        :empty-lines-after  2
                                        :jump-to-captured   t)

                                       ("c"
                                        "          Caputere a new code task"
                                        entry
                                        (file "~/org/agenda/capture.org")
                                        "* TODO %^{Title} \nSCHEDULED: %^t\n#+description: %^{Description 🖋 }\n:LINK:  %A\n%?"
                                        :empty-lines-before 2
                                        :empty-lines-after  2
                                        :created            t))
         )
   )

  ;; Personal type of enviroment
  ('personal
   (setq org-agenda-files            (list luiznux-personal-agenda luiznux-general-agenda)
         org-agenda-custom-commands  '(("x" "My Agenda :)"
                                        ((agenda
                                          ""
                                          ((org-agenda-overriding-header        "My Agenda 📅")
                                           (org-agenda-span                     '2)))

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
                                           (org-agenda-todo-ignore-scheduled    'all))))
                                        )

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
                                           (org-agenda-todo-ignore-scheduled    'all))))
                                        )

                                       ("b" "Birthdays Agenda"
                                        ((agenda
                                          ""
                                          ((org-agenda-overriding-header        "Birthdays  ")
                                           (org-agenda-files                    (list luiznux-birthday-agenda))
                                           (org-agenda-show-all-dates           nil)
                                           (org-agenda-span                     '182) ;; half of a year
                                           (org-agenda-prefix-format            '((agenda . "")))
                                           (org-agenda-sorting-strategy         '(category-keep)))))
                                        )
                                       )

         org-capture-templates       '(("a"
                                        "          Add an event on the agenda calendar"
                                        entry
                                        (file+headline "~/org/agenda/agenda.org" "My TODOs 🍩")
                                        "** %^{Is it a todo?|TODO|MEETING|WARNING} %^{Title}\nSCHEDULED: %^t\n#+description: %^{Description 🖋 }\n%?"
                                        :empty-lines-before 2
                                        :empty-lines-after  2
                                        :jump-to-captured   t)

                                       ("c"
                                        "          Caputere a new code task"
                                        entry
                                        (file "~/org/agenda/capture.org")
                                        "* TODO %^{Title} \nSCHEDULED: %^t\n#+description: %^{Description 🖋 }\n:LINK:  %A\n%?"
                                        :empty-lines-before 2
                                        :empty-lines-after  2
                                        :created            t)

                                       ("t"
                                        "          Create a new general task"
                                        entry
                                        (file "~/org/personal/tasks.org")
                                        "* TODO %^{Title}\n#+description: %^{Description 🖋 }\n%?"
                                        :empty-lines-before 2
                                        :empty-lines-after  2
                                        :jump-to-captured   t
                                        :created            t))
         )
   )

  ;; Nil is the default value
  ('nil
   (setq org-agenda-files             (list org-directory luiznux-general-agenda)
         org-agenda-custom-commands   '(("x" "My Agenda :)"
                                         ((agenda
                                           ""
                                           ((org-agenda-overriding-header        "My Agenda 📅")
                                            (org-agenda-remove-tags              t)
                                            (org-agenda-span                     '2))))
                                         )

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
                                            (org-agenda-todo-ignore-scheduled    'all))))
                                         )

                                        ("b" "Birthdays Agenda"
                                         ((agenda
                                           ""
                                           ((org-agenda-overriding-header        "Birthdays  ")
                                            (org-agenda-files                    (list luiznux-birthday-agenda))
                                            (org-agenda-show-all-dates           nil)
                                            (org-agenda-span                     '182) ;; half of a year
                                            (org-agenda-prefix-format            '((agenda . "")))
                                            (org-agenda-sorting-strategy         '(category-keep)))))
                                         )
                                        )

         org-capture-templates        '(("a"
                                         "          Add an event on the agenda calendar"
                                         entry
                                         (file+headline "~/org/agenda/agenda.org" "My TODOs 🍩")
                                         "** %^{Is it a todo?|TODO|MEETING|WARNING} %^{Title}\nSCHEDULED: %^t\n#+description: %^{Description 🖋 }\n%?"
                                         :empty-lines-before 2
                                         :empty-lines-after  2
                                         :jump-to-captured   t)

                                        ("c"
                                         "          Caputere a new code task"
                                         entry
                                         (file "~/org/agenda/capture.org")
                                         "* TODO %^{Title} \nSCHEDULED: %^t\n#+description: %^{Description 🖋 }\n:LINK:  %A\n%?"
                                         :empty-lines-before 2
                                         :empty-lines-after  2
                                         :created            t))
         )
   )
  )


(provide 'org-env-config)
;;; org-env-config.el ends here
