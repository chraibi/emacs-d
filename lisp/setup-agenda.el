;; org-agenda-setup.el
;; Org agenda-specific configurations

(use-package org-agenda
  :after org
  :commands (org-agenda)
  :config
  (setq org-agenda-files
      (append '("~/.emacs.d")
              (file-expand-wildcards "/Users/chraibi/Library/CloudStorage/Dropbox/Orgfiles/org-files/org-roam/ppl/*.org")
              (file-expand-wildcards "/Users/chraibi/Library/CloudStorage/Dropbox/Orgfiles/org-files/org-roam/administration/*.org")
              (file-expand-wildcards "/Users/chraibi/Library/CloudStorage/Dropbox/Orgfiles/org-files/org-roam/notes/*.org")
              (file-expand-wildcards "/Users/chraibi/Library/CloudStorage/Dropbox/Orgfiles/org-files/org-roam/projects/*.org")
))
;; Agenda clock report parameters
(setq org-agenda-clockreport-parameter-plist
      '(:link t :maxlevel 2 :fileskip0 t :compact t :narrow 60 :score 0))

  )

(use-package org-super-agenda
  :after org-agenda
  :config
  (org-super-agenda-mode)
  (setq org-agenda-custom-commands
        '(
          ("z" "Super view"
           (
            (alltodo "" ((org-agenda-overriding-header "")
                         (org-super-agenda-groups
                          '(
                            (:name "Today"
                                   :scheduled today
                                   :face (:background "AliceBlue" :underline nil)
                                   :order 2)
                            (:name "  📌 Due Today"
                                   :deadline today
                                   :face (:background "AliceBlue" :underline nil)
                                   :order 3
                                   )
                            (:name "  ☕ Scheduled soon"
                                   :scheduled future
                                   :order 1)
                            (:name "  ☕ Deadline soon"
                                   :deadline future
                                   :order 5)
                            (:name "Important"
                                   :priority "A"
                                   :order 6)
                            (:name "  ⛔ Overdue Scheduled"
                                   :face (:background "LavenderBlush" :underline nil)
                                   :and (:scheduled past :todo ("NEXT" "TODO" "PROG"))
                                   :order 10) ;  
                            (:name "  ⛔ Overdue Deadline"
                                   :face (:background "LavenderBlush" :underline nil)
                                   :and (:deadline past :todo ("NEXT" "TODO" "PROG"))
                                   :order 11)
                            (:discard (:anything))                                 
                            )
                          )
                         )
                     )
            )
           )
          )
        )
  ;;(org-agenda-list)
  )

(provide 'org-agenda-setup)
