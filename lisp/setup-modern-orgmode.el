;;; package --- modern org-mode, org-roam, super-calendar
;;; Code:
;;; Commentary:
;; Local Variables:
;; End:



(use-package org-modern
  :ensure t
  :init
  (progn
    (add-hook 'org-mode-hook #'org-modern-mode)
    (add-hook 'org-agenda-finalize-hook #'org-modern-agenda)

    (setq
     ;; Edit settings
     org-auto-align-tags nil
     org-tags-column 0
     org-catch-invisible-edits 'show-and-error
     org-special-ctrl-a/e t
     org-insert-heading-respect-content t

     ;; Org styling, hide markup etc.
     org-hide-emphasis-markers t
     org-pretty-entities t
     org-ellipsis "…"

     ;; Agenda styling
     org-agenda-tags-column 0
     org-agenda-block-separator ?─
     org-agenda-time-grid
     '((daily today require-timed)
       (800 1000 1200 1400 1600 1800 2000)
       " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄")
     org-agenda-current-time-string
     "⭠ now  ─────────────────────────────────────────────────")
    )
  )


;; Calenar 

;;==================== BEGIN AGENDA ===================
(use-package org-agenda
  :after org
  :commands (org-agenda)
  :config
  (setq
   org-agenda-start-on-weekday 1
   org-agenda-include-diary t
   org-agenda-window-setup 'current-window
   org-agenda-skip-scheduled-if-done nil
   org-agenda-compact-blocks t
   org-agenda-sticky t
   org-agenda-span 'day
   org-agenda-todo-ignore-scheduled 'future
   org-agenda-todo-ignore-time-comparison-use-seconds t
   org-agenda-skip-deadline-prewarning-if-scheduled 'pre-scheduled                                       ;;https://stackoverflow.com/questions/22888785/is-it-possible-to-get-org-mode-to-show-breadcrumbs-in-agenda-todo-list/22900459#22900459
   org-agenda-prefix-format '((agenda . " %i %-12:c%?-12t% s")
                              (timeline . "  % s")
                              (todo .
                                    " %i %-12:c %(concat \"[ \"(org-format-outline-path (org-get-outline-path)) \" ]\") ")
                              (tags .
                                    " %i %-12:c %(concat \"[ \"(org-format-outline-path (org-get-outline-path)) \" ]\") ")
                              (search . " %i %-12:c"))
   )
  )

(setq 
 org-agenda-time-grid '((daily today require-timed) "----------------------" nil)
 org-agenda-skip-scheduled-if-done t
 org-agenda-skip-deadline-if-done t
 org-agenda-include-deadlines t
 org-agenda-include-diary t
 org-agenda-block-separator nil
 org-agenda-compact-blocks t
 org-agenda-start-with-log-mode t)

(use-package org-super-agenda
  :after org-agenda
  :config
  (org-super-agenda-mode)
  (setq org-agenda-custom-commands
        '(
          ("w" "Week"
           (
            (tags-todo "SCHEDULED<\"<+1d>\"&PRIORITY=\"A\""
                       ((org-agenda-skip-function
                         '(org-agenda-skip-entry-if 'todo 'done))
                        (org-agenda-overriding-header "High-priority unfinished tasks:"))
                       )
            (tags-todo "SCHEDULED<\"<+7d>\""
                       ((org-agenda-skip-function
                         '(or (org-agenda-skip-entry-if 'done)
                              ))
                        (org-agenda-overriding-header "Week")))
            
            )
           )

          ("z" "Super view"
           (
            (alltodo "" ((org-agenda-overriding-header "")
                         (org-super-agenda-groups
                          '(
                            (:name "Calendar"
                                   :todo "CAL"
                                   :face (:background "GhostWhite" :underline nil)
                                   )
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
                            (:name "Projects"
                                   :and (:tag "Project" :todo ("NEXT" "TODO" "PROG"))
                                   :order 7)
                            (:name "Habilitation"
                                   :and (:tag "habilitation" :todo ("NEXT" "TODO" "PROG"))
                                   :order 8)                            
                            (:name "Research"
                                   :and (:tag ("idea" "modeling") :todo ("NEXT" "TODO" "PROG"))                                   
                                   :order 9)
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
  )


;; org-roam ==========

(use-package org-roam
  :ensure t
  :init
  (message "init org-roam")
  (setq org-roam-v2-ack t)
  
  :after org
  :hook 
  (after-init . org-roam-db-autosync-mode)
  (org-roam-backlinks-mode . visual-line-mode)
  :custom
  (org-roam-directory (concat org-directory "org-roam/"))
  (org-roam-complete-everywhere t)
  (setq org-roam-db-update-method 'immediate)
  :custom-face
  ;; (org-roam-link ((t (:inherit org-link :foreground "#C991E1"))))
  :bind (
         ("C-c o f" . org-roam-node-find)
         ("C-c o l" . org-roam-buffer-toggle)
         ("C-c o i" . org-roam-node-insert)
         ("C-c o o" . jethro/open-with)
         ("C-M-i" . completion-at-point)
         ("C-c o d" . org-roam-dailies-capture-today)
         ("C-c o y" . org-roam-dailies-capture-yesterday)
         ("C-c o t" . org-roam-dailies-capture-tomorrow)
         ("C-c o D" . org-roam-dailies-goto-today)
         ("C-c o Y" . org-roam-dailies-goto-yesterday)
         ("C-c o T" . org-roam-dailies-goto-tomorrow)
         )
  :config
  (org-roam-db-autosync-mode)
  (setq org-roam-db-gc-threshold most-positive-fixnum)
  (setq org-roam-auto-replace-fuzzy-links nil)
  (setq org-roam-dailies-capture-templates
        '(("d" "default" entry "** %<%I:%M %p>: %?"
           :if-new (file+head "%<%Y-%m-%d>.org" "#+title: %<%Y-%m-%d> (%A)\n"))))

  (setq org-roam-capture-templates
        '(
          ("k" "Start" plain
           "%?"
           :target (file+head "notes/journal.org"
                              "#+title: ${title}\n#+roam_aliases:\n#+category: ${slug}\n#+filetags:\n#+date: %U\n\n")
           :immediate-finish t
           :unnarrowed t)
          ("n" "Note" plain
           "%?"
           :target (file+head "notes/%<%Y%m%d%H%M%S>-${slug}.org"
                              "#+title: ${title}\n#+roam_aliases:\n#+category: ${slug}\n#+filetags:\n#+date: %U\n\n")
           :immediate-finish t
           :unnarrowed t)        
          ("p" "Person" plain
           "%?"
           :target (file+head "ppl/%<%Y%m%d%H%M%S>-${slug}.org"
                              "#+title: ${title}\n#+roam_aliases:\n#+category: ${slug}\n#+filetags:\n#+date: %U\n\n")
           :immediate-finish t
           :unnarrowed t)
          ("a" "Administration" plain
           "%?"
           :target (file+head "administration/%<%Y%m%d%H%M%S>-${slug}.org"
                              "#+title: ${title}\n#+roam_aliases:\n#+category: ${slug}\n#+filetags:\n#+date: %U\n\n")
           :immediate-finish t
           :unnarrowed t)
          ("r" "ref" plain 
           "%?"
           :target (file+head "papers/${citekey}.org"
                              "#+title: ${citekey}\n#+roam_key:${ref}\n#+category: ${citekey}\n#+filetags:\n#+date: %U\n- keywords :: ${keywords}\n
\n* ${title}\n  :PROPERTIES:\n  :Custom_ID: ${=key=}\n  :URL: ${url}\n  :AUTHOR: ${author-or-editor}\n  :NOTER_DOCUMENT: [[file:%(orb-process-file-field \"${=key=}\")][PDF]]\n   :NOTER_PAGE: \n  :END:\n\n")
           :unnarrowed t)
          
          ))

  (require 'org-roam-protocol)
  )

(use-package org-roam-timestamps
  :init
  (message "Init timestamp")
  :after org-roam
  :config 
  (setq org-roam-timestamps-parent-file t)
  (setq org-roam-timestamps-remember-timestamps t)  
  )

(use-package org-roam-ui
  :after org-roam
  :commands flycheck-mode
  :hook (after-init . org-roam-ui-mode)
  :config
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow nil
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start t))



(use-package deft
  :init
  (message "Load deft")
  :ensure t
  :after org org-roam
  :bind
  ("C-c o s" . deft)
  :custom
  (deft-recursive t)
  (deft-use-filter-string-for-filename t)
  (deft-default-extension "org")
  :config
  (with-eval-after-load 'org-roam
    (setq deft-directory org-roam-directory)
    )
  )

;;--------------- crypt
(use-package org-crypt
  :after (org)
  :config
  (org-crypt-use-before-save-magic)
  (setq org-crypt-key nil
        auto-save-default nil
        org-tags-exclude-from-inheritance '("crypt")))

;; org config . TODO add to use-package

(defun org-find-dangling-clock ()
  "Find a dangling clock entry in an `org-mode' buffer."
  (interactive)
  (re-search-forward "CLOCK: \\[[^]]*\\] *$")
  )

(setq org-capture-templates
      (quote (("j" "Doing" entry (file (concat org-directory "org-roam/journal.org"))
               "** PROG %?" :empty-lines 1)
              ("t" "todo" entry (file (concat org-directory "org-roam/administration/work-notes.org"))
               "** TODO %?" :empty-lines 1)
              ("n" "fleeting note" entry (file (concat org-directory "org-roam/notes/fleeting-notes.org"))
               "* %?")
              ("p" "private" entry (file (concat org-directory "org-roam/notes/private-notes.org"))
               "* TODO %?" :empty-lines 1)
              ("l" "literature" entry (file (concat org-directory "org-roam/notes/literature-notes.org"))
               "* TODO %?" :empty-lines 1))))

              (setq org-startup-indented t)
(setq org-startup-folded "overview")


(provide 'setup-modern-orgmode)
;;; setup-modern-orgmode.el ends here
