;; org-agenda-setup.el
;; Org agenda-specific configurations


(use-package org-agenda
  :ensure org ; Ensure the 'org' package is installed
  :config
  (setq org-agenda-window-setup 'only-window)
  (setq org-agenda-files
        (append         
         (file-expand-wildcards (concat org-directory "org-roam/ppl/*.org"))
         (file-expand-wildcards (concat org-directory "org-roam/administration/*.org"))
         (file-expand-wildcards (concat org-directory"org-roam/notes/*.org"))))
  (setq org-agenda-clockreport-parameter-plist
        '(:link t :maxlevel 2 :fileskip0 t :compact t :narrow 60 :score 0))
  (setq org-agenda-skip-scheduled-if-done t))


(use-package org-super-agenda
  :after org-agenda
  :config
  (org-super-agenda-mode)
  (setq org-agenda-custom-commands
        '(
          ("z" "Super view"
           (
                     (agenda "" ((org-agenda-span 'day)
                      (org-super-agenda-groups
                       '((:name "Today"
                                :time-grid t
                                :date today
                                :scheduled today
                                :order 1)))))
            (alltodo ""
                     ((org-agenda-overriding-header "")
                      (org-super-agenda-groups
                       '(
                         (:name "🔥 Ongoing"
                                :tag "ongoing"
                                 :order 2)
                         (:name "📝 Review"
                                 :tag "review"
                                 :order 6)
                         (:name "📙 Books"
                                :tag "reading"                                 
                                :order 7)
                         (:name "  📌 Due Today"
                                :deadline today
                                :face (:background "AliceBlue" :underline nil)
                                :order 1)
                         (:name "  ☕ Scheduled soon"
                                :scheduled future
                                :order 3)
                         (:name "  ☕ Deadline soon"
                                :deadline future
                                :order 4)
                         (:name "Important"
                                :priority "A"
                                :order 5)
                         (:name "  ⛔ Overdue Scheduled"
                                :face (:background "LavenderBlush" :underline nil)
                                :and (:scheduled past :todo ("NEXT" "TODO" "PROG"))
                                :order 10)
                         (:name "  ⛔ Overdue Deadline"
                                :face (:background "LavenderBlush" :underline nil)
                                :and (:deadline past :todo ("NEXT" "TODO" "PROG"))
                                :order 11)
                         (:discard (:anything))
                         (:name "Done today"
                                :and (:regexp "State \"DONE\""
                                              :log t))
                         (:name "Clocked today"
                                :log t)
                         ))))))))
  )
(defun autocompile ()
  "Automatically compile Emacs Lisp files upon saving."
  (interactive)
    (require 'bytecomp)
    (byte-compile-file (buffer-file-name)))

(add-hook 'after-save-hook 'autocompile)

(provide 'org-agenda-setup)
