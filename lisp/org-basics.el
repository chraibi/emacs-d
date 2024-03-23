;; org-basics.el
;; Basic Org mode configurations
;; defined: org-find-dangling-clok function

(use-package org
  :mode (("\\.org$" . org-mode))
  :custom
  (org-timer-default-timer 25)
  (org-export-with-toc nil)
  (org-clock-history-length 23)
  (org-clock-in-resume t)
  (org-clock-into-drawer t)
  (org-clock-out-remove-zero-time-clocks t)
  (org-clock-out-when-done t)
  (org-clock-persist t)
  (org-clock-persist-query-resume nil)
  (org-clock-auto-clock-resolution 'when-no-clock-is-running)
  (org-clock-report-include-clocking-task t)
  (org-cycle-include-plain-lists t)
  (org-clock-in-switch-to-state "PROG")
  (org-drawers '("PROPERTIES" "LOGBOOK"))
  (org-confirm-babel-evaluate nil)
  (org-src-preserve-indentation t)
  (org-startup-indented t)
  (org-startup-folded "overview")
  ;; Place task and log settings directly within :custom
  (org-log-done 'time)
  (org-todo-keywords '((sequence "TODO(t)" "NEXT(n)" "PROG(p)" "WAITING(w)" "CANCEL(l)" "CAL(c)" "|" "DONE(d!/!)")))
  (org-columns-default-format "%50ITEM(Task) %2PRIORITY %10Effort(Effort){:} %10CLOCKSUM")
  (org-global-properties '(("Effort_ALL" . "0:15 0:30 0:45 1:00 1:30 2:00 2:30 3:00 3:30 4:00 4:30 5:00 5:30 6:00 7:00 8:00")))
  
  :config
  (org-clock-persistence-insinuate)
  ;; IDO for target completion
  (setq org-completion-use-ido t)
  ;; Refile targets
  (setq org-refile-targets '((org-agenda-files :maxlevel . 5) (nil :maxlevel . 5)))
  ;; Capture templates
  ;; Cc-key: t, n, p, l
  (setq org-capture-templates
        (quote (("j" "Doing" entry (file "/Users/chraibi/Library/CloudStorage/Dropbox/Orgfiles/org-files/org-roam/journal.org")
                 "** PROG " :empty-lines 1)
                ("t" "todo" entry (file "/Users/chraibi/Library/CloudStorage/Dropbox/Orgfiles/org-files/org-roam/administration/work-notes.org")
                 "** TODO " :empty-lines 1)
                ("n" "fleeting note" entry (file "/Users/chraibi/Library/CloudStorage/Dropbox/Orgfiles/org-files/org-roam/notes/fleeting-notes.org")
                 "* ")
                ("p" "private" entry (file "/Users/chraibi/Library/CloudStorage/Dropbox/Orgfiles/org-files/org-roam/notes/private-notes.org")
                 "* TODO " :empty-lines 1)
                ("l" "literature" entry (file "/Users/chraibi/Library/CloudStorage/Dropbox/Orgfiles/org-files/org-roam/notes/literature-notes.org")
                 "* TODO " :empty-lines 1)
                )))
  ;; Bullet settings
  (with-eval-after-load 'org-bullets
    (setq org-bullets-bullet-list '("●" "✦" "✭" "■" "▲" "✺" "✹" "✸" "✷" "✶")))
  )

;; Setting `org-directory` outside of `use-package` as it's a global setting
(setq org-directory "/Users/chraibi/Library/CloudStorage/Dropbox/Orgfiles/org-files/")

;; Emphasis settings - Adjust if necessary
(with-eval-after-load 'org
  ;; Check if the emphasis setting already exists before adding it.
  (unless (member '("*" (:foreground "red")) org-emphasis-alist)
    ;; Add your custom emphasis if it's not already present.
    (add-to-list 'org-emphasis-alist '("*" (:foreground "red"))))
)


;; functions
(defun org-find-dangling-clock ()
  "Find a dangling clock entry in an `org-mode' buffer."
  (interactive)
  (re-search-forward "CLOCK: \\[[^]]*\\] *$")
  )

;; Keybindings
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)
(global-set-key "\C-cc" 'org-capture)

(provide 'org-basics)
