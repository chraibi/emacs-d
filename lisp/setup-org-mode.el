;;; package --- Summary
;;; Code:
;;; Commentary:
;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:
                                        ; settings for calendar, journal, clocks
(require 'ox-latex)
(require 'org-tempo)


(message "Enter setup org-mode")

(add-to-list 'org-emphasis-alist
             '("*" (:foreground "red")
               ))

(setq org-image-actual-width nil)

;;https://emacs.stackexchange.com/questions/29758/yasnippets-and-org-mode-yas-next-field-or-maybe-expand-does-not-expand
(defun yas-org-very-safe-expand ()
  (let ((yas-fallback-behavior 'return-nil)) (yas-expand)))
(add-hook 'org-mode-hook
      (lambda ()
        (add-to-list 'org-tab-first-hook 'yas-org-very-safe-expand)
        (define-key yas-keymap [tab] 'yas-next-field)))

;;================ BEGIN GENERAL ===========================
(use-package org-bullets
  :init
  (message "Using org-bullets!")
  :ensure t
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
  )

(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
(setq org-directory "~/Dropbox/Orgfiles/org-files/")
(add-to-list 'auto-mode-alist '("\\.\\(org\\|org_archive\\|txt\\)$" . org-mode))
(add-to-list 'auto-mode-alist '(".*/[0-9]*$" . org-mode))

;; Allow setting single tags without the menu
(setq org-alphabetical-lists t)
;(setq org-time-stamp-custom-formats '())
; Set default column view headings: Task Effort Clock_Summary
(setq org-columns-default-format "%80ITEM(Task) %10Effort(Estimated Effort){:} %10CLOCKSUM")
;; Set default column view headings: Task Priority Effort Clock_Summary
(setq org-columns-default-format "%50ITEM(Task) %2PRIORITY %10Effort(Effort){:} %10CLOCKSUM")

;; global Effort estimate values
(setq org-global-properties
      '(("Effort_ALL" .
         "0:15 0:30 0:45 1:00 2:00 3:00 4:00 5:00 6:00 0:00")))
;;        1    2    3    4    5    6    7    8    9    0
;; These are the hotkeys ^^


(eval-after-load 'org-bullets
  '(setq org-bullets-bullet-list '("●" "✦" "✭" "■" "▲" "✺" "✹" "✸" "✷" "✶")))
; Use IDO for target completion
(setq org-completion-use-ido t)
; Targets include this file and any file contributing to the agenda - up to 5 levels deep
(setq org-refile-targets (quote ((org-agenda-files :maxlevel . 5) (nil :maxlevel . 5))))

; Targets start with the file name - allows creating level 1 tasks
(setq org-refile-use-outline-path (quote file))

; Targets complete in steps so we start with filename, TAB shows the next level of targets etc
(setq org-outline-path-complete-in-steps t)

; Allow refile to create parent tasks with confirmation
(setq org-refile-allow-creating-parent-nodes (quote confirm))

;; strike through dones
(set-face-attribute 'org-headline-done nil :strike-through t)

;;;;----------------------------- subtasks
(defun my-org-insert-sub-task ()
  (interactive)
  (let ((parent-deadline (org-get-deadline-time nil)))
    (org-goto-sibling)
    (org-insert-todo-subheading t)
    (when parent-deadline
      (org-deadline nil parent-deadline))))

(define-key org-mode-map (kbd "C-c s") 'my-org-insert-sub-task)
(setq org-enforce-todo-dependencies t)
(add-hook 'org-mode-hook
          \t  (lambda ()
                \t    'turn-on-font-lock
                \t    (setq word-wrap 1)
                \t    (setq truncate-lines nil)
                \t    (flyspell-mode 1)
                \t    (org-journal-mode 1)
                )
          )


(setq require-final-newline t)
;;================ END GENERAL =============================
  (global-set-key "\C-cl" 'org-store-link)
   (global-set-key "\C-ca" 'org-agenda)
   (global-set-key "\C-cb" 'org-iswitchb)
   (define-key global-map "\C-cc" 'org-capture)


;;==================== BEGIN JOURNAL ===================
(use-package org-journal
  :ensure t
  :defer t
  :custom
  (org-journal-dir "/Users/chraibi/Dropbox/Orgfiles/org-files/journal")
  (org-journal-date-format "%A, %d %B %Y")
  )
(setq org-journal-file-type "weekly")


(defvar org-journal-file "~/Dropbox/Orgfiles/org-files/journal.org"
  "Path to OrgMode journal file.")
(defvar org-journal-date-format "%Y-%m-%d"  
  "Date format string for journal headings.")  

(defun org-goto-local-search-headings (string bound noerror)
   "Search and make sure that anu matches are in headlines."
   (catch 'return
    (while (if isearch-forward
               (search-forward string bound noerror)
             (search-backward string bound noerror))
       (when (let ((context (mapcar 'car (save-match-data (org-context)))))
              (and (member :headline context)
                   (not (member :tags context))))))))

(defun org-journal-entry ()  
  "Create a new diary entry for today or append to an existing one."  
  (interactive)  
  (switch-to-buffer (find-file org-journal-file))  
  (widen)  
  (let ((today (format-time-string org-journal-date-format)))  
    (beginning-of-buffer)  
    (unless (org-goto-local-search-forward-headings today nil t)  
      ((lambda ()   
         (org-insert-heading)  
         (insert today)  
         (insert "\n\n  \n"))))  
    (beginning-of-buffer)  
    (org-show-entry)  
    (org-narrow-to-subtree)  
    (end-of-buffer)  
    (backward-char 2)  
    (unless (= (current-column) 2)  
      (insert "\n\n  "))))

(global-set-key "\C-cj" 'org-journal-entry)
(add-to-list 'auto-mode-alist '(".*/[0-9]*$" . org-mode))

;;==================== END JOURNAL =====================

;;==================== BEGIN AGENDA ===================
(use-package org-agenda
  :after org
  :commands (org-agenda)
  :config
  (setq org-agenda-files (directory-files-recursively org-directory "\\.org$"))
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
  (use-package org-super-agenda
    :ensure t
    :config (org-super-agenda-mode))
  (setq 
   org-agenda-custom-commands
   `(

     ("d" "Today"
      (
       (tags-todo "SCHEDULED<\"<+1d>\"&PRIORITY=\"A\""
                     ((org-agenda-skip-function
                       '(org-agenda-skip-entry-if 'todo 'done))
                      (org-agenda-overriding-header "High-priority unfinished tasks:"))
                     )

          (tags-todo "SCHEDULED<\"<+1d>\""
                     ((org-agenda-skip-function
                       '(or (org-agenda-skip-entry-if 'done)
                            ))
                      (org-agenda-overriding-header "Tasks:")))
          ))
     
     ("w" "Agenda"
      (       
       (tags-todo "-goals-incubate-inbox+TODO=\"CAL\""
                  ((org-agenda-overriding-header " =========  CAL TASKS  ========= ")))
       
       (tags-todo "-goals-incubate-inbox+TODO=\"INTR\""
                  ((org-agenda-overriding-header " =========  INTR TASKS  ========= ")))            
       (tags-todo "-goals-incubate-inbox+TODO=\"PROG\""
                  ((org-agenda-overriding-header " =========  PROG TASKS  ========= ")))
       (tags-todo "-goals-incubate-inbox+TODO=\"NEXT\""
                  ((org-agenda-overriding-header " =========  NEXT TASKS  ========= ")))
       )
      ((org-super-agenda-groups
             '(
               (:name "Done today"
                :and (:regexp "State \"DONE\""
                              :log t))
               
               (:name " ❤ Today"
                      :scheduled today :order 1)
               (:name "  📌 Due today"
                      :deadline today :order 1)
               (:name "  ⛔ Overdue"
                      :deadline past :face (:background "RosyBrown1" :underline nil))
               (:name "  ️ Important" :priority "A" :face (:background "AliceBlue" :underline nil) :order 1)
               (:name "  ⭐ Due soon" 
                      :deadline future :log t :order 2)
               
               ;; (:name "  ☕ Scheduled"
               ;;        :tag "NEXT"
               ;;        :time-grid t
               ;;        :scheduled future
               ;;        :order 1) ;
               (:name "  💀 Unsorted" :todo "PROG" :order 100)
               (:name "  💀 Unsorted" :todo "INTR" :order 100)
               (:name "  💀 Unsorted" :todo "NEXT" :order 100)
               ;; (:name "  💀 Unsorted" :todo "CAL")
                (:discard (:anything))
               )
             )
            )       
           (org-agenda-list)
           ))
        ))




;; Agenda clock report parameters
(setq org-agenda-clockreport-parameter-plist
      '(:link t :maxlevel 6 :fileskip0 t :compact t :narrow 60 :score 0))

;; TODO keywords.
(setq org-todo-keywords
      '(
        (sequence "TODO(t)" "NEXT(n)" "PROG(p)" "INTR(i)" "CAL(c)" "|" "DONE(d!/!)")
        )
      )
                                        ; Tags with fast selection keys
(setq org-tag-alist '((:startgroup)
                      ("@work" . ?w)
                      ("@home" . ?h)
                      (:endgroup)
                      ("PERSONAL" . ?p)
                      ("NOTE" . ?n)
                      ("CANCELLED" . ?c)
                      ))
(setq org-fast-tag-selection-single-key (quote expert))
(setq org-default-notes-file (concat org-directory "org-roam/notes.org"))
;; (custom-set-variables
;;  '(org-display-custom-times t)
;;  '(org-time-stamp-custom-formats (quote ("<%Y-%m-%d %H:%M>" . "<%Y-%m-%d %H:%M>")))
;;  ) 
                                        ;--------------------------
;  (concat org-roam-directory "administration/work-notes.org")
;; Capture templates for: TODO tasks, Notes, appointments, phone calls, meetings, and org-protocol
(setq org-capture-templates
      (quote (("t" "todo" entry (file "~/Dropbox/Orgfiles/org-files/org-roam/administration/work-notes.org")
               "** TODO %?" :empty-lines 1)
              ("n" "fleeting note" entry (file "~/Dropbox/Orgfiles/org-files/org-roam/notes/fleeting-notes.org")
               "* %?")
              ("p" "private" entry (file "~/Dropbox/Orgfiles/org-files/org-roam/notes/private-notes.org")
               "* TODO %?" :empty-lines 1)
              ("l" "literature" entry (file "~/Dropbox/Orgfiles/org-files/org-roam/notes/literature-notes.org")
               "* TODO %?" :empty-lines 1)
              
              ;; ("j" "Journal entry" plain (file+datetree+prompt "~/Dropbox/Orgfiles/org-files/org-roam/journal/journal.org")
              ;;  "**** %?         :@journal:\n %U" :clock-in t :clock-resume t)              
)))
; Use the current window for indirect buffer display
(setq org-indirect-buffer-display 'current-window)
(setq org-log-done 'time)
(use-package org
  :mode (("\\.org$" . org-mode))
  :config
  (progn
    (setq org-timer-default-timer 25
          org-latex-listings 'minted
          org-latex-packages-alist '(("" "minted"))
          org-latex-minted-options '(("frame" "lines") ("linenos=true"))
          org-export-latex-hyperref-format "\\ref{%s}"
          ;;--------------ORG latex Code
          org-export-latex-listings t
          org-export-latex-listings 'minted
          org-src-fontify-natively t
          org-latex-pdf-process
          '("xelatex --shell-escape -interaction nonstopmode -output-directory %o %f"
            "bibtex %b"
            "xelatex --shell-escape -interaction nonstopmode -output-directory %o %f"
            "xelatex --shell-escape -interaction nonstopmode -output-directory %o %f")
          org-export-with-toc nil)
    ;;--------------
    ;; Resume clocking task when emacs is restarted
    (org-clock-persistence-insinuate)
    ;; Show lot of clocking history so it's easy to pick items off the C-F11 list
    (setq org-clock-history-length 23
          ;; Resume clocking task on clock-in if the clock is open
          org-clock-in-resume t
          ;; Change tasks to NEXT when clocking in
          ;; Separate drawers for clocking and logs
          org-drawers (quote ("PROPERTIES" "LOGBOOK"))
          ;; Save clock data and state changes and notes in the LOGBOOK drawer
          org-clock-into-drawer t
          ;; Sometimes I change tasks I'm clocking quickly - this removes clocked tasks with 0:00 duration
          org-clock-out-remove-zero-time-clocks t
          ;; Clock out when moving task to a done state
        org-clock-out-when-done t
        ;; Save the running clock and all clock history when exiting Emacs, load it on startup
        org-clock-persist t
        ;; Do not prompt to resume an active clock
        org-clock-persist-query-resume nil
        ;; Enable auto clock resolution for finding open clocks
        org-clock-auto-clock-resolution (quote when-no-clock-is-running)
        ;; Include current clocking task in clock reports
        org-clock-report-include-clocking-task t
        org-cycle-include-plain-lists t
        org-clock-in-switch-to-state "PROG"
        ;; use pretty things for the clocktable
        org-pretty-entities t
        ;;Use return to follow links in org-mode
;; https://emacs.stackexchange.com/questions/62731/changing-the-default-binding-to-open-a-link-in-an-org-mode-file-using-ret
        org-return-follows-link t
        )
    )
  )
;; cycle  lists
 
;;------------------------ ORG-Templates
(setq org-structure-template-alist
      (quote (("s" . "src"))))

(setq org-export-html-validation-link nil)

;;ORG- code
(org-babel-do-load-languages
 'org-babel-load-languages
  '( (perl . t)         
     (ruby . t)
     (C . t)
     (latex . t)
     (python . t)
     (emacs-lisp . t)   
   )
 )

(setq org-confirm-babel-evaluate nil)
(setq org-src-preserve-indentation t)
(setq org-startup-indented t)
(setq org-startup-folded "overview")

;;--------------- crypt
(use-package org-crypt
  :after (org)
  :config
  (org-crypt-use-before-save-magic)
  (setq org-crypt-key nil
        auto-save-default nil
        org-tags-exclude-from-inheritance '("crypt")))
     

;; Auto-saving does not cooperate with org-crypt.el: so you need
;; to turn it off if you plan to use org-crypt.el quite often.
;; Otherwise, you'll get an (annoying) message each time you
;; start Org.
;; To turn it off only locally, you can insert this:
;;
;; # -*- buffer-auto-save-file-name: nil; -*-

;; active Org-babel languages
(org-babel-do-load-languages
 'org-babel-load-languages
 '(;; other Babel languages
   (plantuml . t)))


(defun org-find-dangling-clock ()
  "Find a dangling clock entry in an `org-mode' buffer."
  (interactive)
  (re-search-forward "CLOCK: \\[[^]]*\\] *$")
  )


;; helm-bibtex
(use-package helm-bibtex
  :ensure t
  :config
  (setq bibtex-completion-bibliography
        '(
          "~/Zotero/DB.bib"
          )
        )
  (setq bibtex-completion-library-path '("~/Zotero/storage/"))
  (setq bibtex-completion-pdf-field "file")
  (setq bibtex-completion-notes-path "~/Dropbox/Orgfiles/org-files/org-roam/papers/bibnote.org")
  (setq bibtex-completion-display-formats
        '((article       . "${=has-pdf=:1}${=has-note=:1} ${=type=:3} ${year:4} ${author:36} ${title:*} ${journal:40}")
          (inbook        . "${=has-pdf=:1}${=has-note=:1} ${=type=:3} ${year:4} ${author:36} ${title:*} Chapter ${chapter:32}")
          (incollection  . "${=has-pdf=:1}${=has-note=:1} ${=type=:3} ${year:4} ${author:36} ${title:*} ${booktitle:40}")
          (inproceedings . "${=has-pdf=:1}${=has-note=:1} ${=type=:3} ${year:4} ${author:36} ${title:*} ${booktitle:40}")
          (t             . "${=has-pdf=:1}${=has-note=:1} ${=type=:3} ${year:4} ${author:36} ${title:*}")))

  (setq bibtex-completion-additional-search-fields '(keywords))

  (setq bibtex-completion-pdf-symbol "⌘")
  (setq bibtex-completion-notes-symbol "✎")

  (setq bibtex-completion-format-citation-functions
        '((org-mode      . bibtex-completion-format-citation-org-title-link-to-PDF)
          (latex-mode    . bibtex-completion-format-citation-cite)
          (markdown-mode . bibtex-completion-format-citation-pandoc-citeproc)
          (default       . bibtex-completion-format-citation-default))
        )

  ;; (setq bibtex-completion-pdf-open-function
  ;;     (lambda (fpath)
  ;;         (message "field  %s" bibtex-completion-pdf-field )
  ;;         (message "Opening Path : %s " bibtex-completion-library-path)
  ;;         (message "Opening Path : %s " fpath)
  ;;         (call-process "open" nil 0 nil "-a" "/Applications/Skim.app" fpath)
  ;;         )
  ;;       )


  (setq bibtex-completion-additional-search-fields '(tags))
  )


(defun jethro/open-with (arg)
  "Open visited file in default external program.
When in dired mode, open file under the cursor.
With a prefix ARG always prompt for command to use."
  (interactive "P")
  (let* ((current-file-name
          (if (eq major-mode 'dired-mode)
              (dired-get-file-for-visit)
            buffer-file-name))
         (open (pcase system-type
                 (`darwin "open")
                 ((or `gnu `gnu/linux `gnu/kfreebsd) "xdg-open")))
         (program (if (or arg (not open))
                      (read-shell-command "Open current file with: ")
                    open)))
    (call-process program nil 0 nil current-file-name)))

;; org-roam ==========

(use-package org-roam
  :init
  (message "init org-roam")
  (setq org-roam-v2-ack t)
  :ensure t
  :after org
  :hook 
  (after-init . org-roam-db-autosync-mode)
  (org-roam-backlinks-mode . visual-line-mode)
  :custom
  (org-roam-directory (concat org-directory "org-roam/"))
  (org-roam-complete-everywhere t)
  (setq org-roam-db-update-method 'immediate)
  :custom-face
  (org-roam-link ((t (:inherit org-link :foreground "#C991E1"))))
  :bind (
         ("C-c o f" . org-roam-node-find)
         ("C-c o l" . org-roam-buffer-toggle)
         ("C-c o i" . org-roam-node-insert)
         ("C-c o o" . jethro/open-with)
         ("C-M-i" . completion-at-point)
         )
  :config
  (org-roam-db-autosync-mode)
  (setq org-roam-db-gc-threshold most-positive-fixnum)
  (setq org-roam-auto-replace-fuzzy-links nil)
  (require 'org-roam-protocol)
  )

(use-package org-roam-timestamps
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
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start t))

;; org-ref ==============
(use-package org-ref
  :init
  (message "Init org-ref")
  :ensure t
  :after org-roam
  :config  
  (setq bibtex-completion-bibliography '("~/Zotero/DB.bib")
        bibtex-completion-library-path '("~/Zotero/storage")
	bibtex-completion-notes-path (concat org-roam-directory "papers/")
	bibtex-completion-notes-template-multiple-files "* ${author-or-editor}, ${title}, ${journal}, (${year}) :${=type=}: \n\nSee [[cite:&${=key=}]]\n"
	bibtex-completion-additional-search-fields '(keywords)
	bibtex-completion-display-formats
	'((article       . "${=has-pdf=:1}${=has-note=:1} ${year:4} ${author:36} ${title:*} ${journal:40}")
	  (inbook        . "${=has-pdf=:1}${=has-note=:1} ${year:4} ${author:36} ${title:*} Chapter ${chapter:32}")
	  (incollection  . "${=has-pdf=:1}${=has-note=:1} ${year:4} ${author:36} ${title:*} ${booktitle:40}")
	  (inproceedings . "${=has-pdf=:1}${=has-note=:1} ${year:4} ${author:36} ${title:*} ${booktitle:40}")
	  (t             . "${=has-pdf=:1}${=has-note=:1} ${year:4} ${author:36} ${title:*}"))
	bibtex-completion-pdf-open-function
	(lambda (fpath)
	  (call-process "open" nil 0 nil fpath))
        org-ref-default-citation-link "cite"        
        )
  )

(with-eval-after-load 'org-ref
  (defun org-ref-open-pdf-at-point ()
    "Open the pdf for bibtex key under point if it exists.
Tweak from https://github.com/jkitchin/org-ref/issues/172#issuecomment-207626125"
    (interactive)
    (let* ((results (org-ref-get-bibtex-key-and-file))
           (key (car results))
           (pdf-other (bibtex-completion-find-pdf key)))
      ;;(find-file (car pdf-other))
      (org-open-file (car pdf-other))
      )
    )
  )

(define-key org-mode-map (kbd "C-c )") 'org-ref-insert-link)



(use-package org-ref-ivy
  :ensure t
  :config
  (setq org-ref-insert-link-function 'org-ref-insert-link-hydra/body
      org-ref-insert-cite-function 'org-ref-cite-insert-ivy
      org-ref-insert-label-function 'org-ref-insert-label-link
      org-ref-insert-ref-function 'org-ref-insert-ref-link
      org-ref-cite-onclick-function (lambda (_) (org-ref-citation-hydra/body)))
)


;; org-roam-bibtex ======================
(use-package org-roam-bibtex
  :ensure t
  :after org-roam
  :hook (org-roam-mode . org-roam-bibtex-mode)
  :config
  (setq orb-preformat-keywords
        '("=key=" "title" "url" "file" "author-or-editor" "keywords" "citekey")
        orb-process-file-keyword t
        )   
  )


(use-package pdf-tools
  :ensure t
  :pin manual ;; don't reinstall when package updates
  :mode  ("\\.pdf\\'" . pdf-view-mode)
  :config
  (setq-default pdf-view-display-size 'fit-page)
  (setq pdf-annot-activate-created-annotations t)
  (pdf-tools-install :no-query)
  (require 'pdf-occur))




(use-package org-noter
  :ensure t
  :after (:any org pdf-view)
  :config
  (setq
   ;; org-noter-always-create-frame nil
   ;; org-noter-insert-note-no-questions nil
   org-noter-separate-notes-from-heading t
   ;; org-noter-auto-save-last-location t
   org-noter-notes-search-path '("~/Dropbox/Orgfiles/org-files/org-roam/papers/")
   )
    ;; (defun org-noter-init-pdf-view ()
    ;;   (pdf-view-fit-page-to-window)
    ;;   (pdf-view-auto-slice-minor-mode)
    ;;   (run-at-time "0.5 sec" nil #'org-noter))
    ;; (add-hook 'pdf-view-mode-hook 'org-noter-init-pdf-view)
    (require 'org-noter-pdftools)
  )

(use-package org-noter-pdftools
  :after org-noter
  :config
  ;; Add a function to ensure precise note is inserted
  (defun org-noter-pdftools-insert-precise-note (&optional toggle-no-questions)
    (interactive "P")
    (org-noter--with-valid-session
     (let ((org-noter-insert-note-no-questions (if toggle-no-questions
                                                   (not org-noter-insert-note-no-questions)
                                                 org-noter-insert-note-no-questions))
           (org-pdftools-use-isearch-link t)
           (org-pdftools-use-freestyle-annot t))
       (org-noter-insert-note (org-noter--get-precise-info)))))

  ;; fix https://github.com/weirdNox/org-noter/pull/93/commits/f8349ae7575e599f375de1be6be2d0d5de4e6cbf
  (defun org-noter-set-start-location (&optional arg)
    "When opening a session with this document, go to the current location.
With a prefix ARG, remove start location."
    (interactive "P")
    (org-noter--with-valid-session
     (let ((inhibit-read-only t)
           (ast (org-noter--parse-root))
           (location (org-noter--doc-approx-location (when (called-interactively-p 'any) 'interactive))))
       (with-current-buffer (org-noter--session-notes-buffer session)
         (org-with-wide-buffer
          (goto-char (org-element-property :begin ast))
          (if arg
              (org-entry-delete nil org-noter-property-note-location)
            (org-entry-put nil org-noter-property-note-location
                           (org-noter--pretty-print-location location))))))))
  (with-eval-after-load 'pdf-annot
    (add-hook 'pdf-annot-activate-handler-functions #'org-noter-pdftools-jump-to-note)))

;; (use-package org-noter-pdftools
;;   :after org-noter
;;   :config
;;   (with-eval-after-load 'pdf-annot
;;     (add-hook 'pdf-annot-activate-handler-functions #'org-noter-pdftools-jump-to-note)))


(use-package org-pdftools
  :hook (org-mode . org-pdftools-setup-link))



;; (setq org-file-apps
;;       '(
;;         ("\\.pdf::\\(\\d+\\)\\'" org-pdfview-open link)
;;         (directory . emacs)
;;         (auto-mode . emacs)
;;         ("\\.x?html?\\'" . default)
;;         )
;;       )



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


;; (setq org-roam-dailies-directory (concat org-roam-directory "dailies/"))
;; (setq org-roam-dailies-capture-templates
;;       '(
;;         ("d" "daily" entry
;;          #'org-roam-capture--get-point
;;          "* %?"
;;          :file-name "dailies/%<%Y-%m-%d>"
;;          :head "#+title: %<%Y-%m-%d>\n"
;;          :olp ("Journal"))))

(setq org-roam-capture-templates
      '(
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


(org-babel-do-load-languages
 'org-babel-load-languages
 '((python . t)
   (latex . t)))

(add-to-list 'org-latex-classes
             '("iitmdiss"
               "\\documentclass{iitmdiss}"
               ("\\chapter{%s}" . "\\chapter*{%s}")
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
               ("\\paragraph{%s}" . "\\paragraph*{%s}")
               ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))



(setf (cdr (rassoc 'find-file-other-window org-link-frame-setup)) 'find-file)

;;---------- org-roam v2 stuff. TODO reorganize later

;; Showing the number of backlinks for each node in org-roam-node-find ;
;; https://github.com/org-roam/org-roam/wiki/Hitchhiker's-Rough-Guide-to-Org-roam-V2#showing-the-number-of-backlinks-for-each-node-in-org-roam-node-find
(cl-defmethod org-roam-node-directories ((node org-roam-node))
  (if-let ((dirs (file-name-directory (file-relative-name (org-roam-node-file node) org-roam-directory))))
      (format "(%s)" (car (f-split dirs)))
    ""))

(cl-defmethod org-roam-node-backlinkscount ((node org-roam-node))
  (let* ((count (caar (org-roam-db-query
                       [:select (funcall count source)
                                :from links
                                :where (= dest $s1)
                                :and (= type "id")]
                       (org-roam-node-id node)))))
    (format "[%d]" count)))

(setq org-roam-node-display-template "${directories:10} ${tags:10} ${title:100} ${backlinkscount:6}")

;; -------
                                        ; https://github.com/org-roam/org-roam/wiki/Hitchhiker's-Rough-Guide-to-Org-roam-V2#showing-node-hierarchy

(cl-defmethod org-roam-node-filetitle ((node org-roam-node))
  "Return the file TITLE for the node."
  (org-roam-get-keyword "TITLE" (org-roam-node-file node)))

(cl-defmethod org-roam-node-hierarchy ((node org-roam-node))
  "Return the hierarchy for the node."
  (let ((title (org-roam-node-title node))
        (olp (org-roam-node-olp node))
        (level (org-roam-node-level node))
        (filetitle (org-roam-node-filetitle node)))
    (concat
     (if (> level 0) (concat filetitle " > "))
     (if (> level 1) (concat (string-join olp " > ") " > "))
     title))
  )

(setq org-roam-node-display-template "${hierarchy:*} ${tags:30}")

;; for org-roam-buffer-toggle
;; Recommendation in the official manual
(add-to-list 'display-buffer-alist
               '("\\*org-roam\\*"
                  (display-buffer-in-direction)
                  (direction . right)
                  (window-width . 0.33)
                  (window-height . fit-window-to-buffer)))


(use-package svg-tag-mode
  :ensure t
  :config
  (defconst date-re "[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}")
  (defconst time-re "[0-9]\\{2\\}:[0-9]\\{2\\}")
  (defconst day-re "[A-Za-z]\\{3\\}")
  (defun svg-progress-percent (value)
    (svg-image (svg-lib-concat
                (svg-lib-progress-bar (/ (string-to-number value) 100.0)
                                      nil :margin 0 :stroke 2 :radius 3 :padding 2 :width 11)
                (svg-lib-tag (concat value "%")
                             nil :stroke 0 :margin 0)) :ascent 'center))

  (defun svg-progress-count (value)
    (let* ((seq (mapcar #'string-to-number (split-string value "/")))
           (count (float (car seq)))
           (total (float (cadr seq))))
      (svg-image (svg-lib-concat
                  (svg-lib-progress-bar (/ count total) nil
                                        :margin 0 :stroke 2 :radius 3 :padding 2 :width 11)
                  (svg-lib-tag value nil
                               :stroke 0 :margin 0)) :ascent 'center)))
  (setq svg-tag-tags
        `(
          ;; Org tags
          (":\\([A-Za-z0-9]+\\)" . ((lambda (tag) (svg-tag-make tag))))
          ("CLOCK" . ((svg-tag-make "CLOCK" :face 'font-lock-comment-face
                                     :inverse nil :margin 0 :radius 0)))   
          (":\\([A-Za-z0-9]+[ \-]\\)" . ((lambda (tag) tag)))     
          ;; Task priority
          ("\\[#[A-Z]\\]" . ( (lambda (tag)
                                (svg-tag-make tag :face 'org-priority 
                                              :beg 2 :end -1 :margin 0))))
        ;; Progress
        ("\\(\\[[0-9]\\{1,3\\}%\\]\\)" . ((lambda (tag)
                                            (svg-progress-percent (substring tag 1 -2)))))
        ("\\(\\[[0-9]+/[0-9]+\\]\\)" . ((lambda (tag)
                                          (svg-progress-count (substring tag 1 -1)))))
        ;; TODO / DONE
        ("TODO" . ((lambda (tag) (svg-tag-make "TODO" :face 'org-todo :inverse t :margin 0))))
        ("INTR" . ((lambda (tag) (svg-tag-make "INTR" :face 'org-todo :inverse t :margin 0))))
        ("PROG" . ((lambda (tag) (svg-tag-make "PROG" :face 'org-todo :inverse t :margin 0))))
        ("NEXT" . ((lambda (tag) (svg-tag-make "NEXT" :face 'org-todo :inverse t :margin 0))))
        ("HELLO" . ((lambda (tag) (svg-tag-make "HELLO" :face 'org-todo :inverse t :margin 0))))
        ("DONE" . ((lambda (tag) (svg-tag-make "DONE" :face 'org-done :margin 0))))
        ;; Citation of the form [cite:@Knuth:1984] 
        ("\\(\\[cite:@[A-Za-z]+:\\)" . ((lambda (tag)
                                          (svg-tag-make tag
                                                        :inverse t
                                                        :beg 7 :end -1
                                                        :crop-right t))))
        ("\\[cite:@[A-Za-z]+:\\([0-9]+\\]\\)" . ((lambda (tag)
                                                (svg-tag-make tag
                                                              :end -1
                                                              :crop-left t))))
        ;; Active date (without day name, with or without time)
        (,(format "\\(<%s>\\)" date-re) .
         ((lambda (tag)
            (svg-tag-make tag :beg 1 :end -1 :margin 0))))
        
        (,(format "\\(<%s *\\)%s>" date-re time-re) .
         ((lambda (tag)
            (svg-tag-make tag :beg 1 :inverse nil :crop-right t :margin 0))))


        
        (,(format "<%s *\\(%s>\\)" date-re time-re) .
         ((lambda (tag)
            (svg-tag-make tag :end -1 :inverse t :crop-left t :margin 0))))

                                        ; MC test
        ;; [2022-01-03 MON 14:07]
        ;; <2022-01-03 MON 14:07>
        ;; [2022-01-03 14:08]
        ;; <2022-01-03   14:08>
       ;;https://www.reddit.com/r/emacs/comments/jczet6/svg_tag_minor_mode/hr6njcm/?context=3
        (,(format "<%s %s*\\(%s>\\)" date-re day-re time-re) .
          ((lambda (tag)
             (svg-tag-make tag :beg 1 :end -1 :margin 0 :face 'org-date))))




        
        ;; Inactive date  (without day name, with or without time)
         (,(format "\\(\\[%s\\]\\)" date-re) .
          ((lambda (tag)
             (svg-tag-make tag :beg 1 :end -1 :margin 0 :face 'org-date))))
         (,(format "\\(\\[%s *\\)%s\\]" date-re time-re) .
          ((lambda (tag)
             (svg-tag-make tag :beg 1 :inverse nil :crop-right t :margin 0 :face 'org-date))))
         (,(format "\\[%s *\\(%s\\]\\)" date-re time-re) .
          ((lambda (tag)
             (svg-tag-make tag :end -1 :inverse t :crop-left t :margin 0 :face 'org-date))))
         )
        )
  )




;; To do:         TODO DONE NEXT PROG INTR NEXT HELLO
;; Tags:          :TAG1:TAG2:TAG3:
;; Priorities:    [#A] [#B] [#C]
;; Progress:      [1/3]
;;                [42%]
;; Active date:   <2021-12-24>
;;                <2021-12-24 14:00>
;;CLOCK: [2022-01-03 MON 14:07]--[2022-01-03 14:08]


                                        ;:LOGBOOK:
;;  <2022-01-04 Tue 13:53>--[2022-01-03 Mon 13:54] => -23:59
;;  <2022-01-04 Tue>--<2022-01-03 Mon>

;; :END:

;; Inactive date: [2021-12-24 Mon]
;; Inactive date: <MC>

;;              [2021-12-24 14:00]--[2021-12-24 14:00]
;; Citation:      [cite:@Knuth:1984] 



(provide 'setup-org-mode)
;;; setup-org-mode.el ends here
