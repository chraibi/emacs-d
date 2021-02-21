;;; package --- Summary
;;; Code:
;;; Commentary:
; settings for calendar, journal, clocks
(message "Enter setup org-mode")


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
(setq-default org-display-custom-times t)
(setq org-time-stamp-custom-formats '())
(custom-set-variables
  '(org-display-custom-times t)
  '(org-time-stamp-custom-formats (quote ("<%d-%m-%Y %a>" . "<%d/%m/%Y  %a>")))
  )
; Set default column view headings: Task Effort Clock_Summary
(setq org-columns-default-format "%80ITEM(Task) %10Effort(Estimated Effort){:} %10CLOCKSUM")
; global Effort estimate values
(setq org-global-properties (quote (("Effort_ALL" . "0:10 0:30 1:00 2:00 3:00 4:00 5:00 6:00 7:00 8:00"))))

(eval-after-load 'org-bullets
  '(setq org-bullets-bullet-list '("✺" "✹" "✸" "✷" "✶" "✭" "✦" "■" "▲" "●" )))
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
(setq org-journal-dir "~/Dropbox/Orgfiles/org-files/journal/")
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
  (setq org-agenda-files (list
                          (expand-file-name org-directory)
                          )
        )
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
           ((agenda "" ) 
            (tags-todo "-goals-incubate-inbox+TODO=\"INTR\""
                       ((org-agenda-overriding-header "                                                        INTR TASKS                                                                       ")))            
            (tags-todo "-goals-incubate-inbox+TODO=\"PROG\""
                      ((org-agenda-overriding-header "                                                         PROG TASKS                                                                       ")))
            (tags-todo "-goals-incubate-inbox+TODO=\"NEXT\""
                      ((org-agenda-overriding-header "                                                         NEXT TASKS                                                                       ")))
            )
           ((org-super-agenda-groups
             '(
               (:name "  📌 Today"
                      :scheduled today :order 1)
               (:name "  📌 Due today"
                      :deadline today :order 1)
               (:name "  ⛔ Overdue"
                      :deadline past :face (:background "RosyBrown1" :underline nil))
               (:name "  ⭐ Due soon" 
                      :deadline future :log t)
               (:name "  ❌ Scheduled earlier"
                      :scheduled past)
               (:name "  👀 Important" :priority "A" :face (:background "AliceBlue" :underline nil))
               (:name "  ☕ Scheduled" :time-grid t)
               (:name "  💀 Unsorted" :todo "PROG")
               (:name "  💀 Unsorted" :todo "INTR")
               (:name "  💀 Unsorted" :todo "NEXT")
               (:discard (:anything))
               )
             )
            )       
           (org-agenda-list)
           ))
        ))





;; TODO keywords.
(setq org-todo-keywords
      '(
        (sequence "TODO(t)" "NEXT(n)" "PROG(p)" "INTR(i)" "|" "DONE(d!/!)")
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
(setq org-default-notes-file (concat org-directory "notes.org"))
(custom-set-variables
 '(org-time-stamp-custom-formats (quote ("<%d/%m/%Y %a>" . "<%d/%m/%Y  %a [%H:%M]>")))

 ) 
;--------------------------
;; Capture templates for: TODO tasks, Notes, appointments, phone calls, meetings, and org-protocol
;; (setq org-capture-templates
;;       (quote (("t" "todo" entry (file "~/Dropbox/Orgfiles/org-files/work.org")
;;                "*** TODO %?\n")
;;               ("n" "note" entry (file "~/Dropbox/Orgfiles/org-files/notes.org")
;;                "* %? :NOTE:\n%U\n\n" :clock-in t :clock-resume t)
;;               ("j" "Journal entry" plain (file+datetree+prompt "~/Dropbox/Orgfiles/org-files/journal.org")
;;                "**** %?         :@journal:\n %U" :clock-in t :clock-resume t)
;;               ("m" "Meeting" entry (file "~/Dropbox/Orgfiles/org-files/meeting.org")
;;                "**  %? :MEETING:\n%U" :clock-in t :clock-resume t)
;; )))
; Use the current window for indirect buffer display
(setq org-indirect-buffer-display 'current-window)
(setq org-log-done 'time)
(use-package org
  :mode (("\\.org$" . org-mode))
  :config
  (progn
    (setq org-timer-default-timer 25
          org-latex-listings 'minted
          org-latex-minted-options '(("frame" "lines") ("linenos=true"))
          org-export-latex-hyperref-format "\\ref{%s}"
          ;;--------------ORG latex Code
          org-export-latex-listings t
          org-export-latex-listings 'minted
          org-src-fontify-natively t
          org-latex-pdf-process
          '("pdflatex -interaction nonstopmode -output-directory %o %f"
            "bibtex %b"
            "pdflatex -interaction nonstopmode -output-directory %o %f"
            "pdflatex -interaction nonstopmode -output-directory %o %f")
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
        )
    )
  )
;; cycle  lists
 
;;------------------------ ORG-Templates
(setq org-structure-template-alist
      (quote (("s" "#+begin_src ?\n\n#+end_src" "<src lang=\"?\">\n\n</src>")
              ("e" "#+begin_example\n?\n#+end_example" "<example>\n?\n</example>")
              ("q" "#+begin_quote\n?\n#+end_quote" "<quote>\n?\n</quote>")
              ("v" "#+begin_verse\n?\n#+end_verse" "<verse>\n?\n</verse>")
              ("c" "#+begin_center\n?\n#+end_center" "<center>\n?\n</center>")
              ("l" "#+begin_latex\n?\n#+end_latex" "<literal style=\"latex\">\n?\n</literal>")
              ("L" "#+latex: " "<literal style=\"latex\">?</literal>")
              ("h" "#+begin_html\n?\n#+end_html" "<literal style=\"html\">\n?\n</literal>")
              ("H" "#+html: " "<literal style=\"html\">?</literal>")
              ("a" "#+begin_abstract\n?\n#+end_abstract")
;              ("i" "#+index: ?" "#+index: ?")
              ("i" "#+include %file ?" "<include file=%file markup=\"?\">"))))

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
;; open pdf with system pdf viewer (works on mac)
(use-package helm-bibtex
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
          (default       . bibtex-completion-format-citation-default)))

  (setq bibtex-completion-pdf-open-function
      (lambda (fpath)
          (message "field  %s" bibtex-completion-pdf-field )
          (message "Opening Path : %s " bibtex-completion-library-path)
          (message "Opening Path : %s " fpath)
          (call-process "open" nil 0 nil "-a" "/Applications/Skim.app" fpath)
          )
        )


  (setq bibtex-completion-additional-search-fields '(tags))
  )

;; org-roam ==========
(use-package org-roam
  :init
  (message "init org-roam")
  :ensure t
  :after org
  :hook 
  (after-init . org-roam-mode)
  :custom
  (org-roam-directory (concat org-directory "org-roam/"))
  (setq org-roam-db-update-method 'immediate)
  (require 'org-roam-protocol)
    :custom-face
    (org-roam-link ((t (:inherit org-link :foreground "#C991E1"))))
  :bind (:map org-roam-mode-map
              (("C-c o l" . org-roam)
               ("C-c o f" . org-roam-find-file)
               ("C-c o t" . org-roam-tag-add)
               ("C-c o j" . org-roam-dailies-capture-today)
               ("C-c o p" . org-roam-dailies-capture-tomorrow)
               ("C-c o g" . org-roam-graph))
              :map org-mode-map
              (("C-c o i" . org-roam-insert))
              (("C-c o I" . org-roam-insert-immediate))))

;; org-ref ==============
(use-package org-ref
  :init
  (message "Init org-ref")
  :ensure t
  :after org-roam
  :config
  (setq
   org-ref-notes-directory (concat org-roam-directory "papers/")
   org-ref-pdf-directory "~/Zotero/storage/"
   org-ref-default-bibliography '("~/Zotero/DB.bib")
   org-ref-completion-library 'org-ref-ivy-cite
   org-ref-get-pdf-filename-function 'org-ref-get-pdf-filename-helm-bibtex
   org-ref-bibliography-notes (concat org-ref-notes-directory "bibnote.org")
   org-ref-note-title-format "* TODO %y - %t\n :PROPERTIES:\n  :Custom_ID: %k\n  :NOTER_DOCUMENT: %F\n :ROAM_KEY: cite:%k\n  :AUTHOR: %9a\n  :JOURNAL: %j\n  :YEAR: %y\n  :VOLUME: %v\n  :PAGES: %p\n  :DOI: %D\n  :URL: %U\n :END:\n\n"
   org-ref-notes-function 'orb-edit-notes
   )
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
    (setq orb-templates
        '(("r" "ref" plain (function org-roam-capture--get-point)
           ""
           :file-name "papers/${citekey}"
           :head "#+TITLE: ${citekey}: ${title}\n#+ROAM_KEY: ${ref}\n#+ROAM_TAGS:

- keywords :: ${keywords}

\n* ${title}\n  :PROPERTIES:\n  :Custom_ID: ${=key=}\n  :URL: ${url}\n  :AUTHOR: ${author-or-editor}\n  :NOTER_DOCUMENT: %(orb-process-file-field \"${=key=}\")\n   :NOTER_PAGE: \n  :END:\n\n"
           :unnarrowed t)))

  )

  (use-package org-pdftools
    :hook (org-load . org-pdftools-setup-link))

(use-package org-noter
  :ensure t
  :after (:any org pdf-view)
  :custom (org-noter-always-create-frame nil))

;;(progn
(use-package org-noter-pdftools
  :init
  (message "call pdf-tools-install when installing for the first time")
  :ensure t
  :after org-noter
  :config
  (with-eval-after-load 'pdf-annot
    (add-hook 'pdf-annot-activate-handler-functions #'org-noter-pdftools-jump-to-note)))

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

(use-package org-roam-server
  :ensure t
  :config
  (setq org-roam-server-host "127.0.0.1"
        org-roam-server-port 8080
        org-roam-server-authenticate nil
        org-roam-server-export-inline-images t
        org-roam-server-serve-files nil
        org-roam-server-served-file-extensions '("pdf" "mp4" "ogv")
        org-roam-server-network-poll t
        org-roam-server-network-arrows nil
        org-roam-server-network-label-truncate t
        org-roam-server-network-label-truncate-length 60
        org-roam-server-network-label-wrap-length 20))

(setq org-roam-dailies-directory (concat org-roam-directory "journal/"))
(setq org-roam-dailies-capture-templates
      '(
        ("j" "journal" entry
         #'org-roam-capture--get-point
         "* %?"
         :file-name "daily/%<%Y-%m-%d>"
         :head "#+title: %<%Y-%m-%d>\n"
         :olp ("Journal"))))

(setq org-roam-capture-templates
      '(
        ("d" "default" plain (function org-roam--capture-get-point)
         "%? \n\n* Settings \n#+roam_alias: \n#+roam_tags: \n#+CREATED: %U\n#+STARTUP: fold"
         :file-name "%<%Y%m%d%H%M%S>-${slug}"
         :head "* ${title}\n"
         :unnarrowed t)
        ("c" "categorized" plain (function org-roam--capture-get-point)
         "%?\n\n* Settings \n#+roam_alias: \n#+roam_tags: \n#+CREATED: %U\n#+STARTUP: fold"
         :file-name "%^{Category|ppl|meeting|project|note}/%<%Y%m%d%H%M%S>-${slug}"
         :head "* ${title}\n"
         :unnarrowed t)
        ;; ppl
        ("p" "ppl" plain (function org-roam--capture-get-point)
         "** Facts \n- @ \n%?\n\n* Settings \n#+roam_alias: \n#+roam_tags: \n#+CREATED: %U\n#+STARTUP: fold"
         :file-name "ppl/%<%Y%m%d%H%M%S>-${slug}"
         :head "* ${title}\n"
         :unnarrowed t)
        ;; meetings
        ("m" "meeting" plain (function org-roam--capture-get-point)
         "\n%?\n\n* Settings \n#+roam_alias: \n#+roam_tags: \n#+CREATED: %U\n#+STARTUP: fold"
         :file-name "meetings/%<%Y%m%d%H%M%S>-${slug}"
         :head "* ${title}\n %U"
         :clock-in t
         :clock-resume t
         :unnarrowed t)
        ;; projects
        ("r" "project" plain (function org-roam--capture-get-point)
         "\n%?\n\n* Settings \n#+roam_alias: \n#+roam_tags: \n#+CREATED: %U\n#+STARTUP: fold"
         :file-name "projects/%<%Y%m%d%H%M%S>-${slug}"
         :head "* ${title}\n"
         :unnarrowed t)
        ;;notes 
        ("n" "note" plain (function org-roam--capture-get-point)
         "\n%?\n\n* Settings \n#+roam_alias: \n#+roam_tags: \n#+CREATED: %U\n#+STARTUP: fold"
         :file-name "notes/%<%Y%m%d%H%M%S>-${slug}"
         :head "* ${title}\n"
         :unnarrowed t)
      ;; papers
      ("b" "paper" plain (function org-roam--capture-get-point)
       "\n%?\n\n* Settings \n#+roam_alias: \n#+roam_tags: \n#+CREATED: %U\n#+STARTUP: fold"
       :file-name "papers/%<%Y%m%d%H%M%S>-${slug}"
       :head "* ${title}\n"
       :unnarrowed t)
      ;; administration
      ("a" "administration" plain (function org-roam--capture-get-point)
         "\n%?\n\n* Settings \n#+roam_alias: \n#+roam_tags: \n#+CREATED: %U\n#+STARTUP: fold"
         :file-name "administration/%<%Y%m%d%H%M%S>-${slug}"
         :head "* ${title}\n"
         :unnarrowed t)
      ;; notes
      ("l" "lecture" plain (function org-roam--capture-get-point)
         "\n%?\n\n* Settings \n#+roam_alias: \n#+roam_tags: \n#+CREATED: %U\n#+STARTUP: fold"
         :file-name "lectures/%<%Y%m%d%H%M%S>-${slug}"
         :head "* ${title}\n"
         :unnarrowed t)
      ;; chess
      ("s" "chess" plain (function org-roam--capture-get-point)
       "\n%?\n\n* Settings \n#+roam_alias: \n#+roam_tags: \n#+CREATED: %U\n#+STARTUP: fold"
       :file-name "chess/%<%Y%m%d%H%M%S>-${slug}"
       :head "* ${title}\n"
       :unnarrowed t)
      )
      ;;----
      )

(provide 'setup-org-mode)
;;; setup-org-mode.el ends here
