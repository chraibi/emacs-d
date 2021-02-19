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
(setq org-agenda-prefix-format '((agenda . " %i %-12:c%?-12t% s")
         (timeline . "  % s")
         (todo .
               " %i %-12:c %(concat \"[\"(org-format-outline-path (org-get-outline-path)) \"]\") ")
         (tags .
               " %i %-12:c %(concat \"[\"(org-format-outline-path (org-get-outline-path)) \"]\") ")
         (search . " %i %-12:c"))
      )
(setq org-agenda-files '(,org-directory))
(setq org-agenda-window-frame-fractions '(1.0 . 1.0))
(setq org-agenda-dim-blocked-tasks 'invisible)
(setq org-journal-enable-agenda-integration t
      org-icalendar-store-UID t
      org-icalendar-include-todo "all"
      org-icalendar-combined-agenda-file "/Users/chraibi/Dropbox/Orgfiles/org-files/cal_private.ics")

;; Customized view for the daily workflow. (Command: "C-c a n")
(setq org-agenda-custom-commands
  '(("n" "Agenda / INTR / PROG / NEXT"
     ((agenda "" nil)
      (todo "INTR" nil)
      (todo "PROG" nil)
      (todo "NEXT" nil))
     nil))
  )
;; TODO keywords.
(setq org-todo-keywords
      '(
        (sequence "TODO(t)" "NEXT(n)" "PROG(p)" "INTR(i)" "|" "DONE(d!/!)")
        )
)

;; (custom-set-variables
;; ;;   ;; custom-set-variables was added by Custom.
;; ;;   ;; If you edit it by hand, you could mess it up, so be careful.
;; ;;   ;; Your init file should contain only one such instance.
;; ;;   ;; If there is more than one, they won't work right.
;;  ;; '(org-agenda-ndays 7)
;;  ;; '(org-agenda-show-all-dates t)
;;  '(org-agenda-skip-deadline-if-done t)
;;  '(org-agenda-skip-scheduled-if-done t)
;;  '(org-agenda-start-on-weekday nil)
;;  '(org-deadline-warning-days 14)
;;  '(org-fast-tag-selection-single-key (quote expert))
;;  '(org-reverse-note-order t))

;; Show the daily agenda by default.
(setq org-agenda-span 'day)

;; Hide tasks that are scheduled in the future.
(setq org-agenda-todo-ignore-scheduled 'future)

;; Use "second" instead of "day" for time comparison.
;; It hides tasks with a scheduled time like "<2020-11-15 Sun 11:30>"
(setq org-agenda-todo-ignore-time-comparison-use-seconds t)

;; Hide the deadline prewarning prior to scheduled date.
(setq org-agenda-skip-deadline-prewarning-if-scheduled 'pre-scheduled)



(defun load-org-agenda-files-recursively (dir) "Find all directories in DIR."
       (unless (file-directory-p dir) (error "Not a directory `%s'" dir))
       (unless (equal (directory-files dir nil org-agenda-file-regexp t) nil)
         (add-to-list 'org-agenda-files dir)
         )
       (dolist (file (directory-files dir nil nil t))
         (unless (member file '("." ".."))
           (let ((file (concat dir file "/")))
             (when (file-directory-p file)
               (load-org-agenda-files-recursively file)
               )
             )
           )
         )
       )
(load-org-agenda-files-recursively "~/Dropbox/Orgfiles/org-files/" ) ; trailing slash required
;; For tag searches ignore tasks with scheduled and deadline dates
;(setq org-agenda-tags-todo-honor-ignore-options t)
;;==================== END AGENDA =====================
;;-------------------------------------------------------
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

(defun notes ()
  "Switch to my work dir."
   (interactive)
   (find-file "~/Dropbox/Orgfiles/org-files/notes.org")
   )

(setq org-default-notes-file (concat org-directory "~/Dropbox/Orgfiles/org-files/notes.org"))

(custom-set-variables
 '(org-time-stamp-custom-formats (quote ("<%d/%m/%Y %a>" . "<%d/%m/%Y  %a [%H:%M]>")))

 ) 
;--------------------------

;; Resume clocking task when emacs is restarted
(org-clock-persistence-insinuate)
;;
;; Show lot of clocking history so it's easy to pick items off the C-F11 list
(setq org-clock-history-length 23)
;; Resume clocking task on clock-in if the clock is open
(setq org-clock-in-resume t)
;; Change tasks to NEXT when clocking in
;;(setq org-clock-in-switch-to-state 'bh/clock-in-to-next)
;; Separate drawers for clocking and logs
(setq org-drawers (quote ("PROPERTIES" "LOGBOOK")))
;; Save clock data and state changes and notes in the LOGBOOK drawer
(setq org-clock-into-drawer t)
;; Sometimes I change tasks I'm clocking quickly - this removes clocked tasks with 0:00 duration
(setq org-clock-out-remove-zero-time-clocks t)
;; Clock out when moving task to a done state
(setq org-clock-out-when-done t)
;; Save the running clock and all clock history when exiting Emacs, load it on startup
(setq org-clock-persist t)
;; Do not prompt to resume an active clock
(setq org-clock-persist-query-resume nil)
;; Enable auto clock resolution for finding open clocks
(setq org-clock-auto-clock-resolution (quote when-no-clock-is-running))
;; Include current clocking task in clock reports
(setq org-clock-report-include-clocking-task t)


;; Capture templates for: TODO tasks, Notes, appointments, phone calls, meetings, and org-protocol
(setq org-capture-templates
      (quote (("t" "todo" entry (file "~/Dropbox/Orgfiles/org-files/work.org")
               "*** TODO %?\n")
              ("n" "note" entry (file "~/Dropbox/Orgfiles/org-files/notes.org")
               "* %? :NOTE:\n%U\n%a\n" :clock-in t :clock-resume t)
              ("j" "Journal entry" plain (file+datetree+prompt "~/Dropbox/Orgfiles/org-files/journal.org")
               "**** %?         :@journal:\n %U" :clock-in t :clock-resume t)
              ("m" "Meeting" entry (file "~/Dropbox/Orgfiles/org-files/meeting.org")
               "**  %? :MEETING:\n%U" :clock-in t :clock-resume t)
)))



; Use the current window for indirect buffer display
(setq org-indirect-buffer-display 'current-window)
(setq org-log-done 'time)
;; Set a default value for the timer, for example :
(setq org-timer-default-timer 25)

;; Modify the org-clock-in so that a timer is started with the default
;; value except if a timer is already started :



;; (add-hook 'org-clock-in-hook (lambda ()
;;       (if (not org-timer-current-timer) 
;;      (org-timer-set-timer '(16)))))


;;----------------------------------------
;; AUCTex
;;----------------------------------------
 

(setq org-latex-listings 'minted)
(setq org-latex-minted-options
      '(("frame" "lines") ("linenos=true")))
(setq org-export-latex-hyperref-format "\\ref{%s}")
;;--------------ORG latex Code
(setq org-export-latex-listings t)
(setq org-export-latex-listings 'minted)
;(add-to-list 'org-export-latex-packages-alist '("" "minted"))
(setq org-src-fontify-natively t)


(setq org-latex-pdf-process
      '("pdflatex -interaction nonstopmode -output-directory %o %f"
        "bibtex %b"
        "pdflatex -interaction nonstopmode -output-directory %o %f"
        "pdflatex -interaction nonstopmode -output-directory %o %f"))

;; (setq org-latex-pdf-process (quote ("texi2dvi --pdf --clean --verbose
;; --batch %f" "bibtex %b" "texi2dvi --pdf --clean --verbose --batch %f"
;; "texi2dvi --pdf --clean --verbose --batch %f")))

; (setq org-latex-to-pdf-process (list "latexmk -pdf %f"))


;;(setq org-latex-to-pdf-process (list "latexmk -pdf %f"))
(setq org-export-with-toc nil)
;; (add-to-list 'org-export-latex-classes
;;              '("koma-article"
;;                "\\documentclass{scrartcl}"
;;                "\\usepackage{libertine}"
;;                "\\usepackage[T1]{fontenc}"
;;                "\\usepackage[libertine]{newtxmath}"
;;                "\\usepackage{amsmath}"
;;                "\\usepackage{graphicx}"
;;                "\\usepackage[usenames,dvipsnax1mes]{color}"
;;                ("\\section{%s}" . "\\section*{%s}")
;;                ("\\subsection{%s}" . "\\subsection*{%s}")
;;                ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
;;                ("\\paragraph{%s}" . "\\paragraph*{%s}")
;;                ("\\subparagraph{%s}" . "\\subparagraph*{%s}")
;;                )
;;              )

;; cycle  lists
(setq org-cycle-include-plain-lists t)
 
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

     (org-crypt-use-before-save-magic)
     (setq org-tags-exclude-from-inheritance (quote ("crypt")))
     
     (setq org-crypt-key nil)
       ;; GPG key to use for encryption

       ;; Either the Key ID or set to nil to use symmetric encryption.
     
     (setq auto-save-default nil)
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
(setq org-plantuml-jar-path
      (expand-file-name "~/.emacs.d/plantuml.jar"))


(defun org-find-dangling-clock ()
  "Find a dangling clock entry in an `org-mode' buffer."
  (interactive)
  (re-search-forward "CLOCK: \\[[^]]*\\] *$")
  )

;; helm-bibtex
;; open pdf with system pdf viewer (works on mac)
;; (use-package helm-bibtex
;;   :config
;;   (setq bibtex-completion-bibliography
;;         '(
;;           "~/sciebo/zotero/_Research_/_Research_.bib"
;;           )
;;         )
;;   (setq bibtex-completion-library-path '("/Users/chraibi/sciebo/zotero/_Research_/files/"))
;;   (setq bibtex-completion-pdf-field "file")
;;   (setq bibtex-completion-notes-path "~/Dropbox/Orgfiles/org-files/roam/bib/bibnote.org")
;;   (setq bibtex-completion-display-formats
;;         '((article       . "${=has-pdf=:1}${=has-note=:1} ${=type=:3} ${year:4} ${author:36} ${title:*} ${journal:40}")
;;           (inbook        . "${=has-pdf=:1}${=has-note=:1} ${=type=:3} ${year:4} ${author:36} ${title:*} Chapter ${chapter:32}")
;;           (incollection  . "${=has-pdf=:1}${=has-note=:1} ${=type=:3} ${year:4} ${author:36} ${title:*} ${booktitle:40}")
;;           (inproceedings . "${=has-pdf=:1}${=has-note=:1} ${=type=:3} ${year:4} ${author:36} ${title:*} ${booktitle:40}")
;;           (t             . "${=has-pdf=:1}${=has-note=:1} ${=type=:3} ${year:4} ${author:36} ${title:*}")))

;;   (setq bibtex-completion-additional-search-fields '(keywords))

;;   (setq bibtex-completion-pdf-symbol "⌘")
;;   (setq bibtex-completion-notes-symbol "✎")

;;   (setq bibtex-completion-format-citation-functions
;;         '((org-mode      . bibtex-completion-format-citation-org-title-link-to-PDF)
;;           (latex-mode    . bibtex-completion-format-citation-cite)
;;           (markdown-mode . bibtex-completion-format-citation-pandoc-citeproc)
;;           (default       . bibtex-completion-format-citation-default)))

;;   (setq bibtex-completion-pdf-open-function
;;       (lambda (fpath)
;;           (message "field  %s" bibtex-completion-pdf-field )
;;           (message "Opening Path : %s " bibtex-completion-library-path)
;;           (message "Opening Path : %s " fpath)
;;           (call-process "open" nil 0 nil "-a" "/Applications/Skim.app" fpath)
;;           )
;;         )


;;   (setq bibtex-completion-additional-search-fields '(tags))
;;   )

;; org-roam ==========
(use-package org-roam
  :init
  (message "Loading org-roam!")
  :ensure t
  :hook
  (after-init . org-roam-mode org-roam-server-mode)
  :custom
  (setq org-roam-directory (concat org-directory "org-roam/"))
  (setq org-roam-db-update-method 'immediate)
  (require 'org-roam-protocol)
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
;; (with-eval-after-load 'org-roam

;;    )

;; org-roam-bibtex ======================
(use-package org-roam-bibtex
  :ensure t
  :after org-roam
  :hook (org-roam-mode . org-roam-bibtex-mode)
  :config
  (setq orb-preformat-keywords
        '("=key=" "title" "url" "file" "author-or-editor" "keywords"))
  (setq orb-templates
        '(("r" "ref" plain (function org-roam-capture--get-point)
           ""
           :file-name "papers/%<%Y%m%d%H%M%S>-${slug}"
           :head "#+TITLE: ${=key=}: ${title}\n#+ROAM_KEY: ${ref}\n#+ROAM_TAGS:

- keywords :: ${keywords}

\n* ${title}\n  :PROPERTIES:\n  :Custom_ID: ${=key=}\n  :URL: ${url}\n  :AUTHOR: ${author-or-editor}\n  :NOTER_DOCUMENT: %(orb-process-file-field \"${=key=}\")\n  :NOTER_PAGE: \n  :END:\n\n"
           :unnarrowed t))))

(use-package deft
  :init
  (message "Load deft C-c o s")
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
         "%? \n\n* Settings \n#+roam_alias: \n#+roam_tags: \n#+CREATED: %U"
         :file-name "%<%Y%m%d%H%M%S>-${slug}"
         :head "* ${title}\n"
         :unnarrowed t)
        ("c" "categorized" plain (function org-roam--capture-get-point)
         "%?\n\n* Settings \n#+roam_alias: \n#+roam_tags: \n#+CREATED: %U"
         :file-name "%^{Category|ppl|meeting|project|note}/%<%Y%m%d%H%M%S>-${slug}"
         :head "* ${title}\n"
         :unnarrowed t)
        ;; ppl
        ("p" "ppl" plain (function org-roam--capture-get-point)
         "** Facts \n- @ \n%?\n\n* Settings \n#+roam_alias: \n#+roam_tags: \n#+CREATED: %U"
         :file-name "ppl/%<%Y%m%d%H%M%S>-${slug}"
         :head "* ${title}\n"
         :unnarrowed t)
        ;; meetings
        ("m" "meeting" plain (function org-roam--capture-get-point)
         "\n%?\n\n* Settings \n#+roam_alias: \n#+roam_tags: \n#+CREATED: %U"
         :file-name "meetings/%<%Y%m%d%H%M%S>-${slug}"
         :head "* ${title}\n"
         :unnarrowed t)
        ;; projects
        ("r" "project" plain (function org-roam--capture-get-point)
         "\n%?\n\n* Settings \n#+roam_alias: \n#+roam_tags: \n#+CREATED: %U"
         :file-name "projects/%<%Y%m%d%H%M%S>-${slug}"
         :head "* ${title}\n"
         :unnarrowed t)
        ;;notes 
        ("n" "note" plain (function org-roam--capture-get-point)
         "\n%?\n\n* Settings \n#+roam_alias: \n#+roam_tags: \n#+CREATED: %U"
         :file-name "notes/%<%Y%m%d%H%M%S>-${slug}"
         :head "* ${title}\n"
         :unnarrowed t)
      ;; papers
      ("b" "paper" plain (function org-roam--capture-get-point)
       "\n%?\n\n* Settings \n#+roam_alias: \n#+roam_tags: \n#+CREATED: %U"
       :file-name "papers/%<%Y%m%d%H%M%S>-${slug}"
       :head "* ${title}\n"
       :unnarrowed t)
      ;; administration
      ("a" "administration" plain (function org-roam--capture-get-point)
         "\n%?\n\n* Settings \n#+roam_alias: \n#+roam_tags: \n#+CREATED: %U"
         :file-name "administration/%<%Y%m%d%H%M%S>-${slug}"
         :head "* ${title}\n"
         :unnarrowed t)
      ;; notes
      ("l" "lecture" plain (function org-roam--capture-get-point)
         "\n%?\n\n* Settings \n#+roam_alias: \n#+roam_tags: \n#+CREATED: %U"
         :file-name "lectures/%<%Y%m%d%H%M%S>-${slug}"
         :head "* ${title}\n"
         :unnarrowed t)
      )
      )

(provide 'setup-org-mode)
;;; setup-org-mode.el ends here
