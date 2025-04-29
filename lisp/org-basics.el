;; org-basics.el
;; Basic Org mode configurations
;; defined: org-find-dangling-clok function



;; Setting `org-directory` outside of `use-package` as it's a global setting
(setq org-directory "/Users/chraibi/Library/CloudStorage/Dropbox/Orgfiles/org-files/")


(use-package org
  :mode (("\\.org$" . org-mode))
  :custom
  (org-return-follows-link t) ;; follow links with enter
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
  (remove-hook 'org-mode-hook 'flyspell-mode)   ;; flyspell causes slow agenda start up
  :config
  (org-clock-persistence-insinuate)
  ;; IDO for target completion
  (setq org-completion-use-ido t)
  ;; Refile targets
  (setq org-refile-targets '((org-agenda-files :maxlevel . 5) (nil :maxlevel . 5)))
  ;; Capture templates
  ;; Cc-key: t, n, p, l
  (message "directory ")
  (message org-directory)
  
  (setq org-capture-templates
        (quote (
                ("t" "todo" entry (file "org-roam/administration/work-notes.org")
                 "** TODO " :empty-lines 1)
                ("n" "fleeting note" entry (file "org-roam/notes/fleeting-notes.org")
                 "* ")
                ("p" "private" entry (file "org-roam/notes/private-notes.org")
                 "* TODO " :empty-lines 1)
                ("l" "literature" entry (file "org-roam/notes/literature-notes.org")
                 "* TODO " :empty-lines 1)
                )))
 
  )



;; use org-bullets-mode for utf8 symbols as org bullets

(require 'org-bullets)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

;; ;; make available "org-bullet-face" such that I can control the font size individually
;; (setq org-bullets-face-name (quote org-bullet-face))
;; (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
;; (setq org-bullets-bullet-list '("✙" "♱" "♰" "☥" "✞" "✟" "✝" "†" "✠" "✚" "✜" "✛" "✢" "✣" "✤" "✥"))


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

(defun my-org-todo-set-keyword-faces ()
  (setq org-todo-keyword-faces
        `(("TODO" . (:foreground ,(modus-themes-get-color-value 'blue-warmer) :weight bold))
          ("DONE" . (:foreground ,(modus-themes-get-color-value 'green-warmer) :weight bold))
          ("WAITING" . (:foreground ,(modus-themes-get-color-value 'red-warmer) :weight bold))
          ("SOMEDAY" . (:foreground ,(modus-themes-get-color-value 'fg-dim) :weight bold))))
  (when (derived-mode-p 'org-mode)
    (font-lock-fontify-buffer)))
(with-eval-after-load 'modus-themes
  (add-hook 'modus-themes-after-load-theme-hook #'my-org-todo-set-keyword-faces))


;; org-babel stuff for latex export and coding withing org-mode
(require 'ox-latex)
;; (add-to-list 'org-latex-packages-alist '("" "minted"))
;; (setq org-latex-listings 'minted) 
(setq org-latex-listings t) ;; Uses listings package for code exports
;; Custom listings configuration for better code highlighting
(setq org-latex-listings-options
      '(("basicstyle" "\\ttfamily\\small")   ;; Font and size for code
        ("keywordstyle" "\\color{blue}\\bfseries")  ;; Style for keywords
        ("commentstyle" "\\color{gray}\\itshape")  ;; Style for comments
        ("stringstyle" "\\color{orange}")  ;; Style for strings
        ("showstringspaces" "false")  ;; Don't show spaces in strings
        ("numbers" "left")  ;; Line numbers on the left
        ("numberstyle" "\\tiny\\color{gray}")  ;; Style for line numbers
        ("stepnumber" "1")  ;; Number every line
        ("frame" "single")  ;; Frame around the code
        ("breaklines" "true")  ;; Automatically break long lines
        ("frameround" "tttt")))  ;; Rounded frame corners

;; Add listings and xcolor to required LaTeX packages
(setq org-latex-packages-alist
      (delete '("" "xcolor" t) org-latex-packages-alist))
(setq org-latex-compiler "xelatex") ;; XeLaTex rather than pdflatex
(setq org-latex-pdf-process
      '("xelatex -interaction nonstopmode -output-directory %o %f"
        "xelatex -interaction nonstopmode -output-directory %o %f"))

(add-hook 'org-mode-hook
          (lambda ()
            (setq TeX-view-program-selection '((output-pdf "Skim")))))
(setq org-latex-view-pdf-after-export t) ; Automatically view the PDF after export

(setq org-src-fontify-natively t)
(setq org-export-with-broken-links t
      org-export-with-smart-quotes t
      org-export-allow-bind-keywords t)
; From https://stackoverflow.com/questions/23297422/org-mode-timestamp-format-when-exported
(defun org-export-filter-timestamp-remove-brackets (timestamp backend info)
  "removes relevant brackets from a timestamp"
  (cond
   ((org-export-derived-backend-p backend 'latex)
    (replace-regexp-in-string "[<>]\\|[][]" "" timestamp))
   ((org-export-derived-backend-p backend 'html)
    (replace-regexp-in-string "&[lg]t;\\|[][]" "" timestamp))))

;; LaTeX Specific
(eval-after-load 'ox '(add-to-list
                       'org-export-filter-timestamp-functions
                       'org-export-filter-timestamp-remove-brackets))

;; LaTeX Classes
(with-eval-after-load 'ox-latex
  (add-to-list 'org-latex-classes
               '("org-plain-latex" ;; I use this in base class in all of my org exports.
                 "\\documentclass{article}
[NO-DEFAULT-PACKAGES]
[PACKAGES]
[EXTRA]"
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
  )

;; ========================

(setq org-src-window-setup 'current-window)  ;; Keep focus in current window when editing
(setq org-src-fontify-natively t)  ;; Syntax highlighting in code blocks
(setq org-src-tab-acts-natively t)  ;; Tab works as in the major mode

;; ===
(setq org-structure-template-alist
      '(("s" . "src")
        ("a" . "export ascii")
            
        ("c" . "center")
        ("C" . "comment")
        ("e" . "example")
        ("E" . "export")
        ("h" . "export html")
        ("l" . "export latex")
        ("q" . "quote")
        ("v" . "verse")))

(org-babel-do-load-languages
 'org-babel-load-languages
 '(
   (shell . t)
   (C . t)
   )
 )
;; https://github.com/jkitchin/ox-ipynb/tree/master
;;(require 'ox-ipynb)
;; org-notification
;; org-notification
(use-package emacs
  :hook
  ;; when agenda is opened and showed, refresh appt
  (org-agenda-after-show . org-agenda-to-appt)
  ;; when any org file is saved, refresh appt if it's an org file
  (after-save . (lambda ()
                  (when (derived-mode-p 'org-mode)
                    (org-agenda-to-appt t))))
  :custom
  ;; start warning 60 minutes before the appointment
  (appt-message-warning-time 30)
  ;; warn every 5 minutes
  (appt-display-interval 15)
  ;; Define notification function based on system type
  (appt-disp-window-function
   (if (eq system-type 'darwin)
       ;; macOS: use org-show-notification
       (lambda (remaining new-time msg)
         (org-show-notification 
          (format "%s (in %s minutes)" msg remaining)))
       ;; Other systems: use notifications-notify
       (lambda (remaining new-time msg)
         (notifications-notify
          :title (format "Appointment in %s minutes" remaining)
          :body msg
          :urgency 'critical))))
  :config
  ;; Ensure appointments are activated
  (appt-activate t))

  ;; Automatically clean up old appointments
  (run-at-time nil 3600 'org-agenda-to-appt t)
  ;; Add additional time warnings (e.g., warn at 60, 30, 15, and 5 minutes)
(setq appt-message-warning-time-list '(60 30 15 5))



;; Keybindings
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)
(global-set-key "\C-cc" 'org-capture)
(global-set-key (kbd "\C-cs") 'org-insert-structure-template)

(provide 'org-basics)
