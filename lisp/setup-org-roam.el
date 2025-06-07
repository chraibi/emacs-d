;; org-roam-setup.el
;; Org Roam specific configurations
;; org-roam, org-roam-ui, helm-rg (C-c o s)
;; todo check:
;; - org-roam-protocol
;; - org-roam-timestamps

(use-package org-roam
  :ensure t
  :after org
  :init
  (message "Initializing org-roam")
  :custom
  (org-roam-directory (file-name-as-directory (concat org-directory "org-roam")))
  (org-roam-dailies-directory "journals/")
  (org-roam-complete-everywhere t)
  (org-roam-db-update-method 'immediate)
  :bind (("C-c o f" . org-roam-node-find)
         ("C-c o l" . org-roam-buffer-toggle)
         ("C-c o i" . org-roam-node-insert)
         ("C-c o o" . jethro/open-with)
         ("C-M-i" . completion-at-point)
         ("C-c o d" . org-roam-dailies-capture-today)
         ("C-c o y" . org-roam-dailies-capture-yesterday)
         ("C-c o t" . org-roam-dailies-capture-tomorrow)
         ("C-c o D" . org-roam-dailies-goto-today)
         ("C-c o Y" . org-roam-dailies-goto-yesterday)
         ("C-c o T" . org-roam-dailies-goto-tomorrow))
  :config
  (org-roam-db-autosync-mode)
  (setq org-roam-db-gc-threshold most-positive-fixnum
        org-roam-auto-replace-fuzzy-links nil)
  (require 'org-roam-protocol))

(use-package org-roam-ui
  :after org-roam
  :init
  (message "Load org-roam-ui")
  :defer t
  :hook (after-init . org-roam-ui-mode)
  :config
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow nil
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start t))


;; template
;; C o d: daily

(setq org-roam-dailies-capture-templates
      '(("d" "default" entry "** %<%I:%M %p>: %?"
         :if-new (file+head "%<%Y-%m-%d>.org" "#+title: %<%Y-%m-%d>\n"))))
(setq org-roam-capture-templates
      '(("k" "Start" plain "%?"
         :target (file+head "notes/journal.org" "#+title: ${title}\n#+roam_aliases:\n#+category: ${slug}\n#+filetags:\n#+date: %U\n\n")
         :immediate-finish t
         :unnarrowed t)
        
        ("n" "Note" plain
         "%?"
         :target (file+head "notes/%<%Y%m%d%H%M%S>-${slug}.org"
                            "#+title: ${title}\n#+roam_aliases:\n#+category: ${slug}\n#+filetags:\n#+date: %U\n#+stage: %^{Stage|Idea|Work-in-Progress|Final}\n#+why: %^{Why (purpose of this profile)}\n\n")
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
          ;; Add more templates here as needed
        )))


;; Utility functions and extensions for org-roam

;; Node display adjustments
;; Listing 3: Creating the property “type” on my nodes.
;; https://jethrokuan.github.io/org-roam-guide/
(with-eval-after-load 'org-roam
  (cl-defmethod org-roam-node-type ((node org-roam-node))
    "Return the TYPE of NODE."
    (condition-case nil
        (file-name-nondirectory
         (directory-file-name
          (file-name-directory
           (file-relative-name (org-roam-node-file node) org-roam-directory))))
      (error ""))))

(setq org-roam-node-display-template
      (concat "${type:15} ${title:*} " (propertize "${tags:10}" 'face 'org-tag)))

;; For org-roam-buffer-toggle, recommendations from official manual
(add-to-list 'display-buffer-alist
             '("\\*org-roam\\*"
               (display-buffer-in-direction)
               (direction . right)
               (window-width . 0.33)
               (window-height . fit-window-to-buffer)))


;; -------
;; utility functions
                                        ; https://github.com/org-roam/org-roam/wiki/Hitchhiker's-Rough-Guide-to-Org-roam-V2#showing-node-hierarchy

;; for org-roam-buffer-toggle
;; Recommendation in the official manual
(add-to-list 'display-buffer-alist
               '("\\*org-roam\\*"
                  (display-buffer-in-direction)
                  (direction . right)
                  (window-width . 0.33)
                  (window-height . fit-window-to-buffer)))


;https://systemcrafters.net/build-a-second-brain-in-emacs/5-org-roam-hacks/
(defun my/org-roam-copy-todo-to-today ()
  (interactive)
  (let ((org-refile-keep t) ;; Set this to nil to delete the original!
        (org-roam-dailies-capture-templates
          '(("t" "tasks" entry "%?"
             :if-new (file+head+olp "%<%Y-%m-%d>.org" "#+title: %<%Y-%m-%d>\n" ("Tasks")))))
        (org-after-refile-insert-hook #'save-buffer)
        today-file
        pos)
    (save-window-excursion
      (org-roam-dailies--capture (current-time) t)
      (setq today-file (buffer-file-name))
      (setq pos (point)))

    ;; Only refile if the target file is different than the current file
    (unless (equal (file-truename today-file)
                   (file-truename (buffer-file-name)))
      (org-refile nil nil (list "Tasks" today-file nil pos)))))

(add-to-list 'org-after-todo-state-change-hook
             (lambda ()
               (when (equal org-state "DONE")
                 (my/org-roam-copy-todo-to-today))))





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


(defun my/org-roam-find-node-by-tag (tag)
  "Find org-roam nodes with a specific TAG."
  (interactive "sTag: ")
  (org-roam-node-find
   nil
   nil
   (lambda (node)
     (member tag (org-roam-node-tags node)))))



;(package-installed-p 'jinx)

;; (defun autocompile ()
;;   "Automatically compile Emacs Lisp files upon saving."
;;   (interactive)
;;     (require 'bytecomp)
;;     (byte-compile-file (buffer-file-name)))

;; (add-hook 'after-save-hook 'autocompile)
