;; org-ref.el  -*- lexical-binding: t; -*-
;; Initialisation of org-ref. Eventually related to helm-bibtex.el
;; also config for org-roam-bibtex



(use-package org-ref
  :init
  (message "Init org-ref")
  :ensure t
  :defer t
  :commands (org-ref-insert-link)
  :bind (:map org-mode-map ("C-c )" . org-ref-insert-link))
  :config
  (setq bibtex-completion-bibliography '(
                                         "~/Zotero/PED-Modeling.bib"
                                         "~/Zotero/Writing.bib"
                                         )
        bibtex-completion-pdf-field "file"
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
        ))



(use-package org-roam-bibtex
  :init
  (message "Init org-roam-bibtex")
  :ensure t
  :after org-roam
  :hook (org-roam-mode . org-roam-bibtex-mode)
  :config
  (setq orb-preformat-keywords
        '("=key=" "title" "url" "file" "author-or-editor" "keywords" "citekey")
        orb-process-file-keyword t
        )   
  )

(with-eval-after-load 'org-ref-helm
  (setq org-ref-insert-cite-function 'org-ref-cite-insert-helm))

;; Background-load the citation/bibliography stack ~1s after Emacs goes idle,
;; instead of eagerly during init.  This pulls org-ref-helm -> org-ref ->
;; helm/citeproc/bibtex, triggering the :config above off the critical path.
(run-with-idle-timer 1 nil (lambda () (require 'org-ref-helm)))

(provide 'org-ref)
