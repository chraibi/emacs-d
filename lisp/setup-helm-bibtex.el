;; helm-bibtex.el
;; Config for helm-bibtex


(use-package helm-bibtex
  :ensure t
  :config
  (setq bibtex-completion-bibliography
        '(
          "~/Zotero/PED-Modeling.bib"
          "~/Zotero/Writing.bib"
          )
        )
  (setq bibtex-completion-library-path '("~/Zotero/storage/"))
  (setq bibtex-completion-pdf-field "file")
  (setq bibtex-completion-notes-path "/Users/chraibi/Library/CloudStorage/Orgfiles/org-files/org-roam/papers/bibnote.org")
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

(provide 'helm-bibtex)
