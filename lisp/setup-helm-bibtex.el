;; helm-bibtex.el
;; Config for helm-bibtex


(use-package helm-bibtex
  :ensure t
  :config
  (setq bibtex-completion-notes-path "/Users/chraibi/Library/CloudStorage/Orgfiles/org-files/org-roam/papers/bibnote.org")

  (setq bibtex-completion-additional-search-fields '(keywords tags))
  (setq bibtex-completion-pdf-symbol "⌘")
  (setq bibtex-completion-notes-symbol "✎")

  (setq bibtex-completion-format-citation-functions
        '((org-mode      . bibtex-completion-format-citation-org-title-link-to-PDF)
          (latex-mode    . bibtex-completion-format-citation-cite)
          (markdown-mode . bibtex-completion-format-citation-pandoc-citeproc)
          (default       . bibtex-completion-format-citation-default))
        )


   (setq bibtex-completion-pdf-open-function
        (lambda (fpath)
          (message "Opening PDF with Skim: %s" fpath)
          (call-process "open" nil 0 nil "-a" "/Applications/Skim.app" fpath))))
  

(provide 'helm-bibtex)
