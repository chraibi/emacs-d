;; deft-setup.el
;; Deft configurations


(use-package deft
  :init
  (message "Load deft")
  :ensure t
  :after org org-roam
  :bind
  ("C-c o s" . deft)
  :custom
  ((deft-recursive t)
   (deft-use-filter-string-for-filename t)
   (deft-default-extension "org"))
  :config
  (setq deft-directory (if (bound-and-true-p org-roam-directory)
                           org-roam-directory
                           "~/org-roam")) ;; Default path as fallback
)

(provide 'deft-setup)
