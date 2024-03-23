;; org-crypt.el
;; org-crypt


;;--------------- crypt
(use-package org-crypt
  :after (org)
  :config
  (org-crypt-use-before-save-magic)
  (setq org-crypt-key nil
        auto-save-default nil
        org-tags-exclude-from-inheritance '("crypt")))

(provide 'org-crypt)
