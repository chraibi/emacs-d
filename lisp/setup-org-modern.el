;; org-modern.el
;; Modern Org mode

(use-package org-modern
  :ensure t
  :init
  (add-hook 'org-mode-hook #'org-modern-mode)
  (add-hook 'org-agenda-finalize-hook #'org-modern-agenda)
  :config
  (setq
   ;; Edit settings
   org-auto-align-tags nil
   org-tags-column 0
   org-catch-invisible-edits 'show-and-error
   org-special-ctrl-a/e t
   org-insert-heading-respect-content t

   ;; Org styling, hide markup etc.
   org-hide-emphasis-markers t
   org-pretty-entities t
   org-ellipsis "…"

   ;; Agenda styling
   ; Uncomment and adjust the following lines as needed:
   ;; org-agenda-tags-column 0
   ;; org-agenda-block-separator ?─
   org-agenda-time-grid
   '((daily today require-timed)
     (800 1000 1200 1400 1600 1800 2000)
     " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄")
   org-agenda-current-time-string
   "⭠ now ─────────────────────────────────────────────────"))

;; (defun autocompile ()
;;   "Automatically compile Emacs Lisp files upon saving."
;;   (interactive)
;;     (require 'bytecomp)
;;     (byte-compile-file (buffer-file-name)))

;; (add-hook 'after-save-hook 'autocompile)



(provide 'org-modern)
