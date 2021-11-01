;;; Package --- Summary
;;; --- () and []
;;; Code:
;;; Commentary:


(setq  electric-pair-mode t)

(defun electric-pair ()
  "If at end of line, insert character pair without surrounding spaces.  Otherwise, just insert the typed character."
  (interactive)
  (if (eolp) (let (parens-require-spaces) (insert-pair)) (self-insert-command 1)))

(provide 'setup-electric)
;;; setup-electric.el ends here
