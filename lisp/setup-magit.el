;;; Package --- Summary
;;; Commentary:
;;; Code:
;(require 'magit)

(define-advice magit-status (:around (orig &rest args) magit-fullscreen)
  "Open `magit-status' fullscreen, saving the window layout for restore."
  (window-configuration-to-register :magit-fullscreen)
  (apply orig args)
  (delete-other-windows))

(defun magit-quit-session ()
  "Restores the previous window configuration and kills the magit buffer"
  (interactive)
  (kill-buffer)
  (jump-to-register :magit-fullscreen))

(define-key magit-status-mode-map (kbd "q") 'magit-quit-session)

(provide 'setup-magit)
;;; setup-magit.el ends here
