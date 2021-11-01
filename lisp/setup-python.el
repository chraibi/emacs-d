;;; package --- summary
;;; Commentary:


(use-package python
  :mode ("\\.py\\'" . python-mode)
  :interpreter ("python3" . python-mode)
  :init
  (message "Activate python mode")
  (flycheck-mode)
  :config
  (setq python-shell-interpreter "/usr/local/bin/python3")
  ;(setq py-python-command "/usr/local/bin/python3")
  ;(setq flycheck-python-pycompile-executable "python3")
  )



 

 (use-package elpy
   :ensure t
   :init
   (elpy-enable)
   )

;;; Code:


(message "Provide setup-python")
(provide 'setup-python)
;;; setup-python.el ends here
