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

                                        ; sphinx-doc to C-c M-d
(use-package sphinx-doc
  :ensure t
  :init
  (sphinx-doc-mode t)
  (setq sphinx-doc-include-types t)
  :hook (python-mode . sphinx-doc-mode)
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
