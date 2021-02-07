;;; package --- summary
;;; Commentary:

(require 'python)
(elpy-enable)
;(add-hook 'python-mode-hook 'fci-mode)
;(require 'setup-hlinum)
(message "Activate python mode")
;(package-initialize)
;;; Code:
(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))
(setq ipython-command "/usr/local/bin/ipython")
(setq python-shell-interpreter "/usr/local/bin/python3")
(setq py-python-command "/usr/local/bin/python3")
(setq flycheck-python-pycompile-executable "python3")
(setq indent-tabs-mode nil)
(setq python-indent-guess-indent-offset 1)
(linum-mode 1)

(auto-complete-mode 1)
;; (python-indent-offset . 4)

;; (require 'fill-column-indicator)

;; (setq fci-rule-color "black")

(defun flycheck-python-setup ()
  (flycheck-mode)
  )
;(add-hook 'python-mode-hook #'flycheck-python-setup)


;; ;; Automatically remove trailing whitespace when file is saved.


(add-hook 'python-mode-hook (lambda ()
                              (require 'sphinx-doc)
                              (sphinx-doc-mode t)))

;; (setq py-python-command-args '( "--colors=Linux"))
(message "leave python setup")
(provide 'setup-python)
;;; setup-python.el ends here
