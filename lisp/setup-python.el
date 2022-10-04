
;;; package --- summary
;;; Commentary:


(use-package python
  :mode ("\\.py\\'" . python-mode)
  :interpreter ("python3" . python-mode)
  :init
  (message "Activate python mode")
  ;; (setq flycheck-python-flake8-executable "/usr/local/bin/flake8")
  ;; (flycheck-select-checker 'python-flake8)
  (flycheck-mode 1)
  :config
  (setq python-shell-interpreter "/usr/local/bin/python3")
  (setq py-python-command "/usr/local/bin/python3")
  (setq org-babel-python-command "/usr/local/bin/python3")
  ;(setq flycheck-python-pycompile-executable "python3")
  )

;; (when (load "flycheck" t t)
;;   (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
;;   (add-hook 'elpy-mode-hook 'flycheck-mode))

;; (use-package lsp-mode
;;   :config
;;   (setq lsp-idle-delay 0.5
;;         lsp-enable-symbol-highlighting t
;;         lsp-enable-snippet nil  ;; Not supported by company capf, which is the recommended company backend
;;         lsp-pyls-plugins-flake8-enabled t)
;;   :hook
;;   ((python-mode . lsp)))

;; (use-package lsp-python-ms
;;   :ensure t
;;   :init (setq lsp-python-ms-auto-install-server t)
;;   :hook (python-mode . (lambda ()
;;                           (require 'lsp-python-ms)
;;                           (lsp))))  ; or lsp-deferred

(setq lsp-ui-doc-enable t
      lsp-ui-sideline-enable nil
      lsp-ui-doc-delay 1
      lsp-eldoc-hook nil
      lsp-ui-sideline-show-diagnostics t
      lsp-ui-doc-enable nil
      lsp-signature-doc-lines 2
      lsp-ui-doc-mode t
      )

(use-package lsp-jedi
  :ensure t
  :config
  (with-eval-after-load "lsp-mode"
    (add-to-list 'lsp-disabled-clients 'pyls)
    (add-to-list 'lsp-enabled-clients 'jedi)))


(use-package python-black
  :demand t
  :ensure t
  :after python
  :hook (python-mode . python-black-on-save-mode-enable-dwim))


(use-package python-isort
  :demand t
  :ensure t
  :after python
  hook (python-mode . py-isort-before-save)
  )

;(add-hook 'before-save-hook 'py-isort-before-save)

                                        ; sphinx-doc to C-c M-d
(use-package sphinx-doc
  :ensure t
  :init
  (sphinx-doc-mode t)
  (setq sphinx-doc-include-types t)
  :hook (python-mode . sphinx-doc-mode)
  )
  ;; sphinx-doc to C-c M-d
 

 (use-package elpy
   :ensure t
   :init
   (elpy-enable)
   (setq elpy-rpc-python-command "/usr/local/bin/python3")
   )

;;; Code:


(message "Provide setup-python")
(provide 'setup-python)
;;; setup-python.el ends here
