
;;; package --- summary
;;; Commentary:


(use-package python
  :mode ("\\.py\\'" . python-mode)
  :interpreter ("python3" . python-mode)
  :init
  (message "Activate python mode")
  ;; (setq flycheck-python-flake8-executable "/usr/local/bin/flake8")
  ;; (flycheck-select-checker 'python-flake8)
  ;; (flycheck-mode 1)
  :config
  (setq python-shell-interpreter "/usr/local/bin/python3")
  (setq py-python-command "/usr/local/bin/python3")
  (setq org-babel-python-command "/usr/local/bin/python3")
  (setq 
      python-shell-interpreter-args "-i"
      python-shell-completion-native-enable nil
      python-sort-imports-on-save t
      python-sort-imports-on-save-before-answers t
      python-sort-imports-on-save-with-isort t
      python-sort-imports-on-save-isort-options '("--settings-path" "~/.config/isort.cfg"))
  )

;; pyright and elpy somehow use node. node uses extensive cpu time.

;;(setq py-install-directory "~/.emacs.d/lisp/pdee-master")
;;(add-to-list 'load-path py-install-directory)

;; (use-package lsp-pyright
;;   :ensure t
;;   :hook (python-mode . (lambda () (require 'lsp-pyright)))
;;   :init (when (executable-find "python3")
;;           (setq lsp-pyright-python-executable-cmd "python3")))

;; (setq pyright-args '("--workers" "2"))  ;

;; emacs 29.1 has eglot built-in
;; Install eglot (if needed)
(unless (package-installed-p 'eglot)
  (package-refresh-contents)
  (package-install 'eglot))
(add-hook 'python-mode-hook 'eglot-ensure)
(setq eglot-python-server 'pylsp)


(use-package python-black
  :demand t
  :ensure t
  :after python
  :hook (python-mode . python-black-on-save-mode) 
  )

(use-package py-isort
  :demand t
  :ensure t
  :after python
  hook (python-mode . py-isort-before-save )
  )
; sphinx-doc to C-c M-d
(use-package sphinx-doc
  :ensure t
  :init
  (sphinx-doc-mode t)
  (setq sphinx-doc-include-types t)
  :hook (python-mode . sphinx-doc-mode)
  )
  ;; sphinx-doc to C-c M-d

 ;; (use-package elpy
 ;;   :ensure t
 ;;   :init
 ;;   (elpy-enable)
 ;;   (setq elpy-rpc-python-command "/usr/local/bin/python3")   
 ;;   )

;; (when (load "flycheck" t t)
;;   (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
;;   (add-hook 'elpy-mode-hook 'flycheck-mode))

(setq flycheck-enabled-checkers '(python-mypy))
(setq flycheck-disabled-checkers '(python-pylint))
(setq flycheck-select-checker 'python-mypy)

        
;;; Code:


(message "Provide setup-python")
(provide 'setup-python)
;;; setup-python.el ends here
