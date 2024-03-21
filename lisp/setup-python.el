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
      python-sort-imports-on-save nil
      python-sort-imports-on-save-before-answers nil
      python-sort-imports-on-save-with-isort nil
      python-sort-imports-on-save-isort-options '("--settings-path" "~/.config/isort.cfg"))
  )


(setenv "WORKON_HOME" "/Users/chraibi/virtualenv/.pedpy_env/")
(use-package pyvenv
  :ensure t
  :config
  (pyvenv-mode t)

  ;; Set correct Python interpreter
  (setq pyvenv-post-activate-hooks
        (list (lambda ()
                (setq python-shell-interpreter (concat pyvenv-virtual-env "bin/python3")))))
  (setq pyvenv-post-deactivate-hooks
        (list (lambda ()
                (setq python-shell-interpreter "python3")))))


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
;; (unless (package-installed-p 'eglot)
;;   (package-refresh-contents)
;;   (package-install 'eglot))
;; (add-hook 'python-mode-hook 'eglot-ensure)
;; (setq eglot-python-server 'pylsp)


(use-package python-black
  :demand t
  :ensure t
  :after python
  :hook (python-mode .) 
  )

(use-package py-isort
  :demand t
  :ensure t
  :after python
  hook (python-mode .)
  )
; sphinx-doc to C-c M-d
;; (use-package sphinx-doc
;;   :ensure t
;;   :init
;;   (sphinx-doc-mode t)
;;   (setq sphinx-doc-include-types t)
;;   :hook (python-mode . sphinx-doc-mode)
;;   )
  ;; sphinx-doc to C-c M-d

 (use-package elpy
   :ensure t
   :init
   (elpy-enable)
   (setq elpy-rpc-python-command "/usr/local/bin/python3")   
   )


(use-package reformatter
  :hook 
  (python-mode . ruff-format-on-save-mode)
  :config
  (reformatter-define ruff-format
    :program "ruff"
    :args `("format" "--stdin-filename" ,buffer-file-name "-")))



;; (when (load "flycheck" t t)
;;   (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
;;   (add-hook 'elpy-mode-hook 'flycheck-mode))

;; (setq flycheck-enabled-checkers '(python-mypy))
;; (setq flycheck-disabled-checkers '(python-pylint))
;; (setq flycheck-select-checker 'python-mypy)

        
;;; Code:


(message "Provide setup-python")
(provide 'setup-python)
;;; setup-python.el ends here
