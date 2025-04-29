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
  (setq python-shell-interpreter "/usr/bin/python3")
  (setq py-python-command "/usr/bin/python3")
  (setq org-babel-python-command "/usr/bin/python3")
  (setq 
      python-shell-interpreter-args "-i"
      python-shell-completion-native-enable nil
      python-sort-imports-on-save nil
      python-sort-imports-on-save-before-answers nil
      python-sort-imports-on-save-with-isort nil
      python-sort-imports-on-save-isort-options '("--settings-path" "~/.config/isort.cfg"))
  )


;; Set the directory where your virtual environments are located

;; Install and configure pyvenv
(use-package pyvenv
  :ensure t
  :config
  ;; Set the directory where your virtual environments are located
  (setq pyvenv-workon-home "/Users/chraibi/.venvs")
  ;; Enable pyvenv mode
  (pyvenv-mode t)

  ;; Set the correct Python interpreter when activating a virtual environment
  (setq pyvenv-post-activate-hooks
        '(;; Set the Python interpreter to the one in the activated virtual environment
          (lambda ()
            (setq python-shell-interpreter (concat pyvenv-virtual-env "bin/python3")))))

  ;; Reset the Python interpreter when deactivating the virtual environment
  (setq pyvenv-post-deactivate-hooks
        '(;; Restore the default system Python interpreter
          (lambda ()
            (setq python-shell-interpreter "python3")))))


;; Enable Org Babel for Python
(org-babel-do-load-languages
 'org-babel-load-languages
 '((python . t)))

(use-package reformatter
  :hook 
  (python-mode . ruff-format-on-save-mode)
  :config
  (reformatter-define ruff-format
    :program "ruff"
    :args `("format" "--stdin-filename" ,buffer-file-name "-")))




(message "Provide setup-python")
(provide 'setup-python)
;;; setup-python.el ends here
