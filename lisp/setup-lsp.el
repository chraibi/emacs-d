;;; Package --- summary
;;; Code:
;;; Commentary:
;;; relevant for c++, python and rust

(message "Enter setup-lsp")
;; https://www.reddit.com/r/emacs/comments/gocrlq/i_really_dont_understand_why_lspui_shows/
(use-package lsp-mode
  :hook (
         (c++-mode . lsp)
         (c-mode . lsp)
         (python-mode . lsp))
  :commands lsp
  :custom
  ;; LSP Settings
  (lsp-clients-clangd-executable "clangd")
  (lsp-auto-guess-root t)
  (lsp-log-io nil)
  (lsp-restart 'auto-restart)
  (lsp-enable-symbol-highlighting t)
  (lsp-enable-on-type-formatting nil)
  (lsp-signature-auto-activate nil)
  (lsp-signature-render-documentation nil)
  (lsp-eldoc-hook nil)
  (lsp-lens-enable t)
  (lsp-modeline-code-actions-enable t)
  (lsp-modeline-diagnostics-enable t)
  (lsp-headerline-breadcrumb-enable t)
  (lsp-semantic-tokens-enable nil)
  (lsp-enable-folding nil)
  (lsp-enable-imenu nil)
  (lsp-enable-snippet nil)
  (lsp-diagnostics-provider :auto)
  (lsp-signature-doc-lines 1)

  ;; Miscellaneous settings
  (read-process-output-max (* 1024 1024)) ; 1M
  (lsp-idle-delay 0.5)
  (lsp-diagnostics-debounce-interval 1)
  ;; Completion settings
  (lsp-completion-show-detail t)
  (lsp-completion-show-kind t)

  :config
  (define-key lsp-mode-map (kbd "<f2>") lsp-command-map)
  (lsp-register-custom-settings
   '(("pyls.plugins.pyls_mypy.enabled" t t)
     ("pyls.plugins.pyls_mypy.live_mode" nil t)
     ("pyls.plugins.pyls_isort.enabled" t t)))
  
  )



(use-package helm-lsp
  :ensure t)



(setq imenu-auto-rescan t)
;;(setq lsp-ui-imenu-auto-refresh t)
(setq flycheck-python-ruff-executable "ruff")


; From https://github.com/flycheck/flycheck/issues/1974#issuecomment-1343495202
(flycheck-define-checker python-ruff
  "A Python syntax and style checker using the ruff utility.
To override the path to the ruff executable, set
`flycheck-python-ruff-executable'.
See URL `http://pypi.python.org/pypi/ruff'."
  :command ("ruff"
            "check"
            "--output-format=concise"
            (eval (when buffer-file-name
                    (concat "--stdin-filename=" buffer-file-name)))
            "-")
  :standard-input t
  :error-filter (lambda (errors)
                  (let ((errors (flycheck-sanitize-errors errors)))
                    (seq-map #'flycheck-flake8-fix-error-level errors)))
  :error-patterns
  ((warning line-start
            (file-name) ":" line ":" (optional column ":") " "
            (id (one-or-more (any alpha)) (one-or-more digit)) " "
            (message (one-or-more not-newline))
            line-end))
  :modes (python-mode python-ts-mode))


(add-to-list 'flycheck-checkers 'python-ruff)

(setq-default flycheck-disabled-checkers '(python-flake8 python-pylint))



(setq gc-cons-threshold (* 100 1024 1024)
      read-process-output-max (* 1024 1024)
      treemacs-space-between-root-nodes nil
      company-idle-delay 0.0
      company-minimum-prefix-length 1
      )  ;; clangd is fast


(setq company-transformers nil company-lsp-async t company-lsp-cache-candidates nil)


(message "Provide setup-lsp")
(provide 'setup-lsp)
;;; setup-lsp.el ends here
