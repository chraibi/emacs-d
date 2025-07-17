;;; setup-lsp.el --- LSP configuration for C++, Python and Rust -*- lexical-binding: t; -*-
;;; Commentary:
;;; LSP configuration relevant for C++, Python and Rust
;;; Code:

(message "Enter setup-lsp")

;; Performance tuning - set early
(setq gc-cons-threshold (* 100 1024 1024)
      read-process-output-max (* 1024 1024))

;; Main LSP mode configuration
(use-package lsp-mode
  :ensure t
  :hook ((c++-mode . lsp-deferred)
         (c-mode . lsp-deferred)
         (python-mode . lsp-deferred)
         (python-ts-mode . lsp-deferred)  ; Add support for tree-sitter mode
         (rust-mode . lsp-deferred)       ; Add rust support as mentioned
         (lsp-mode . lsp-enable-which-key-integration))
  :commands (lsp lsp-deferred)
  :custom
  ;; LSP Core Settings
  (lsp-auto-guess-root t)
  (lsp-log-io nil)  ; Set to t for debugging
  (lsp-restart 'auto-restart)
  (lsp-idle-delay 0.5)
  
  ;; UI Features
  (lsp-enable-symbol-highlighting t)
  (lsp-lens-enable t)
  (lsp-modeline-code-actions-enable t)
  (lsp-modeline-diagnostics-enable t)
  (lsp-headerline-breadcrumb-enable t)
  (lsp-semantic-tokens-enable t)  ; Enable semantic highlighting
  
  ;; Completion (company integration is built-in)
  (lsp-completion-provider :capf)  ; Use completion-at-point
  (lsp-completion-show-detail t)
  (lsp-completion-show-kind t)
  (lsp-completion-enable t)
  
  ;; Diagnostics (flycheck integration is built-in)
  (lsp-diagnostics-provider :flycheck)  ; Use flycheck for diagnostics
  (lsp-flycheck-live-reporting t)       ; Real-time error reporting
  (lsp-diagnostics-debounce-interval 0.2)
  
  ;; Formatting and other features
  (lsp-enable-on-type-formatting nil)
  (lsp-signature-auto-activate t)       ; Enable signature help
  (lsp-signature-render-documentation t)
  (lsp-eldoc-render-all t)
  (lsp-enable-folding t)                ; Enable folding
  (lsp-enable-imenu t)                  ; Enable imenu
  (lsp-enable-snippet t)                ; Enable snippets
  
  ;; Language-specific settings
  (lsp-clients-clangd-executable "clangd")
  (lsp-pyls-plugins-pylint-enabled nil) ; Use ruff instead
  (lsp-pyls-plugins-flake8-enabled nil) ; Use ruff instead
  
  :config
  ;; Key bindings
  (define-key lsp-mode-map (kbd "C-c l") lsp-command-map)
  
  ;; Python-specific LSP settings
  (lsp-register-custom-settings
   '(("pyls.plugins.pyls_mypy.enabled" t t)
     ("pyls.plugins.pyls_mypy.live_mode" nil t)
     ("pyls.plugins.pyls_isort.enabled" t t)
     ("pyls.plugins.rope_completion.enabled" t t)))
  
  ;; Ensure LSP servers are available
  (add-to-list 'lsp-language-id-configuration '(python-ts-mode . "python")))

;; LSP UI for enhanced interface
(use-package lsp-ui
  :ensure t
  :after lsp-mode
  :custom
  (lsp-ui-doc-enable t)
  (lsp-ui-doc-show-with-cursor nil)
  (lsp-ui-doc-show-with-mouse t)
  (lsp-ui-doc-position 'at-point)
  (lsp-ui-sideline-enable t)
  (lsp-ui-sideline-show-hover nil)
  (lsp-ui-sideline-show-diagnostics t)
  (lsp-ui-sideline-show-code-actions t)
  (lsp-ui-peek-enable t)
  (lsp-ui-imenu-enable t)
  :bind (:map lsp-ui-mode-map
              ("C-c u" . lsp-ui-imenu)
              ("M-." . lsp-ui-peek-find-definitions)
              ("M-?" . lsp-ui-peek-find-references)))

;; Helm integration for LSP
(use-package helm-lsp
  :ensure t
  :after (helm lsp-mode)
  :bind (:map lsp-mode-map
              ("C-c h l" . helm-lsp-workspace-symbol)
              ("C-c h g" . helm-lsp-global-workspace-symbol)))

;; Company completion is now built into lsp-mode
;; No separate company-lsp package needed

;; Python-specific: Ruff checker configuration
(when (executable-find "ruff")
  (setq flycheck-python-ruff-executable "ruff")
  
  ;; Define ruff checker
  (flycheck-define-checker python-ruff
    "A Python syntax and style checker using the ruff utility."
    :command ("ruff"
              "check"
              "--output-format=text"  ; Changed from concise for better error info
              "--force-exclude"       ; Respect .gitignore and exclude patterns
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
  
  ;; Add ruff to flycheck checkers and disable conflicting ones
  (add-to-list 'flycheck-checkers 'python-ruff)
  (setq-default flycheck-disabled-checkers '(python-flake8 python-pylint python-pycompile)))

;; Company configuration for better completion
(use-package company
  :ensure t
  :hook (prog-mode . company-mode)
  :custom
  (company-idle-delay 0.1)
  (company-minimum-prefix-length 1)
  (company-selection-wrap-around t)
  (company-tooltip-align-annotations t)
  (company-frontends '(company-pseudo-tooltip-frontend
                       company-echo-metadata-frontend))
  :bind (:map company-active-map
              ("C-n" . company-select-next)
              ("C-p" . company-select-previous)
              ("TAB" . company-complete-selection)))

;; Which-key integration for LSP commands
(use-package which-key
  :ensure t
  :config
  (which-key-mode))

;; Additional settings
(setq imenu-auto-rescan t
      treemacs-space-between-root-nodes nil)

;; Auto-start LSP for Python files
(add-hook 'python-mode-hook #'lsp-deferred)
(add-hook 'python-ts-mode-hook #'lsp-deferred)

(message "Provide setup-lsp")
(provide 'setup-lsp)
;;; setup-lsp.el ends here
