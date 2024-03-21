;;; Package --- summary
;;; Code:
;;; Commentary:
;;; relevant for c++, python and rust

(message "Enter setup-lsp")
;; https://www.reddit.com/r/emacs/comments/gocrlq/i_really_dont_understand_why_lspui_shows/
(use-package lsp-mode
  :hook (
          (c-or-c++-mode . lsp-deferred)
          (python-mode . lsp-deferred))
  :commands lsp
  :custom
  ;; LSP Settings
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
  (lsp-signature-doc-lines 2)

  ;; Miscellaneous settings
  (read-process-output-max (* 1024 1024)) ; 1M
  (lsp-idle-delay 1)
  (lsp-diagnostics-debounce-interval 1)
  ;; Completion settings
  (lsp-completion-show-detail t)
  (lsp-completion-show-kind t)

  :config
  (define-key lsp-mode-map (kbd "<f2>") lsp-command-map)

  ;; Add comments to explain settings
  (setq lsp-auto-guess-root t
        lsp-completion-show-detail t
        lsp-completion-show-kind t
        lsp-log-io nil
        lsp-restart 'auto-restart
        lsp-enable-symbol-highlighting t
        lsp-enable-on-type-formatting nil
        lsp-signature-auto-activate nil
        lsp-signature-render-documentation t
        lsp-eldoc-hook nil
        lsp-lens-enable t
        lsp-modeline-code-actions-enable t
        lsp-modeline-diagnostics-enable t
        lsp-headerline-breadcrumb-enable t
        lsp-semantic-tokens-enable nil
        lsp-enable-folding t
        lsp-enable-imenu nil
        lsp-enable-snippet nil
        read-process-output-max (* 1024 1024) ;; 1M
        lsp-idle-delay 5
        lsp-diagnostics-provider :auto
        lsp-signature-doc-lines 2
        lsp-headerline-arrow "➤"
        )
  (lsp-register-custom-settings
   '(("pyls.plugins.pyls_mypy.enabled" t t)
     ("pyls.plugins.pyls_mypy.live_mode" nil t)
     ("pyls.plugins.pyls_black.enabled" t t)
     ("pyls.plugins.pyls_isort.enabled" t t)))
  
  )





(use-package lsp-ui
  :ensure t
  :after lsp
  :requires lsp-mode flycheck
  :hook (lsp-mode . lsp-ui-mode)
  :config
  (setq 
        lsp-ui-doc-enable t
        lsp-ui-doc-mode t
        lsp-ui-doc-show-with-cursor t        
        lsp-ui-sideline-enable t
        lsp-ui-sideline-show-diagnostics t
        lsp-ui-sideline-show-code-actions t
        lsp-modeline-code-actions-enable nil
        lsp-ui-doc-use-childframe nil
        lsp-ui-doc-position 'top
        lsp-ui-doc-include-signature t
        lsp-ui-flycheck-enable t
        lsp-ui-flycheck-list-position 'right
        lsp-ui-flycheck-live-reporting t
        lsp-ui-peek-enable t
        lsp-ui-doc-delay 1                
        lsp-ui-peek-list-width 60
        lsp-ui-peek-peek-height 25)
  )

;; (use-package lsp-pyright
;;   :ensure t
;;   :hook (python-mode . (lambda () (require 'lsp-pyright)))
;;   :init (when (executable-find "python3")
;;           (setq lsp-pyright-python-executable-cmd "python3")))


(use-package helm-lsp
  :ensure t)


(use-package lsp-clangd
  :init
  (add-hook 'c-mode--hook #'lsp-clangd-c-enable)
  (add-hook 'c++-mode-hook #'lsp-clangd-c++-enable)
  (setq lsp-clangd-executable "/usr/local/opt/llvm/bin/clangd")
  (setq lsp-clangd-binary-path "/usr/local/opt/llvm/bin/clangd")
  )


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
