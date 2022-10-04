;;; Package --- summary
;;; Code:
;;; Commentary:

(message "Enter setup-cc")
(reftex-mode 0)
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.cc\\'" . c++-mode))


;; try clang-format+ because format on save does not work for me
(use-package clang-format+
  :ensure t
  :init
  (add-hook 'c-mode-common-hook #'clang-format+-mode) ;
  :hook (c-mode-common-hook . clang-format+-mode)
  )

;; ----- flycheck
(use-package flycheck
  :ensure t
  :init
  (global-flycheck-mode t)
  (setq flycheck-idle-change-delay 10)
  (setq flycheck-check-syntax-automatically '(mode-enabled save))
  )

(use-package flycheck-clang-tidy
  :ensure t
  :after flycheck
  :hook
  (flycheck-mode . flycheck-clang-tidy-setup)
  )


;;----------------- ccls now using clangd
;; (use-package ccls
;;   :ensure t
;;   :config
;;   (setq ccls-executable "ccls")
;;   (setq lsp-prefer-flymake nil)
;;   ;; (setq-default flycheck-disabled-checkers '(c/c++-clang c/c++-cppcheck c/c++-gcc))
;;   :hook ((c++-mode) .
;;          (lambda () (require 'ccls) (lsp)))
;;   )

;; (setq ccls-executable "/usr/local/Cellar/ccls/0.20220729_1/bin/ccls")



;; ---- lsp-mode
(use-package lsp-mode
  :ensure t
  :defer t
  :diminish (lsp-mode . "lsp")
  :config
  (add-hook 'c++-mode-hook #'lsp)
  (add-hook 'python-mode-hook #'lsp)
  (add-hook 'rust-mode-hook #'lsp)
  (setq lsp-disabled-clients '(ccls))
  (setq lsp-clients-clangd-args '("-j=4" "-background-index" "-log=error"))
  :init
  (setq lsp-auto-guess-root t       ; Detect project root
        lsp-keep-workspace-alive nil
        lsp-enable-imenu t
        lsp-signature-doc-lines 5
        lsp-idle-delay 0.1
        lsp-prefer-provider t
        lsp-restart 'auto-restart
        lsp-client-packages nil
        lsp-modeline-diagnostics-enable t
        lsp-enable-symbol-highlighting t
        lsp-enable-snippet nil;; Not supported by company capf, which is the recommended company backend
        lsp-pyls-plugins-flake8-enabled t
        lsp-modeline-code-actions-mode '(count icon name)
        )
  )

(use-package lsp-clangd
  :init
  (add-hook 'c-mode--hook #'lsp-clangd-c-enable)
  (add-hook 'c++-mode-hook #'lsp-clangd-c++-enable)
  (add-hook 'objc-mode-hook #'lsp-clangd-objc-enable)
  (setq lsp-clangd-executable "/usr/local/opt/llvm/bin/clangd")
  (setq lsp-clangd-binary-path "/usr/local/opt/llvm/bin/clangd")
  )


(with-eval-after-load 'lsp-mode
  (yas-global-mode)
  (lsp-treemacs-sync-mode 1)
  (add-hook 'lsp-mode-hook #'lsp-enable-which-key-integration)
  (push "[/\\\\][^/\\\\]*\\.\\(.github\\|.cache\\|.idea\\|build\\|bin\\|third-party\\|demos\\|docs\\|jpsreport\\)$" lsp-file-watch-ignored-directories)
  (add-to-list 'lsp-file-watch-ignored-directories "/Users/chraibi/workspace/jupedsim/jpscore/build/")
  (add-to-list 'lsp-file-watch-ignored-directories "/workspace/jupedsim/jpscore/third-party/")
  (add-to-list 'lsp-file-watch-ignored-directories "~/workspace/jupedsim/jpscore/systemtest/")
  (add-to-list 'lsp-file-watch-ignored-directories "~/workspace/jupedsim/jpscore/third-party/")
  )

(define-key lsp-mode-map (kbd "<f2>") lsp-command-map)

(setq gc-cons-threshold (* 100 1024 1024)
      read-process-output-max (* 1024 1024)
      treemacs-space-between-root-nodes nil
      company-idle-delay 0.0
      company-minimum-prefix-length 1
      lsp-idle-delay 0.1)  ;; clangd is fast



(use-package lsp-ui
  :ensure t
  :after lsp
  :requires lsp-mode flycheck
  :hook (lsp-mode . lsp-ui-mode)
  :config
  (setq lsp-ui-doc-enable nil
        lsp-ui-doc-show-with-cursor nil
        lsp-lens-enable t
        lsp-ui-sideline-enable nil
        lsp-ui-sideline-show-code-actions ni        
        lsp-ui-doc-use-childframe t
        lsp-ui-doc-position 'top
        lsp-ui-doc-include-signature t
        lsp-ui-sideline-enable nil
        lsp-ui-flycheck-enable t
        lsp-ui-flycheck-list-position 'right
        lsp-ui-flycheck-live-reporting t
        lsp-ui-peek-enable t
        lsp-ui-peek-list-width 60
        lsp-ui-peek-peek-height 25)
)





(use-package helm-lsp
  :ensure t)

(define-key lsp-mode-map [remap xref-find-apropos] #'helm-lsp-workspace-symbol)
(lsp--client-capabilities)



(setq company-transformers nil company-lsp-async t company-lsp-cache-candidates nil)


;; (use-package company-capf
;;   :ensure t
;;   :config
;;  (push 'company-capf company-backends)
;; )


(use-package modern-cpp-font-lock
  :ensure t
  :init
  (modern-c++-font-lock-global-mode t)
  )


;-------------------------------

(defun my-recompile ()
  "Run compile and resize the compile window closing the old one if necessary."
  (interactive)
  (progn
    (if (get-buffer "*compilation*") ; If old compile window exists
        (progn
          (delete-windows-on (get-buffer "*compilation*")) ; Delete the compilation windows
          (kill-buffer "*compilation*") ; and kill the buffers
          )
      )
    (call-interactively 'compile)
    (enlarge-window 20)
    )
  )

(defun my-next-error ()
  "Move point to next error and highlight it."
  (interactive)
  (progn
    (next-error)
    (end-of-line-nomark)
    (beginning-of-line-mark)
    )
  )

(defun my-previous-error ()
  "Move point to previous error and highlight it."
  (interactive)
  (progn
    (previous-error)
    (end-of-line-nomark)
    (beginning-of-line-mark)
    )
  )

;; (global-set-key (kbd "C-n") 'my-next-error)
;; (global-set-key (kbd "C-p") 'my-previous-error)
;; (global-set-key (kbd "C-x <f9>") 'my-recompile)


(setq compilation-scroll-output 'first-error)



(defun my-javadoc-return ()
  "Advanced C-m for Javadoc multiline comments.
Inserts `*' at the beggining of the new line if
unless return was pressed outside the comment"
  (interactive)
  (setq last (point))
  (setq is-inside
        (if (search-backward "*/" nil t)
            ;; there are some comment endings - search forward
            (if (search-forward "/*" last t)
                't
              'nil)
          ;; it's the only comment - search backward
          (goto-char last)
          (if (search-backward "/*" nil t)
              't
            'nil
            )
          )
        )
  ;; go to last char position
  (goto-char last)
  ;; the point is inside some comment, insert `*'
  (if is-inside
      (progn
        (insert "\n*")
        (indent-for-tab-command))
    ;; else insert only new-line
    (insert "\n")))
(add-hook 'c++-mode-hook (lambda ()
  (local-set-key "\r" 'my-javadoc-return)))

;; ---------------- 

(use-package lsp-treemacs
  :after (lsp-mode treemacs)
  :ensure t
  :commands lsp-treemacs-errors-list
  :bind (:map lsp-mode-map
              ("M-9" . lsp-treemacs-errors-list)))



(message "Provide setup-cc")
(provide 'setup-cc)
;;; setup-cc.el ends here
