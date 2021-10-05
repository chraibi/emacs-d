;;; Package --- summary
;;; Code:
;;; Commentary:

(message "Enter setup-cc")
(reftex-mode 0)
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.cc\\'" . c++-mode))


(use-package clang-format
  :after (s)
  :init
  (defun get-clang-format-option (config-str field is-num)
    "Retrieve a config option from a clang-format config.

CONFIG-STR is a string containing the entire clang-format config.
FIELD is specific option, e.g. `IndentWidth'.  IS-NUM is a
boolean that should be set to 1 if the option is numeric,
otherwise assumed alphabetic."
    (if is-num
        (let ((primary-match (s-match (concat "^" field ":[ \t]*[0-9]+") config-str)))
          (if primary-match
              (string-to-number (car (s-match "[0-9]+" (car primary-match))))
            0))
      (let ((primary-match (s-match (concat "^" field ":[ \t]*[A-Za-z]+") config-str)))
        (if primary-match
            (car (s-match "[A-Za-z]+$" (car primary-match)))
          ""))))
  :hook (c-mode-common . (lambda ()
                           (let* ((clang-format-config
                                   (shell-command-to-string "clang-format -dump-config"))
                                  (c-offset (get-clang-format-option clang-format-config "IndentWidth" t))
                                  (tabs-str (get-clang-format-option clang-format-config "UseTab" nil))
                                  (base-style
                                   (get-clang-format-option clang-format-config "BasedOnStyle" nil)))
                             (progn
                               (if (> c-offset 0)
                                   (setq-local c-basic-offset c-offset)
                                 (if (not (equal "" base-style))
                                     (cond ((or (equal "LLVM" base-style)
                                                (equal "Google" base-style)
                                                (equal "Chromium" base-style)
                                                (equal "Mozilla" base-style))
                                            (setq-local c-basic-offset 2))
                                           ((equal "WebKit" base-style)
                                            (setq-local c-basic-offset 4)))))
                               (if (not (equal "" tabs-str))
                                   (if (not (string-equal "Never" tabs-str))
                                       (setq-local indent-tabs-mode t)
                                     (setq-local indent-tabs-mode nil))
                                 (if (not (equal "" base-style))
                                     (cond ((or (equal "LLVM" base-style)
                                                (equal "Google" base-style)
                                                (equal "Chromium" base-style)
                                                (equal "Mozilla" base-style)
                                                (equal "WebKit" base-style))
                                            (setq-local indent-tabs-mode nil))))))))))


;; ----- flycheck
(use-package flycheck
  :ensure t
  :init
  (global-flycheck-mode t)
  (setq flycheck-idle-change-delay 10)
  (setq flycheck-check-syntax-automatically '(mode-enabled save))
  )


(with-eval-after-load 'cc-mode
  (fset 'c-indent-region 'clang-format-region)
  ;; (bind-keys :map c-mode-base-map
  ;;            ("<C-tab>" . company-complete)
  ;;            ("M-." . my-goto-symbol)
  ;;            ("M-," . xref-pop-marker-stack)
  ;;            ("C-M-\\" . clang-format-region)
  ;;            ("C-i" . clang-format)
  ;;            ("C-." . my-imenu)
  ;;            ("M-o" . cff-find-other-file)
  )

;;----------------- ccls
(use-package ccls
  :ensure t
  :config
  (setq ccls-executable "ccls")
  (setq lsp-prefer-flymake nil)
  ;; (setq-default flycheck-disabled-checkers '(c/c++-clang c/c++-cppcheck c/c++-gcc))
  :hook ((c++-mode) .
         (lambda () (require 'ccls) (lsp))))

(setq ccls-executable "/usr/local/Cellar/ccls/0.20210330_1/bin/ccls")

;;----------------



;; ---- lsp-mode
(use-package lsp-mode
  :ensure t
  :defer t
  :config
  (add-hook 'c++-mode-hook #'lsp)
  (add-hook 'python-mode-hook #'lsp)
  (add-hook 'rust-mode-hook #'lsp)
  (setq lsp-clients-clangd-args '("-j=4" "-background-index" "-log=error"))
  :init
  (setq lsp-keep-workspace-alive nil
        lsp-signature-doc-lines 5
        lsp-idle-delay 0.5
        lsp-prefer-capf t
        lsp-client-packages nil)
)
(define-key lsp-mode-map (kbd "<f2>") lsp-command-map)

;(push "[/\\\\][^/\\\\]*\\.\\(.github\\|.cache\\|.idea\\|build\\|bin\\)$" lsp-file-watch-ignored-directories) ; json

(with-eval-after-load 'lsp-mode
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]\\.build\\'")
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]\\build\\'")
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]\\third-party\\'")
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]\\systemtest\\'")
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]\\demos\\'")
  )



(use-package lsp-ui
  :after lsp
  :requires lsp-mode flycheck
  :hook (lsp-mode . lsp-ui-mode)
  :config
  (setq lsp-ui-doc-enable t
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
    ;; (setq lsp-ui-sideline-enable t)
    ;; (setq lsp-ui-sideline-show-hover nil)
    ;; (setq lsp-ui-doc-position 'bottom)
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


(require 'modern-cpp-font-lock)
(modern-c++-font-lock-global-mode t)



;; (require 'popup)
;; (setq c-auto-newline nil)



                                        ;deactivate reftex


; activate snippets
;(yas-global-mode +1)
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
(global-set-key [f9] 'compile)
(setq compilation-scroll-output 'first-error)


                                        ; --- doxymacs

;; (require 'doxymacs)
;; (add-hook 'c-mode-common-hook 'doxymacs-mode)
;; (defun my-doxymacs-font-lock-hook ()
;;     (if (or (eq major-mode 'c-mode) (eq major-mode 'c++-mode))
;;         (doxymacs-font-lock)))
;; (add-hook 'font-lock-mode-hook 'my-doxymacs-font-lock-hook)

;; (defun my-doxymacs-font-lock-hook ()
;;     (if (or (eq major-mode 'c-mode) (eq major-mode 'c++-mode))
;;         (doxymacs-font-lock)))
;;   (add-hook 'font-lock-mode-hook 'my-doxymacs-font-lock-hook)


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


(message "Provide setup-cc")
(provide 'setup-cc)
;;; setup-cc.el ends here
