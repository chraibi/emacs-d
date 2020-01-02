;;; Package --- summary
;;; Code:
;;; Commentary:

(message "Enter setup-cc")
(reftex-mode 0)
(require 'hlinum)
(linum-mode)
(setq linum-format "%3d \u2502 ")

;; ----- flycheck
(use-package flycheck
  :ensure t
  :init
  (global-flycheck-mode t)
  (global-flycheck-mode)
  (setq flycheck-idle-change-delay 10)
  (setq flycheck-check-syntax-automatically '(mode-enabled save))
  )



;; ----- clang-tidy
(load "/usr/local/Cellar/clang-format/2019-01-18/share/clang/clang-format.el")
;(require 'clang-format)
(global-set-key (kbd "C-c r") 'clang-format-region)
(global-set-key (kbd "C-c u") 'clang-format-buffer)
(eval-after-load 'flycheck
  '(add-hook 'flycheck-mode-hook #'flycheck-clang-tidy-setup))


;; ---- lsp-mode
(use-package lsp-mode
  :ensure t
  :commands lsp
  :custom
  (lsp-auto-guess-root nil)
  (lsp-enable-snippet nil)
  (lsp-prefer-flymake nil) ; Use flycheck instead of flymake
  :bind (:map lsp-mode-map ("C-c C-f" . lsp-format-buffer))
  :hook ((c-mode c++-mode) . lsp))


(use-package lsp-ui
  :after lsp-mode
  :diminish
  :commands lsp-ui-mode
  :custom-face
  (lsp-ui-doc-background ((t (:background nil))))
  (lsp-ui-doc-header ((t (:inherit (font-lock-string-face italic)))))
  :bind (:map lsp-ui-mode-map
              ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
              ([remap xref-find-references] . lsp-ui-peek-find-references)
              ("C-c u" . lsp-ui-imenu))
  :custom
  (lsp-ui-doc-enable t)
  (lsp-ui-doc-header t)
  (lsp-ui-doc-include-signature t)
  (lsp-ui-doc-position 'top)
  (lsp-ui-doc-border (face-foreground 'default))
  (lsp-ui-sideline-enable nil)
  (lsp-ui-sideline-ignore-duplicate t)
  (lsp-ui-sideline-show-code-actions nil)
  :config
  ;; Use lsp-ui-doc-webkit only in GUI
  (setq lsp-ui-doc-use-webkit t)
  ;; WORKAROUND Hide mode-line of the lsp-ui-imenu buffer
  ;; https://github.com/emacs-lsp/lsp-ui/issues/243
  (defadvice lsp-ui-imenu (after hide-lsp-ui-imenu-mode-line activate)
    (setq mode-line-format nil)))

(require 'helm-lsp)
(define-key lsp-mode-map [remap xref-find-apropos] #'helm-lsp-workspace-symbol)


;;(setq lsp-clangd-executable "clangd-6.0")
;;(setq lsp-clients-clangd-executable "clangd-6.0")


(use-package ccls
  :hook ((c-mode c++-mode) .
         (lambda () (require 'ccls) (lsp))))

(lsp--client-capabilities)
(setq company-transformers nil company-lsp-async t company-lsp-cache-candidates nil)
(setq ccls-executable "/usr/local/Cellar/ccls/0.20190823.3/bin/ccls")





;; ---- company

(use-package company
:ensure t
:config
(setq company-idle-delay 0)
(setq company-minimum-prefix-length 3)
;; (setq company-clang-executable "/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/clang")
(global-company-mode t)
)

(use-package company-lsp
  :ensure t
  :config
 (push 'company-lsp company-backends)
)

;; ---------------- counsel-etags
(use-package counsel-etags
  :ensure t
  :bind (("C-]" . counsel-etags-find-tag-at-point))
  :init
  (add-hook 'prog-mode-hook
        (lambda ()
          (add-hook 'after-save-hook
            'counsel-etags-virtual-update-tags 'append 'local)))
  :config
  (setq counsel-etags-update-interval 60)
  (add-to-list 'counsel-etags-ignore-directories "build"))


(eval-after-load 'counsel-etags
  '(progn
     ;; counsel-etags-ignore-directories does NOT support wildcast
     (add-to-list 'counsel-etags-ignore-directories "build")
     (add-to-list 'counsel-etags-ignore-directories "lib")
     (add-to-list 'counsel-etags-ignore-directories "demos")
     (add-to-list 'counsel-etags-ignore-directories "inifiles")
     (add-to-list 'counsel-etags-ignore-directories "scripts")
     (add-to-list 'counsel-etags-ignore-directories "visiLibity")
     (add-to-list 'counsel-etags-ignore-directories "Utest")
     (add-to-list 'counsel-etags-ignore-directories "poly2tri")
     (add-to-list 'counsel-etags-ignore-directories "bin")
     (add-to-list 'counsel-etags-ignore-directories "cnpy")
     (add-to-list 'counsel-etags-ignore-directories "xsd")
     (add-to-list 'counsel-etags-ignore-directories "cmake_build_debug")
     (add-to-list 'counsel-etags-ignore-directories "doc")
     (add-to-list 'counsel-etags-ignore-directories "cmake_modules")
     ;; counsel-etags-ignore-filenames supports wildcast
     (add-to-list 'counsel-etags-ignore-filenames "TAGS")
     (add-to-list 'counsel-etags-ignore-filenames "compile_commands.json")
     (add-to-list 'counsel-etags-ignore-filenames "LICENSE")
     (add-to-list 'counsel-etags-ignore-filenames "Makefile.pgi")
     (add-to-list 'counsel-etags-ignore-filenames "*.txt")
     (add-to-list 'counsel-etags-ignore-filenames "*.xml")
     (add-to-list 'counsel-etags-ignore-filenames "packages.config")
     (add-to-list 'counsel-etags-ignore-filenames "*.md")
     (add-to-list 'counsel-etags-ignore-filenames "*.json")))




(require 'modern-cpp-font-lock)
(modern-c++-font-lock-global-mode t)


(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.cc\\'" . c++-mode))

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

;; probleme mit python
;; (add-to-list 'semantic-default-submodes 'global-semantic-stickyfunc-mode)
;; (semantic-mode 1)
;; (require 'stickyfunc-enhance)

; help on hover
;(add-hook 'c-mode-hook 'c-turn-on-eldoc-mode)
;(add-hook 'c++-mode-hook 'c-turn-on-eldoc-mode)
;(setq c-eldoc-cpp-command "/usr/bin/clang")

;; turn on semantic
;(semantic-mode 1)

(global-set-key (kbd "C-c /") 'counsel-etags-grep-symbol-at-point)


(message "Provide setup-cc")
(provide 'setup-cc)
;;; setup-cc.el ends here
