;;; Package --- load coding configs
;;; Code:
;;; Commentary: python, cmake, c++ clang-format
(message "Loading load_coding.el")

;; ;;------ cmake support
(message "> load cmake-mode")
(use-package cmake-mode
  :ensure t
  :mode ("CMakeLists\\.txt\\'" "\\.cmake\\'"))

(use-package cmake-font-lock
  :after (cmake-mode)
  :hook (cmake-mode . cmake-font-lock-activate))



(defun my/compile-split-right (orig &rest args)
  (let ((split-width-threshold 0)
        (split-height-threshold nil))
    (apply orig args)))

(advice-add 'compile :around #'my/compile-split-right)
;;Replace defadvice with advice-add
;; (defadvice compile (around split-horizontally activate)
;;   (let ((split-width-threshold 0)
;;         (split-height-threshold nil))
;;     ad-do-it))



(make-variable-buffer-local 'compile-command)
(message "> finished loading cmake-mode")

(use-package clang-format
  :init
  (message "Loading clang-format")
  :ensure t
  :bind
  (("C-c r" . 'clang-format-region)
   ("C-c u" . 'clang-format-buffer)
   )
  :config
  (setq clang-format-executable "/opt/homebrew/bin/clang-format"))

(use-package setup-cc
  :init
  (message "loading setup-cc")
  :after (clang-format)
  :defer t
  :config
  (when (string-suffix-p ".cpp" (buffer-file-name))
    (message "Loading setup-cc!"))
  )

;; ------ Python setup (local file)
(message "load python-setup")
(defun my/python-mode-setup ()
  "Load Python setup."
  (message "Custom python hook run")
  (require 'setup-python nil 'noerror))

(add-hook 'python-mode-hook #'my/python-mode-setup)
(add-hook 'python-ts-mode-hook #'my/python-mode-setup)


(use-package setup-lsp
  :init
  (message "loading setup-lsp")
  ; :defer t
  )


(use-package copilot
  :ensure t
  :hook ((python-mode python-ts-mode c++-mode c++-ts-mode c-mode c-ts-mode) . copilot-mode)
  :bind
  (:map copilot-completion-map
        ("<tab>"   . copilot-accept-completion)
        ("TAB"     . copilot-accept-completion)
        ("C-<tab>" . copilot-accept-completion-by-word)
        ("C-TAB"   . copilot-accept-completion-by-word)
        ("C-n"     . copilot-next-completion)
        ("C-p"     . copilot-previous-completion))
)

;; Completion: Company + CAPF + Yasnippet
(use-package yasnippet
  :ensure t
  :init
  ;; Make sure yas is available early enough for completion backends.
  (yas-global-mode 1)
  :config
  (yas-reload-all))

(use-package company
  :ensure t
  :after yasnippet
  :init
  (global-company-mode 1)
  :custom
  (company-idle-delay 0.2)
  (company-minimum-prefix-length 2)
  (company-tooltip-align-annotations t)
  :config

  ;; Prefer CAPF but allow snippet completion
  (setq company-backends
        '((company-capf company-yasnippet)
          company-files
          company-keywords))

  ;; Frame-aware company colors
  (defun my/setup-company-faces ()
    "Setup company colors based on current frame."
    (if (display-graphic-p)
        (progn
          (set-face-attribute 'company-tooltip nil
                              :background "#4f4f4f"
                              :foreground "#dcdccc")
          (set-face-attribute 'company-tooltip-selection nil
                              :background "#8cd0d3"
                              :foreground "#000000")
          (set-face-attribute 'company-tooltip-common nil
                              :foreground "#f0dfaf"
                              :weight 'bold))
      (progn
        (set-face-attribute 'company-tooltip nil
                            :background "#f0f0f0"
                            :foreground "#000000")
        (set-face-attribute 'company-tooltip-selection nil
                            :background "#4a90e2"
                            :foreground "#ffffff")
        (set-face-attribute 'company-tooltip-common nil
                            :foreground "#d33682"
                            :weight 'bold))))

  (add-hook 'focus-in-hook #'my/setup-company-faces)
  (add-hook 'after-make-frame-functions
            (lambda (frame)
              (with-selected-frame frame
                (my/setup-company-faces))))
  (my/setup-company-faces))



(message "Finished loading coding settings")
(provide 'load_coding)

