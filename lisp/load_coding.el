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


(defadvice compile (around split-horizontally activate)
  (let ((split-width-threshold 0)
        (split-height-threshold nil))
    ad-do-it))

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
  (load "/usr/local/Cellar/clang-format/12.0.1/share/clang/clang-format.el")
  )

(use-package setup-cc
  :init
  (message "loading setup-cc")
  :after (clang-format)
  :defer t
  :config
  (when (string-suffix-p ".cpp" (buffer-file-name))
    (message "Loading setup-cc!"))
  )

(message "load python-setup")
(defun python-mode-setup ()
 "Load python mode."
 (when (eq major-mode 'python-mode)
   (message "Custom python hook run")
   (load-library "setup-python")))

(add-hook 'python-mode-hook 'python-mode-setup)

(use-package setup-lsp
  :init
  (message "loading setup-lsp")
  :defer t
  )

(message "Finished loading coding settings")
(provide 'load_configs)

