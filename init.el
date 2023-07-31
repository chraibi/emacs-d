;;; package ---- summary
;;; Code:
;;; Commentary:
;; Make startup faster by reducing the frequency of garbage
;; collection.  The default is 0.8MB.  Measured in bytes.

(add-to-list 'load-path (expand-file-name "~/.emacs.d/lisp"))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))

;; (add-to-list 'load-path "~/.emacs.d/lisp/benchmark-init-el")


;(setq org-roam-v2-ack t)
(require 'package)
(setq package-enable-at-startup nil)
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))


;----------------  load setups ----------------------------


 (use-package load_configs
   :init
   (message "load my setups from file")
   )
(message "setups loaded")


;;-----------------------------
;; https://github.com/magnars/.emacs.d/blob/master/init.el
;; ;; Use a hook so the message doesn't get clobbered by other messages.
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Emacs ready in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)))

;;-----------------------
(defun my-test-emacs ()
  "Debuging start of Emacs."
  (interactive)
  (require 'async)
  (async-start
   (lambda () (shell-command-to-string
               "emacs --batch --eval \"
(condition-case e
    (progn
      (load \\\"~/.emacs.d/init.el\\\")
      (message \\\"-OK-\\\"))
  (error
   (message \\\"ERROR!\\\")
   (signal (car e) (cdr e))))\""))
   `(lambda (output)
      (if (string-match "-OK-" output)
          (when ,(called-interactively-p 'any)
            (message "All is well"))
        (switch-to-buffer-other-window "*startup error*")
        (delete-region (point-min) (point-max))
        (insert output)
        (search-backward "ERROR!")))))

(defun auto-test-emacs ()
  "Test starting Emacs for bugs."
  (when (eq major-mode 'emacs-lisp-mode)
    (my-test-emacs)))

(add-hook 'after-save-hook 'auto-test-emacs)

;(byte-recompile-directory (expand-file-name "~/.emacs.d") 0)
  
(message "done loading emacs!")

(provide 'init)

;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(org-ref-ivy zotxt zenburn-theme xcscope which-key w32-browser use-package undo-tree treemacs-projectile treemacs-persp treemacs-magit treemacs-icons-dired treemacs-evil tree-sitter-langs swiper-helm sphinx-doc solarized-theme sml-modeline smartparens smart-mode-line-powerline-theme rustic python-mode python-black projectile-sift poetry peep-dired ox-latex-subfigure origami org-roam-ui org-roam-bibtex org-ref org-noter-pdftools org-journal org-bullets org-analyzer nav multiple-cursors modern-cpp-font-lock marginalia lsp-ui lsp-treemacs lsp-pyright ivy-rich image-dired+ ido-vertical-mode hlinum highlight-indent-guides helm-projectile helm-org-ql helm-lsp helm-git-grep helm-dash helm-bibtex guide-key git-gutter fzf flymake-cursor flycheck-rust flycheck-clang-tidy fira-code-mode fill-column-indicator expand-region exec-path-from-shell embark-consult emacsql-sqlite-module emacsql-sqlite elpy eglot doom-themes doom-modeline-now-playing dired-subtree dired-ranger dired-rainbow dired-narrow dired-filter dired-filetype-face dired-collapse diminish deft crux cpputils-cmake counsel-etags company-lsp color-theme cmake-project cmake-mode cmake-ide clang-format+ citar-org-roam ccls browse-kill-ring beacon autopair auto-complete-clang-async auto-complete-clang auctex-latexmk all-the-icons ag ace-jump-mode))
 '(warning-suppress-types '((use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(show-paren-match ((((class color) (background light)) (:background "blue")))))
