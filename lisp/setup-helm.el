;;; Package --- Summary
;;; Commentary:
;;; Code:

;; The default "C-x c" is quite close to "C-x C-c", which quits Emacs.
;; Changed to "C-c h". Note: We must set "C-c h" globally, because we
;; cannot change `helm-command-prefix-key' once `helm-config' is loaded.
(global-set-key (kbd "C-c h") 'helm-command-prefix)
(defalias 'helm-buffer-match-major-mode 'helm-buffers-match-function)
(defalias 'helm-buffers-match-function 'helm-buffers-list--match-fn)
; helm-buffers-match-function
(global-unset-key (kbd "C-x c"))

(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to run persistent action
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB works in terminal
(define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z

(when (executable-find "curl")
  (setq helm-google-suggest-use-curl-p t))

(setq helm-split-window-in-side-p           t ; open helm buffer inside current window, not occupy whole other window
      helm-move-to-line-cycle-in-source     t ; move to end or beginning of source when reaching top or bottom of source.
      helm-ff-search-library-in-sexp        t ; search for library in `require' and `declare-function' sexp.
      helm-scroll-amount                    8 ; scroll 8 lines other window using M-<next>/M-<prior>
      helm-ff-file-name-history-use-recentf t)

(helm-mode 1)

;(helm-projectile-on)
(projectile-global-mode)
(setq projectile-enable-caching nil)
(setq projectile-switch-project-action 'projectile-dired)
(setq projectile-remember-window-configs t )
(setq projectile-completion-system 'helm)
(setq projectile-switch-project-action 'helm-projectile)
;(setq helm-projectile-fuzzy-match nil)


(defun my/setup-helm-keybindings ()
  ;(global-set-key (kbd "C-c n") 'helm-git-grep)
  (define-key isearch-mode-map (kbd "C-c n") 'helm-git-grep-from-isearch)
  (define-key helm-map (kbd "C-c n") 'helm-git-grep-from-helm))

(add-hook 'helm-mode-hook #'my/setup-helm-keybindings)

;; ;(require 'helm-git-grep) ;; Not necessary if installed by package.el
;; (global-set-key (kbd "C-c n") 'helm-git-grep)
;; ;; Invoke `helm-git-grep' from isearch.
;; (define-key isearch-mode-map (kbd "C-c n") 'helm-git-grep-from-isearch)
;; ;; Invoke `helm-git-grep' from other helm.
;; (eval-after-load 'helm
;;   '(define-key helm-map (kbd "C-c n") 'helm-git-grep-from-helm))


(provide 'setup-helm)
;;; setup-helm.el ends here
