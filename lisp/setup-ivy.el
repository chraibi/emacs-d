;;; Package --- summary
;;; Code:
;;; Commentary:

;; https://github.com/minad/vertico
;; this is not so nice, since the package in elpy can
;; not be installed with use-package.

(message "Enter setup-ivy")

(use-package counsel
  :after ivy
  :config (counsel-mode))

(use-package ivy
  :ensure t
  :diminish
  :bind (("C-c C-r" . ivy-resume)
         )
  :custom
  (ivy-count-format "(%d/%d) ")
  (ivy-use-virtual-buffers t)
  :config (ivy-mode))

(use-package ivy-rich
  :ensure t
  :after ivy
  :defer 2
  :init
  (ivy-rich-mode)
  :custom
  (ivy-virtual-abbreviate 'full
                          ivy-rich-switch-buffer-align-virtual-buffer t
                          ivy-rich-path-style 'abbrev)
  )



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
  (push "build" counsel-etags-ignore-directories))

(setq tags-revert-without-query t)
;; Don't warn when TAGS files are large
(setq large-file-warning-threshold nil)


(use-package swiper
  :after (ivy ivy-rich)
  :bind (("C-s" . swiper-isearch)
         ("C-r" . swiper-isearch)))

(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x b") 'counsel-switch-buffer)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "C-c n") 'counsel-git-grep)
(global-set-key (kbd "C-c k") 'counsel-ag)
(global-set-key (kbd "C-x l") 'counsel-locate)
(global-set-key (kbd "C-S-o") 'counsel-rhythmbox)
(define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history)
(message "Provide setup-ivy")
(provide 'setup-ivy)
;;; setup-ivy.el ends here
