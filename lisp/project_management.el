;;; package ---- Project management packages
;;; Code:
;;; Commentary: projectile, helm, helm-projectile, magit, fzf
(message "Loading project_management.el")
(use-package projectile
  :init
  (message "Loading projectile")
  :defer t
  :ensure t
  :diminish projectile-mode
  :bind
  (("C-c p f" . helm-projectile-find-file)
   ("C-c p p" . helm-projectile-switch-project)
   ("C-c p s" . projectile-save-project-buffers)
   ("C-c p c" . projectile-compile-project)
   )
  :config
  (projectile-mode +1)
  )
(use-package helm
  :init
  (message "loading helm!")
  :ensure t
  :demand t
  :after (projectile helm-projectile)
  :config
  (require 'setup-helm)
  )

(use-package helm-projectile
  :init
  (message "loading helm-projectile")
  :ensure t
  :after projectile
  :config
  (helm-projectile-on))

(use-package fzf
  :init
  (setenv "FZF_DEFAULT_OPTS" "--ansi --height 100%")
  (message "loading fzf")
  :ensure t
  
  :bind
  (("C-c n" . fzf-git-grep)
   ("C-c C-f" . fzf-find-file)
   ("C-c C-d" . fzf-directory)))


(use-package magit
  :init
  (message "loading magit!")
  :ensure t
  :config
  (require 'setup-magit)
  :defer 3)


(message "Finished loading project_management.el")
(provide 'project_management)


