;;; Package --- Summary
;;; --- Editor defaults
;;; Code:
;;; Commentary:
(message "loading my settings")
;; ------cleanup this
(setq gc-cons-threshold (* 50 1000 1000))
;;; Code:
;; Turn off mouse interface early in startup to avoid momentary display

;;(setq epg-gpg-program "/usr/local/bin/gpg")
;;(setq multi-term-program "/bin/zsh")

;; (when (string= system-type "darwin")
;;   (setq dired-use-ls-dired nil))

;; ;;--------------------------  Backup
(setq backup-directory-alist `(("." . "~/.saves")))
(setq delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)

(setq is-mac (equal system-type 'darwin))
(if (equal system-type 'darwin)
    (setq locate-command "mdfind")
  (global-set-key (kbd "M-s") 'locate)
 )

(defvar *emacs-load-start* (current-time))
;; My location for external packages.

;(getenv "PATH")
(setenv "PATH"
        (concat
         "/usr/texbin" ":"
         "/usr/local/bin/" ":"
         (getenv "PATH")))

(setq preview-gs-options '("-q" "-dNOSAFER" "-dNOPAUSE" "-DNOPLATFONTS" "-dPrinted" "-dTextAlphaBits=4" "-dGraphicsAlphaBits=4"))


;; Always load the newer .el or .elc file.
(setq load-prefer-newer t)

;; ------ end cleanup
;; Avoid performance issues in files with very long lines.
(global-so-long-mode 1)

;https://batsov.com/articles/2011/11/25/emacs-tip-number-3-whitespace-cleanup/
;(add-hook 'before-save-hook 'whitespace-cleanup)
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(setq inhibit-splash-screen t);; Disable splash screen
(setq inhibit-startup-message t)
(setq inhibit-startup-echo-area-message t)
(setq inhibit-startup-screen t)
(setq initial-scratch-message nil)
(setq custom-safe-themes t)
;; use smart line

(setq user-full-name "MC")
(setq user-mail-address "m.chraibi@gmail.com")



;; no beep
(setq ring-bell-function 'ignore)

;; Use Emacs terminfo, not system terminfo
(setq system-uses-terminfo nil)
;;----------------- KEYbindings --------------


(global-set-key (kbd "C-c n") 'fzf-git-grep)

(global-set-key [f9] 'projectile-compile-project)
(global-set-key (kbd "C-c s") 'window-swap-states)
(global-set-key "\C-z" 'nil)
(global-set-key (kbd "C-c ;") 'comment-or-uncomment-region-or-line)
(global-set-key "\C-x\C-k" 'kill-region)
(global-set-key "\C-c\C-k" 'kill-region)
(global-set-key (kbd "C-x C-r") 'ido-recentf-open)
(global-set-key (kbd "M-i") 'ido-goto-symbol)
(global-set-key "\C-cy" 'browse-kill-ring)
(global-set-key (kbd "\C-cm") 'magit-status)   ;; ...git mode
                                        ;(global-set-key (kbd "<f4>") 'nav-toggle)
(global-set-key (kbd "<f4>") 'treemacs)
(global-set-key [f5] 'buffer-menu)

(global-set-key [end] 'end-of-line)
(global-set-key [home] 'beginning-of-line)

(global-set-key [next]    'pager-page-down)
(global-set-key [prior]   'pager-page-up)
;; Page down/up move the point, not the screen.
;; In practice, this means that they can move the
;; point to the beginning or end of the buffer.
(global-set-key [next]
                (lambda () (interactive)
                  (condition-case nil (scroll-up)
                    (end-of-buffer (goto-char (point-max))))))

(global-set-key [prior]
                (lambda () (interactive)
                  (condition-case nil (scroll-down)
                    (beginning-of-buffer (goto-char (point-min))))))
(global-set-key [f11] 'toggle-fullscreen)
(global-set-key [f1] 'helm-dash)
(bind-key "C-x p" 'pop-to-mark-command)
(setq set-mark-command-repeat-pop t)

(global-set-key (kbd "<f8>") 'ispell-word)
(global-set-key (kbd "C-<f8>") 'flyspell-mode)
(global-set-key (kbd "C-<f9>") 'reftex-mode)
(global-set-key (kbd "C-x C-b") 'ibuffer)

(global-set-key (kbd "C-c z") 'mc/edit-lines)
(global-set-key (kbd "C-c i") 'mc/insert-numbers)
(global-set-key (kbd "C-c C-n") 'mc/mark-next-like-this)
(global-set-key (kbd "C-c C-p") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-s") 'mc/mark-all-like-this)



;;-------------------------------------------
;; ;;; Make all yes-or-no questions as y-or-n
(fset 'yes-or-no-p 'y-or-n-p)

;; (transient-mark-mode 1)
(global-visual-line-mode 1) ; 1 for on, 0 for off.

(column-number-mode 1) ; show column number

;; I hate tabs!
(setq-default indent-tabs-mode nil)
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))


;; ;; scroll one line at a time (less "jumpy" than defaults)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq scroll-step 1) ;; keyboard scroll one line at a time

;; ;;    split vertically
(setq split-height-threshold nil)
(setq split-width-threshold 0)


(setq
 uniquify-buffer-name-style 'post-forward
 uniquify-separator ":"
 uniquify-after-kill-buffer-p t
 uniquify-ignore-buffers-re "^\\*")


;; after copy Ctrl+c in X11 apps, you can paste by `yank' in emacs
(setq select-enable-clipboard t)

;; after mouse selection in X11, you can paste by `yank' in emacs
(setq select-enable-primary t)

(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)


;------------------------------

;; define function to shutdown emacs server instance
(defun server-shutdown ()
  "Save buffers, Quit, and Shutdown (kill) server."
  (interactive)
  (save-some-buffers)
  (kill-emacs)
  )
;; overwrite selected text
(delete-selection-mode t)




(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)


;;(global-set-key (kbd "C-a") 'load-file)

;; (defun autocompile ()
;;   "Automatically compile Emacs Lisp files upon saving."
;;   (interactive)
;;     (require 'bytecomp)
;;     (byte-compile-file (buffer-file-name)))

;; (add-hook 'after-save-hook 'autocompile)

(use-package god-mode
  :ensure t
  :config
  (global-set-key (kbd "<escape>") #'god-local-mode)
  (define-key god-local-mode-map (kbd "i") #'god-local-mode)
  (setq god-mode-enable-function-key-translation nil)
  (add-hook 'minibuffer-setup-hook
            (lambda () (god-local-mode -1)))
  )

(defun my-god-mode-update-cursor-type ()
  (setq cursor-type (if (or god-local-mode buffer-read-only) 'box 'bar)))

(add-hook 'post-command-hook #'my-god-mode-update-cursor-type)

(defconst my-solarized
  '((base03 . "#002b36")
    (base01 . "#586e75")
    (base00 . "#657b83")
    (base2  . "#eee8d5")
    (base3  . "#fdf6e3")
    (cyan   . "#2aa198")
    (blue   . "#268bd2")
    (violet . "#6c71c4")))

(defun my-god-mode-update-mode-line ()
  (let-alist my-solarized-light
    (if god-local-mode
        (progn
          ;; Active mode-line (God Mode ON)
          (set-face-attribute 'mode-line nil
                    :background .violet
                    :foreground .base00
                    :box `(:line-width 2 :color ,.cyan))
          (set-face-attribute 'mode-line-inactive nil
                              :foreground .base01
                              :background .violet
                              :box nil))
      ;; Normal mode
      (set-face-attribute 'mode-line nil
                          :foreground .base00
                          :background .base2
                          :box nil)
      (set-face-attribute 'mode-line-inactive nil
                          :foreground .base01
                          :background .base3
                          :box nil))))


(add-hook 'god-mode-enabled-hook #'my-god-mode-update-mode-line)
(add-hook 'god-mode-disabled-hook #'my-god-mode-update-mode-line)




(message "end my settings")
(provide 'my-core-settings)
;;; my-core-settings.el ends here
