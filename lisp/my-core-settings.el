;;; Package --- Summary
;;; --- Editor defaults
;;; Code:
;;; Commentary:


;; ------cleanup this
(setq gc-cons-threshold (* 50 1000 1000))
;;; Code:
;; Turn off mouse interface early in startup to avoid momentary display

;;(setq epg-gpg-program "/usr/local/bin/gpg")
;; frame font
;; Setting English Font
;;(setq multi-term-program "/bin/zsh")

;; (when (string= system-type "darwin")
;;   (setq dired-use-ls-dired nil))

;; ;;--------------------------  Backup
(setq backup-directory-alist `(("." . "~/.saves")))
(setq delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)

(require 'ansi-color)
(defun my/ansi-colorize-buffer ()
  "Comments."
  (let ((buffer-read-only nil))
    (ansi-color-apply-on-region (point-min) (point-max))))
(add-hook 'compilation-filter-hook 'my/ansi-colorize-buffer)

;; (find-file "~/Dropbox/Orgfiles/org-files/master.org") ;

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

(setq py-install-directory "~/.emacs.d/lisp/pdee-master")
(add-to-list 'load-path py-install-directory)
(setq display-battery-mode t) (display-battery-mode 1) ;; will make the display of date and time persistent.

;; Always load the newer .el or .elc file.
(setq load-prefer-newer t)

;; ------ end cleanup


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

;(setq-default cursor-type 'bar)
(setq-default cursor-type 'box)
(set-cursor-color "#4a90e2")


;; no beep
(setq ring-bell-function 'ignore)

;; Use Emacs terminfo, not system terminfo
(setq system-uses-terminfo nil)
;;----------------- KEYbindings --------------

(global-set-key [f9] 'projectile-compile-project)

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


(global-set-key (kbd "M-o") 'ace-window)
(setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
(setq aw-background nil)

;;-------------------------------------------
;; ;;; Make all yes-or-no questions as y-or-n
(fset 'yes-or-no-p 'y-or-n-p)

;; (transient-mark-mode 1)
(global-visual-line-mode 1) ; 1 for on, 0 for off.

(column-number-mode 1) ; show column number

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(show-paren-match ((((class color) (background light)) (:background "blue")))))

(set-face-attribute 'region nil :background "#ff7f00" :foreground "#000000")

(show-paren-mode t) ;; will highlight matching parentheses next to cursor.



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

;;--------------------- highlight line
;;https://stackoverflow.com/questions/2718189/emacshighlight-the-current-line-by-underline-it
(global-hl-line-mode 1)
(set-face-background 'highlight "#222")
(set-face-foreground 'highlight nil)
(set-face-underline 'highlight t)



;(global-set-key (kbd "C-SPC") 'set-mark-command)

(provide 'my-core-settings)
;;; my-core-settings.el ends here
