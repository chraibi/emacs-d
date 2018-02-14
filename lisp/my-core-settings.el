;;; Package --- Summary
;;; --- Editor defaults
;;; Code:
;;; Commentary:

(yas-global-mode 0)
(add-hook 'before-save-hook 'whitespace-cleanup)
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
(setq sml/no-confirm-load-theme t)
(add-hook 'after-init-hook 'display-time)
(setq display-time-24hr-format t)
(setq display-time-day-and-date t)

(setq user-full-name "M. Chraibi")
(setq user-mail-address "m.chraibi@gmail.com")
(set-default 'cursor-type 'bar)
(setq cursor-type 'bar)
(blink-cursor-mode 1)
(setq-default cursor-type '(hbar . 1))
;; Use Emacs terminfo, not system terminfo
(setq system-uses-terminfo nil)
(global-set-key "\C-z" 'nil)
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
(autopair-global-mode) ;; to enable in all buffers


(setq  electric-pair-mode t)



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
(set-face-underline-p 'highlight t)



;; ;; this variable
;;(setq auto-indent-on-visit-file t) ;; If you want auto-indent on for files
;; (require 'auto-indent-mode)
;; (auto-indent-global-mode)



(provide 'my-core-settings)
;;; my-core-settings.el ends here
