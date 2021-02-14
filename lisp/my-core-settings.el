;;; Package --- Summary
;;; --- Editor defaults
;;; Code:
;;; Commentary:

(yas-global-mode 0)
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

(setq sml/no-confirm-load-theme t)
;(add-hook 'after-init-hook 'display-time)
;(setq display-time-24hr-format t)
;(setq display-time nil)
;(setq display-time-day-and-date nil)
;(setq mode-line-mule-info nil)
;(display-time-mode 0)
;; powerline
;(powerline-default-theme)
;(setq powerline-arrow-shape 'curve)

(setq user-full-name "M. Chraibi")
(setq user-mail-address "m.chraibi@gmail.com")
(set-default 'cursor-type 'bar)
(setq cursor-type 'bar)
(setq-default cursor-type '(hbar . 1))
(set-cursor-color "#ff0000")
;; (blink-cursor-mode 1)

;; Use Emacs terminfo, not system terminfo
(setq system-uses-terminfo nil)
;;----------------- KEYbindings --------------
(global-set-key "\C-z" 'nil)
(global-set-key (kbd "M-2") #'er/expand-region)
;(define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
;(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
(global-set-key (kbd "C-c ;") 'comment-or-uncomment-region-or-line)
(global-set-key "\C-x\C-k" 'kill-region)
(global-set-key "\C-c\C-k" 'kill-region)
(global-set-key (kbd "C-x C-r") 'ido-recentf-open)
(global-set-key (kbd "M-i") 'ido-goto-symbol)
(global-set-key "\C-cy" 'browse-kill-ring)
(global-set-key "\C-cg" 'goto-line)
                                        ;(global-set-key [f4] 'speedbar-get-focus)
                                        ;(global-set-key [f4] 'speedbar-get-focus)
(global-set-key (kbd "\C-cm") 'magit-status)   ;; ...git mode
                                        ;(global-set-key (kbd "<f4>") 'nav-toggle)
(global-set-key (kbd "<f4>") 'projectile-speedbar-open-current-buffer-in-tree)
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
(set-face-underline 'highlight t)



;(global-set-key (kbd "C-SPC") 'set-mark-command)

(provide 'my-core-settings)
;;; my-core-settings.el ends here
