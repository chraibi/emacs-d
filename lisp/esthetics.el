;;; Package --- Summary
;;; --- Le style a sont importance
;;; Code:
;;; Commentary:
;; ================ esthetics =============
(message "start loading esthetics")

(require 'ansi-color)
(defun my/ansi-colorize-buffer ()
  "Comments."
  (let ((buffer-read-only nil))
    (ansi-color-apply-on-region (point-min) (point-max))))
(add-hook 'compilation-filter-hook 'my/ansi-colorize-buffer)

;(setq-default cursor-type 'bar)
(setq-default cursor-type 'box)
(set-cursor-color "#4a90e2")

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(show-paren-match ((((class color) (background light)) (:background "blue")))))

(set-face-attribute 'region nil :background "#ff7f00" :foreground "#000000")

(show-paren-mode t) ;; will highlight matching parentheses next to cursor.

;;--------------------- highlight line
;;https://stackoverflow.com/questions/2718189/emacshighlight-the-current-line-by-underline-it
(global-hl-line-mode 1)
(set-face-background 'highlight "#222")
(set-face-foreground 'highlight nil)
(set-face-underline 'highlight t)


(setq-default global-visual-line-mode t)
(setq show-paren-style 'parenthesis) ; highlight just brackets

                                        ;(global-set-key (kbd "C-SPC") 'set-mark-command)


(when (display-graphic-p)
    (use-package fira-code-mode
      :ensure t
      :custom
      (fira-code-mode-disabled-ligatures '("[]" "#{" "#(" "#_" "#_(" "x")) ;; List of ligatures to turn off
      :hook prog-mode
      )
                                        ;FiraCode Nerd Font
  ;; size in 1/10pt
  (set-face-attribute 'default nil
                      :family "Fira Code Retina"
                      :height 180
                      :weight 'normal)
  ;; ---
  (set-frame-font "Fira Code" nil t)
  (setq default-frame-alist nil)
  (message "set font Fira Code Retina")
  )

 (use-package solarized-theme
 :ensure t
 :config
(load-theme 'solarized-light t))


;;Whenever the window scrolls a light will shine on top of your cursor so you know where it is.
(use-package beacon
  :ensure t
  :config
  (setq beacon-push-mark 35)
  (setq beacon-color "#e56911")
  (beacon-mode 1)
  );
;(use-package all-the-icons
;  :ensure t
;  )
;(use-package nerd-icons
;  :ensure t
  ;; :custom
  ;; The Nerd Font you want to use in GUI
  ;; "Symbols Nerd Font Mono" is the default and is recommended
  ;; but you can use any other Nerd Font if you want
  ;; (nerd-icons-font-family "Symbols Nerd Font Mono")
;  )
(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :config
  (setq doom-modeline-lsp t)
  (setq doom-modeline-minor-modes nil)
  (setq doom-modeline-buffer-encoding nil)
  (setq doom-modeline-workspace-name t)
  (setq doom-modeline-project-detection 'project)
  (setq doom-modeline-buffer-file-name-style 'filename)
  (setq doom-modeline-time t)
  (setq doom-modeline-lsp t)
  (setq doom-modeline-unicode-fallback t)
  (setq doom-modeline-enable-word-count t)
  (setq doom-modeline-icon nil)
  (setq doom-modeline-gnus-timer nil)
  (display-time-mode 1)
  )
;;(setq display-battery-mode t) (display-battery-mode 1) ;; will make the display of date and time persistent.

;; ================ esthetics ==================
(message "finished loading esthetics")
(provide 'esthetics)
;;; esthetics.el ends here
