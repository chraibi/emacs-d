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
  (setq doom-modeline-unicode-fallback t)
  (setq doom-modeline-enable-word-count t)
  (setq doom-modeline-icon t)
  (setq doom-modeline-major-mode-icon t)
  (setq doom-modeline-major-mode-color-icon t)
  (setq doom-modeline-gnus-timer nil)
  (setq doom-modeline-time-live-icon t)
  (setq doom-modeline-mail-icon nil)  ;; Disable the mail segment
  (setq doom-modeline-env-enable-mail nil)  ;; Disable mail notifications
  (setq doom-modeline-mu4e nil)
  (setq doom-modeline-gnus nil)
  ;; Ensure the clock is shown in the modeline
  (setq org-clock-modeline-total 'current)  ;; Display only the current clocked time
  ;; Display the clock in the modeline
  (setq doom-modeline-time-icon t) ;; Optional: toggle time icon visibility
  (display-time-mode 1)
)

;; Ensure org-clock is enabled for the modeline to display correctly
(defun my/org-clock-update ()
  "Update the doom modeline with the current clocked time."
  (when (org-clock-is-active)
    (setq doom-modeline-gnus-timer (org-clock-get-clock-string))))

(add-hook 'org-clock-out-hook 'my/org-clock-update)
(add-hook 'org-clock-in-hook 'my/org-clock-update)
(add-hook 'org-clock-update-hook 'my/org-clock-update)

(setq org-clock-clocked-in-display 'both) ;; Display clocked-in time in the frame title



;; (use-package doom-modeline
;;   :ensure t
;;   :init (doom-modeline-mode 1)
;;   :config
;;   (setq doom-modeline-lsp t)
;;   (setq doom-modeline-minor-modes nil)
;;   (setq doom-modeline-buffer-encoding nil)
;;   (setq doom-modeline-workspace-name t)
;;   (setq doom-modeline-project-detection 'project)
;;   (setq doom-modeline-buffer-file-name-style 'filename)
;;   (setq doom-modeline-time t)
;;   (setq doom-modeline-lsp t)
;;   (setq doom-modeline-unicode-fallback t)
;;   (setq doom-modeline-enable-word-count t)
;;   (setq doom-modeline-icon nil)
;;   (setq doom-modeline-gnus-timer nil)
;;   (setq org-clock-modeline-total 'current)  ;; Display the time for the current clock in the mode line
;;   (setq org-clock-clocked-in-display 'both) ;; Display the clocked-in time in the frame title
;;   (display-time-mode 1)
;;   )
;; ; To automatically show the clock status in the modeline across all buffers, add this to your Emacs configuration:
;; (add-hook 'org-clock-in-hook
;;           (lambda () (org-clock-update-mode-line)))
;; (add-hook 'org-clock-out-hook
;;           (lambda () (org-clock-remove-overlays)))


;; ;; Modify the global mode line format to ensure the Org-mode clock is included in all buffers, including Python buffers.
;; (setq-default mode-line-format
;;   (append mode-line-format
;;           '(:eval (when (and (fboundp 'org-clocking-p)
;;                              (org-clocking-p))
;;                     (format " [%s]" (substring-no-properties org-mode-line-string))))))

;; ;; Ensure Org-mode clock updates are reflected properly by adding hooks for clock-in and clock-out
;; (add-hook 'org-clock-in-hook
;;           (lambda () (org-clock-update-mode-line)))
;; (add-hook 'org-clock-out-hook
;;           (lambda () (org-clock-remove-overlays)))
;; ;; to ensure the mode line is updated correctly for Python files:
;; (add-hook 'python-mode-hook
;;           (lambda () (force-mode-line-update)))


;;(setq display-battery-mode t) (display-battery-mode 1) ;; will make the display of date and time persistent.

;; (defun autocompile ()
;;   "Automatically compile Emacs Lisp files upon saving."
;;   (interactive)
;;     (require 'bytecomp)
;;     (byte-compile-file (buffer-file-name)))

;; (add-hook 'after-save-hook 'autocompile)


;; ================ esthetics ==================
(message "finished loading esthetics")
(provide 'esthetics)
;;; esthetics.el ends here
