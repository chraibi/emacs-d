;;; Package --- Summary
;;; --- Le style a sont importance (GUI + Terminal)
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

;; Suppress fira-code-ligatures warning
(setq warning-suppress-log-types '((fira-code-ligatures)))

;; Cursor configuration
(setq-default cursor-type 'box)

(when (display-graphic-p)
  (set-cursor-color "#4a90e2"))
;(global-hl-line-mode 1)
;; (set-face-background 'highlight "#222")
;; (set-face-foreground 'highlight nil)
;; (set-face-underline 'highlight t)

(setq show-paren-style 'parenthesis) ; highlight just brackets

;; ---------------- GUI-specific ----------------
(when (display-graphic-p)
  ;; Fira Code with ligatures (GUI only)
  (use-package fira-code-mode
    :ensure t
    :custom
    (fira-code-mode-disabled-ligatures '("[]" "#{" "#(" "#_" "#_(" "x"))
    :hook (prog-mode . fira-code-mode))

  (defvar my/gui-font-height 180
    "Default GUI font height.")

  (defun my/apply-gui-font (frame)
    "Apply preferred GUI font to FRAME."
    (with-selected-frame frame
      (when (display-graphic-p frame)
        (cond
         ((find-font (font-spec :name "Fira Code Retina"))
          (set-face-attribute 'default frame
                              :family "Fira Code Retina"
                              :height my/gui-font-height
                              :weight 'normal))
         ((find-font (font-spec :name "Fira Code"))
          (set-face-attribute 'default frame
                              :family "Fira Code"
                              :height my/gui-font-height
                              :weight 'normal))))))

  (my/apply-gui-font (selected-frame))
  (add-hook 'after-make-frame-functions #'my/apply-gui-font)
  )



;; --- Solarized everywhere (GUI + terminal), frame-aware ---
(use-package solarized-theme
  :ensure t
  :init
  ;; Important for TTY: make solarized use 256-color palette if available
  (setq solarized-termcolors 256
        solarized-use-variable-pitch nil
        solarized-use-less-bold t
        solarized-use-more-italic t)
  :config
  (defun my/apply-solarized (frame)
    "Apply solarized-light consistently for FRAME."
    (with-selected-frame frame
      ;; Make sure Emacs treats the frame as light; helps in TTY.
      (setq frame-background-mode 'light)
      (frame-set-background-mode frame)
      
      ;; Disable other themes first (prevents mixed faces).
      (mapc #'disable-theme custom-enabled-themes)
      
      (load-theme 'solarized-light t)
      
      ))
  
  ;; Apply to current frame (non-daemon) and future frames (daemon/emacsclient)
  (my/apply-solarized (selected-frame))
  (add-hook 'after-make-frame-functions #'my/apply-solarized)
  )


(use-package beacon
  :ensure t
  :if (display-graphic-p)
  :config
  (setq beacon-push-mark 35
        beacon-color "#e56911")
  (beacon-mode 1))


(use-package doom-modeline
  :ensure t
  :init
  ;; Set variables before enabling the mode so they take effect immediately.
  (setq doom-modeline-lsp t
        doom-modeline-minor-modes nil
        doom-modeline-buffer-encoding nil
        doom-modeline-workspace-name t
        doom-modeline-project-detection 'project
        doom-modeline-buffer-file-name-style 'filename
        doom-modeline-time t
        doom-modeline-unicode-fallback t
        doom-modeline-enable-word-count t
        doom-modeline-major-mode-icon t
        doom-modeline-major-mode-color-icon t
        doom-modeline-gnus-timer nil
        doom-modeline-time-live-icon t
        doom-modeline-mail-icon nil
        doom-modeline-env-enable-mail nil
        doom-modeline-mu4e nil
        doom-modeline-gnus nil)

  ;; Icons: enable only in GUI
  (setq doom-modeline-icon (display-graphic-p)
        doom-modeline-time-icon (display-graphic-p))

  ;; Org-clock integration
  (setq org-clock-modeline-total 'current)

  (doom-modeline-mode 1)
  (display-time-mode 1))

;; Org-clock hooks (works in both GUI and terminal)
(defun my/org-clock-update ()
  "Update the doom modeline with the current clocked time."
  (when (org-clock-is-active)
    (setq doom-modeline-gnus-timer (org-clock-get-clock-string))))

(add-hook 'org-clock-out-hook 'my/org-clock-update)
(add-hook 'org-clock-in-hook 'my/org-clock-update)
(add-hook 'org-clock-update-hook 'my/org-clock-update)

(setq org-clock-clocked-in-display 'both)

;; ================ ADDITIONAL ENHANCEMENTS ================

;; Better scrolling
(setq scroll-margin 3)
(setq scroll-conservatively 10000)
(setq scroll-preserve-screen-position t)

;; Improved search highlighting (frame-aware)
(defun my/setup-search-faces ()
  "Setup search highlighting based on current frame."
  (if (display-graphic-p)
      (progn
        (set-face-attribute 'isearch nil
                            :background "#8cd0d3"
                            :foreground "#000000"
                            :weight 'bold)
        (set-face-attribute 'lazy-highlight nil
                            :background "#cc9393"
                            :foreground "#000000"))
    (progn
      (set-face-attribute 'isearch nil
                          :background "#ff7f00"
                          :foreground "#000000"
                          :weight 'bold)
      (set-face-attribute 'lazy-highlight nil
                          :background "#ffff00"
                          :foreground "#000000"))))

(use-package isearch
  :config
  (add-hook 'focus-in-hook 'my/setup-search-faces)
  (add-hook 'after-make-frame-functions 
            (lambda (frame) 
              (with-selected-frame frame 
                (my/setup-search-faces))))
  (my/setup-search-faces)
  )


;; Which-key
(use-package which-key
  :ensure t
  :config
  (which-key-mode 1)
  (setq which-key-idle-delay 0.5)
  (setq which-key-max-description-length 50)
  
  ;; Frame-aware which-key setup
  (defun my/setup-which-key ()
    "Setup which-key based on current frame."
    (if (display-graphic-p)
        (progn
          (setq which-key-popup-type 'side-window)
          (setq which-key-side-window-location 'bottom)
          (setq which-key-side-window-max-height 0.25))
      (progn
        (setq which-key-popup-type 'frame)
        (setq which-key-frame-max-width 60)
        (setq which-key-frame-max-height 20))))
  
  (add-hook 'focus-in-hook 'my/setup-which-key)
  (add-hook 'after-make-frame-functions 
            (lambda (frame) 
              (with-selected-frame frame 
                (my/setup-which-key))))
  (my/setup-which-key))

;; Rainbow delimiters
(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

;; Global line highlighting
(global-hl-line-mode 1)

;; Display current function
(which-function-mode 1)
(setq which-func-unknown "")


;; ================ FINISH ================
(message "finished loading esthetics")
(provide 'esthetics)
;;; esthetics.el ends here

