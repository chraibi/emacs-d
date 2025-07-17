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
(set-cursor-color "#4a90e2")

;; Basic UI improvements
(custom-set-faces
 '(show-paren-match ((((class color) (background light)) (:background "blue")))))

(set-face-attribute 'region nil :background "#ff7f00" :foreground "#000000")
(show-paren-mode t) ;; highlight matching parentheses

;;--------------------- highlight line
(global-hl-line-mode 1)
(set-face-background 'highlight "#222")
(set-face-foreground 'highlight nil)
(set-face-underline 'highlight t)

(setq-default global-visual-line-mode t)
(setq show-paren-style 'parenthesis) ; highlight just brackets

;; ================ GUI-SPECIFIC CONFIGURATION ================
(when (display-graphic-p)
  (message "Configuring GUI-specific settings...")
  
  ;; Fira Code with ligatures (GUI only)
  (use-package fira-code-mode
    :if (display-graphic-p)
    :ensure t
    :custom
    (fira-code-mode-disabled-ligatures '("[]" "#{" "#(" "#_" "#_(" "x"))
    :hook prog-mode)
  
  ;; Font setup with fallback
  (cond
   ((find-font (font-spec :name "Fira Code Retina"))
    (set-face-attribute 'default nil :family "Fira Code Retina" :height 180 :weight 'normal)
    (set-frame-font "Fira Code Retina" nil t)
    (message "Using Fira Code Retina"))
   ((find-font (font-spec :name "Fira Code"))
    (set-face-attribute 'default nil :family "Fira Code" :height 180 :weight 'normal)
    (set-frame-font "Fira Code" nil t)
    (message "Using Fira Code"))
   (t (message "Fira Code not found, using default font")))
  
  ;; GUI-specific theme
  (use-package solarized-theme
    :ensure t
    :config
    (load-theme 'solarized-light t))
  
  ;; Beacon mode (works better in GUI)
  (use-package beacon
    :ensure t
    :config
    (setq beacon-push-mark 35)
    (setq beacon-color "#e56911")
    (beacon-mode 1))
  ) ; End of (when (display-graphic-p))

;; ================ TERMINAL-SPECIFIC CONFIGURATION ================
(unless (display-graphic-p)
  (message "Configuring terminal-specific settings...")
  
  ;; Fix terminal encoding and display issues
  (set-terminal-coding-system 'utf-8)
  (set-keyboard-coding-system 'utf-8)
  (prefer-coding-system 'utf-8)
  (setq locale-coding-system 'utf-8)
  (set-default-coding-systems 'utf-8)
  (set-selection-coding-system 'utf-8)
  
  ;; Terminal-friendly theme
  (use-package zenburn-theme
    :ensure t
    :config
    (load-theme 'zenburn t))
  
  ;; Alternative good terminal themes (uncomment to try):
  ;; (use-package gruvbox-theme
  ;;   :ensure t
  ;;   :config
  ;;   (load-theme 'gruvbox-dark-medium t))
  
  ;; (use-package dracula-theme
  ;;   :ensure t
  ;;   :config
  ;;   (load-theme 'dracula t))
  
  ;; Enhanced terminal colors
  (setq term-default-bg-color "#3f3f3f")
  (setq term-default-fg-color "#dcdccc")
  
  ;; Better terminal cursor
  (setq cursor-type 'box)
  (add-hook 'term-mode-hook
            (lambda ()
              (setq cursor-type 'box)))
  
  ;; Terminal-specific UI tweaks
  (set-face-background 'highlight "#4f4f4f")
  (set-face-foreground 'highlight "#ffffff")
  (set-face-underline 'highlight t)
  
  ;; Improve region selection in terminal
  (set-face-attribute 'region nil :background "#cc9393" :foreground "#000000")
  
  ;; Better parentheses highlighting for terminal
  (set-face-attribute 'show-paren-match nil
                      :background "#8cd0d3"
                      :foreground "#000000"
                      :weight 'bold)
  
  ;; Terminal-friendly line numbers (if you use them)
  (when (fboundp 'display-line-numbers-mode)
    (set-face-attribute 'line-number nil
                        :foreground "#7f7f7f"
                        :background "#2b2b2b")
    (set-face-attribute 'line-number-current-line nil
                        :foreground "#ffffff"
                        :background "#4f4f4f"
                        :weight 'bold))
  
  ;; Enable mouse support in terminal
  (unless (display-graphic-p)
    (xterm-mouse-mode 1)
    (global-set-key [mouse-4] 'scroll-down-line)
    (global-set-key [mouse-5] 'scroll-up-line))
  
  ;; Fix terminal font rendering issues
  (setq inhibit-compacting-font-caches t)
  (setq use-default-font-for-symbols nil)
  
  ;; Ensure proper character display in terminal
  (setq-default buffer-file-coding-system 'utf-8-unix)
  (setq-default default-buffer-file-coding-system 'utf-8-unix)
  
  ) ; End of (unless (display-graphic-p))

;; ================ UNIVERSAL CONFIGURATION (GUI + Terminal) ================

;; Enhanced modeline for both GUI and terminal
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
  (setq doom-modeline-major-mode-icon t)
  (setq doom-modeline-major-mode-color-icon t)
  (setq doom-modeline-gnus-timer nil)
  (setq doom-modeline-time-live-icon t)
  (setq doom-modeline-mail-icon nil)
  (setq doom-modeline-env-enable-mail nil)
  (setq doom-modeline-mu4e nil)
  (setq doom-modeline-gnus nil)
  
  ;; Adjust icons for terminal
  (if (display-graphic-p)
      (setq doom-modeline-icon t)
    (setq doom-modeline-icon nil)) ; Disable icons in terminal
  
  ;; Org-clock integration
  (setq org-clock-modeline-total 'current)
  (setq doom-modeline-time-icon (display-graphic-p))
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
  (my/setup-search-faces))

;; Company completion
(use-package company
  :ensure t
  :config
  (global-company-mode 1)
  (setq company-idle-delay 0.2)
  (setq company-minimum-prefix-length 2)
  
  ;; Frame-aware company colors
  (defun my/setup-company-faces ()
    "Setup company colors based on current frame."
    (if (display-graphic-p)
        (progn
          (set-face-attribute 'company-tooltip nil
                              :background "#4f4f4f"
                              :foreground "#dcdccc")
          (set-face-attribute 'company-tooltip-selection nil
                              :background "#8cd0d3"
                              :foreground "#000000")
          (set-face-attribute 'company-tooltip-common nil
                              :foreground "#f0dfaf"
                              :weight 'bold))
      (progn
        (set-face-attribute 'company-tooltip nil
                            :background "#f0f0f0"
                            :foreground "#000000")
        (set-face-attribute 'company-tooltip-selection nil
                            :background "#4a90e2"
                            :foreground "#ffffff")
        (set-face-attribute 'company-tooltip-common nil
                            :foreground "#d33682"
                            :weight 'bold))))
  
  (add-hook 'focus-in-hook 'my/setup-company-faces)
  (add-hook 'after-make-frame-functions 
            (lambda (frame) 
              (with-selected-frame frame 
                (my/setup-company-faces))))
  (my/setup-company-faces))

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

;; ================ EMACSCLIENT HELPERS ================
;; Commands to switch themes when using emacsclient
(defun my/switch-to-gui-theme ()
  "Switch to GUI theme for current frame."
  (interactive)
  (load-theme 'solarized-light t)
  (my/setup-gui-frame (selected-frame))
  (message "Switched to GUI theme"))

(defun my/switch-to-terminal-theme ()
  "Switch to terminal theme for current frame."
  (interactive)
  (load-theme 'zenburn t)
  (my/setup-terminal-frame (selected-frame))
  (message "Switched to terminal theme"))

(defun my/reload-frame-config ()
  "Reload frame configuration for current frame."
  (interactive)
  (my/setup-frame-appearance (selected-frame))
  (message "Reloaded frame configuration"))

;; ================ FINISH ================
(message "finished loading esthetics")
(provide 'esthetics)
;;; esthetics.el ends here
