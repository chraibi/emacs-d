;;; Package --- load my configs
;;; Code:
;;; Commentary:


(message "enter load configs")
(use-package my-core-settings
  :init
  (message "Loading my-settings!")
  )

;; ================ esthetics =============
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

;; (setq current-theme "dark")
;; (defconst light-theme 'solarized-light)
;; (defconst dark-theme 'zenburn)


;;Whenever the window scrolls a light will shine on top of your cursor so you know where it is.
(use-package beacon
  :ensure t
  :config
  (setq beacon-push-mark 35)
  (setq beacon-color "#e56911")
  (beacon-mode 1)
  )

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
;  (setq doom-modeline-minor-modes (featurep 'minions))
  )
(use-package all-the-icons
  :ensure t
  :if (display-graphic-p)
  )
;; ================ esthetics ==================

;; =================== windows and editing ===============

(defun comment-or-uncomment-region-or-line ()
  "Comments or uncomments the region or the current line if there's no active region."
  (interactive)
  (let (beg end)
    (if (region-active-p)
        (setq beg (region-beginning) end (region-end))
      (setq beg (line-beginning-position) end (line-end-position)))
    (comment-or-uncomment-region beg end)
    (next-line)))

(defun comment-region-lines (beg end &optional arg)
  "Like `comment-region', but comment/uncomment whole lines."
  (interactive "*r\nP")
  (if (> beg end) (let (mid) (setq mid beg beg end end mid)))
  (let ((bol  (save-excursion (goto-char beg) (line-beginning-position)))
        (eol  (save-excursion (goto-char end) (line-end-position))))
    (comment-region bol end arg)))


(use-package undo-tree
  :ensure t
  :diminish undo-tree-mode
  :config
  (progn
    (global-undo-tree-mode)
    (setq undo-tree-visualizer-timestamps t)
    (setq undo-tree-visualizer-diff t)))

(use-package winner
  :init
  (message "loading winner!")
  :ensure t
  )
(winner-mode 1)


(use-package windmove
  :init
  (message "loading windmove!")
  :ensure t
  :bind
  (("C-x <right>" . windmove-right)
   ("C-x <left>" . windmove-left)
   ("C-x <up>" . windmove-up)
   ("C-x <down>" . windmove-down)
   ))


;; (use-package expand-region
;;   :ensure t
;;   :bind ("M-2" . er/expand-region)
;;   )
;; (use-package avy
;;   :ensure t
;;   :bind
;;   ("C-." . avy-goto-char)
;;   ("C-c ." . avy-goto-char-2)
;;   :config
;;   (avy-setup-default)  
;;   )
;; (global-set-key "\C-cg" 'avy-goto-line)
;; (global-set-key (kbd "C-c C-j") 'avy-resume)

;; (use-package browse-kill-ring
;;   :ensure t
;;   :config
;;   (setq browse-kill-ring-quit-action 'save-and-restore)
;;   (setq browse-kill-ring-highlight-current-entry t
;;   ))

;; =================== windows ===============

;; ================ directories and files  ==========
;; ;;--------------------------------- ibuffer
;; (autoload 'ibuffer "ibuffer" "List buffers." t)
;; (setq ibuffer-default-sorting-mode 'major-mode)
;; (setq ibuffer-expert t)
;; (setq ibuffer-show-empty-filter-groups nil)

;; (defun ido-goto-symbol (&optional symbol-list)
;;   "Refresh imenu and jump to a place in the buffer using Ido.
;; argument SYMBOL-LIST"
;;   (interactive)
;;   (unless (featurep 'imenu)
;;     (require 'imenu nil t))
;;   (cond
;;    ((not symbol-list)
;;     (let ((ido-mode ido-mode)
;;           (ido-enable-flex-matching
;;            (if (boundp 'ido-enable-flex-matching)
;;                ido-enable-flex-matching t))
;;           name-and-pos symbol-names position)
;;       (unless ido-mode
;;         (ido-mode 1)
;;         (setq ido-enable-flex-matching t))
;;       (while (progn
;;                (imenu--cleanup)
;;                (setq imenu--index-alist nil)
;;                (ido-goto-symbol (imenu--make-index-alist))
;;                (setq selected-symbol
;;                      (ido-completing-read "Symbol? " symbol-names))
;;                    (string= (car imenu--rescan-item) selected-symbol)))
;;           (unless (and (boundp 'mark-active) mark-active)
;;             (push-mark nil t nil))
;;           (setq position (cdr (assoc selected-symbol name-and-pos)))
;;           (cond
;;            ((overlayp position)
;;             (goto-char (overlay-start position)))
;;            (t
;;             (goto-char position)))))
;;        ((listp symbol-list)
;;         (dolist (symbol symbol-list)
;;           (let (name position)
;;             (cond
;;              ((and (listp symbol) (imenu--subalist-p symbol))
;;               (ido-goto-symbol symbol))
;;              ((listp symbol)
;;               (setq name (car symbol))
;;               (setq position (cdr symbol)))
;;              ((stringp symbol)
;;               (setq name symbol)
;;               (setq position
;;                     (get-text-property 1 'org-imenu-marker symbol))))
;;             (unless (or (null position) (null name)
;;                         (string= (car imenu--rescan-item) name))
;;               (add-to-list 'symbol-names name)
;;               (add-to-list 'name-and-pos (cons name position))))))))
;; ;;get rid of `find-file-read-only' and replace it with something
;; ;; ;; more useful.

(use-package setup-dired
  :init
  (message "Loading setup-dired!")
  )

(use-package projectile
  :init
  (message "Loading projectile")
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
(use-package fzf
  :init
  (setenv "FZF_DEFAULT_OPTS" "--ansi --height 100%")
  (message "loading fzf")
  :ensure t
   :bind
  (("C-c C-f" . fzf-find-file)
   ("C-c C-d" . fzf-directory))
  )

(use-package helm-projectile
  :init
  (message "loading helm-projectile")
  :ensure t
  :config
  (helm-projectile-on))

(use-package helm
  :init
  (message "loading helm!")
  :ensure t
  :after (projectile helm-projectile)
  :config
  (require 'setup-helm)
  :defer 2)

(use-package org
  :init
  (message "Loading org-mode!")
  :config
  (require 'setup-org-mode)
  )

;; ================ project management ==========

;; ================ niceties ==========
;; (use-package async
;;   :ensure t)


;; -------------- EMBARK
;; (use-package marginalia
;;   :ensure t
;;   :config
;;   (marginalia-mode))

;; (use-package embark
;;   :ensure t

;;   :bind
;;   (("C-." . embark-act)         ;; pick some comfortable binding
;;    ("C-;" . embark-dwim)        ;; good alternative: M-.
;;    ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'

;;   :init

;;   ;; Optionally replace the key help with a completing-read interface
;;   (setq prefix-help-command #'embark-prefix-help-command)

;;   :config

;;   ;; Hide the mode line of the Embark live/completions buffers
;;   (add-to-list 'display-buffer-alist
;;                '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
;;                  nil
;;                  (window-parameters (mode-line-format . none)))))

;; ;; Consult users will also want the embark-consult package.
;; (use-package embark-consult
;;   :ensure t
;;   :after (embark consult)
;;   :demand t ; only necessary if you have the hook below
;;   ;; if you want to have consult previews as you move around an
;;   ;; auto-updating embark collect buffer
;;   :hook
;;   (embark-collect-mode . consult-preview-at-point-mode))

;; (use-package which-key
;;   :ensure t
;;   :init
;;   (which-key-mode)
;;   )
;; 
;; (use-package git-gutter
;;   :ensure t
;;   :init (global-git-gutter-mode)
;;   )

;; (use-package highlight-indent-guides
;;   :ensure t
;;   :hook prog-mode-hook highlight-indent-guides-mode
;;   :custom
;;   (setq highlight-indent-guides-character "|")
;;   )

;; (use-package smartparens
;;   :ensure t
;;   :diminish smartparens-mode
;;   :init
;;   (smartparens-global-mode)
;;   )

(use-package magit
  :init
  (message "loading magit!")
  :ensure t
  :config
  (require 'setup-magit)
  :defer 3)

;; (use-package server
;;   :init
;;   (message "loading server!")
;;   :ensure t
;;   :defer 2
;;   :config
;;   (unless (server-running-p)
;;   (server-start))
;; )

;; (use-package multiple-cursors
;;   :init
;;   (message "loading multiple-cursors!")
;;   :ensure t
;;   :defer 5)
;; (use-package recentf
;;   :init
;;   (message "loading recentf!")
;;   :ensure t
;;   :defer 2
;;   :config
;;   (setq recentf-exclude
;;         (append recentf-exclude
;;                 '("~$"
;;                   "\\.emacs.d*")))
;;   (setq
;;    recentf-max-saved-items 30
;;    recentf-max-menu-items 15)      ;; max 15 in menu
;;   )


;; (use-package autorevert
;;   :init
;;   (message "loading autorevert!")
;;   :ensure t
;;   :defer 2)


;; (use-package ivy
;;   :init
;;   (message "loading ivy!") ;; TODO rename. this is not ivy
;;   :ensure t
;;   :config
;;   ;; (ivy-mode 1)
;;   (require 'setup-ivy)
;;   :defer 1
;;   )

;; ================ niceties ==========

;; ================ programming ======
;; (defun python-mode-setup ()
;;   "Load python mode."
;;   (message "Custom python hook run")
;;   (load-library "setup-python"))
;; (add-hook 'python-mode-hook 'python-mode-setup)


;; (add-to-list 'load-path "~/.emacs.d/auto-complete-clang/")

;; (use-package setup-cc
;;   :init
;;   (message "Loading setup-cc!")
;;   :after (clang-format)
;;   :defer 2)

;; (use-package setup-rust
;;   :init
;;   (message "Loading setup-rust!")
;;   :defer 2)

;; (use-package clang-format
;;   :init
;;   (message "Loading clang-format")
;;   :ensure t
;;   :bind
;;   (("C-c r" . 'clang-format-region)
;;    ("C-c u" . 'clang-format-buffer)
;;    )
;;   :config
;;   (load "/usr/local/Cellar/clang-format/12.0.1/share/clang/clang-format.el")
;;   )

;; (autoload 'markdown-mode "markdown-mode"
;;    "Major mode for editing Markdown files" t)
;; (add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
;; (add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

;; ;; ;;------ cmake support
;; (use-package cmake-mode
;;   :ensure t
;;   :mode ("CMakeLists\\.txt\\'" "\\.cmake\\'"))

;; (use-package cmake-font-lock
;;   :after (cmake-mode)
;;   :hook (cmake-mode . cmake-font-lock-activate))


;; (defadvice compile (around split-horizontally activate)
;;   (let ((split-width-threshold 0)
;;         (split-height-threshold nil))
;;     ad-do-it))


;; (make-variable-buffer-local 'compile-command)

;; (use-package company
;; :ensure t
;; :config
;; (setq company-idle-delay 0)
;; (setq company-idle-delay 0.25)
;; (add-to-list 'company-backends 'company-capf)
;; ;; This enables candidates matching to be case-insensitive
;; (setq completion-ignore-case t)
;; (setq company-minimum-prefix-length 3)
;; (global-company-mode t)
;; )
;; (add-hook 'after-init-hook 'global-company-mode)
;; (with-eval-after-load 'company
;;   (define-key company-active-map (kbd "C-n") #'company-select-next)
;;   (define-key company-active-map (kbd "C-p") #'company-select-previous))


;; ================ programming ======

;; (use-package crux
;;   :ensure t
;;   :bind (("C-c C-o" . crux-open-with)
;;          ("C-a" . crux-move-beginning-of-line))
;;   )


;; (use-package yasnippet
;;   ;; Loads after 1 second of idle time.
;;   :defer 1
;;   :ensure t
;;   :config
;;   (yas-load-directory "~/.emacs.d/snippets")
;;   (yas-global-mode 0)
;; )
;(setq yas-triggers-in-field t)          

;; ;; DICCTIONARIES
(let ((langs '("american" "francais" "german")))
  (setq lang-ring (make-ring (length langs)))
  (dolist (elem langs) (ring-insert lang-ring elem)))

(defun cycle-ispell-languages ()
  (interactive)
  (let ((lang (ring-ref lang-ring -1)))
    (ring-insert lang-ring lang)
    (ispell-change-dictionary lang)))

(global-set-key [f6] 'cycle-ispell-languages)



;;---------------------- ispell
;; (define-key ctl-x-map "\C-i"
;;   #'endless/ispell-word-then-abbrev)

;; (defun endless/simple-get-word ()
;;   (car-safe (save-excursion (ispell-get-word nil))))

;; (defun endless/ispell-word-then-abbrev (p)
;;   "Call `ispell-word', then create an abbrev for it.
;; With prefix P, create local abbrev. Otherwise it will
;; be global.
;; If there's nothing wrong with the word at point, keep
;; looking for a typo until the beginning of buffer. You can
;; skip typos you don't want to fix with `SPC', and you can
;; abort completely with `C-g'."
;;   (interactive "P")
;;   (let (bef aft)
;;     (save-excursion
;;       (while (if (setq bef (endless/simple-get-word))
;;                  ;; Word was corrected or used quit.
;;                  (if (ispell-word nil 'quiet)
;;                      nil ; End the loop.
;;                    ;; Also end if we reach `bob'.
;;                    (not (bobp)))
;;                ;; If there's no word at point, keep looking
;;                ;; until `bob'.
;;                (not (bobp)))
;;         (backward-word)
;;         (backward-char))
;;       (setq aft (endless/simple-get-word)))
;;     (if (and aft bef (not (equal aft bef)))
;;         (let ((aft (downcase aft))
;;               (bef (downcase bef)))
;;           (define-abbrev
;;             (if p local-abbrev-table global-abbrev-table)
;;             bef aft)
;;           (message "\"%s\" now expands to \"%s\" %sally"
;;                    bef aft (if p "loc" "glob")))
;;       (user-error "No typo at or before point"))))

;; (setq save-abbrevs 'silently)
;; (setq-default abbrev-mode t)

;; todo this should use melpa, when ui is ready
;; ------ org-roam-ui
;; (use-package f
;;   :ensure t)
;; (use-package simple-httpd
;;   :ensure t)
;; (use-package websocket
;;   :ensure t)

;; ;; ----- flycheck
;; (use-package flycheck
;;   :ensure t
;;   :init
;;   (global-flycheck-mode t)
;;   (setq flycheck-idle-change-delay 10)
;;   (setq flycheck-check-syntax-automatically '(mode-enabled save))
;;   )
;; (setq flymake-mode nil)



(message "Finished loading files in load_configs")
(provide 'load_configs)
;;; load_configs.el ends here


