;;; package ---- summary
;;; Code:
;;; Commentary:
;; Make startup faster by reducing the frequency of garbage
;; collection.  The default is 0.8MB.  Measured in bytes.

; this is called in org-roam-setup. For some reason, thast I don't know, it goes here!
(setq org-roam-v2-ack t)
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))


(add-to-list 'load-path (expand-file-name "~/.emacs.d/lisp"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/GitHubPackages/org-roam-ui"))
(add-to-list 'load-path "~/.emacs.d/auto-complete-clang/")
(add-to-list 'load-path "~/.emacs.d/lisp/benchmark-init-el")


(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)

(require 'my-core-settings)
;; auto revert mode
(global-auto-revert-mode 1)
(setq auto-revert-use-notify nil)

(use-package solarized-theme
:ensure t
:config
(load-theme 'solarized-light t))

(use-package svg-tag-mode
  (global-svg-tag-mode t)
  )



(when (display-graphic-p)
    (use-package fira-code-mode
      :ensure t
      :custom (fira-code-mode-disabled-ligatures '("[]" "#{" "#(" "#_" "#_(" "x")) ;; List of ligatures to turn off
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


(if  (daemonp)
    (message "in DEAMON")
  (fira-code-mode)
  )
    
(unless (display-graphic-p)
   (message "in TERMNIAL")
  )

(use-package git-gutter
  :ensure t
  :init (global-git-gutter-mode)
  )


;;-------------------------
;;Whenever the window scrolls a light will shine on top of your cursor so you know where it is.
(use-package beacon
  :ensure t
  :config
  (setq beacon-push-mark 35)
  (setq beacon-color "#e56911")
  (beacon-mode 1)
  )

(use-package crux
  :ensure t
  :bind (("C-c C-o" . crux-open-with)
         ("C-a" . crux-move-beginning-of-line))
  )

;;-------------------------
(use-package yasnippet
  ;; Loads after 1 second of idle time.
  :defer 1
  :ensure t
  :config
  (yas-load-directory "~/.emacs.d/snippets")
  (yas-global-mode 0)
)

;-------------------------------------------------
;; Don't ask before rereading the TAGS files if they have changed


;; Setup auto update now

;; (use-package smart-mode-line-powerline-theme
;;   :ensure t)

;; (use-package smart-mode-line
;;   :ensure t
;;   :config
;;   (setq sml/theme 'powerline)
;;   (setq sml/no-confirm-load-theme t)
;;   (setq sml/show-time t)
;;   (add-hook 'after-init-hook 'sml/setup))
;; ;; (setq display-time-format "%d|%H:%M")
 (defface egoge-display-time
   '((((type x w32 mac))
      ;; #060525 is the background colour of my default face.
      (:foreground "#060525" :inherit bold))
     (((type tty))
      (:foreground "blue")))
   "Face used to display the time in the mode line.")

;;  ;; This causes the current time in the mode line to be displayed in
;;  ;; `egoge-display-time-face' to make it stand out visually.
;;  (setq display-time-string-forms
;;        '((propertize (concat ">>" month day " | " 24-hours ":" minutes "<< ")
;;  		     'face 'egoge-display-time)))

 (setq display-time-string-forms
       '((propertize (concat day"/"month " | " 24-hours ":" minutes ""))))

(setq display-time-day-and-date t)
(display-time-mode 1)

;; ---- company
(use-package company
:ensure t
:config
(setq company-idle-delay 0)
(setq company-idle-delay 0.25)
(add-to-list 'company-backends 'company-capf)
;; This enables candidates matching to be case-insensitive
(setq completion-ignore-case t)
(setq company-minimum-prefix-length 3)
(global-company-mode t)
)
(add-hook 'after-init-hook 'global-company-mode)
(with-eval-after-load 'company
  (define-key company-active-map (kbd "C-n") #'company-select-next)
  (define-key company-active-map (kbd "C-p") #'company-select-previous))

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

(use-package which-key
  :ensure t
  :init
  (which-key-mode)
  )


;-------------------------------------------------------------------
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

;; when solved  activate projectile settings in my-core-settings
;; -------------------- require

;; (autoload 'yasnippet "yasnippet" "load yasnippet" t)

(use-package highlight-indent-guides
  :ensure t
  :hook prog-mode-hook highlight-indent-guides-mode
  ;;  (add-hook 'prog-mode-hook 'highlight-indent-guides-mode)
  :custom
  (setq highlight-indent-guides-character "|")
  )

(use-package smartparens
  :ensure t
  :diminish smartparens-mode
  :init
  (smartparens-global-mode)
  )

;; (use-package setup-electric
;;   :defer 3)

(use-package hlinum
  :init
  (message "loading hlinum!")
  :ensure t
  :config
  (setq linum-format "%3d \u2502 ")
  (hlinum-activate)
  )
(use-package magit
  :init
  (message "loading magit!")
  :ensure t
  :config
  (require 'setup-magit)
  :defer 3)


(use-package org
  :init
  (message "Loading org-mode!")
  :config
  (require 'setup-org-mode)
  )

(use-package autorevert
  :init
  (message "loading autorevert!")
  :ensure t
  :defer 2)


(use-package ivy
  :init
  (message "loading ivy!") ;; TODO rename. this is not ivy
  :ensure t
  :config
  ;; (ivy-mode 1)
  (require 'setup-ivy)
  :defer 1
  )
;; todo 

(use-package recentf
  :init
  (message "loading reventf!")
  :ensure t
  :defer 2
  :config
  (setq recentf-exclude
        (append recentf-exclude
                '("~$"
                  "\\.emacs.d*")))
  (setq
   recentf-max-saved-items 30
   recentf-max-menu-items 15)      ;; max 15 in menu
  )

(use-package server
  :init
  (message "loading server!")
  :ensure t
  :defer 2
  :config
  (unless (server-running-p)
  (server-start))
)

(use-package multiple-cursors
  :init
  (message "loading multiple-cursors!")
  :ensure t
  :defer 5)

;; (use-package paren
;;   :init
;;   (message "loading paren!")
;;   :ensure t
;;   :diminish paren-mode
;;   :defer 3
;; )
(defun python-mode-setup ()
  "Load python mode."
  (message "Custom python hook run")
  (load-library "setup-python"))
(add-hook 'python-mode-hook 'python-mode-setup)

(use-package setup-dired
  :init
  (message "Loading setup-dired!")
  )

(use-package setup-cc
  :init
  (message "Loading setup-cc!")
  :after (clang-format)
  :defer 2)

(use-package setup-rust
  :init
  (message "Loading setup-rust!")
  :defer 2)


(use-package clang-format
  :init
  (message "Loading clang-format")
  :ensure t
  :bind
  (("C-c r" . 'clang-format-region)
   ("C-c u" . 'clang-format-buffer)
   )
  :config
  (load "/usr/local/Cellar/clang-format/12.0.1/share/clang/clang-format.el")
  )


;; (use-package setup-tex
;;   ;; Loads after 2 second of idle time.
;;   :defer 3)


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


(use-package undo-tree
  :ensure t
  :diminish undo-tree-mode
  :config
  (progn
    (global-undo-tree-mode)
    (setq undo-tree-visualizer-timestamps t)
    (setq undo-tree-visualizer-diff t)))

;----------------  load setups ----------------------------
(message "load my setups")

;; overwrite selected text
(delete-selection-mode t)

;; When popping the mark, continue popping until the cursor
;; actually moves
(defadvice pop-to-mark-command (around ensure-new-position activate)
  (let ((p (point)))
    (dotimes (i 10)
      (when (= p (point)) ad-do-it))))

(setq set-mark-command-repeat-pop t)

;; http://endlessparentheses.com/new-in-emacs-25-1-have-prettify-symbols-mode-reveal-the-symbol-at-point.html
(setq prettify-symbols-unprettify-at-point 'right-edge)

(message "setups loaded")
;;-----------------------------

(defun comment-or-uncomment-region-or-line ()
  "Comments or uncomments the region or the current line if there's no active region."
  (interactive)
  (let (beg end)
    (if (region-active-p)
        (setq beg (region-beginning) end (region-end))
      (setq beg (line-beginning-position) end (line-end-position)))
    (comment-or-uncomment-region beg end)
    (next-line)))

;(global-set-key "\C-c\C-c" 'comment-region)

(defun comment-region-lines (beg end &optional arg)
  "Like `comment-region', but comment/uncomment whole lines."
  (interactive "*r\nP")
  (if (> beg end) (let (mid) (setq mid beg beg end end mid)))
  (let ((bol  (save-excursion (goto-char beg) (line-beginning-position)))
        (eol  (save-excursion (goto-char end) (line-end-position))))
    (comment-region bol end arg)))

;; after copy Ctrl+c in X11 apps, you can paste by `yank' in emacs
(setq select-enable-clipboard t)

;; after mouse selection in X11, you can paste by `yank' in emacs
(setq select-enable-primary t)

(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;; ;;-------------------------- Macros

(defadvice compile (around split-horizontally activate)
  (let ((split-width-threshold 0)
        (split-height-threshold nil))
    ad-do-it))

(defvar server-buffer-clients)
(when (and (fboundp 'server-start) (string-equal (getenv "TERM") 'xterm))
  (server-start)
  (defun fp-kill-server-with-buffer-routine ()
    (and server-buffer-clients (server-done)))
  (add-hook 'kill-buffer-hook 'fp-kill-server-with-buffer-routine))

(setq flymake-gui-warnings-enabled nil)

(setq
 uniquify-buffer-name-style 'post-forward
 uniquify-separator ":"
 uniquify-after-kill-buffer-p t
 uniquify-ignore-buffers-re "^\\*")

;;https://github.com/magnars/expand-region.el
(use-package expand-region
  :ensure t
  :bind ("M-2" . er/expand-region)
  )
(use-package avy
  :ensure t
  :bind
  ("C-." . avy-goto-char)
  ("C-c ." . avy-goto-char-2)
  :config
  (avy-setup-default)  
  )
(global-set-key "\C-cg" 'avy-goto-line)
(global-set-key (kbd "C-c C-j") 'avy-resume)

;------------------------------

;; define function to shutdown emacs server instance
(defun server-shutdown ()
  "Save buffers, Quit, and Shutdown (kill) server."
  (interactive)
  (save-some-buffers)
  (kill-emacs)
  )

(defun ido-goto-symbol (&optional symbol-list)
  "Refresh imenu and jump to a place in the buffer using Ido.
argument SYMBOL-LIST"
  (interactive)
  (unless (featurep 'imenu)
    (require 'imenu nil t))
  (cond
   ((not symbol-list)
    (let ((ido-mode ido-mode)
          (ido-enable-flex-matching
           (if (boundp 'ido-enable-flex-matching)
               ido-enable-flex-matching t))
          name-and-pos symbol-names position)
      (unless ido-mode
        (ido-mode 1)
        (setq ido-enable-flex-matching t))
      (while (progn
               (imenu--cleanup)
               (setq imenu--index-alist nil)
               (ido-goto-symbol (imenu--make-index-alist))
               (setq selected-symbol
                     (ido-completing-read "Symbol? " symbol-names))
                   (string= (car imenu--rescan-item) selected-symbol)))
          (unless (and (boundp 'mark-active) mark-active)
            (push-mark nil t nil))
          (setq position (cdr (assoc selected-symbol name-and-pos)))
          (cond
           ((overlayp position)
            (goto-char (overlay-start position)))
           (t
            (goto-char position)))))
       ((listp symbol-list)
        (dolist (symbol symbol-list)
          (let (name position)
            (cond
             ((and (listp symbol) (imenu--subalist-p symbol))
              (ido-goto-symbol symbol))
             ((listp symbol)
              (setq name (car symbol))
              (setq position (cdr symbol)))
             ((stringp symbol)
              (setq name symbol)
              (setq position
                    (get-text-property 1 'org-imenu-marker symbol))))
            (unless (or (null position) (null name)
                        (string= (car imenu--rescan-item) name))
              (add-to-list 'symbol-names name)
              (add-to-list 'name-and-pos (cons name position))))))))
;;get rid of `find-file-read-only' and replace it with something
;; ;; more useful.


;; https://github.com/magnars/.emacs.d/blob/master/init.el

(use-package browse-kill-ring
  :ensure t
  :config
  (setq browse-kill-ring-quit-action 'save-and-restore)
  (setq browse-kill-ring-highlight-current-entry t
  ))


(defun nolinum ()
  "No lines."
  (global-linum-mode 0)
  )
(add-hook 'org-mode-hook 'nolinum)

(setq ical-pull-list `("https://www.google.com/calendar/ical/s1ilvt2buhj2adrg7363t4k77g%40group.calendar.google.com/private-8ed0f1ebe7b7fcce8ba154c6d823d71c/basic.ics"))

(defun aj-toggle-fold ()
  "Toggle fold all lines larger than indentation on current line."
  (interactive)
  (let ((col 1))
    (save-excursion
      (back-to-indentation)
      (setq col (+ 1 (current-column)))
      (set-selective-display
       (if selective-display nil (or col 1))))))
(global-set-key [(M C i)] 'aj-toggle-fold)

(setq ansi-color-for-comint-mode t)

;; ;; KEYBINDINGS
(defun toggle-fullscreen (&optional f)
  (interactive)
  (let ((current-value (frame-parameter nil 'fullscreen)))
    (set-frame-parameter nil 'fullscreen
                         (if (equal 'fullboth current-value)
                             (if (boundp 'old-fullscreen) old-fullscreen nil)
                           (progn (setq old-fullscreen current-value)
                                  'fullboth)))))

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
(define-key ctl-x-map "\C-i"
  #'endless/ispell-word-then-abbrev)

(defun endless/simple-get-word ()
  (car-safe (save-excursion (ispell-get-word nil))))

(defun endless/ispell-word-then-abbrev (p)
  "Call `ispell-word', then create an abbrev for it.
With prefix P, create local abbrev. Otherwise it will
be global.
If there's nothing wrong with the word at point, keep
looking for a typo until the beginning of buffer. You can
skip typos you don't want to fix with `SPC', and you can
abort completely with `C-g'."
  (interactive "P")
  (let (bef aft)
    (save-excursion
      (while (if (setq bef (endless/simple-get-word))
                 ;; Word was corrected or used quit.
                 (if (ispell-word nil 'quiet)
                     nil ; End the loop.
                   ;; Also end if we reach `bob'.
                   (not (bobp)))
               ;; If there's no word at point, keep looking
               ;; until `bob'.
               (not (bobp)))
        (backward-word)
        (backward-char))
      (setq aft (endless/simple-get-word)))
    (if (and aft bef (not (equal aft bef)))
        (let ((aft (downcase aft))
              (bef (downcase bef)))
          (define-abbrev
            (if p local-abbrev-table global-abbrev-table)
            bef aft)
          (message "\"%s\" now expands to \"%s\" %sally"
                   bef aft (if p "loc" "glob")))
      (user-error "No typo at or before point"))))

(setq save-abbrevs 'silently)
(setq-default abbrev-mode t)

;;--------------------------------------- PAREN

;(setq-default truncate-lines t) ;; will trucate lines if they are too long.
(transient-mark-mode t) ;; will highlight region between point and mark.
(setq-default global-visual-line-mode t)
(setq show-paren-style 'parenthesis) ; highlight just brackets
                                        ;(setq show-paren-style 'expression) ; highlight entire bracket expression

(autoload 'markdown-mode "markdown-mode"
   "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

;; ;;------ cmake support
(use-package cmake-mode
  :ensure t
  :mode ("CMakeLists\\.txt\\'" "\\.cmake\\'"))

(use-package cmake-font-lock
  :after (cmake-mode)
  :hook (cmake-mode . cmake-font-lock-activate))

;; ;;--------------------------------- ibuffer
(autoload 'ibuffer "ibuffer" "List buffers." t)
(setq ibuffer-default-sorting-mode 'major-mode)
(setq ibuffer-expert t)
(setq ibuffer-show-empty-filter-groups nil)


;; https://github.com/magnars/multiple-cursors.el

;;-----
(make-variable-buffer-local 'compile-command)


;; Use a hook so the message doesn't get clobbered by other messages.
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Emacs ready in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)))

(use-package async
  :ensure t)


(use-package treemacs
  :ensure t
  :after (lsp-mode)
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :config
  (progn
    (setq treemacs-collapse-dirs                   (if treemacs-python-executable 3 0)
          treemacs-deferred-git-apply-delay        0.5
          treemacs-directory-name-transformer      #'identity
          treemacs-display-in-side-window          t
          treemacs-eldoc-display                   t
          treemacs-file-event-delay                5000
          treemacs-file-extension-regex            treemacs-last-period-regex-value
          treemacs-file-follow-delay               0.2
          treemacs-file-name-transformer           #'identity
          treemacs-follow-after-init               t
          treemacs-expand-after-init               t
          treemacs-git-command-pipe                ""
          treemacs-goto-tag-strategy               'refetch-index
          treemacs-indentation                     2
          treemacs-indentation-string              " "
          treemacs-is-never-other-window           nil
          treemacs-max-git-entries                 5000
          treemacs-missing-project-action          'ask
          treemacs-move-forward-on-expand          nil
          treemacs-no-png-images                   nil
          treemacs-no-delete-other-windows         t
          treemacs-project-follow-cleanup          nil
          treemacs-persist-file                    (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
          treemacs-position                        'left
          treemacs-read-string-input               'from-child-frame
          treemacs-recenter-distance               0.1
          treemacs-recenter-after-file-follow      nil
          treemacs-recenter-after-tag-follow       nil
          treemacs-recenter-after-project-jump     'always
          treemacs-recenter-after-project-expand   'on-distance
          treemacs-litter-directories              '("/node_modules" "/.venv" "/.cask")
          treemacs-show-cursor                     nil
          treemacs-show-hidden-files               t
          treemacs-silent-filewatch                nil
          treemacs-silent-refresh                  nil
          treemacs-sorting                         'alphabetic-asc
          treemacs-select-when-already-in-treemacs 'move-back
          treemacs-space-between-root-nodes        t
          treemacs-tag-follow-cleanup              t
          treemacs-tag-follow-delay                1.5
          treemacs-text-scale                      nil
          treemacs-user-mode-line-format           nil
          treemacs-user-header-line-format         nil
          treemacs-width                           35
          treemacs-width-is-initially-locked       t
          treemacs-workspace-switch-cleanup        nil)

    ;; The default width and height of the icons is 22 pixels. If you are
    ;; using a Hi-DPI display, uncomment this to double the icon size.
    ;;(treemacs-resize-icons 44)

    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode 'always)

    (pcase (cons (not (null (executable-find "git")))
                 (not (null treemacs-python-executable)))
      (`(t . t)
       (treemacs-git-mode 'deferred))
      (`(t . _)
       (treemacs-git-mode 'simple)))

    (treemacs-hide-gitignored-files-mode nil))
  :bind
  (:map global-map
        ("M-0"       . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t t"   . treemacs)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag)))

;; (use-package treemacs-evil
;;   :after (treemacs evil)
;;   :ensure t)

(use-package treemacs-projectile
  :after (treemacs projectile)
  :ensure t)

(use-package treemacs-icons-dired
  :after (treemacs dired)
  :ensure t
  :config (treemacs-icons-dired-mode))

(use-package treemacs-magit
  :after (treemacs magit)
  :ensure t)

(use-package treemacs-persp ;;treemacs-perspective if you use perspective.el vs. persp-mode
  :after (treemacs persp-mode) ;;or perspective vs. persp-mode
  :ensure t
  :config (treemacs-set-scope-type 'Perspectives))


;; todo this should use melpa, when ui is ready
;; ------ org-roam-ui
(use-package f
  :ensure t)
(use-package simple-httpd
  :ensure t)
(use-package websocket
  :ensure t)

(load-library "org-roam-ui")

;; grammarly needs online connections and is often annoying
;;--------------- grammarly
;;; grammarly
;; (use-package lsp-grammarly
;;   :ensure t
;;   :hook (text-mode . (lambda ()
;;                        (require 'lsp-grammarly)
;;                        (lsp))))  ; or lsp-deferred


;; (use-package flycheck-grammarly
;;   :ensure t
;;   :config
;;   (setq flycheck-grammarly-check-time 0.8)
;;   )

;; (use-package keytar
;;   :ensure t
;;   :init
;;   (keytar-install)
;;   )

;;--------------- grammarly

;;-----------------------
(defun my-test-emacs ()
  "Debuging start of Emacs."
  (interactive)
  (require 'async)
  (async-start
   (lambda () (shell-command-to-string
               "emacs --batch --eval \"
(condition-case e
    (progn
      (load \\\"~/.emacs.d/init.el\\\")
      (message \\\"-OK-\\\"))
  (error
   (message \\\"ERROR!\\\")
   (signal (car e) (cdr e))))\""))
   `(lambda (output)
      (if (string-match "-OK-" output)
          (when ,(called-interactively-p 'any)
            (message "All is well"))
        (switch-to-buffer-other-window "*startup error*")
        (delete-region (point-min) (point-max))
        (insert output)
        (search-backward "ERROR!")))))

(defun auto-test-emacs ()
  "Test starting Emacs for bugs."
  (when (eq major-mode 'emacs-lisp-mode)
    (my-test-emacs)))

(add-hook 'after-save-hook 'auto-test-emacs)

(setq current-theme "dark")
(defconst light-theme 'solarized-light)
(defconst dark-theme 'zenburn)

(setq yas-triggers-in-field t)


; profile:
;    emacs -Q -l ~/.emacs.d/lisp/profile-dotemacs.el --eval "(setq profile-dotemacs-file (setq load-file-name \"$(abspath init.el)\"))" -f profile-dotemacs
(message "done loading emacs!")

(provide 'init)

;;; init.el ends here
