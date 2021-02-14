
;;; package ---- summary
;;; Code:
;;; Commentary:
;; Make startup faster by reducing the frequency of garbage
;; collection.  The default is 0.8MB.  Measured in bytes.

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
(add-to-list 'load-path "~/.emacs.d/auto-complete-clang/")
(add-to-list 'load-path "~/.emacs.d/lisp/benchmark-init-el")


(require 'my-core-settings)

;; (setq abbrev-file-name             ;; tell emacs where to read abbrev
;;         "~/.emacs.d/abbrev_defs")    ;; definitions from...

;; (setq save-abbrevs t)              ;; save abbrevs when files are saved
;; you will be asked before the abbreviations are saved
(message "load solarized-light") ;zenburn
(load-theme 'solarized-light t)
(if (member "Monaco" (font-family-list))
    (set-face-attribute
     'default nil :font "Monaco 18")
  (message "set font Monaco 18")
  )

(when (memq window-system '(mac ns))
  (message "init exec-path")
  (exec-path-from-shell-initialize))

;;-------------------------
;;Whenever the window scrolls a light will shine on top of your cursor so you know where it is.
(use-package beacon
  :ensure t
  :config
  (setq beacon-push-mark 35)
  (setq beacon-color "#e56911")
  (beacon-mode 1)
  )

;;-------------------------
(use-package yasnippet
  ;; Loads after 1 second of idle time.
  :defer 1
  :config
  (yas-load-directory "~/.emacs.d/snippets")
  (yas-global-mode 0)
)

;-------------------------------------------------
;; Don't ask before rereading the TAGS files if they have changed
(setq tags-revert-without-query t)
;; Don't warn when TAGS files are large
(setq large-file-warning-threshold nil)
;; Setup auto update now
(add-hook 'prog-mode-hook
  (lambda ()
    (add-hook 'after-save-hook
              'counsel-etags-virtual-update-tags 'append 'local)))
                                        ;DEFINE

(use-package smart-mode-line-powerline-theme
  :ensure t)

(use-package smart-mode-line
  :ensure t
  :config
  (setq sml/theme 'powerline)
  (setq sml/no-confirm-load-theme t)
  (add-hook 'after-init-hook 'sml/setup))

;-------------------------------------------------------------------
(use-package projectile
  :ensure t
  :diminish projectile-mode
  :bind
  (("C-c p f" . helm-projectile-find-file)
   ("C-c p p" . helm-projectile-switch-project)
   ("C-c p s" . projectile-save-project-buffers))
  :config
  (projectile-mode +1)
  )
(use-package helm-projectile
  :ensure t
  :config
  (helm-projectile-on))
;; when solved  activate projectile settings in my-core-settings
;; -------------------- require

;; (autoload 'yasnippet "yasnippet" "load yasnippet" t)
(require 'highlight-indentation) ;; visual guides for indentation
(require 'autopair)
;(require 'server)
;(require 'recentf)

(use-package setup-electric
  :defer 3)

(use-package hlinum
  :init
  (message "Loading hlinum!")
  :ensure t
  :config
  (setq linum-format "%3d \u2502 ")
  (hlinum-activate)
  )
(use-package magit
  :init
  (message "Loading Magit!")
  :ensure t
  :config
  (require 'setup-magit)
  :defer 3)


(use-package org
  :init
  (message "Loading org-mode!")
  :config
  (require 'setup-org-mode)
  :defer 3)

(use-package autorevert
  :init
  (message "loading autorevert!")
  :ensure t
  :defer 2)

(use-package helm
  :init
  (message "loading helm!")
  :ensure t
  :after (projectile helm-projectile)
  :config
  (require 'setup-helm)
  :defer 2)

(use-package ivy
  :init
  (message "loading ivy!")
  :ensure t
  :config
  (ivy-mode 1)
  (require 'setup-ivy)
  :defer 1
  )

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

(use-package paren
  :init
  (message "loading paren!")
  :ensure t
  :diminish paren-mode
  :defer 3
)
(defun python-mode-setup ()
  "Load python mode."
  (message "Custom python hook run")
  (load-library "setup-python"))
(add-hook 'python-mode-hook 'python-mode-setup)

(add-hook 'dired-load-hook (lambda () (load "dired-x")))
(setq dired-guess-shell-alist-user
      '(
        ("\\.xls\\'" "open &") ("\\.xlsx\\'" "open &")
        ("\\.doc\\'" "open &") ("\\.docx\\'" "open &")
        ("\\.ppt\\'" "open &") ("\\.pptx\\'" "open &")
        ("\\.pdf\\'" "open &")
        )
      )

(use-package setup-cc
  :init
  (message "Loading setup-cc!")
  :after (clang-format)
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
  (load "/usr/local/Cellar/clang-format/10.0.0/share/clang/clang-format.el")
  )

;; TODO conflict with helm
;; (use-package ido-vertical-mode
;;   :init
;;   (message "ido-vertical-mode")
;;   :config
;;   (ido-vertical-mode 1)

;;   )
;; (use-package ido
;;   :init
;;   (message "Loading Ido!")
;;   :ensure t
;;   :config
;;   (ido-mode 1)
;;   (require 'setup-ido)
;;   :after (ido-vertical-mode)
;;   :defer 2)

;; (use-package setup-tex
;;   ;; Loads after 2 second of idle time.
;;   :defer 3)


(use-package winner
  :init
  (message "loading winner!")
  :ensure t
  :defer 2)

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

(use-package guide-key
  :ensure t
  :defer 3
  :diminish guide-key-mode
  :config
  (progn
  (setq guide-key/guide-key-sequence '("C-x r" "C-x 4" "C-c"))
  (guide-key-mode 1)))  ; Enable guide-key-mode

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

;; Emacs server

                                        ; avy
                                        ;-----------------------------
(autoload
  'ace-jump-mode
  "ace-jump-mode"
  "Emacs quick move minor mode"
  t)
;; you can select the key you prefer to
(define-key global-map (kbd "C-c SPC") 'ace-jump-mode)
;;
;; enable a more powerful jump back function from ace jump mode
;;
(autoload
  'ace-jump-mode-pop-mark
  "ace-jump-mode"
  "Ace jump back:-)"
  t)
(eval-after-load "ace-jump-mode"
  '(ace-jump-mode-enable-mark-sync))
(define-key global-map (kbd "C-x SPC") 'ace-jump-mode-pop-mark)

(autoload
  'ace-window
  "ace-window"
  "Emacs quick move minor mode"
  t)

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

;; (nav-disable-overeager-window-splitting)

;; https://github.com/magnars/.emacs.d/blob/master/init.el

;; Browse kill ring
(require 'browse-kill-ring)
(setq browse-kill-ring-quit-action 'save-and-restore)
(setq browse-kill-ring-highlight-current-entry t)

;(global-linum-mode 1)

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
;; (setq  flyspell-make t)

;(setq flyspell-mode 0)

;(add-hook 'text-mode-hook 'flyspell-mode) ;
;(add-hook 'prog-mode-hook 'flyspell-prog-mode)

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

(global-set-key (kbd "C-c B") 'ebib)

;; (setq
;;  ebib-file-search-dirs '("~/LitDB/pdf/"))
;; (setq ebib-preload-bib-files
;;       (list "~/LitDB/ped.bib")
;;       )

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
(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

;; ;;------ cmake support
;; Add cmake listfile names to the mode list.
(setq auto-mode-alist
      (append
       '(("CMakeLists\\.txt\\'" . cmake-mode))
       '(("\\.cmake\\'" . cmake-mode))
       auto-mode-alist))

(use-package cmake-mode
:ensure t
)

;; ;;--------------------------------- ibuffer
(autoload 'ibuffer "ibuffer" "List buffers." t)
(setq ibuffer-default-sorting-mode 'major-mode)
(setq ibuffer-expert t)
(setq ibuffer-show-empty-filter-groups nil)

;; ;; dired
(setq dired-dwim-target t)
;; (require 'dired-details)
;; (setq-default dired-details-hidden-string "--- ")
;; (dired-details-install)
;; Move files between split panes
(setq dired-dwim-target t)

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

(defun my-test-emacs ()
  "debuging start of emacs"
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
  "test starting emacs for bugs"
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
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-display-custom-times t)
 '(org-time-stamp-custom-formats '("<%d/%m/%Y %a>" . "<%d/%m/%Y  %a [%H:%M]>"))
 '(package-selected-packages
   '(clang-format uniquify org-mode zotxt zenburn-theme xcscope w32-browser use-package undo-tree solarized-theme sml-modeline smartparens smart-mode-line-powerline-theme python-mode projectile-sift org-roam-server org-roam-bibtex org-journal nav multiple-cursors modern-cpp-font-lock magit lsp-ui ido-vertical-mode hlinum helm-projectile helm-lsp helm-git-grep helm-bibtex guide-key flymake-cursor flycheck-clang-tidy fill-column-indicator expand-region exec-path-from-shell elpy doom-modeline diminish cpputils-cmake counsel-etags company-lsp color-theme cmake-project cmake-mode ccls browse-kill-ring beacon autopair auto-complete-clang-async auto-complete-clang auctex-latexmk ag ace-window)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(show-paren-match ((((class color) (background light)) (:background "blue")))))
