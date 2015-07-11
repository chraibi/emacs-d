
;;; Code:

;; Turn off mouse interface early in startup to avoid momentary display
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(setq inhibit-splash-screen t);; Disable splash screen
(setq inhibit-startup-message t)
(setq inhibit-startup-echo-area-message t)
(setq inhibit-startup-screen t)
(setq initial-scratch-message nil)


;; Use Emacs terminfo, not system terminfo
(setq system-uses-terminfo nil)

(defvar *emacs-load-start* (current-time))
;; My location for external packages.

(add-to-list 'load-path (expand-file-name "~/.emacs.d/lisp"))
(add-to-list 'load-path "~/.emacs.d/auto-complete-clang/")
(global-set-key "\C-z" 'nil)
;; ;;; Make all yes-or-no questions as y-or-n
(fset 'yes-or-no-p 'y-or-n-p)



(getenv "PATH")
(setenv "PATH"
        (concat
         "/usr/texbin" ":"
         "/usr/local/bin/" ":"
         (getenv "PATH")))

(add-to-list 'exec-path "/usr/local/bin")

(global-set-key (kbd "M-2") #'er/expand-region)


(setq py-install-directory "~/.emacs.d/lisp/pdee-master") 
(add-to-list 'load-path py-install-directory)
(setq display-battery-mode t) (display-battery-mode 1) ;; will make the display of date and time persistent.

(transient-mark-mode 1)
(global-visual-line-mode 1) ; 1 for on, 0 for off.
;(global-hl-line-mode 1) ; turn on highlighting current line
;(set-face-background hl-line-face "gray35")
                                        ; ELPA package support
(when (>= emacs-major-version 24)
  (require 'package)
  (message "add repos")
  (add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
  (add-to-list 'package-archives '("ELPA" . "http://tromey.com/elpa/"))
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))
  (add-to-list 'package-archives '("MELPA" . "http://melpa.milkbox.net/packages/"))
  (package-initialize)
  )


; list the packages
(setq package-list '(
		     cl
		     sml-modeline 
		     zenburn-theme
		     highlight-indentation
		     org-journal
                     auctex
		     yasnippet
                     expand-region
		     multiple-cursors
		     dired+
		     sx
		     linum
		     server
		     nav
		     recentf
		     ;mode-mapping
		     semantic
		     ido
		     xcscope
		     cmake-project
		     cpputils-cmake
		     paren ;; will highlight matching parentheses next to cursor.
		     auto-complete
		     auto-complete-clang-async
		     auto-complete-clang
		     company	;
		     autopair ;; to enable in all buffers
		     flymake-cursor
		     python
		     python-mode
		     ipython
		     elpy
		     ;;ob-plantuml
		    ; uniquify		;
		     projectile
		     helm-projectile
		     undo-tree
		     )
      )	     

; activate all the packages (in particular autoloads)
(package-initialize)
(message "packages initialized")
; fetch the list of packages available 
(unless package-archive-contents
  (package-refresh-contents))
(message "fetch packages")
; install the missing packages
(dolist (package package-list)
  (message ">> install %s" package)
  (unless  (package-installed-p package)
    (package-install package)
    )
)
(message "done with installation")
(setq abbrev-file-name             ;; tell emacs where to read abbrev
        "~/.emacs.d/abbrev_defs")    ;; definitions from...

(setq save-abbrevs t)              ;; save abbrevs when files are saved
                                     ;; you will be asked before the abbreviations are saved

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ag-executable "/usr/local/bin/ag")
 '(c-basic-offset 6)
 '(c-default-style (quote ((c-mode . "stroustrup") (c++-mode . "stroustrup"))))
 '(custom-safe-themes
   (quote
    ("ace9f12e0c00f983068910d9025eefeb5ea7a711e774ee8bb2af5f7376018ad2" "c74e83f8aa4c78a121b52146eadb792c9facc5b1f02c917e3dbb454fca931223" "a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" "9dae95cdbed1505d45322ef8b5aa90ccb6cb59e0ff26fef0b8f411dfc416c552" default)))
 '(ecb-options-version "2.40")
 '(org-agenda-files
   (quote
    ("~/Lectures/2015_Ingenieurinformatik/Vorlesungen/todo.org" "~/Orgfiles/org-files/journal/20150422" )))
 '(org-agenda-ndays 7)
 '(org-agenda-show-all-dates t)
 '(org-agenda-skip-deadline-if-done t)
 '(org-agenda-skip-scheduled-if-done t)
 '(org-agenda-start-on-weekday nil)
 '(org-deadline-warning-days 14)
 '(org-display-custom-times t)
 '(org-fast-tag-selection-single-key (quote expert))
 '(org-reverse-note-order t)
 '(org-time-stamp-custom-formats (quote ("<%d/%m/%Y %a>" . "<%d/%m/%Y  %a [%H:%M]>")))
 '(python-indent-guess-indent-offset nil)
 '(sml/battery-format " [%p] ")
 '(sml/show-client t)
)
(add-hook 'after-init-hook 'sml/setup) ;todo
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(show-paren-match ((((class color) (background light)) (:background "blue")))))


(cond (( >= emacs-major-version 24)
         (message "load zenburn")
       (load-theme 'zenburn t)
       (if (member "Monaco" (font-family-list))
           (set-face-attribute
            'default nil :font "Monaco 18")
         (message "set Monaco")
         )
       
       )
      );Version 24

;; frame font
;; Setting English Font

(setq user-full-name "M. Chraibi")
(setq user-mail-address "m.chraibi@gmail.com")
(set-default 'cursor-type 'bar)
(setq cursor-type 'bar)
(blink-cursor-mode 1)
(setq-default cursor-type '(hbar . 1))
;; ;; this variable
;;(setq auto-indent-on-visit-file t) ;; If you want auto-indent on for files
;; (require 'auto-indent-mode)
;; (auto-indent-global-mode)


(message "load packages")
;; -------------------- require
(require 'cl)
(require 'company)			;
(require 'auto-complete-clang)
(require 'org-journal)
(require 'yasnippet)
(require 'multiple-cursors)
(require 'dired-x)
;(require 'mode-mapping)			;
(require 'semantic/ia)
(require 'ido)
(require 'xcscope)
(require 'cpputils-cmake)
(require 'paren)
(require 'auto-complete)
(require 'auto-complete-config)
(require 'auto-complete-clang-async)
(require 'cmake-project)
;(require 'ipython)
(require 'highlight-indentation) ;; visual guides for indentation
(require 'autopair)
(require 'ob-plantuml)
(require 'linum)
(require 'server)
(require 'nav)
(require 'recentf)
(require 'flymake-cursor)
;; uniquify: unique buffer names
;(require 'uniquify) ;; make buffer names more unique
(require 'projectile)
(require 'helm-projectile)
;------  require setups
(message "load my setups")
(cscope-setup)
; todo
;(require 'setup-electric)		;
;(require 'setup-magit)
;(require 'setup-org-mode)
;(require 'setup-helm)
;(require 'flymake-setup)
;; (cscope-setup)
;;setup-electric

;-----------------------------
(show-paren-mode t) ;; will highlight matching parentheses next to cursor.
(autopair-global-mode) ;; to enable in all buffers
;; ----- todo make setup-helm.el
(helm-projectile-on)
(projectile-global-mode)
(setq projectile-enable-caching nil)
(setq projectile-switch-project-action 'projectile-dired)
(setq projectile-remember-window-configs t )
(setq projectile-completion-system 'helm)
(setq projectile-switch-project-action 'helm-projectile)
(setq helm-projectile-fuzzy-match nil)
;;-----------------------------
(setq  electric-pair-mode t)

(yas-global-mode 1)

(c-add-style "my-style" 
             '("stroustrup"
               (indent-tabs-mode . nil)        ; use spaces rather than tabs
               (c-basic-offset . 5)            ; indent by four spaces
               (c-offsets-alist . ((inline-open . 0)  ; custom indentation rules
                                   (brace-list-open . 0)
                                   (statement-case-open . +)))))

(defun my-c++-mode-hook ()
  (c-set-style "my-style")        ; use my-style defined above
  (auto-fill-mode)         
  (c-toggle-auto-hungry-state 1))

(add-hook 'c++-mode-hook 'my-c++-mode-hook)


;; I hate tabs!
(setq-default indent-tabs-mode nil)
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))

(defun comment-or-uncomment-region-or-line ()
  "Comments or uncomments the region or the current line if there's no active region."
  (interactive)
  (let (beg end)
    (if (region-active-p)
        (setq beg (region-beginning) end (region-end))
      (setq beg (line-beginning-position) end (line-end-position)))
    (comment-or-uncomment-region beg end)
    (next-line)))
(global-set-key (kbd "M-;") 'comment-or-uncomment-region-or-line)


;(add-hook 'after-init-hook #'global-flycheck-mode) ;

(autoload 'flyspell-mode "flyspell" "On-the-fly spelling checker." t)
(modify-frame-parameters nil '((wait-for-wm . nil)))

;; after copy Ctrl+c in X11 apps, you can paste by `yank' in emacs
(setq x-select-enable-clipboard t)

;; after mouse selection in X11, you can paste by `yank' in emacs
(setq x-select-enable-primary t)



(global-auto-revert-mode t)
(global-set-key "\C-x\C-k" 'kill-region)
(global-set-key "\C-c\C-k" 'kill-region)
                                        ;(global-set-key "\C-x\C-c" 'kill-emacs) ;; STRANGE
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)


;; ;;-------------------------- Macros

;; ;; scroll one line at a time (less "jumpy" than defaults)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq scroll-step 1) ;; keyboard scroll one line at a time

;; ;;    split vertically
(setq split-height-threshold nil)
(setq split-width-threshold 0)
(defadvice compile (around split-horizontally activate)
  (let ((split-width-threshold 0)
        (split-height-threshold nil))
    ad-do-it))

(defun my-recompile ()
  "Run compile and resize the compile window closing the old one if necessary"
  (interactive)
  (progn
    (if (get-buffer "*compilation*") ; If old compile window exists
        (progn
          (delete-windows-on (get-buffer "*compilation*")) ; Delete the compilation windows
          (kill-buffer "*compilation*") ; and kill the buffers
          )
      )
    (call-interactively 'compile)
    (enlarge-window 20)
    )
  )

(defun my-next-error ()
  "Move point to next error and highlight it"
  (interactive)
  (progn
    (next-error)
    (end-of-line-nomark)
    (beginning-of-line-mark)
    )
  )

(defun my-previous-error ()
  "Move point to previous error and highlight it"
  (interactive)
  (progn
    (previous-error)
    (end-of-line-nomark)
    (beginning-of-line-mark)
    )
  )

(global-set-key (kbd "C-n") 'my-next-error)
(global-set-key (kbd "C-p") 'my-previous-error)
(global-set-key (kbd "C-x <f9>") 'my-recompile)
(global-set-key [f9] 'compile)
(setq compilation-scroll-output 'first-error)

(winner-mode 1)

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




;; ;; get rid of `find-file-read-only' and replace it with something
;; ;; more useful.
(global-set-key (kbd "C-x C-r") 'ido-recentf-open)

;; ;; enable recent files mode.
(recentf-mode t)
(setq recentf-exclude
      (append recentf-exclude
              '("~$"
                "\\.emacs.d*")))


(setq
 recentf-max-saved-items 30
 recentf-max-menu-items 15)      ;; max 15 in menu

(defun ido-recentf-open ()
  "Use `ido-completing-read' to \\[find-file] a recent file"
  (interactive)
  (if (find-file (ido-completing-read "Find recent file: " recentf-list))
      (message "Opening file...")
    (message "Aborting")))



;; (nav-disable-overeager-window-splitting)


(or (server-running-p)
    (server-start))

;; define function to shutdown emacs server instance
(defun server-shutdown ()
  "Save buffers, Quit, and Shutdown (kill) server"
  (interactive)
  (save-some-buffers)
  (kill-emacs)
  )



(setq linum-format "%4d  ")
(global-linum-mode 1)

(defun nolinum ()
  (global-linum-mode 0)
  )
(add-hook 'org-mode-hook 'nolinum)


(setq ical-pull-list `("https://www.google.com/calendar/ical/s1ilvt2buhj2adrg7363t4k77g%40group.calendar.google.com/private-8ed0f1ebe7b7fcce8ba154c6d823d71c/basic.ics"))
;; ;;--------------------------  Backup
(setq backup-directory-alist `(("." . "~/.saves")))
(setq delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)


(find-file "~/Orgfiles/org-files/master.org")
;; use smart line
(setq sml/no-confirm-load-theme t)
(add-hook 'after-init-hook 'display-time)  
(setq display-time-24hr-format t)
(setq display-time-day-and-date t)


;; python-setup
;; ;;----------------------- PYTHON
                                        ; http://www.masteringemacs.org/articles/2013/03/11/whats-new-emacs-24-3
(add-hook 'python-mode-hook
          (lambda ()
            (setq indent-tabs-mode nil)
            (setq python-indent-guess-indent-offset 1)
            (setq python-indent-offset 4)))


                                        ; use python-mode instead downloaded from package manager
;; (autoload 'python-mode "python-mode" "Python Mode." t)
(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))
;; (add-to-list 'interpreter-mode-alist '("python" . python-mode))

(package-initialize)
(elpy-enable)







(defun aj-toggle-fold ()
  "Toggle fold all lines larger than indentation on current line"
  (interactive)
  (let ((col 1))
    (save-excursion
      (back-to-indentation)
      (setq col (+ 1 (current-column)))
      (set-selective-display
       (if selective-display nil (or col 1))))))
(global-set-key [(M C i)] 'aj-toggle-fold)


(setq ipython-command "/usr/local/bin/ipython")


(setq
 python-shell-interpreter "/usr/local/bin/ipython"
 python-shell-interpreter-args ""
 python-shell-prompt-regexp "In \\[[0-9]+\\]: "
 python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: "
 python-shell-completion-setup-code
 "from IPython.core.completerlib import module_completion"
 python-shell-completion-module-string-code
 "';'.join(module_completion('''%s'''))\n"
 python-shell-completion-string-code
 "';'.join(get_ipython().Completer.all_completions('''%s'''))\n")

(setq py-python-command-args '( "--colors=Linux"))
(setq ansi-color-for-comint-mode t)
(setq  flyspell-make t)

;; ;; KEYBINDINGS
(global-set-key "\C-cg" 'goto-line)
                                        ;(global-set-key [f4] 'speedbar-get-focus)
                                        ;(global-set-key [f4] 'speedbar-get-focus)
(global-set-key (kbd "\C-cm") 'magit-status)   ;; ...git mode
(global-set-key (kbd "<f4>") 'nav-toggle)
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
(defun toggle-fullscreen (&optional f)
  (interactive)
  (let ((current-value (frame-parameter nil 'fullscreen)))
    (set-frame-parameter nil 'fullscreen
                         (if (equal 'fullboth current-value)
                             (if (boundp 'old-fullscreen) old-fullscreen nil)
                           (progn (setq old-fullscreen current-value)
                                  'fullboth)))))

(global-set-key [f11] 'toggle-fullscreen)

(when (locate-library "windmove")
  (global-set-key (kbd "C-x <left>") 'windmove-left)
  (global-set-key (kbd "C-x <right>") 'windmove-right)
  (global-set-key (kbd "C-x <up>") 'windmove-up)
  (global-set-key (kbd "C-x <down>") 'windmove-down)
  )

(global-undo-tree-mode)



(add-hook 'ido-setup-hook
          (lambda ()
            ;; Go straight home
            (define-key ido-file-completion-map
              (kbd "~")
              (lambda ()
                (interactive)
                (if (looking-back "/")
                    (insert "~/")
                  (call-interactively 'self-insert-command))))))



;; (global-set-key (kbd "M-j")
;;                 (lambda ()
;;                   (interactive)
;;                   (join-line -1)))


;; Move more quickly
;; (global-set-key (kbd "C-S-n")
;;                 (lambda ()
;;                   (interactive)
;;                   (ignore-errors (next-line 5))))

;; (global-set-key (kbd "C-S-p")
;;                 (lambda ()
;;                   (interactive)
;;                   (ignore-errors (previous-line 5))))

;; (global-set-key (kbd "C-S-f")
;;                 (lambda ()
;;                   (interactive)
;;                   (ignore-errors (forward-char 5))))

;; (global-set-key (kbd "C-S-b")
;;                 (lambda ()
;;                   (interactive)
;;                   (ignore-errors (backward-char 5))))

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

(setq
 ebib-file-search-dirs '("~/LitDB/pdf/"))
(setq ebib-preload-bib-files
      (list "~/LitDB/ped.bib")
      )


;; ;; LATEX----------------------
(add-to-list 'load-path "~/.emacs.d/elpa/auctex-11.88")
(setq reftex-plug-into-AUCTeX t)
(setq  reftex-mode t)
(global-set-key (kbd "<f8>") 'ispell-word)
(global-set-key (kbd "C-<f8>") 'flyspell-mode)
(global-set-key (kbd "C-<f9>") 'reftex-mode)





;; (defun ac-cc-mode-setup ()
;;   (setq ac-clang-complete-executable "~/.emacs.d/clang-complete")
;;   (setq ac-sources '(ac-source-clang-async))
;;   (ac-clang-launch-completion-process)
;;   )

;; (defun my-ac-config ()
;;   (add-hook 'c-mode-common-hook 'ac-cc-mode-setup)
;;   (add-hook 'auto-complete-mode-hook 'ac-common-setup)
;;   (global-auto-complete-mode t))
;; (my-ac-config)
;;--------------------------

(setq ac-auto-start nil)
(setq ac-quick-help-delay 0.5)
(defun ac-cc-mode-setup ()
  (setq ac-clang-complete-executable "~/.emacs.d/clang-complete")
  (setq ac-sources '(ac-source-clang-async))
  (ac-clang-launch-completion-process)
  )
(defun ac-common-setup ()
  ())
(defun my-ac-config ()
  (add-hook 'c-mode-common-hook 'ac-cc-mode-setup)
  (add-hook 'auto-complete-mode-hook 'ac-common-setup)
  (global-auto-complete-mode t))
(my-ac-config)


;; auto-completion with company
;; http://tuhdo.github.io/c-ide.html#sec-3


;(add-hook 'after-init-hook 'global-company-mode)
;(setq company-backends (delete 'company-semantic company-backends))
;(define-key c-mode-map  [(tab)] 'company-complete)
;(define-key c++-mode-map  [(tab)] 'company-complete)
;(add-to-list 'company-backends 'company-c-headers)


;; (setenv "ESHELL" (expand-file-name "~/bin/eshell"))

;; C-d to kill buffer if process is dead.

;; (defun comint-delchar-or-eof-or-kill-buffer (arg)
;;   (interactive "p")
;;   (if (null (get-buffer-process (current-buffer)))
;;       (kill-buffer)
;;     (comint-delchar-or-maybe-eof arg)))

;; (add-hook 'shell-mode-hook
;;           (lambda ()
;;             (define-key shell-mode-map (kbd "C-d") 'comint-delchar-or-eof-or-kill-buffer)))



;;--------------------------------------- PAREN

(setq-default truncate-lines t) ;; will trucate lines if they are too long.
(transient-mark-mode t) ;; will highlight region between point and mark. 
(setq-default global-visual-line-mode t)
(setq show-paren-style 'parenthesis) ; highlight just brackets
                                        ;(setq show-paren-style 'expression) ; highlight entire bracket expression


;(load-file "~/.emacs.d/lisp/xcscope.el")


;; ;;------ cmake support
;;                                         ; Add cmake listfile names to the mode list.
(setq auto-mode-alist
      (append
       '(("CMakeLists\\.txt\\'" . cmake-mode))
       '(("\\.cmake\\'" . cmake-mode))
       auto-mode-alist))

(autoload 'cmake-mode "~/.emacs.d/lisp/cmake-mode.el" t)


;; ;;--------------------------------- ibuffer
(global-set-key (kbd "C-x C-b") 'ibuffer)
(autoload 'ibuffer "ibuffer" "List buffers." t)
(setq ibuffer-default-sorting-mode 'major-mode)
(setq ibuffer-expert t)
(setq ibuffer-show-empty-filter-groups nil)


;; ;; ---------------------------------------------------------- [ ido-mode ]

(ido-mode t) ;; for buffers and files  t
(setq ido-default-file-method 'selected-window)
(setq ido-default-buffer-method 'selected-window)
(setq ido-everywhere t)
(setq ido-use-filename-at-point 'guess)
(setq ido-create-new-buffer 'always)
(setq ido-auto-merge-work-directories-length -1)
(setq ido-file-extensions-order '(".cpp" ".c" ".org" ".txt" ".py" ".emacs" ".xml"))

(setq
 ido-save-directory-list-file "~/.emacs.d/cache/ido.last"
 ido-ignore-buffers ;; ignore these guys
 '("\\` " "^\*Mess" "^*log*" "^\*back" ".*Completion" "^\*ido" "^\*trace"
   "^\*compilation" "^\*GTAGS" "*.el" "*.last" "^session\.*" "^\*")
 ido-work-directory-list '("~/" "~/Desktop" "~/Documents" "~/Workspace")
 ido-case-fold  t                 ; be case-insensitive
 ido-enable-last-directory-history t ; remember last used dirs
 ido-max-work-directory-list 30   ; should be enough
 ido-max-work-file-list      50   ; remember many
 ido-use-filename-at-point nil    ; don't use filename at point (annoying)
 ido-use-url-at-point nil         ; don't use url at point (annoying)
 ido-enable-flex-matching nil     ; don't try to be too smart
 ido-max-prospects 8              ; don't spam my minibuffer
 ido-confirm-unique-completion t) ; wait for RET, even with unique completion

;; when using ido, the confirmation is rather annoying...
(setq confirm-nonexistent-file-or-buffer nil)

;; increase minibuffer size when ido completion is active
(add-hook 'ido-minibuffer-setup-hook 
          (function
           (lambda ()
             (make-local-variable 'resize-minibuffer-window-max-height)
             (setq resize-minibuffer-window-max-height 1))))


;; ------------------------------ TAGS

(setq tags-revert-without-query t)                                                                                                                                                                                                          
(global-set-key (kbd "<f7>") 'ctags-create-or-update-tags-table)


(defun my-semantic-hook ()
  (imenu-add-to-menubar "TAGS"))


;; ;;----------------------------- Ctags   How to use ctags in Emacs effectively blog.binchen.org
(defun my-project-name-contains-substring (REGEX)
  (let ((dir (if (buffer-file-name)
                 (file-name-directory (buffer-file-name))
               "")))
    (string-match-p REGEX dir)))

(defun my-create-tags-if-needed (SRC-DIR &optional FORCE)
  "return the full path of tags file"
  (let ((dir (file-name-as-directory (file-truename SRC-DIR)) )
        file)
    (setq file (concat dir "TAGS"))
    (when (or FORCE (not (file-exists-p file)))
      (message "Creating TAGS in %s ..." dir)
      (shell-command
       (format "ctags-exuberant --extra=+fq --exclude=_flymake --exclude=db --exclude=test --exclude=doc --exclude=log --exclude=Utest --exclude=.git --exclude=public -f %s -e -R %s" file dir))
      )
    file
    ))

(defvar my-tags-updated-time nil)

(defun my-update-tags ()
  (interactive)
  "check the tags in tags-table-list and re-create it"
  (dolist (tag tags-table-list)
    (my-create-tags-if-needed (file-name-directory tag) t)
    ))

(defun my-auto-update-tags-when-save ()
  (interactive)
  (cond
   ((not my-tags-updated-time)
    (setq my-tags-updated-time (current-time)))
   ((< (- (float-time (current-time)) (float-time my-tags-updated-time)) 300)
    ;; < 300 seconds
    ;; do nothing
    )
   (t
    (setq my-tags-updated-time (current-time))
    (my-update-tags)
    (message "updated tags after %d seconds." (- (float-time (current-time))  (float-time my-tags-updated-time)))
    )
   ))


(defun my-setup-develop-environment ()
  (when (my-project-name-contains-substring "chraibi")
    (cond
     ((my-project-name-contains-substring "Workspace/")
      ;; C++ project don't need html tags
      (setq tags-table-list (list
                             (my-create-tags-if-needed "~/Workspace/jpscore")
                             
                             (my-create-tags-if-needed "~/Workspace/jpsvis")
                             );list
            );setq
      )
     ) ;cond
    ) ;when
  ) ;fun

(add-hook 'after-save-hook 'my-auto-update-tags-when-save)
;; (add-hook 'js2-mode-hook 'my-setup-develop-environment)
;; (add-hook 'web-mode-hook 'my-setup-develop-environment)
(add-hook 'c++-mode-hook 'my-setup-develop-environment)
(add-hook 'c-mode-hook 'my-setup-develop-environment)

(global-set-key (kbd "M-.") 'find-tag)
(global-set-key (kbd "M-*") 'pop-tag-mark)

;;------------ irony
;; (add-hook 'c++-mode-hook 'irony-mode)
;; (add-hook 'c-mode-hook 'irony-mode)
;; (add-hook 'objc-mode-hook 'irony-mode)

;; ;; replace the `completion-at-point' and `complete-symbol' bindings in
;; ;; irony-mode's buffers by irony-mode's function
;; (defun my-irony-mode-hook ()
;;   (define-key irony-mode-map [remap completion-at-point]
;;     'irony-completion-at-point-async)
;;   (define-key irony-mode-map [remap complete-symbol]
;;     'irony-completion-at-point-async))
;; (add-hook 'irony-mode-hook 'my-irony-mode-hook)
;; (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)

;; -----------------------------------------------------------------------
;; auto-mode-alist
;; -----------------------------------------------------------------------


;; ;; dired
;; (setq dired-listing-switches "-Al --si --time-style long-iso")
;; (setq dired-dwim-target t)



;; (defun ergoemacs-open-in-external-app ()
;;   "Open the current file or dired marked files in external app."
;;   (interactive)
;;   (let ( doIt
;;          (myFileList
;;           (cond
;;            ((string-equal major-mode "dired-mode") (dired-get-marked-files))
;;            (t (list (buffer-file-name))) ) ) )

;;     (setq doIt (if (<= (length myFileList) 5)
;;                    t
;;                  (y-or-n-p "Open more than 5 files?") ) )

;;     (when doIt
;;       (cond
;;        ((string-equal system-type "windows-nt")
;;         (mapc (lambda (fPath) (w32-shell-execute "open" (replace-regexp-in-string "/" "\\" fPath t t)) ) myFileList)
;;         )
;;        ((string-equal system-type "darwin")
;;         (mapc (lambda (fPath) (shell-command (format "open \"%s\"" fPath)) )  myFileList) )
;;        ((string-equal system-type "gnu/linux")
;;         (mapc (lambda (fPath) (let ((process-connection-type nil)) (start-process "" nil "xdg-open" fPath)) ) myFileList) ) ) ) ) )




;; (defun xah-open-in-external-app (&optional file)
;;   "Open the current file or dired marked files in external app.

;; The app is chosen from your OS's preference."
;;   (interactive)
;;   (let ( doIt
;;          (myFileList
;;           (cond
;;            ((string-equal major-mode "dired-mode") (dired-get-marked-files))
;;            ((not file) (list (buffer-file-name)))
;;            (file (list file)))))

;;     (setq doIt (if (<= (length myFileList) 5)
;;                    t
;;                  (y-or-n-p "Open more than 5 files? ") ) )
    
;;     (when doIt
;;       (cond
;;        ((string-equal system-type "windows-nt")
;;         (mapc (lambda (fPath) (w32-shell-execute "open" (replace-regexp-in-string "/" "\\" fPath t t)) ) myFileList))
;;        ((string-equal system-type "darwin")
;;         (mapc (lambda (fPath) (shell-command (format "open \"%s\"" fPath)) )  myFileList) )
;;        ((string-equal system-type "gnu/linux")
;;         (mapc (lambda (fPath) (let ((process-connection-type nil)) (start-process "" nil "xdg-open" fPath)) ) myFileList) ) ) ) ) )



;; https://github.com/magnars/multiple-cursors.el

(global-set-key (kbd "C-c z") 'mc/edit-lines)
(global-set-key (kbd "C-c n") 'mc/mark-next-like-this)
(global-set-key (kbd "C-c p") 'mc/mark-previous-like-this)


(defun org-find-dangling-clock ()
  "Find a dangling clock entry in an org-mode buffer"
  (interactive)
  (re-search-forward "CLOCK: \\[[^]]*\\] *$")
  )


(defadvice LaTeX-fill-region-as-paragraph (around LaTeX-sentence-filling)
  "Start each sentence on a new line."
  (let ((from (ad-get-arg 0))
        (to-marker (set-marker (make-marker) (ad-get-arg 1)))
        tmp-end)
    (while (< from (marker-position to-marker))
      (forward-sentence)
      ;; might have gone beyond to-marker --- use whichever is smaller:
      (ad-set-arg 1 (setq tmp-end (min (point) (marker-position to-marker))))
      ad-do-it
      (ad-set-arg 0 (setq from (point)))
      (unless (or
               (bolp)
               (looking-at "\\s *$"))
        (LaTeX-newline)))
    (set-marker to-marker nil)))
(ad-activate 'LaTeX-fill-region-as-paragraph)

;; (global-set-key (kbd "C-c d") 'osx-dictionary-search-pointer)