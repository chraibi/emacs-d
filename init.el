;;; Code:
;; Turn off mouse interface early in startup to avoid momentary display
(add-to-list 'load-path (expand-file-name "~/.emacs.d/lisp"))
(add-to-list 'load-path "~/.emacs.d/auto-complete-clang/")
(add-to-list 'load-path "~/.emacs.d/lisp/benchmark-init-el")
(add-to-list 'load-path "/usr/local/share/emacs/site-lisp/doxymacs")
(require 'benchmark-init-loaddefs)
(benchmark-init/activate)



(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(setq inhibit-splash-screen t);; Disable splash screen
(setq inhibit-startup-message t)
(setq inhibit-startup-echo-area-message t)
(setq inhibit-startup-screen t)
(setq initial-scratch-message nil)
(setenv "ESHELL" (expand-file-name "~/bin/eshell"))
(setq custom-safe-themes t)
;; use smart line
(setq sml/no-confirm-load-theme t)
(add-hook 'after-init-hook 'display-time)
(setq display-time-24hr-format t)
(setq display-time-day-and-date t)
(setq epg-gpg-program "/usr/local/bin/gpg")
;; frame font
;; Setting English Font

(when (string= system-type "darwin")
  (setq dired-use-ls-dired nil))
  
;; ;;--------------------------  Backup
(setq backup-directory-alist `(("." . "~/.saves")))
(setq delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)

(require 'ansi-color)
(defun my/ansi-colorize-buffer ()
  (let ((buffer-read-only nil))
    (ansi-color-apply-on-region (point-min) (point-max))))
(add-hook 'compilation-filter-hook 'my/ansi-colorize-buffer)

;; (find-file "~/Dropbox/Orgfiles/org-files/master.org") ;

(setq user-full-name "M. Chraibi")
(setq user-mail-address "m.chraibi@gmail.com")
(set-default 'cursor-type 'bar)
(setq cursor-type 'bar)
(blink-cursor-mode 1)
(setq-default cursor-type '(hbar . 1))


(setq is-mac (equal system-type 'darwin))

(if (equal system-type 'darwin)
    (setq locate-command "mdfind")
  )

;; Use Emacs terminfo, not system terminfo
(setq system-uses-terminfo nil)

(defvar *emacs-load-start* (current-time))
;; My location for external packages.

(global-set-key "\C-z" 'nil)
;; ;;; Make all yes-or-no questions as y-or-n
(fset 'yes-or-no-p 'y-or-n-p)

;; (getenv "PATH")
;; (setenv "PATH"
;;         (concat
;;          "/usr/texbin" ":"
;;          "/usr/local/bin/" ":"
;;          (getenv "PATH")))



(global-set-key (kbd "M-2") #'er/expand-region)

(setq preview-gs-options '("-q" "-dNOSAFER" "-dNOPAUSE" "-DNOPLATFONTS" "-dPrinted" "-dTextAlphaBits=4" "-dGraphicsAlphaBits=4"))

(setq py-install-directory "~/.emacs.d/lisp/pdee-master")
(add-to-list 'load-path py-install-directory)
(setq display-battery-mode t) (display-battery-mode 1) ;; will make the display of date and time persistent.

;; (transient-mark-mode 1)
 (global-visual-line-mode 1) ; 1 for on, 0 for off.

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

(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

;; (getenv "PATH")
;;  (setenv "PATH"
;;          (concat
;;           "/usr/texbin" ":"
;;           (getenv "PATH")
;;           )
;;          )


;; ;; (setenv "PATH" (concat (getenv "PATH") ":/path/to/gs/folder"))

;; (add-to-list 'exec-path "/usr/local/bin")


;; Setup packages
;(require 'setup-package)
; list the packages
(setq package-list '(
 ;            dash
		     cl
		     sml-modeline
		     zenburn-theme
		     highlight-indentation
                     ag
		     auctex-latexmk
		     org-journal
                     org-fstree
                     remember
                     color-theme
                     auctex
		     yasnippet
                     expand-region
		     multiple-cursors
		     dired+
		     sx
		     linum
                     hlinum
		     server
		     nav
		     recentf
                     fill-column-indicator
                     browse-kill-ring
		     ;mode-mapping
		     semantic
		     ido
                     ido-vertical-mode
                     magit
		     xcscope
		     cmake-project
		     cpputils-cmake
		     paren ;; will highlight matching parentheses next to cursor.
		     auto-complete
		     auto-complete-clang-async
		     auto-complete-clang
		     company	;
                     ;; irony ; needed by company-irony
		     autopair ;; to enable in all buffers
		     flymake-cursor
		     python
		     python-mode
		     ipython
		     elpy
                     flycheck
		     ;;ob-plantuml
                                        ; uniquify		;
                     epl      ; needed for projectile
                     async    ; needed for hlem
                     pkg-info ; needed for projectile
		     projectile
		     helm-projectile
		     undo-tree
                     exec-path-from-shell
                     ;; gitlab
		     )
      )



; activate all the packages (in particular autoloads)

(setq package-enable-at-startup nil) (package-initialize)
(message "packages initialized")
; fetch the list of packages available
(unless package-archive-contents
  (package-refresh-contents))
(message "fetch packages")
; install the missing packages
(dolist (package package-list)
  (message ">> load %s" package)
  (unless  (package-installed-p package)
    (package-install package)
    )
)
(message "done with loading pkgs")
;; Setup environment variables from the user's shell.
;; (when is-mac
;;   (require 'exec-path-from-shell)
;;   (exec-path-from-shell-initialize))

(setq abbrev-file-name             ;; tell emacs where to read abbrev
        "~/.emacs.d/abbrev_defs")    ;; definitions from...

(setq save-abbrevs t)              ;; save abbrevs when files are saved
                                     ;; you will be asked before the abbreviations are saved

(cond (( >= emacs-major-version 24)
       ;; (message "load solarized-light") ;zenburn
       (message "load zenburn") ;zenburn
       ;; (load-theme 'solarized-light t)
       (load-theme 'zenburn t)
       (if (member "Monaco" (font-family-list))
           (set-face-attribute
            'default nil :font "Monaco 18")
         (message "set Monaco")
         )

       )
      );Version 24

;(defun on-after-init ()
;  (unless (display-graphic-p (selected-frame))
;    (set-face-background 'default "unspecified-bg" (selected-frame))))

;; (add-hook 'window-setup-hook 'on-after-init)

(require 'helm-git-grep) ;; Not necessary if installed by package.el
(global-set-key (kbd "C-c n") 'helm-git-grep)
;; Invoke `helm-git-grep' from isearch.
(define-key isearch-mode-map (kbd "C-c n") 'helm-git-grep-from-isearch)
;; Invoke `helm-git-grep' from other helm.
(eval-after-load 'helm
  '(define-key helm-map (kbd "C-c n") 'helm-git-grep-from-helm))

;; Keep emacs Custom-settings in separate file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)
;; Set up appearance early

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ag-executable "/usr/local/bin/ag")
 '(c-basic-offset 6)
 '(c-default-style (quote ((c-mode . "stroustrup") (c++-mode . "stroustrup"))))
 '(ecb-options-version "2.40")
 '(python-indent-guess-indent-offset nil)
 '(sml/battery-format " [ %p ] ")
 '(sml/show-client t)
 )
;; hide modes
;(diminish 'projectile-mode)
;(diminish 'undo-tree-mode)
(diminish 'abbrev-mode)
;(diminish 'auto-fill-function-mode)
;(diminish 'pair-mode)
;; (add-hook 'after-init-hook 'sml/setup) ;todo
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(show-paren-match ((((class color) (background light)) (:background "blue")))))

(set-face-attribute 'region nil :background "#ff7f00" :foreground "#000000")

;; ;; this variable
;;(setq auto-indent-on-visit-file t) ;; If you want auto-indent on for files
;; (require 'auto-indent-mode)
;; (auto-indent-global-mode)


(message "load packages")
;; -------------------- require
(require 'cl)
;; (autoload 'yasnippet "yasnippet" "load yasnippet" t)
(require 'yasnippet)
;;(require 'flycheck)
(yas/initialize)
(yas/load-directory "~/.emacs.d/snippets")

;; (autoload 'flycheck "flycheck" "load flycheck" t)
;(autoload 'multiple-cursors "multiple-cursors" "load multiple-cursors" t)
(require 'multiple-cursors)
;; (require 'semantic/ia)
(require 'xcscope)
(require 'paren)
(require 'highlight-indentation) ;; visual guides for indentation
(require 'autopair)
;; (require 'ob-plantuml)
(require 'linum)
(require 'server)
(require 'nav)
(require 'recentf)
;;(require 'flymake-cursor)
;; uniquify: unique buffer names
                                        ;(require 'uniquify) ;; make buffer names more unique

(require 'org-alert)
(setq alert-default-style 'libnotify)
;----------------  load setups ----------------------------
(message "load my setups")
(require 'setup-electric)		;
;(require 'setup-mu4e)		;
(autoload 'setup-magit "setup-magit" "load magit")



(require 'setup-org-mode)
;; (autoload 'setup-helm "setup-helm" "load helm")
(require 'setup-helm)
;;(require 'flymake-setup)
(require 'setup-hlinum)
(require 'setup-python)
(require 'setup-cc)
(require 'setup-ido)
(require 'org-inlinetask)

;; overwrite selected text
(delete-selection-mode t)

;;(require 'git-gutter)

;; If you enable global minor mode
;; (global-git-gutter-mode t)
;; (custom-set-variables
;;  '(git-gutter:update-interval 2))
;; (custom-set-variables
;;  '(git-gutter:window-width 2)
;;   '(git-gutter:modified-sign "☁")
;;   '(git-gutter:added-sign "☀")
;;   '(git-gutter:deleted-sign "☂"))

;; (set-face-background 'git-gutter:modified "blue") ;; background color
;; (set-face-foreground 'git-gutter:added "green")
;; (set-face-foreground 'git-gutter:deleted "red")


;;(require 'thingatpt)
;; When popping the mark, continue popping until the cursor
;; actually moves
(defadvice pop-to-mark-command (around ensure-new-position activate)
  (let ((p (point)))
    (dotimes (i 10)
      (when (= p (point)) ad-do-it))))

(setq set-mark-command-repeat-pop t)


;; http://endlessparentheses.com/new-in-emacs-25-1-have-prettify-symbols-mode-reveal-the-symbol-at-point.html
(setq prettify-symbols-unprettify-at-point 'right-edge)


;;store link to message if in header view, not to header query
;(setq org-mu4e-link-query-in-headers-mode nil)

(message "setups loaded")
(cscope-setup)
;;setup-electric
(package-initialize)
;----------------------------
(show-paren-mode t) ;; will highlight matching parentheses next to cursor.
(autopair-global-mode) ;; to enable in all buffers
;;-----------------------------
(setq  electric-pair-mode t)

(yas-global-mode 1)


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

(global-set-key "\C-c\C-c" 'comment-region)

(defun comment-region-lines (beg end &optional arg)
  "Like `comment-region', but comment/uncomment whole lines."
  (interactive "*r\nP")
  (if (> beg end) (let (mid) (setq mid beg beg end end mid)))
  (let ((bol  (save-excursion (goto-char beg) (line-beginning-position)))
        (eol  (save-excursion (goto-char end) (line-end-position))))
    (comment-region bol end arg)))

 (global-set-key (kbd "C-c ;") 'comment-or-uncomment-region-or-line)

;; (global-set-key (kbd "M-;") 'comment-region-lines)


;; MC
;; (add-hook 'after-init-hook #'global-flycheck-mode) ;

;; (autoload 'flyspell-mode "flyspell" "On-the-fly spelling checker." t)
;; (modify-frame-parameters nil '((wait-for-wm . nil)))

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
(unless (server-running-p)
  (server-start))

;; define function to shutdown emacs server instance
(defun server-shutdown ()
  "Save buffers, Quit, and Shutdown (kill) server"
  (interactive)
  (save-some-buffers)
  (kill-emacs)
  )



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




;; (nav-disable-overeager-window-splitting)

;; https://github.com/magnars/.emacs.d/blob/master/init.el
(require 'fill-column-indicator)
(setq fci-rule-color "white")
;; Browse kill ring
(require 'browse-kill-ring)
(setq browse-kill-ring-quit-action 'save-and-restore)
(setq browse-kill-ring-highlight-current-entry t)
(global-set-key "\C-cy" 'browse-kill-ring)


   (global-linum-mode 1)

(defun nolinum ()
  (global-linum-mode 0)
  )
(add-hook 'org-mode-hook 'nolinum)


(setq ical-pull-list `("https://www.google.com/calendar/ical/s1ilvt2buhj2adrg7363t4k77g%40group.calendar.google.com/private-8ed0f1ebe7b7fcce8ba154c6d823d71c/basic.ics"))


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

(setq ansi-color-for-comint-mode t)
;; (setq  flyspell-make t)

;(setq flyspell-mode 0)

;(add-hook 'text-mode-hook 'flyspell-mode) ;
;(add-hook 'prog-mode-hook 'flyspell-prog-mode)

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

;;-----------------------



;;---------
;; AucTeX
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master nil)
(add-hook 'LaTeX-mode-hook 'visual-line-mode)
;(add-hook 'LaTeX-mode-hook 'flyspell-mode)
(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(setq reftex-plug-into-AUCTeX t)
(setq TeX-PDF-mode t)
(add-hook 'LaTeX-mode-hook
          (lambda () (local-set-key (kbd "<S-s-mouse-1>") #'TeX-view))
          )
;; Use Skim as viewer, enable source <-> PDF sync
;; make latexmk available via C-c C-c
;; Note: SyncTeX is setup via ~/.latexmkrc (see below)
(add-hook 'LaTeX-mode-hook (lambda ()
  (push
    '("latexmk" "latexmk -pdf %s" TeX-run-TeX nil t
      :help "Run latexmk on file")
    TeX-command-list)))
(add-hook 'TeX-mode-hook '(lambda () (setq TeX-command-default "latexmk")))

;; use Skim as default pdf viewer
;; Skim's displayline is used for forward search (from .tex to .pdf)
;; option -b highlights the current line; option -g opens Skim in the background
(setq TeX-view-program-selection '((output-pdf "PDF Viewer")))
(setq TeX-view-program-list
     '(("PDF Viewer" "/Applications/Skim.app/Contents/SharedSupport/displayline -b -g %n %o %b")))
;;---------


;;--------------------------------------- PAREN

(setq-default truncate-lines t) ;; will trucate lines if they are too long.
(transient-mark-mode t) ;; will highlight region between point and mark.
(setq-default global-visual-line-mode t)
(setq show-paren-style 'parenthesis) ; highlight just brackets
                                        ;(setq show-paren-style 'expression) ; highlight entire bracket expression

(autoload 'markdown-mode "markdown-mode"
   "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

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

;;--------------------- highlight line
;; https://stackoverflow.com/questions/2718189/emacshighlight-the-current-line-by-underline-it
(global-hl-line-mode 1)
(set-face-background 'highlight "#222")
(set-face-foreground 'highlight nil)
(set-face-underline-p 'highlight t)

;; ------------------------------ TAGS

;; (setq tags-revert-without-query t)
;; (global-set-key (kbd "<f7>") 'ctags-create-or-update-tags-table)


;; (defun my-semantic-hook ()
;;   (imenu-add-to-menubar "TAGS"))


;; ;;----------------------------- Ctags   How to use ctags in Emacs effectively blog.binchen.org
;; (defun my-project-name-contains-substring (REGEX)
;;   (let ((dir (if (buffer-file-name)
;;                  (file-name-directory (buffer-file-name))
;;                "")))
;;     (string-match-p REGEX dir)))

;; (defun my-create-tags-if-needed (SRC-DIR &optional FORCE)
;;   "return the full path of tags file"
;;   (let ((dir (file-name-as-directory (file-truename SRC-DIR)) )
;;         file)
;;     (setq file (concat dir "TAGS"))
;;     (when (or FORCE (not (file-exists-p file)))
;;       (message "Creating TAGS in %s ..." dir)
;;       (shell-command
;;        (format "ctags-exuberant --extra=+fq --exclude=_flymake --exclude=db --exclude=test --exclude=doc --exclude=log --exclude=Utest --exclude=.git --exclude=public -f %s -e -R %s" file dir))
;;       )
;;     file
;;     ))

;; (defvar my-tags-updated-time nil)

;; (defun my-update-tags ()
;;   (interactive)
;;   "check the tags in tags-table-list and re-create it"
;;   (dolist (tag tags-table-list)
;;     (my-create-tags-if-needed (file-name-directory tag) t)
;;     ))

;; (defun my-auto-update-tags-when-save ()
;;   (interactive)
;;   (cond
;;    ((not my-tags-updated-time)
;;     (setq my-tags-updated-time (current-time)))
;;    ((< (- (float-time (current-time)) (float-time my-tags-updated-time)) 300)
;;     ;; < 300 seconds
;;     ;; do nothing
;;     )
;;    (t
;;     (setq my-tags-updated-time (current-time))
;;     (my-update-tags)
;;     (message "updated tags after %d seconds." (- (float-time (current-time))  (float-time my-tags-updated-time)))
;;     )
;;    ))


;; (defun my-setup-develop-environment ()
;;   (when (my-project-name-contains-substring "chraibi")
;;     (cond
;;      ((my-project-name-contains-substring "Workspace/")
;;       ;; C++ project don't need html tags
;;       (setq tags-table-list (list
;;                              (my-create-tags-if-needed "~/Workspace/jpscore")

;;                              (my-create-tags-if-needed "~/Workspace/jpsvis")
;;                              );list
;;             );setq
;;       )
;;      ) ;cond
;;     ) ;when
;;   ) ;fun

;; (add-hook 'after-save-hook 'my-auto-update-tags-when-save)
;; ;; (add-hook 'js2-mode-hook 'my-setup-develop-environment)
;; ;; (add-hook 'web-mode-hook 'my-setup-develop-environment)
;; (add-hook 'c++-mode-hook 'my-setup-develop-environment)
;; (add-hook 'c-mode-hook 'my-setup-develop-environment)

;; (global-set-key (kbd "M-.") 'find-tag)
;; (global-set-key (kbd "M-*") 'pop-tag-mark)


;; (add-hook 'c++-mode-hook 'irony-mode)
;; (add-hook 'c-mode-hook 'irony-mode)
;; (add-hook 'objc-mode-hook 'irony-mode)

;; replace the `completion-at-point' and `complete-symbol' bindings in
;; irony-mode's buffers by irony-mode's function
;; (defun my-irony-mode-hook ()
;;   (define-key irony-mode-map [remap completion-at-point]
;;     'irony-completion-at-point-async)
;;   (define-key irony-mode-map [remap complete-symbol]
;;     'irony-completion-at-point-async))
;; (add-hook 'irony-mode-hook 'my-irony-mode-hook)
;; (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)

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
(setq dired-dwim-target t)
(require 'dired-details)
(setq-default dired-details-hidden-string "--- ")
(dired-details-install)
;; Move files between split panes
(setq dired-dwim-target t)

;; (use-package dired-quick-sort
;;   :ensure t
;;   :config
;;   (dired-quick-sort-setup))


;; (require 'dired-quick-sort)
;; (dired-quick-sort-setup)


;; using ls-lisp with these settings gives case-insensitve
;; sorting on OS X
;; using ls-lisp with these settings gives case-insensitve
;; sorting on OS X
(require 'ls-lisp)
(setq dired-listing-switches "-alhG")
(setq ls-lisp-use-insert-directory-program nil)
(setq ls-lisp-ignore-case t)
(setq ls-lisp-use-string-collate nil)
;; customise the appearance of the listing
(setq ls-lisp-verbosity '(links uid))
(setq ls-lisp-format-time-list '("%b %e %H:%M" "%b %e  %Y"))
(setq ls-lisp-use-localized-time-format t)

;; (require 'ls-lisp)
;; (setq dired-listing-switches "-alhG")
;; (setq ls-lisp-use-insert-directory-program nil)
;; (setq ls-lisp-ignore-case t)
;; (setq ls-lisp-use-string-collate nil)
;; ;; customise the appearance of the listing
;; (setq ls-lisp-verbosity '(links uid))
;; (setq ls-lisp-format-time-list '("%b %e %H:%M" "%b %e  %Y"))
;; (setq ls-lisp-use-localized-time-format t)

;; (setq dired-listing-switches "-Al --si --time-style long-iso")



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
(global-set-key (kbd "C-c i") 'mc/insert-numbers)
(global-set-key (kbd "C-c C-n") 'mc/mark-next-like-this)
(global-set-key (kbd "C-c C-p") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-s") 'mc/mark-all-like-this)

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


;profile:
;    emacs -Q -l ~/.emacs.d/lisp/profile-dotemacs.el --eval "(setq profile-dotemacs-file (setq load-file-name \"$(abspath init.el)\"))" -f profile-dotemacs

(message "emacs loaded!")

;; (global-set-key (kbd "C-c d") 'osx-dictionary-search-pointer)
