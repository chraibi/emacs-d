;; Make startup faster by reducing the frequency of garbage
;; collection.  The default is 0.8MB.  Measured in bytes.
(setq gc-cons-threshold (* 50 1000 1000))
;;; Code:
;; Turn off mouse interface early in startup to avoid momentary display
(add-to-list 'load-path (expand-file-name "~/.emacs.d/lisp"))
(add-to-list 'load-path "~/.emacs.d/auto-complete-clang/")
(add-to-list 'load-path "~/.emacs.d/lisp/benchmark-init-el")
(add-to-list 'load-path "/usr/local/share/emacs/site-lisp/doxymacs")
(require 'benchmark-init-loaddefs)
(benchmark-init/activate)


(setenv "ESHELL" (expand-file-name "~/bin/eshell"))
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

(setq is-mac (equal system-type 'darwin))
(if (equal system-type 'darwin)
    (setq locate-command "mdfind")
  (global-set-key (kbd "M-s") 'locate)
 )


(defvar *emacs-load-start* (current-time))
;; My location for external packages.


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


;; Always load the newer .el or .elc file.
(setq load-prefer-newer t)

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
		     ;sx
		     ;linum
                     ;hlinum
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
                     ;uniquify		;
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
(setq abbrev-file-name             ;; tell emacs where to read abbrev
        "~/.emacs.d/abbrev_defs")    ;; definitions from...

(setq save-abbrevs t)              ;; save abbrevs when files are saved
                                     ;; you will be asked before the abbreviations are saved

(cond (( >= emacs-major-version 24) 
       (message "load solarized-light") ;zenburn
       ;; (message "load zenburn") ;zenburn
       ;; (load-theme 'solarized-light t)
       ;;(load-theme 'zenburn t)
       (load-theme 'solarized-light t)
       (if (member "Monaco" (font-family-list))
           (set-face-attribute
            'default nil :font "Monaco 18")
         (message "set Monaco")
         )

       )
      );Version 24


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

(message "load packages")
;; -------------------- require
(require 'cl)
;; (autoload 'yasnippet "yasnippet" "load yasnippet" t)
;(require 'paren)
(require 'highlight-indentation) ;; visual guides for indentation
(require 'autopair)
;(require 'server)
;(require 'recentf)
(require 'setup-electric)
(autoload 'setup-magit "setup-magit" "load magit")

(use-package setup-org-mode
  ;; Loads after 2 second of idle time.
  :defer 3)

;(global-auto-revert-mode t)
(use-package autorevert
  ;; Loads after 2 second of idle time.
  :defer 2)


;(require 'setup-org-mode)

(use-package setup-helm
  ;; Loads after 2 second of idle time.
  :defer 2)

(use-package recentf
  ;; Loads after 2 second of idle time.
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
  ;; Loads after 2 second of idle time.
  :defer 2
  
  :config
  (unless (server-running-p)
  (server-start))
)


(use-package multiple-cursors
  ;; Loads after 2 second of idle time.
  :defer 5)
(use-package paren
  ;; Loads after 2 second of idle time.
  :defer 1)

;(require 'multiple-cursors)

(require 'my-core-settings)

(defun python-mode-setup ()
  (message "Custom python hook run")
  (load-library "setup-python"))
(add-hook 'python-mode-hook 'python-mode-setup)

;; install your hook so it is called when python-mode is invoked
(add-hook 'python-mode-hook 'python-mode-setup)


(defun cc-mode-setup ()
  (message "Custom cc hook run")
  (load-library "setup-cc"))
(add-hook 'c++-mode 'cc-mode-setup)

(require 'setup-ido)

(use-package setup-tex
  ;; Loads after 2 second of idle time.
  :defer 3)

;(require 'setup-tex)


;(require 'yasnippet)
;; (yas/initialize)
;; (yas/load-directory "~/.emacs.d/snippets")

;; (require 'semantic/ia)
;(require 'xcscope)
;(require 'org-inlinetask)
;;(require 'flycheck)
;; (autoload 'flycheck "flycheck" "load flycheck" t)
;; (require 'flymake-setup)
;; (require 'setup-hlinum)
;; (require 'flymake-cursor)
;; uniquify: unique buffer names
;; (require 'uniquify) ;; make buffer names more unique
;; (require 'org-alert)
;; (setq alert-default-style 'libnotify)
;; (require 'setup-mu4e)
;; (require 'ob-plantuml)
;; (require 'linum)
;; (require 'nav)
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


;;store link to message if in header view, not to header query
;(setq org-mu4e-link-query-in-headers-mode nil)

(message "setups loaded")
;(cscope-setup)
;;setup-electric
;;(package-initialize)
;----------------------------
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

 (global-set-key (kbd "C-c ;") 'comment-or-uncomment-region-or-line)


;; after copy Ctrl+c in X11 apps, you can paste by `yank' in emacs
(setq x-select-enable-clipboard t)

;; after mouse selection in X11, you can paste by `yank' in emacs
(setq x-select-enable-primary t)



(global-set-key "\C-x\C-k" 'kill-region)
(global-set-key "\C-c\C-k" 'kill-region)
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

;; define function to shutdown emacs server instance
(defun server-shutdown ()
  "Save buffers, Quit, and Shutdown (kill) server"
  (interactive)
  (save-some-buffers)
  (kill-emacs)
  )

;;get rid of `find-file-read-only' and replace it with something
;; ;; more useful.
(global-set-key (kbd "C-x C-r") 'ido-recentf-open)





;; (nav-disable-overeager-window-splitting)

;; https://github.com/magnars/.emacs.d/blob/master/init.el
(require 'fill-column-indicator)
(setq fci-rule-color "white")
;; Browse kill ring
(require 'browse-kill-ring)
(setq browse-kill-ring-quit-action 'save-and-restore)
(setq browse-kill-ring-highlight-current-entry t)
(global-set-key "\C-cy" 'browse-kill-ring)


;(global-linum-mode 1)

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


;; ;;------ cmake support
;; Add cmake listfile names to the mode list.
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




;; ;; dired
(setq dired-dwim-target t)
(require 'dired-details)
(setq-default dired-details-hidden-string "--- ")
(dired-details-install)
;; Move files between split panes
(setq dired-dwim-target t)

;; https://github.com/magnars/multiple-cursors.el
(global-set-key (kbd "C-c z") 'mc/edit-lines)
(global-set-key (kbd "C-c i") 'mc/insert-numbers)
(global-set-key (kbd "C-c C-n") 'mc/mark-next-like-this)
(global-set-key (kbd "C-c C-p") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-s") 'mc/mark-all-like-this)


;; Make gc pauses faster by decreasing the threshold.
(setq gc-cons-threshold (* 2 1000 1000))


;; Use a hook so the message doesn't get clobbered by other messages.
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Emacs ready in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)))


; profile:
;    emacs -Q -l ~/.emacs.d/lisp/profile-dotemacs.el --eval "(setq profile-dotemacs-file (setq load-file-name \"$(abspath init.el)\"))" -f profile-dotemacs
