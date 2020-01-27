;;; package ---- summary
;;; Commentary:

;; Make startup faster by reducing the frequency of garbage
;; collection.  The default is 0.8MB.  Measured in bytes.
(setq gc-cons-threshold (* 50 1000 1000))
;;; Code:
;; Turn off mouse interface early in startup to avoid momentary display
(add-to-list 'load-path (expand-file-name "~/.emacs.d/lisp"))
(add-to-list 'load-path "~/.emacs.d/auto-complete-clang/")
(add-to-list 'load-path "~/.emacs.d/lisp/benchmark-init-el")
;(add-to-list 'load-path "/usr/local/share/emacs/site-lisp/doxymacs")
;; (require 'benchmark-init-loaddefs)
;; (benchmark-init/activate)

;;("/usr/local/bin/osx-notifier" )
;(setenv "ESHELL" (expand-file-name "~/bin/eshell"))
(setq epg-gpg-program "/usr/local/bin/gpg")
;; frame font
;; Setting English Font
(setq multi-term-program "/bin/zsh")

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
  "Comments."
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

(getenv "PATH")
(setenv "PATH"
        (concat
         "/usr/texbin" ":"
         "/usr/local/bin/" ":"
         (getenv "PATH")))

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

; Setup packages
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
;                     org-fstree
                     remember
                     color-theme
                     auctex
                     yasnippet
                     expand-region
                     multiple-cursors
		     solarized-theme
		     beacon ; highlight cursor
		     projectile
		     doom-modeline
		     dired-details
		     diminish
;                     dired+
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
                     ;projectile
                     ;;helm-projectile
                     undo-tree
                     exec-path-from-shell
                     ;; gitlab
                     use-package
                     guide-key
                     ivy
                     swiper
                     counsel
                     company-lsp
                     lsp-ui
                     ace-window

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
;;       (message "load zenburn") ;zenburn
       (message "load solarized-light") ;zenburn
       (load-theme 'solarized-light t)
       ;;(load-theme 'zenburn t)
       ;; (load-theme 'zenburn t)
       (if (member "Monaco" (font-family-list))
           (set-face-attribute
            'default nil :font "Monaco 18")
         (message "set Monaco")
         )

       )
      );Version 24

(when (memq window-system '(mac ns))
  (message "init exec-path")
  (exec-path-from-shell-initialize))

;; Keep emacs Custom-settings in separate file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)
;; Set up appearance early

;; (require 'sb-imenu)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ag-executable "/usr/local/bin/ag")
 '(ecb-options-version "2.40")
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

(beacon-mode 1)
(setq beacon-push-mark 35)
(setq beacon-color "#e56911")

(message "load packages")

;;-------------------------
;; (require 'xah-fly-keys)
;; (xah-fly-keys-set-layout "qwerty") ; required if you use qwerty

;; possible layout values:
;; "qwerty"
;; "qwerty-abnt"
;; "qwertz"
;; "dvorak"
;; "programer-dvorak"
;; "colemak"
;; "colemak-mod-dh"
;; "workman"
;; dvorak is the default

;; (xah-fly-keys 1)

;;-------------------------
(use-package yasnippet
  ;; Loads after 1 second of idle time.
  :defer 1
  :config
  (yas/load-directory "~/.emacs.d/snippets")
)


(defun clangd-xref-manual ()
  (setq-local xref-backend-functions (list #'lsp--xref-backend))
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

                                        ;-------------------------------------------------------------------

(message "load packages")
(projectile-mode +1)
(projectile-global-mode +1)
;(setq projectile-completion-system 'ivy)

;; -------------------- require



(require 'cl)
;; (autoload 'yasnippet "yasnippet" "load yasnippet" t)
;(require 'paren)
(require 'highlight-indentation) ;; visual guides for indentation
(require 'autopair)
;(require 'server)
;(require 'recentf)
(require 'setup-electric)
;(autoload 'setup-magit "setup-magit" "load magit")

(use-package setup-magit
  ;; Loads after 2 second of idle time.
  :defer 3)

(use-package setup-org-mode
  ;; Loads after 2 second of idle time.
  :defer 3)

;(global-auto-revert-mode t)
(use-package autorevert
  ;; Loads after 2 second of idle time.
  :defer 2)

(use-package setup-helm
  ;; Loads after 2 second of idle time.
  :defer 2)

(use-package setup-ivy
  :defer 1
  )

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
(global-unset-key (kbd "M-<down-mouse-1>"))
(global-set-key (kbd "M-<mouse-1>") 'mc/add-cursor-on-click)

;(require 'multiple-cursors)

(require 'my-core-settings)



(require 'doom-modeline)
(doom-modeline-mode 1)
;; (setq doom-modeline-mu4e nil)
                                        ;----- moodline
;; (require 'mood-line)
;; (mood-line-activate)
(defun python-mode-setup ()
  (message "Custom python hook run")
  (load-library "setup-python"))
(add-hook 'python-mode-hook 'python-mode-setup)

;; (use-package python-cc
;;   ;; Loads after 2 second of idle time.
;;   :defer 2)


(use-package setup-cc
  ;; Loads after 2 second of idle time.
  :defer 2)

'(use-package setup-ido
  :defer 1)

(use-package setup-tex
  ;; Loads after 2 second of idle time.
  :defer 3)


(use-package winner
  :defer 2)

(use-package windmove
  :bind
  (("C-x <right>" . windmove-right)
   ("C-x <left>" . windmove-left)
   ("C-x <up>" . windmove-up)
   ("C-x <down>" . windmove-down)
   ))

(use-package guide-key
  :defer 3
  :diminish guide-key-mode
  :config
  (progn
  (setq guide-key/guide-key-sequence '("C-x r" "C-x 4" "C-c"))
  (guide-key-mode 1)))  ; Enable guide-key-mode

(use-package undo-tree
  :diminish undo-tree-mode
  :config
  (progn
    (global-undo-tree-mode)
    (setq undo-tree-visualizer-timestamps t)
    (setq undo-tree-visualizer-diff t)))

;; (require 'semantic/ia)
;(require 'xcscope)
;(require 'org-inlinetask)
;;(require 'flycheck)
;; (autoload 'flycheck "flycheck" "load flycheck" t)
;; (require 'flymake-setup)
;; (require 'setup-hlinum)
;; (require 'flymake-cursor)
;; uniquify: unique buffer names
(require 'uniquify) ;; make buffer names more unique
;; (use-package alert
;;   :defer t
;;   :config
;;   (alert-add-rule)
;;   (alert-log-notify nil)
;;   (alert--log-enable-logging)
;;   (setq alert-default-style (quote notifier))
;;   (setq alert-user-configuration (quote ((nil notifier nil))))
;;   ;(setq alert-default-style 'terminal-notifier)
;;   )
;; (require 'org-alert)
;; (setq org-alert-interval 3600)
;; (alert "This is an alert")
;; (message "HUHU")
;; You can adjust the severity for more important messages
;; (alert "This is an alert" :severity 'high)

;; Or decrease it for purely informative ones
;; (alert "This is an alert" :severity 'trivial)

;; Alerts can have optional titles.  Otherwise, the title is the
;; buffer-name of the (current-buffer) where the alert originated.
;; (alert "This is an alert" :title "My Alert")

;; Further, alerts can have categories.  This allows users to
;; selectively filter on them.
;; (alert "This is an alert" :title "My Alert" :category 'debug)

;; If a backend allows replacing alerts, you may pass an id
;; to your alert; then the next one with the same id will replace the
;; first one:
;; This avoids piling up lots of alerts, when only the last one is
;; relevant.
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

;; after copy Ctrl+c in X11 apps, you can paste by `yank' in emacs
(setq x-select-enable-clipboard t)

;; after mouse selection in X11, you can paste by `yank' in emacs
(setq x-select-enable-primary t)

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
  "Refresh imenu and jump to a place in the buffer using Ido."
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

(setq
 ebib-file-search-dirs '("~/LitDB/pdf/"))
(setq ebib-preload-bib-files
      (list "~/LitDB/ped.bib")
      )

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


(require 'cmake-mode)
;; ;;--------------------------------- ibuffer
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

;;-----
(make-variable-buffer-local 'compile-command)

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

(defun my-test-emacs ()
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
  (when (eq major-mode 'emacs-lisp-mode)
    (my-test-emacs)))

(add-hook 'after-save-hook 'auto-test-emacs)

(setq current-theme "dark")
(defconst light-theme 'solarized-light)
(defconst dark-theme 'zenburn)

;(global-unset-key (kbd "C-c ."))

;; Get the backtrace when uncaught errors occur.
;(setq debug-on-error t)               ; Will be unset at the end.

;; Hit `C-g' while it's frozen to get an Emacs Lisp backtrace.
;(setq debug-on-quit t)                ; Will be unset at the end.

;; will apply a dark theme if the room is dark, and a light theme if the room is
;; bright
;; (defun change-theme-for-lighting ()
;;   (let* ((current-light-sensor-reading
;;           (string-to-number
;;            (shell-command-to-string "~/.emacs.d/lmutracker"))))
;;     (if (< current-light-sensor-reading 100000)
;;         (when (not (string-equal current-theme "dark"))
;;           (load-theme dark-theme 1)
;;           (setq current-theme "dark"))
;;       (when (not (string-equal current-theme "light"))
;;         (load-theme light-theme 1)
;;         (setq current-theme "light")))))

;; ;; probably want to run this less frequently than every second
;; (run-with-timer 0 1 #'change-theme-for-lighting)

; profile:
;    emacs -Q -l ~/.emacs.d/lisp/profile-dotemacs.el --eval "(setq profile-dotemacs-file (setq load-file-name \"$(abspath init.el)\"))" -f profile-dotemacs
