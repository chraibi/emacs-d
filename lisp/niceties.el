;;; Package --- Some nice-to-have stuff
;;; Code:
;;; Commentary: server, async, vertico, embark, consult, savehist, marginalia, orderless

(message "Loading niceties")
; The async package provides asynchronous execution capabilities in Emacs Lisp, allowing certain tasks to be executed in the background, without blocking the Emacs user interface. ;

(save-place-mode 1)

; By default Emacs distinguishes between automatic and manual window switching. If you effect a window switch yourself with C-x b, it’s manual — and exempt from any display action rules you create yourself.
; https://www.masteringemacs.org/article/demystifying-emacs-window-manager
(setq switch-to-buffer-obey-display-actions t)

(use-package async
  :init
  (message "loading async")
  :ensure t)

;; ;; Alternative 1: Enable Jinx globally
;; (use-package jinx
;;   :hook (emacs-startup . global-jinx-mode)
;;   :bind (("M-$" . jinx-correct)
;;          ("C-M-$" . jinx-languages)))


(use-package vertico
  :init
  (message "loading vertico")
  :ensure t
  :config
  (vertico-mode)
  ;; Extensions
  (vertico-grid-mode)

  ;; Prefix the current candidate with “» ”. From
  ;; https://github.com/minad/vertico/wiki#prefix-current-candidate-with-arrow
  (advice-add #'vertico--format-candidate :around
                                          (lambda (orig cand prefix suffix index _start)
                                            (setq cand (funcall orig cand prefix suffix index _start))
                                            (concat
                                             (if (= vertico--index index)
                                                 (propertize "» " 'face 'vertico-current)
                                               "  ")
                                             cand)))
  )



;; Load and configure Marginalia
(use-package marginalia
  :ensure t
  :bind (:map minibuffer-local-map
              ("M-A" . marginalia-cycle))
  :custom
  (marginalia-max-relative-age 0)
  (marginalia-align 'right)
  :init
  (marginalia-mode))

;; -------------- EMBARK
; can display annotations such as the file type for a file name completion or the documentation string for a command completion. This additional context can be quite handy, especially when you encounter similarly named candidates or when you need more information about each option.

(use-package embark
  :init
  (message "loading embark")
  :ensure t
  :bind
  (("C-." . embark-act)         ;; pick some comfortable binding
   ("C-;" . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'
  :init
  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)
  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))



(use-package swiper
  :ensure t)
 
;; Consult users will also want the embark-consult package.
(use-package consult
  :ensure t
  :init
  (message "loading consult")
  :bind (("C-s" . swiper-isearch) ; you prefer swiper over consult-line
         ("C-r" . swiper-isearch-backward)
         ("C-M-l" . consult-imenu)
         ("C-M-j" . persp-switch-to-buffer*)
         :map minibuffer-local-map
         ("C-r" . consult-history))
  :custom
  (consult-project-root-function #'dw/get-project-root)
  (completion-in-region-function #'consult-completion-in-region))


(use-package savehist
  :init
    (message "loading savehist")
  (setq history-length 25)
  (setq savehist-additional-variables '(mark-ring
                                        global-mark-ring
                                        search-ring
                                        regexp-search-ring))
  (savehist-mode))


(use-package all-the-icons-completion
  :after (marginalia all-the-icons)
  :hook (marginalia-mode . all-the-icons-completion-marginalia-setup)
  :init
  (all-the-icons-completion-mode)
  (message "loading all-the-icons-completion ")
  )



; Optionally use the `orderless' completion style.
(use-package orderless
  :ensure t
  :init
  (message "loading orderless")
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (setq orderless-style-dispatchers '(+orderless-consult-dispatch orderless-affix-dispatch)
  ;;       orderless-component-separator #'orderless-escapable-split-on-space)
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))
(setq completion-styles '(substring orderless basic))
(defun flex-if-twiddle (pattern _index _total)
  (when (string-suffix-p "~" pattern)
    `(orderless-flex . ,(substring pattern 0 -1))))

(defun first-initialism (pattern index _total)
  (if (= index 0) 'orderless-initialism))

(defun without-if-bang (pattern _index _total)
  (cond
   ((equal "!" pattern)
    '(orderless-literal . ""))
   ((string-prefix-p "!" pattern)
    `(orderless-without-literal . ,(substring pattern 1)))))

(setq orderless-matching-styles '(orderless-regexp)
      orderless-style-dispatchers '(first-initialism
                                    flex-if-twiddle
                                    without-if-bang))


(use-package server
  :init
  (message "loading server!")
  :ensure t
  :defer 2
  :config
  (unless (server-running-p)
  (server-start))
)




;; Use-package configuration for pomm
(use-package pomm
  :ensure t
  ;; Load pomm when the commands `pomm` or `pomm-third-time` are invoked
  :commands (pomm pomm-third-time)
  ;; Optional: Add any additional configurations or hooks for pomm
  :config
  (setq pomm-interval 50) ;; Set the default pomodoro interval to 25 minutes
  (setq pomm-short-break 10) ;; Set the short break interval to 5 minutes
  (setq pomm-long-break 20) ;; Set the long break interval to 15 minutes
  (setq pomm-audio-enabled t) ;; Enable audio notifications
  (setq pomm-mode-line-mode t)
  (setq pomm-csv-history-file-timestamp-format "%F %T")
  (setq pomm-csv-history-file "~/.emacs.d/pomm_work.csv")
  (setq pomm-audio-tick-enabled nil)) ;; Enable ticking sound
;; Configure alert to use libnotify for notifications
(use-package alert
  :ensure t
  :init (setq alert-default-style 'osx-notifier)
  :config
  (setq alert-default-style 'libnotify))


(use-package reveal-in-osx-finder
  :ensure t
  :commands (reveal-in-osx-finder))

;; https://github.com/emacsmirror/expand-region?tab=readme-ov-file

(use-package expand-region
  :ensure t
  :bind ("C-=" . er/expand-region)
  :config
  (message "loading expand-region")
  )


;; (defun autocompile nil
;;   "compile itself if ~/.emacs"
;;   (interactive)
;;   (require 'bytecomp)
;;   (let ((dotemacs (file-truename user-init-file)))
;;     (if (string= (buffer-file-name) (file-chase-links dotemacs))
;;       (byte-compile-file dotemacs))))

;; (add-hook 'after-save-hook 'autocompile)


(message "Finished loading niceties")
(provide 'niceties)
;;; niceties.el ends here
