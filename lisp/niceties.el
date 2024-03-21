;;; Package --- Some nice-to-have stuff
;;; Code:
;;; Commentary: server, async, vertico, embark, consult, savehist, marginalia, orderless

(message "Loading niceties")
; The async package provides asynchronous execution capabilities in Emacs Lisp, allowing certain tasks to be executed in the background, without blocking the Emacs user interface. ;


(use-package async
  :init
  (message "loading async")
  :ensure t)

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

;; Consult users will also want the embark-consult package.
(use-package consult
  :init
  (message "loading consult")
  :ensure t
  :bind (("C-s" . swiper-isearch); consult-line I don't like the result
         ("C-r" . swiper-isearch-backward)
         ("C-M-l" . consult-imenu)
         ("C-M-j" . persp-switch-to-buffer*)
         :map minibuffer-local-map
         ("C-r" . consult-history))
  :custom
  (consult-project-root-function #'dw/get-project-root)
  (completion-in-region-function #'consult-completion-in-region)
  :config
  (consult-preview-mode))


(use-package savehist
  :init
  (message "loading savehist")
  (savehist-mode))

;; Enable rich annotations using the Marginalia package
(use-package marginalia
  :ensure t
  :init
  (message "loading marginalia")
  (marginalia-mode))

(all-the-icons-completion-mode)
(add-hook 'marginalia-mode-hook #'all-the-icons-completion-marginalia-setup)
(defun marginalia-use-builtin ()
  (interactive)
  (mapc
   (lambda (x)
     (setcdr x (cons 'builtin (remq 'builtin (cdr x)))))
   marginalia-annotator-registry))

(use-package all-the-icons-completion
  :ensure t
  :after (marginalia all-the-icons)
  :hook (marginalia-mode . all-the-icons-completion-marginalia-setup)
  :init
  (all-the-icons-completion-mode)
  (message "loading all-the-icons-completion")
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


(message "Finished loading niceties")
(provide 'niceties)
;;; niceties.el ends here
