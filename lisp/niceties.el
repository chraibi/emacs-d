;;; niceties.el --- Some nice-to-have stuff -*- lexical-binding: t; -*-
;;; Commentary:
;;; server, async, vertico, embark, consult, savehist, marginalia, orderless
;;; Code:

(message "Loading niceties")

;; Built-in
(save-place-mode 1)
(setq switch-to-buffer-obey-display-actions t)

;; -------------------- async --------------------
(use-package async
  :ensure t
  :init
  (message "loading async"))

;; -------------------- vertico --------------------
(use-package vertico
  :ensure t
  :init
  (message "loading vertico")
  :config
  (vertico-mode 1)
  ;; If you really want grid always:
  (vertico-grid-mode 1)

  ;; Prefix current candidate with an arrow.
  ;; NOTE: This advises an internal function; keep it isolated.
  (defun my/vertico-prefix-current-candidate (orig cand prefix suffix index start)
    (setq cand (funcall orig cand prefix suffix index start))
    (concat (if (= vertico--index index)
                (propertize "» " 'face 'vertico-current)
              "  ")
            cand))

  (advice-add #'vertico--format-candidate :around #'my/vertico-prefix-current-candidate))


(use-package vertico-multiform
  :ensure nil
  :after vertico
  :config
  (vertico-multiform-mode 1)
  (setq vertico-multiform-commands
        '((consult-imenu grid)
          (consult-line grid)
          (execute-extended-command reverse))))


(use-package ace-window
  :ensure t
  :bind (("C-x o" . ace-window))
  :init
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)
        aw-dispatch-always t)
  :config
  ;; Frame-aware visuals (GUI vs terminal)
  (defun my/ace-window-frame-style (frame)
    (with-selected-frame frame
      (if (display-graphic-p frame)
          (progn
            (setq aw-background t)
            (set-face-attribute 'aw-leading-char-face frame :weight 'bold :height 2.0))
        (progn
          (setq aw-background nil)
          (set-face-attribute 'aw-leading-char-face frame :weight 'bold :height 1.0)))))
  (my/ace-window-frame-style (selected-frame))
  (add-hook 'after-make-frame-functions #'my/ace-window-frame-style))


;(global-set-key (kbd "M-o") 'ace-window)
;; (global-set-key (kbd "C-x o") 'ace-window)

;; (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
;; (setq aw-background t)
;; (setq aw-ignore-on t)
;; (add-to-list 'aw-ignored-buffers '("*Help*" "*Warnings*" "*Messages*"))


;; -------------------- marginalia --------------------
(use-package marginalia
  :ensure t
  :bind (:map minibuffer-local-map
              ("M-A" . marginalia-cycle))
  :custom
  (marginalia-max-relative-age 0)
  (marginalia-align 'right)
  :init
  (marginalia-mode 1))

;; -------------------- embark --------------------
(use-package embark
  :ensure t
  :init
  (message "loading embark")
  (setq prefix-help-command #'embark-prefix-help-command)
  :bind
  (("C-." . embark-act)
   ("C-;" . embark-dwim)
   ("C-h B" . embark-bindings))
  :config
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))




;; -------------------- consult + swiper choice --------------------
;; You currently prefer swiper for C-s/C-r. Keep it, but still use consult for other commands.
(use-package swiper
  :ensure t
  :bind (("C-s" . swiper-isearch)
         ("C-r" . swiper-isearch-backward)))

(use-package consult
  :ensure t
  :init
  (message "loading consult")
  :bind (("C-M-l" . consult-imenu)
         :map minibuffer-local-map
         ("C-r" . consult-history))
  :custom
  ;; Prefer a safe default. If you have dw/get-project-root, you can restore it.
  (consult-project-root-function
   (lambda ()
     (when-let ((pr (project-current nil)))
       (car (project-roots pr))))))


(use-package embark-consult
  :ensure t
  :after (embark consult)
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))


;; -------------------- savehist (built-in) --------------------
(use-package savehist
  :ensure nil
  :init
  (message "loading savehist")
  (setq history-length 200
        savehist-additional-variables '(mark-ring global-mark-ring search-ring regexp-search-ring))
  (savehist-mode 1))

;; -------------------- icons completion (GUI only) --------------------
(use-package all-the-icons-completion
  :ensure t
  :if (display-graphic-p)
  :after (marginalia all-the-icons)
  :hook (marginalia-mode . all-the-icons-completion-marginalia-setup)
  :init
  (message "loading all-the-icons-completion")
  :config
  (all-the-icons-completion-mode 1))

;; -------------------- orderless --------------------
(use-package orderless
  :ensure t
  :init
  (message "loading orderless")
  :config
  ;; Core completion config (single source of truth)
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion))))

  ;; Your dispatchers
  (defun my/orderless-flex-if-twiddle (pattern _index _total)
    (when (string-suffix-p "~" pattern)
      `(orderless-flex . ,(substring pattern 0 -1))))

  (defun my/orderless-first-initialism (_pattern index _total)
    (when (= index 0) 'orderless-initialism))

  (defun my/orderless-without-if-bang (pattern _index _total)
    (cond
     ((equal "!" pattern) '(orderless-literal . ""))
     ((string-prefix-p "!" pattern)
      `(orderless-without-literal . ,(substring pattern 1)))))

  (setq orderless-matching-styles '(orderless-regexp)
        orderless-style-dispatchers '(my/orderless-first-initialism
                                      my/orderless-flex-if-twiddle
                                      my/orderless-without-if-bang)))

;; -------------------- server (built-in) --------------------
(use-package server
  :ensure nil
  :hook (after-init . my/ensure-server)
  :init
  (defun my/ensure-server ()
    "Start the Emacs server unless it's already running."
    (require 'server)
    (unless (server-running-p server-name)
      (server-start))))

;; -------------------- pomm + alert sound --------------------
(use-package pomm
  :ensure t
  :commands (pomm pomm-third-time)
  :config
  (setq pomm-interval 50
        pomm-short-break 10
        pomm-long-break 20
        pomm-audio-enabled t
        pomm-mode-line-mode t
        pomm-csv-history-file-timestamp-format "%F %T"
        pomm-csv-history-file "~/.emacs.d/pomm_work.csv"
        pomm-audio-tick-enabled nil))

(use-package alert
  :ensure t
  :init
  (setq alert-default-style 'osx-notifier)
  :config
  (defun my-alert-afplay (&rest _args)
    (start-process "alert-sound" nil "afplay"
                   (expand-file-name "~/.emacs.d/sounds/chaffinch-singing-sound-effect-384534.wav")))
  (advice-remove 'alert #'my-alert-afplay)
  (advice-add 'alert :after #'my-alert-afplay))


;; improve completion ergonomics
(use-package emacs
  :ensure nil
  :init
  (setq enable-recursive-minibuffers t
        read-extended-command-predicate #'command-completion-default-include-p
        completions-detailed t))



;; -------------------- misc --------------------
(use-package reveal-in-osx-finder
  :ensure t
  :commands (reveal-in-osx-finder))

(use-package expand-region
  :ensure t
  :bind ("C-=" . er/expand-region)
  :init
  (message "loading expand-region"))

(message "Finished loading niceties")
(provide 'niceties)
;;; niceties.el ends here
