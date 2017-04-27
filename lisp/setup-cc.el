(require 'cmake-project)
(require 'cpputils-cmake)
(require 'rtags)
;; https://github.com/Andersbakken/rtags
(cmake-ide-setup)



(require 'popup)
(require 'rtags-ac)
(rtags-start-process-unless-running)

 (add-hook 'c-mode-common-hook 'rtags-start-process-unless-running)
 (add-hook 'c++-mode-common-hook 'rtags-start-process-unless-running)


;(rtags-find-references)			;
;(rtags-diagnostics)
(rtags-enable-standard-keybindings)
;(rtags-imenu)				;
;(rtags-location-stack-back)
;(rtags-location-stack-forward)
;(rtags-symbol-type)
(rtags-print-dependencies)


;; (setq rtags-autostart-diagnostics t)
;; (setq rtags-completions-enabled t)
;; (rtags-enable-standard-keybindings c-mode-base-map)
;; (add-hook 'c++-mode-hook
;;           (lambda ()
;;             (setq ac-sources '(ac-source-rtags)
;;                   )))

(require 'company)
(global-company-mode)
(push 'company-rtags company-backends)
(define-key c-mode-base-map (kbd "<tab>") (function company-complete))

;; (require 'flycheck-rtags)
;; (defun my-flycheck-rtags-setup ()
;;   (flycheck-select-checker 'rtags)
;;   (setq-local flycheck-highlighting-mode nil) ;; RTags creates more accurate overlays.
;;   (setq-local flycheck-check-syntax-automatically nil))
;; ;; c-mode-common-hook is also called by c++-mode
;; (add-hook 'c-mode-common-hook #'my-flycheck-rtags-setup)


(require 'rtags-helm)			;
(setq rtags-use-helm t)


;; (setq mf--source-file-extension "cpp")




;;  ;-------------------------------
(defun use-rtags (&optional useFileManager)
  (and (rtags-executable-find "rc")
       (cond ((not (gtags-get-rootpath)) t)
             ((and (not (eq major-mode 'c++-mode))
                   (not (eq major-mode 'c-mode))) (rtags-has-filemanager))
             (useFileManager (rtags-has-filemanager))
             (t (rtags-is-indexed)))))

;; (defun tags-find-symbol-at-point (&optional prefix)
;;   (interactive "P")
;;   (if (and (not (rtags-find-symbol-at-point prefix)) rtags-last-request-not-indexed)
;;       (gtags-find-tag)))
;; (defun tags-find-references-at-point (&optional prefix)
;;   (interactive "P")
;;   (if (and (not (rtags-find-references-at-point prefix)) rtags-last-request-not-indexed)
;;       (gtags-find-rtag)))
;; (defun tags-find-symbol ()
;;   (interactive)
;;   (call-interactively (if (use-rtags) 'rtags-find-symbol 'gtags-find-symbol)))
;; (defun tags-find-references ()
;;   (interactive)
;;   (call-interactively (if (use-rtags) 'rtags-find-references 'gtags-find-rtag)))
;; (defun tags-find-file ()
;;   (interactive)
;;   (call-interactively (if (use-rtags t) 'rtags-find-file 'gtags-find-file)))
;; (defun tags-imenu ()
;;   (interactive)
;;   (call-interactively (if (use-rtags t) 'rtags-imenu 'idomenu)))

(define-key c-mode-base-map (kbd "M-.") (function tags-find-symbol-at-point))
(define-key c-mode-base-map (kbd "M-,") (function tags-find-references-at-point))
(define-key c-mode-base-map (kbd "M-;") (function tags-find-file))
(define-key c-mode-base-map (kbd "C-.") (function tags-find-symbol))
(define-key c-mode-base-map (kbd "C-,") (function tags-find-references))
(define-key c-mode-base-map (kbd "C-<") (function rtags-find-virtuals-at-point))
(define-key c-mode-base-map (kbd "M-i") (function tags-imenu))

(define-key global-map (kbd "M-.") (function tags-find-symbol-at-point))
(define-key global-map (kbd "M-,") (function tags-find-references-at-point))
(define-key global-map (kbd "M-;") (function tags-find-file))
(define-key global-map (kbd "C-.") (function tags-find-symbol))
(define-key global-map (kbd "C-,") (function tags-find-references))
(define-key global-map (kbd "C-<") (function rtags-find-virtuals-at-point))
(define-key global-map (kbd "M-i") (function tags-imenu))





;-------------------------------

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

                                        ; --- doxymacs

(require 'doxymacs)
(add-hook 'c-mode-common-hook 'doxymacs-mode)
(defun my-doxymacs-font-lock-hook ()
    (if (or (eq major-mode 'c-mode) (eq major-mode 'c++-mode))
        (doxymacs-font-lock)))
(add-hook 'font-lock-mode-hook 'my-doxymacs-font-lock-hook)

(defun my-doxymacs-font-lock-hook ()
    (if (or (eq major-mode 'c-mode) (eq major-mode 'c++-mode))
        (doxymacs-font-lock)))
  (add-hook 'font-lock-mode-hook 'my-doxymacs-font-lock-hook)


(defun my-javadoc-return () 
  "Advanced C-m for Javadoc multiline comments.   
Inserts `*' at the beggining of the new line if 
unless return was pressed outside the comment"
  (interactive)
  (setq last (point))
  (setq is-inside
	(if (search-backward "*/" nil t)
	    ;; there are some comment endings - search forward
	    (if (search-forward "/*" last t)
		't
	      'nil)
	  ;; it's the only comment - search backward
	  (goto-char last)
	  (if (search-backward "/*" nil t)
	      't
	    'nil
	    )
	  )
	)
  ;; go to last char position
  (goto-char last)
  ;; the point is inside some comment, insert `*'
  (if is-inside
      (progn 
	(insert "\n*")
	(indent-for-tab-command))
    ;; else insert only new-line
    (insert "\n")))
(add-hook 'c++-mode-hook (lambda () 
  (local-set-key "\r" 'my-javadoc-return)))

;; probleme mit python
;; (add-to-list 'semantic-default-submodes 'global-semantic-stickyfunc-mode)
;; (semantic-mode 1)
;; (require 'stickyfunc-enhance)


(provide 'setup-cc)
;;; setup-cc.el ends here
