;;; Package --- summary
;;; Code:
;;; Commentary:

(message "Enter setup-cc")

;(require 'cmake-project)
;(require 'cpputils-cmake)
;(require 'rtags)
;; https://github.com/Andersbakken/rtags
;(cmake-ide-setup)

;(auto-complete-mode)
;(global-flycheck-mode)

(require 'modern-cpp-font-lock)
(modern-c++-font-lock-global-mode t)


(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.cc\\'" . c++-mode))

;; (require 'popup)
;; (setq c-auto-newline nil)


(require 'hlinum)
(hlinum-activate)
(setq linum-format "%3d \u2502 ")

                                        ;deactivate reftex
(reftex-mode 0)

; activate snippets
(yas-global-mode +1)
;-------------------------------
;(c-set-offset 'substatement-open 0)

(c-add-style "my-style"
             '("stroustrup"
               (indent-tabs-mode . nil)        ; use spaces rather than tabs
               (c-basic-offset . 5)            ; indent by four spaces
               (c-offsets-alist . ((inline-open . 0)  ; custom indentation rules
                                   (brace-list-open . 0)
                                   (statement-case-open . +)))))

(defun my-c++-mode-hook ()
  "My style."
  (c-set-style "my-style")        ; use my-style defined above
  (auto-fill-mode)
  (c-toggle-auto-hungry-state 1))

(add-hook 'c++-mode-hook 'my-c++-mode-hook)

(defun my-recompile ()
  "Run compile and resize the compile window closing the old one if necessary."
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
  "Move point to next error and highlight it."
  (interactive)
  (progn
    (next-error)
    (end-of-line-nomark)
    (beginning-of-line-mark)
    )
  )

(defun my-previous-error ()
  "Move point to previous error and highlight it."
  (interactive)
  (progn
    (previous-error)
    (end-of-line-nomark)
    (beginning-of-line-mark)
    )
  )

;; (global-set-key (kbd "C-n") 'my-next-error)
;; (global-set-key (kbd "C-p") 'my-previous-error)
;; (global-set-key (kbd "C-x <f9>") 'my-recompile)
(global-set-key [f9] 'compile)
(setq compilation-scroll-output 'first-error)


                                        ; --- doxymacs

;; (require 'doxymacs)
;; (add-hook 'c-mode-common-hook 'doxymacs-mode)
;; (defun my-doxymacs-font-lock-hook ()
;;     (if (or (eq major-mode 'c-mode) (eq major-mode 'c++-mode))
;;         (doxymacs-font-lock)))
;; (add-hook 'font-lock-mode-hook 'my-doxymacs-font-lock-hook)

;; (defun my-doxymacs-font-lock-hook ()
;;     (if (or (eq major-mode 'c-mode) (eq major-mode 'c++-mode))
;;         (doxymacs-font-lock)))
;;   (add-hook 'font-lock-mode-hook 'my-doxymacs-font-lock-hook)


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

; help on hover
;(add-hook 'c-mode-hook 'c-turn-on-eldoc-mode)
;(add-hook 'c++-mode-hook 'c-turn-on-eldoc-mode)
;(setq c-eldoc-cpp-command "/usr/bin/clang")

;; turn on semantic
;(semantic-mode 1)

(global-set-key (kbd "C-c /") 'counsel-etags-grep-symbol-at-point)


(message "Provide setup-cc")
(provide 'setup-cc)
;;; setup-cc.el ends here
