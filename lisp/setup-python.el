(require 'python)
(require 'setup-hlinum)

(package-initialize)
(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))
;; (require 'ipython)
                                        ; http://www.masteringemacs.org/articles/2013/03/11/whats-new-emacs-24-3
;(add-hook 'python-mode-hook
;          (lambda ()

(setq indent-tabs-mode nil)
(setq python-indent-guess-indent-offset 1)
(linum-mode 1)
(elpy-mode 1)
(auto-complete-mode 1)
(setq ipython-command "/usr/local/bin/ipython")
(setq python-shell-interpreter "/usr/local/bin/ipython")
(setq python-indent-offset 4)
(message "Activate python mode")

 
;)
;)
                        

;; (defun flycheck-python-setup ()
;;   (flycheck-mode)
;;   )
;; (add-hook 'python-mode-hook #'flycheck-python-setup)

;; ;; Remove trailing whitespace manually by typing C-t C-w.
;; (add-hook 'python-mode-hook
;;           (lambda ()
;;             (local-set-key (kbd "C-t C-w")
;;                            'delete-trailing-whitespace)))

;; ;; Automatically remove trailing whitespace when file is saved.
;; (add-hook 'python-mode-hook
;;           (lambda()
;;         ((and )dd-hook 'local-write-file-hooks
;;          '(lambda()
;;             (save-excursion
;;               (delete-trailing-whitespace))))))


(setq py-python-command-args '( "--colors=Linux"))
(message "leave python setup")
(provide 'setup-python)
