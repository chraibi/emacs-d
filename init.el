;;; package ---- summary
;;; Code:
;;; Commentary:
;; Make startup faster by reducing the frequency of garbage
;; collection.  The default is 0.8MB.  Measured in bytes.


(add-to-list 'load-path "~/.emacs.d/lisp/")
;;(add-to-list 'load-path (expand-file-name "~/.emacs.d/lisp"))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))
;(add-to-list 'load-path "~/.emacs.d/auto-complete-clang/")

;; (add-to-list 'load-path "~/.emacs.d/lisp/benchmark-init-el")


;(setq org-roam-v2-ack t)
(require 'package)
(setq package-enable-at-startup nil)
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))


;----------------  load setups ----------------------------


 (load-file "/Users/chraibi/.emacs.d/lisp/load_configs.el")
(message "load my setups from file")
   
;; (message "setups loaded")
;;-----------------------------
;; https://github.com/magnars/.emacs.d/blob/master/init.el
;; ;; Use a hook so the message doesn't get clobbered by other messages.
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Emacs ready in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)))

;;-----------------------
(defun my-test-emacs ()
  "Debuging start of Emacs."
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
  "Test starting Emacs for bugs."
  (when (eq major-mode 'emacs-lisp-mode)
    (my-test-emacs)))

(add-hook 'after-save-hook 'auto-test-emacs)

;; (byte-recompile-directory (expand-file-name "~/.emacs.d") 0)




(message "done loading emacs!")

(provide 'init)

;;; init.el ends here
