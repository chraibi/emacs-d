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

(exec-path-from-shell-initialize)
(use-package exec-path-from-shell
  :ensure t
  :config
  (when (daemonp)
    (exec-path-from-shell-initialize)
    (message "deamon mode")
    )
  )

(require 'package)                                    
; Disabling package activation at startup
(setq package-enable-at-startup nil)
;(package-initialize)
(unless (package-installed-p 'use-package)
;(package-refresh-contents)
(package-install 'use-package)
)

(eval-when-compile
 (require 'use-package))

;; helper function for timing loads
(defun load-with-timing (file)
  (let ((start-time (current-time)))
    (load file)
    (message "Loading %s took %s seconds"
             file
             (float-time (time-subtract (current-time) start-time)))))
;;----------------  load setups ----------------------------

(load-with-timing "~/.emacs.d/lisp/niceties.el")
(load-with-timing "~/.emacs.d/lisp/my-core-settings.el")
(load-with-timing "~/.emacs.d/lisp/esthetics.el")
(load-with-timing "~/.emacs.d/lisp/window_editing.el")
(load-with-timing "~/.emacs.d/lisp/load_coding.el")
(load-with-timing "~/.emacs.d/lisp/project_management.el")


;; (use-package org
;;   :init
;;   (message "Loading org-mode!")
;;   :config
;;   (require 'setup-org-mode)
;;   )
(message "Finished loading all packages and configs")
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


(message "byte recompiling directory deactivated. Activate it from time to time")
;(byte-recompile-directory (expand-file-name "~/.emacs.d") 0)

(message "done loading emacs!")
(provide 'init)

;;; init.el ends here
