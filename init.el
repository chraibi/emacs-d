;;; package ---- summary
;;; Code:
;;; Commentary:
;; Make startup faster by reducing the frequency of garbage
;; collection.  The default is 0.8MB.  Measured in bytes.


(add-to-list 'load-path "~/.emacs.d/lisp/")
(use-package exec-path-from-shell
  :ensure t
  :init (exec-path-from-shell-initialize))


(require 'package)
(setq package-enable-at-startup nil)
(package-initialize)



;; Optional: sanity check
(message "Using org from: %s" (locate-library "org"))


(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/"))


(unless (package-installed-p 'use-package)
  (package-refresh-contents) ;; needed
  (package-install 'use-package))

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
(load-with-timing "~/.emacs.d/lisp/project_management.el")
(load-with-timing "~/.emacs.d/lisp/org-basics.el")
(load-with-timing "~/.emacs.d/lisp/setup-org-modern.el")
;(load-with-timing "~/.emacs.d/lisp/setup-org-roam.el")
(load-with-timing "~/.emacs.d/lisp/setup-agenda.el")
(load-with-timing "~/.emacs.d/lisp/setup-org-crypt.el")
(load-with-timing "~/.emacs.d/lisp/load_coding.el")


;(defun load-ref ()
;  "Load setup-ref.el and setup-help-bibtex explicitly when needed."
;  (interactive)
;  (load-with-timing "~/.emacs.d/lisp/setup-helm-bibtex.el")
  (load-with-timing "~/.emacs.d/lisp/setup-org-ref.el")
;  )




;(load-with-timing "~/.emacs.d/lisp/setup-deft.el")

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
;;(byte-recompile-directory (expand-file-name "~/.emacs.d") 0)
(defun autocompile nil
  "compile itself if ~/.emacs"
  (interactive)
  (require 'bytecomp)
  (let ((dotemacs (file-truename user-init-file)))
    (if (string= (buffer-file-name) (file-chase-links dotemacs))
      (byte-compile-file dotemacs))))

(add-hook 'after-save-hook 'autocompile)


(use-package benchmark-init
  :ensure t
  :config
  ;; To disable collection of benchmark data after init is done.
  (add-hook 'after-init-hook 'benchmark-init/deactivate))


(message "done loading emacs!")
(provide 'init)

;;; init.el ends here
