;;; init.el  -*- lexical-binding: t; -*-

;;; package ---- summary
;;; Code:
;;; Commentary:
;; Make startup faster by reducing the frequency of garbage
;; collection.  The default is 0.8MB.  Measured in bytes.


(add-to-list 'load-path "~/.emacs.d/lisp/")

(require 'package)
(package-initialize)



;; Optional: sanity check
(message "Using org from: %s" (locate-library "org"))


(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/"))
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/"))


(unless (package-installed-p 'use-package)
  (package-refresh-contents) ;; needed
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

;; Pick up PATH/env from the login shell.  Must run after `package-initialize'
;; so the package is on the load-path (early-init.el disables auto-activation).
(use-package exec-path-from-shell
  :ensure t
  :init (exec-path-from-shell-initialize))

;; Activate benchmark-init first so it can measure the rest of startup.
(use-package benchmark-init
  :ensure t
  :config
  (benchmark-init/activate)
  (add-hook 'after-init-hook 'benchmark-init/deactivate))

;; helper function for timing loads
(defun load-with-timing (file)
  (let ((start-time (current-time)))
    (load file)
    (message "Loading %s took %s seconds"
             file
             (float-time (time-subtract (current-time) start-time))))) 

;;----------------  load setups ----------------------------
(require 'org-clock)

(defun my/org-clock-save-to-file ()
  "Save clock string to file for tmux."
  (with-temp-file "~/.emacs-clock"
    (insert (if (org-clocking-p)
                (org-clock-get-clock-string)
              ""))))

(add-hook 'org-clock-in-hook #'my/org-clock-save-to-file)
(add-hook 'org-clock-out-hook #'my/org-clock-save-to-file)


(load-with-timing "~/.emacs.d/lisp/niceties.el")
(load-with-timing "~/.emacs.d/lisp/my-core-settings.el")
(load-with-timing "~/.emacs.d/lisp/esthetics.el")
(load-with-timing "~/.emacs.d/lisp/window_editing.el")
(load-with-timing "~/.emacs.d/lisp/project_management.el")
(load-with-timing "~/.emacs.d/lisp/org-basics.el")
(load-with-timing "~/.emacs.d/lisp/setup-org-modern.el")
(load-with-timing "~/.emacs.d/lisp/setup-org-roam.el")
(load-with-timing "~/.emacs.d/lisp/setup-agenda.el")
(load-with-timing "~/.emacs.d/lisp/setup-org-crypt.el")
(load-with-timing "~/.emacs.d/lisp/load_coding.el")
(load-with-timing "~/.emacs.d/lisp/setup-godmode.el")


(require 'org-roam-tree)
(setq org-roam-mode-sections '(org-roam-tree-backlinks-section))


(use-package org
  :ensure t
  :config
  (require 'org-habit)
  (add-to-list 'org-modules 'org-habit))
(setq org-habit-show-habits-only-for-today nil)
(setq org-habit-graph-column 60) ;; Customize graph placement


(load-with-timing "~/.emacs.d/lisp/setup-org-ref.el")




;(load-with-timing "~/.emacs.d/lisp/setup-deft.el")

(message "Finished loading all packages and configs")
;;-----------------------------
;; https://github.com/magnars/.emacs.d/blob/master/init.el
;; ;; Use a hook so the message doesn't get clobbered by other messages.
(add-hook 'emacs-startup-hook
          (lambda ()
            ;; Restore a sane GC threshold after the startup spike set in
            ;; early-init.el.
            (setq gc-cons-threshold (* 50 1000 1000)
                  gc-cons-percentage 0.1)
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


(message "done loading emacs!")
(provide 'init)

;;; init.el ends here
