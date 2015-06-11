;;; timeclock-setup.el ---

;; Copyright 2009 Joseph Brenner
;;
;; Author: doom@kzsu.stanford.edu
;; Version: $Id: timeclock-setup.el,v 0.0 2009/05/13 00:46:48 doom Exp $
;; Keywords:
;; X-URL: not distributed yet

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;;  Additions to timeclock.el and timeclock-x.el (timeclock-x-squared?).
;;  This does some simple set-up tasks, and includes some elisp glue
;;  to run perl utilities.

;; Put this file into your load-path and the following into your ~/.emacs:
;;   (require 'timeclock-setup)

;;; Code:

(provide 'timeclock-setup)
(eval-when-compile
  (require 'cl))


;;;;##########################################################################
;;;;  User Options, Variables
;;;;##########################################################################




(require 'timeclock)

(require 'timeclock-x)

;; define this before we use it below...
(defun timeclock-additional-setup ()
  "Some additional set-up (timeclock-x squared?).
Creates a ~/.timeclock/default.log if it doesn't exist already."
  (interactive)
  (let* (  (location (substitute-in-file-name "$HOME/.timeclock"))
           (log-file-base "default.log")
           (log-file (concat location "/" log-file-base))
          )

    (unless (file-exists-p location)
      (make-directory location t))

    (unless (file-exists-p log-file)
      (save-excursion
        (switch-to-buffer log-file t)
        (write-file log-file t) ;; additional safety: ask for confirmation on over-write
        (kill-buffer log-file-base)
        )))
  )

;; (timeclock-additional-setup)
;; (timeclock-initialize)
;; (timeclock-setup-keys)



;; (timeclock-modeline-display 1) ;; if you want modline display
;(timeclock-mode-line-display)
   (defun timeclock-interval (i) (list (floor (/ (- (timeclock-time-to-seconds (cadr i)) (timeclock-time-to-seconds (car i))) 60)) (caddr i)))
   (defun timeclock-intervals (i) (map 'list 'timeclock-interval i))

;; Next, I want to summarise days according to how long I worked on what:-

(defun timeclock-today (&optional daysago) (timeclock-intervals (cddr (nth (or daysago 0) (timeclock-day-alist)))))

(defun timeclock-total-today (time-projs &optional result)
  (if (equal (car time-projs) nil)
      result
    (let ((tp (sort time-projs (lambda (a b) (string-lessp (cadr b) (cadr a))))))
      (if (equal (cadar tp) (cadadr tp))
          (timeclock-total-today
             (cons
                 (list (+ (caar tp) (caadr tp)) (cadar tp)) 
                     (cddr tp))
             result)
        (timeclock-total-today (cdr tp) (cons (car tp) result))))))

(defun timeclock-today-interactive ()
  (interactive)
  (princ
   (apply '+
          (map 'list 'car 
               (princ (timeclock-total-today (timeclock-today)))))))



(global-set-key (kbd "C-c i") 'timeclock-in)
(global-set-key (kbd "C-c o") 'timeclock-out)
(global-set-key (kbd "C-c v") 'timeclock-visit-timelog)
;; (global-set-key (kbd "C-c p") 'timeclock-display-project-names)
;; (define-key ctl-x-map "cy" 'timeclock-today-interactive)


;;; timeclock-setup.el ends here
