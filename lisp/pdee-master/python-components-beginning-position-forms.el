;;; python-components-beginning-position-forms.el --- Just some more tests 

;; Copyright (C) 2011-2014  Andreas Roehler
;; Author: Andreas Roehler <andreas.roehler@online.de>
;; Keywords: languages, convenience

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This file is generated by function from python-mode-utils.el - see in
;; directory devel. Edits here might not be persistent.

;;; Code:


(defun py--beginning-of-statement-position ()
  "Returns beginning of statement position at beginning-of-line. "
  (save-excursion
    (let ((erg (py-beginning-of-statement)))
      (when (and py-verbose-p (interactive-p)) (message "%s" erg))
      erg)))

(defun py--beginning-of-block-position ()
  "Returns beginning of block position at beginning-of-line. "
  (save-excursion
    (let ((erg (py-beginning-of-block)))
      (when (and py-verbose-p (interactive-p)) (message "%s" erg))
      erg)))

(defun py--beginning-of-clause-position ()
  "Returns beginning of clause position at beginning-of-line. "
  (save-excursion
    (let ((erg (py-beginning-of-clause)))
      (when (and py-verbose-p (interactive-p)) (message "%s" erg))
      erg)))

(defun py--beginning-of-block-or-clause-position ()
  "Returns beginning of block-or-clause position at beginning-of-line. "
  (save-excursion
    (let ((erg (py-beginning-of-block-or-clause)))
      (when (and py-verbose-p (interactive-p)) (message "%s" erg))
      erg)))

(defun py--beginning-of-def-position ()
  "Returns beginning of def position at beginning-of-line. "
  (save-excursion
    (let ((erg (py-beginning-of-def)))
      (when (and py-verbose-p (interactive-p)) (message "%s" erg))
      erg)))

(defun py--beginning-of-class-position ()
  "Returns beginning of class position at beginning-of-line. "
  (save-excursion
    (let ((erg (py-beginning-of-class)))
      (when (and py-verbose-p (interactive-p)) (message "%s" erg))
      erg)))

(defun py--beginning-of-def-or-class-position ()
  "Returns beginning of def-or-class position at beginning-of-line. "
  (save-excursion
    (let ((erg (py-beginning-of-def-or-class)))
      (when (and py-verbose-p (interactive-p)) (message "%s" erg))
      erg)))

(defun py--beginning-of-expression-position ()
  "Returns beginning of expression position at beginning-of-line. "
  (save-excursion
    (let ((erg (py-beginning-of-expression)))
      (when (and py-verbose-p (interactive-p)) (message "%s" erg))
      erg)))

(defun py--beginning-of-partial-expression-position ()
  "Returns beginning of partial-expression position at beginning-of-line. "
  (save-excursion
    (let ((erg (py-beginning-of-partial-expression)))
      (when (and py-verbose-p (interactive-p)) (message "%s" erg))
      erg)))

(defun py--beginning-of-minor-block-position ()
  "Returns beginning of minor-block position at beginning-of-line. "
  (save-excursion
    (let ((erg (py-beginning-of-minor-block)))
      (when (and py-verbose-p (interactive-p)) (message "%s" erg))
      erg)))

(defun py--beginning-of-if-block-position ()
  "Returns beginning of if-block position at beginning-of-line. "
  (save-excursion
    (let ((erg (py-beginning-of-if-block)))
      (when (and py-verbose-p (interactive-p)) (message "%s" erg))
      erg)))

(defun py--beginning-of-try-block-position ()
  "Returns beginning of try-block position at beginning-of-line. "
  (save-excursion
    (let ((erg (py-beginning-of-try-block)))
      (when (and py-verbose-p (interactive-p)) (message "%s" erg))
      erg)))

(defun py--beginning-of-except-block-position ()
  "Returns beginning of except-block position at beginning-of-line. "
  (save-excursion
    (let ((erg (py-beginning-of-except-block)))
      (when (and py-verbose-p (interactive-p)) (message "%s" erg))
      erg)))

(defun py--beginning-of-top-level-position ()
  "Returns beginning of top-level position at beginning-of-line. "
  (save-excursion
    (let ((erg (py-beginning-of-top-level)))
      (when (and py-verbose-p (interactive-p)) (message "%s" erg))
      erg)))

(defun py--beginning-of-statement-position-bol ()
  "Returns beginning of statement position. "
  (save-excursion
    (let ((erg (py-beginning-of-statement-bol)))
      (when (and py-verbose-p (interactive-p)) (message "%s" erg))
      erg)))

(defun py--beginning-of-block-position-bol ()
  "Returns beginning of block position. "
  (save-excursion
    (let ((erg (py-beginning-of-block-bol)))
      (when (and py-verbose-p (interactive-p)) (message "%s" erg))
      erg)))

(defun py--beginning-of-clause-position-bol ()
  "Returns beginning of clause position. "
  (save-excursion
    (let ((erg (py-beginning-of-clause-bol)))
      (when (and py-verbose-p (interactive-p)) (message "%s" erg))
      erg)))

(defun py--beginning-of-block-or-clause-position-bol ()
  "Returns beginning of block-or-clause position. "
  (save-excursion
    (let ((erg (py-beginning-of-block-or-clause-bol)))
      (when (and py-verbose-p (interactive-p)) (message "%s" erg))
      erg)))

(defun py--beginning-of-def-position-bol ()
  "Returns beginning of def position. "
  (save-excursion
    (let ((erg (py-beginning-of-def-bol)))
      (when (and py-verbose-p (interactive-p)) (message "%s" erg))
      erg)))

(defun py--beginning-of-class-position-bol ()
  "Returns beginning of class position. "
  (save-excursion
    (let ((erg (py-beginning-of-class-bol)))
      (when (and py-verbose-p (interactive-p)) (message "%s" erg))
      erg)))

(defun py--beginning-of-def-or-class-position-bol ()
  "Returns beginning of def-or-class position. "
  (save-excursion
    (let ((erg (py-beginning-of-def-or-class-bol)))
      (when (and py-verbose-p (interactive-p)) (message "%s" erg))
      erg)))

(defun py--beginning-of-minor-block-position-bol ()
  "Returns beginning of minor-block position. "
  (save-excursion
    (let ((erg (py-beginning-of-minor-block-bol)))
      (when (and py-verbose-p (interactive-p)) (message "%s" erg))
      erg)))

(defun py--beginning-of-if-block-position-bol ()
  "Returns beginning of if-block position. "
  (save-excursion
    (let ((erg (py-beginning-of-if-block-bol)))
      (when (and py-verbose-p (interactive-p)) (message "%s" erg))
      erg)))

(defun py--beginning-of-try-block-position-bol ()
  "Returns beginning of try-block position. "
  (save-excursion
    (let ((erg (py-beginning-of-try-block-bol)))
      (when (and py-verbose-p (interactive-p)) (message "%s" erg))
      erg)))

(defun py--beginning-of-except-block-position-bol ()
  "Returns beginning of except-block position. "
  (save-excursion
    (let ((erg (py-beginning-of-except-block-bol)))
      (when (and py-verbose-p (interactive-p)) (message "%s" erg))
      erg)))

(defun py--beginning-of-top-level-position-bol ()
  "Returns beginning of top-level position. "
  (save-excursion
    (let ((erg (py-beginning-of-top-level-bol)))
      (when (and py-verbose-p (interactive-p)) (message "%s" erg))
      erg)))

(provide 'python-components-beginning-position-forms)
;;; python-components-beginning-position-forms.el ends here
