;;; ob-plantuml.el --- org-babel functions for plantuml evaluation

;; Author: Zhang Weize
;; Homepage: http://orgmode.org

;;; Commentary:

;; Org-Babel support for evaluating plantuml.
;;

;; Some code in ob-plantuml was adopted from
;; Ian Yang's org-export-blocks-format-plantuml
;; http://www.emacswiki.org/emacs/org-export-blocks-format-plantuml.el

;;; Code:
(require 'ob)

(defvar org-babel-default-header-args:plantuml
  '((:results . "file") (:exports . "results"))
  "Default arguments for evaluating a plantuml source block.")

(defun org-babel-expand-body:plantuml (body params &optional processed-params)
  "Expand BODY according to PARAMS, return the expanded body." body)

(defvar org-plantuml-jar-path)
(defun org-babel-execute:plantuml (body params)
  "Execute a block of plantuml code with org-babel.
This function is called by `org-babel-execute-src-block'."
  (let ((result-params (split-string (or (cdr (assoc :results params)) "")))
        (out-file (cdr (assoc :file params)))
        (cmdline (cdr (assoc :cmdline params)))
        (in-file (make-temp-file "org-babel-plantuml")))
    (unless (file-exists-p org-plantuml-jar-path)
      (error "Could not find plantuml.jar at %s" org-plantuml-jar-path))
    (with-temp-file in-file (insert (concat "@startuml\n" body "\n@enduml")))
    (message (concat "java -jar " org-plantuml-jar-path " -p " cmdline " " in-file))
    (with-temp-buffer
      (call-process-shell-command
       (concat "java -jar " org-plantuml-jar-path " -p " cmdline)
       in-file
       '(t nil))
      (write-region nil nil out-file))
    out-file))

(defun org-babel-prep-session:plantuml (session params)
  "Return an error because plantuml does not support sessions."
  (error "Plantuml does not support sessions"))

(provide 'ob-plantuml)

;;; ob-plantuml.el ends here
