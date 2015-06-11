(defun google-translate-query-auto-completion ()
  "Prepare and return a list with words and phrases for auto
  completion. Words retrieves from the current buffer from just
  its visible part, doesn't takes into account whole buffer
  contents."
  (let* ((buffer-contents
          (buffer-substring-no-properties (window-start) (window-end)))
         (result '())
         (words (mapcar (lambda (w) (when (> (length w) 1) (downcase w)))
                        (split-string buffer-contents "[ [:punct:]0-9\f\t\n\r\v]+" t))))
    (with-temp-buffer
      (insert buffer-contents)
      (dolist (word words)
        (goto-char (point-min))
        (when (and (not (null word))
                   (search-forward word nil t))
          (backward-word)
          (let ((beg (point)) (count 0))
            (while (and 
                    (< (point) (point-max))
                    (not
                     (equal
                      (buffer-substring-no-properties
                       (point)
                       (+ (point) 1))
                      "."))
                    (< count 10))
              (forward-word)
              (incf count)
              (setq result
                    (append
                     result
                     (list (buffer-substring-no-properties beg (point))))))))))
    result))

(provide 'google-translate-query-auto-complete)
