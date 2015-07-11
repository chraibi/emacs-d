;; Import flymake
(require 'flymake)


;; delay
;; Define function
(defun flymake-cc-init ()
  (let* (;; Create temp file which is copy of current file
         (temp-file   (flymake-init-create-temp-buffer-copy
                       'flymake-create-temp-inplace))
         ;; Get relative path of temp file from current directory
         (local-file  (file-relative-name
                       temp-file
                       (file-name-directory buffer-file-name))))

    ;; Construct compile command which is defined list.
    ;; First element is program name, "g++" in this case.
    ;; Second element is list of options.
    ;; So this means "g++ -Wall -Wextra -fsyntax-only tempfile-path"
    (list "g++" (list "-Wall" "-Wextra" "-fsyntax-only" "-std=c++11"
local-file))))

;; Enable above flymake setting for C++ files(suffix is '.cpp')
(push '("\\.cpp$" flymake-cc-init) flymake-allowed-file-name-masks)
(push '("\\.h$" flymake-cc-init) flymake-allowed-file-name-masks)

;; Enable flymake-mode for C++ files.
(add-hook 'c++-mode-hook 'flymake-mode)

;; C
(defun flymake-c-init ()
  (let* (;; Create temp file which is copy of current file
         (temp-file   (flymake-init-create-temp-buffer-copy
                       'flymake-create-temp-inplace))
         ;; Get relative path of temp file from current directory
         (local-file  (file-relative-name
                       temp-file
                       (file-name-directory buffer-file-name))))

    ;; Construct compile command which is defined list.
    ;; First element is program name, "g++" in this case.
    ;; Second element is list of options.
    ;; So this means "g++ -Wall -Wextra -fsyntax-only tempfile-path"
    (list "gcc" (list "-Wall" "-Wextra"
local-file))))

;; Enable above flymake setting for C++ files(suffix is '.cpp')
(push '("\\.c$" flymake-c-init) flymake-allowed-file-name-masks)
(push '("\\.h$" flymake-c-init) flymake-allowed-file-name-masks)

;; Enable flymake-mode for C++ files.
(add-hook 'c-mode-hook 'flymake-mode)








(global-set-key (kbd "C-c \\") 'flymake-display-err-menu-for-current-line)


(provide 'flymake-setup)
