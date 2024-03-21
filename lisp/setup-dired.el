;;; Package --- Summary
;;; --- Editor defaults
;;; Code:
;;; Commentary:
(message "start loading setup-dired")




;; ================ directories and files  ==========
;; ;;--------------------------------- ibuffer
(autoload 'ibuffer "ibuffer" "List buffers." t)
(setq ibuffer-default-sorting-mode 'major-mode)
(setq ibuffer-expert t)
(setq ibuffer-show-empty-filter-groups nil)
;;------------------------------------------------
;; M-i jump to symbol in file
(defun ido-goto-symbol (&optional symbol-list)
  "Refresh imenu and jump to a place in the buffer using Ido.
argument SYMBOL-LIST"
  (interactive)
  (unless (featurep 'imenu)
    (require 'imenu nil t))
  (cond
   ((not symbol-list)
    (let ((ido-mode ido-mode)
          (ido-enable-flex-matching
           (if (boundp 'ido-enable-flex-matching)
               ido-enable-flex-matching t))
          name-and-pos symbol-names position)
      (unless ido-mode
        (ido-mode 1)
        (setq ido-enable-flex-matching t))
      (while (progn
               (imenu--cleanup)
               (setq imenu--index-alist nil)
               (ido-goto-symbol (imenu--make-index-alist))
               (setq selected-symbol
                     (ido-completing-read "Symbol? " symbol-names))
                   (string= (car imenu--rescan-item) selected-symbol)))
          (unless (and (boundp 'mark-active) mark-active)
            (push-mark nil t nil))
          (setq position (cdr (assoc selected-symbol name-and-pos)))
          (cond
           ((overlayp position)
            (goto-char (overlay-start position)))
           (t
            (goto-char position)))))
       ((listp symbol-list)
        (dolist (symbol symbol-list)
          (let (name position)
            (cond
             ((and (listp symbol) (imenu--subalist-p symbol))
              (ido-goto-symbol symbol))
             ((listp symbol)
              (setq name (car symbol))
              (setq position (cdr symbol)))
             ((stringp symbol)
              (setq name symbol)
              (setq position
                    (get-text-property 1 'org-imenu-marker symbol))))
            (unless (or (null position) (null name)
                        (string= (car imenu--rescan-item) name))
              (add-to-list 'symbol-names name)
              (add-to-list 'name-and-pos (cons name position))))))))



;;on macOS, ls doesn't support the --dired option while on Linux it is supported.
(when (string= system-type "darwin")
  (setq dired-use-ls-dired nil))

;; auto refresh dired when file changes
(add-hook 'dired-mode-hook 'auto-revert-mode)

(setq dired-guess-shell-alist-user
      '(
        ("\\.xls\\'" "open &") ("\\.xlsx\\'" "open &")
        ("\\.doc\\'" "open &") ("\\.docx\\'" "open &")
        ("\\.ppt\\'" "open &") ("\\.pptx\\'" "open &")
        ("\\.pdf\\'" "open &")
        )
      )

(use-package dired-rainbow :ensure t)
(use-package dired-hacks-utils :ensure t)

(defconst dired-audio-files-extensions
  '("mp3" "MP3" "ogg" "OGG" "flac" "FLAC" "wav" "WAV" "org" "txt" "md" "el"); I dont like the colors of org-files
  "Dired Audio files extensions.")

(dired-rainbow-define audio "#329EE8" dired-audio-files-extensions)

(defconst dired-video-files-extensions
  '("vob" "VOB" "mkv" "MKV" "mpe" "mpg" "MPG" "mp4" "MP4" "ts" "TS"
    "m2ts" "M2TS" "avi" "AVI" "mov" "MOV" "wmv" "asf" "m2v" "m4v" "mpeg" "MPEG" "tp")
  "Dired Video files extensions.")

(dired-rainbow-define video "#B3CCFF" dired-video-files-extensions)

(use-package image-dired+
  :ensure t
  :config (image-diredx-async-mode 1))


(use-package dired-subtree
  :ensure t
  :bind (:map dired-mode-map
              ("i" . dired-subtree-insert)
              (";" . dired-subtree-remove)
              ("<tab>" . dired-subtree-toggle)
              ("S-<tab>" . dired-subtree-cycle)))

(use-package dired-collapse :ensure t)

(use-package dired-filter :ensure t)
(use-package dired-narrow
   :ensure t
  :bind (:map dired-mode-map
              ("C-S-f" . dired-narrow)))

(use-package dired-ranger :ensure t)

;; TODO problems with gnu ls?
;; (use-package dired-quick-sort
;;   :config (dired-quick-sort-setup))

(setq dired-listing-switches "-lat"
      dired-recursive-copies 'always
      dired-recursive-deletes 'top
      dired-dwim-target t
      )

(use-package dired-filetype-face
  :ensure t
  :config (require 'dired-filetype-face))

(define-key dired-mode-map "b" 'dired-create-empty-file)
(message "finished loading setup-dired")
(provide 'setup-dired)
;;; setup-dired.el ends here
