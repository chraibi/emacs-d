;;; setup-dired.el --- Dired configuration
;;; Commentary:
;;;   Dired enhancements and defaults
;;; Code:

(message "start loading setup-dired")

;;================ directories and files ===============

;;--------------------------------- ibuffer
(autoload 'ibuffer "ibuffer" "List buffers." t)
(setq ibuffer-default-sorting-mode 'major-mode
      ibuffer-expert t
      ibuffer-show-empty-filter-groups nil)

;;--------------------------------- ido-goto-symbol
(defun ido-goto-symbol (&optional symbol-list)
  "Refresh imenu and jump to a place in the buffer using Ido.
Argument SYMBOL-LIST provides symbols to select from."
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
          (add-to-list 'name-and-pos (cons name position)))))))))

;;--------------------------------- Dired base setup
(when (string= system-type "darwin")
  (setq dired-use-ls-dired nil)) ;; macOS fix for ls

(add-hook 'dired-mode-hook #'auto-revert-mode) ;; auto-refresh

(setq dired-guess-shell-alist-user
      '(("\\.\\(?:xls[xm]?\\|doc[xm]?\\|ppt[xm]?\\|pdf\\)\\'" "open &")))

(setq dired-listing-switches "-lat"
      dired-recursive-copies 'always
      dired-recursive-deletes 'top
      dired-dwim-target t)

;;--------------------------------- Dired extensions
(use-package dired-rainbow :ensure t)
(use-package dired-hacks-utils :ensure t)
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
(use-package dired-filetype-face
  :ensure t
  :config (require 'dired-filetype-face))

;;--------------------------------- Dired colors
(defconst dired-audio-files-extensions
  '("mp3" "MP3" "ogg" "OGG" "flac" "FLAC" "wav" "WAV" "org" "txt" "md" "el")
  "Dired Audio files extensions.")
(dired-rainbow-define audio "#329EE8" dired-audio-files-extensions)

(defconst dired-video-files-extensions
  '("vob" "VOB" "mkv" "MKV" "mpg" "MPG" "mp4" "MP4" "mov" "MOV" "avi" "AVI"
    "wmv" "asf" "m4v" "M4V" "m2ts" "M2TS" "mpeg" "MPEG" "tp" "m2v")
  "Dired Video files extensions.")
(dired-rainbow-define video "#B3CCFF" dired-video-files-extensions)

;;--------------------------------- Dired omit (hide hidden/junk files)
(use-package dired
  :ensure nil ;; built-in
  :hook (dired-mode . dired-omit-mode)
  :config
  (require 'dired-x)
  ;; Hide backup (~), autosave (#...#), and dotfiles (.)
  (setq dired-omit-files
        (concat dired-omit-files "\\|^#.*#$\\|~$\\|^\\..+$"))

  ;; Toggle function with feedback
  (defun my/dired-omit-mode-toggle ()
    "Toggle `dired-omit-mode` with a message."
    (interactive)
    (if (bound-and-true-p dired-omit-mode)
        (progn
          (dired-omit-mode -1)
          (message "🔍 Showing hidden files"))
      (dired-omit-mode 1)
      (message "🙈 Hiding hidden files")))

  ;; Keybinding for toggling
  (define-key dired-mode-map (kbd "H") #'my/dired-omit-mode-toggle))

;;--------------------------------- Misc
(define-key dired-mode-map "b" 'dired-create-empty-file)

(message "finished loading setup-dired")
(provide 'setup-dired)
;;; setup-dired.el ends here
