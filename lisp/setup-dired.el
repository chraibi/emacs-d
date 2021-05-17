;;; Package --- Summary
;;; --- Editor defaults
;;; Code:
;;; Commentary:

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


;; https://www.reddit.com/r/emacs/comments/byhf6w/file_management_with_dired_in_emacs/
;; (autoload 'dired-jump "dired-x"
;;   "Jump to Dired buffer corresponding to current buffer." t)

;; (autoload 'dired-jump-other-window "dired-x"
;;   "Like \\[dired-jump] (dired-jump) but in other window." t)

;; (use-package dired-x :ensure t)
;; (use-package dired+ :ensure t)
;; (require 'dired-x)
;; (require 'dired+)

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

;; show the file from point in the other window
;; https://github.com/asok/peep-dired
(use-package peep-dired
  :ensure t
  :defer t ; don't access `dired-mode-map' until `peep-dired' is loaded
  :bind (:map dired-mode-map
              ("P" . peep-dired)))

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

;; (define-key dired-mode-map "ö" 'dired-toggle-read-only)

(provide 'setup-dired)
;;; setup-dired.el ends here
