;;; Package --- windows behavior
;;; Code:
;;; Commentary: undo, copy/paste, comment/uncomment, and avy

(message "load window_editing")
;; =================== windows and editing ===============
(defun comment-or-uncomment-region-or-line ()
  "Comments or uncomments the region or the current line if there's no active region."
  (interactive)
  (let (beg end)
    (if (region-active-p)
        (setq beg (region-beginning) end (region-end))
      (setq beg (line-beginning-position) end (line-end-position)))
    (comment-or-uncomment-region beg end)
    (next-line)))

(defun comment-region-lines (beg end &optional arg)
  "Like `comment-region', but comment/uncomment whole lines."
  (interactive "*r\nP")
  (if (> beg end) (let (mid) (setq mid beg beg end end mid)))
  (let ((bol  (save-excursion (goto-char beg) (line-beginning-position)))
        (eol  (save-excursion (goto-char end) (line-end-position))))
    (comment-region bol end arg)))


(use-package recentf
  :init
  (message "loading recentf!")
  :ensure t
  :defer 5
  :config
  (setq recentf-exclude
        (append recentf-exclude
                '("~$"
                  "\\.emacs.d*")))
  (setq
   recentf-max-saved-items 30
   recentf-max-menu-items 15)      ;; max 15 in menu
  )


(use-package undo-tree
  :ensure t
  :diminish undo-tree-mode
  :config
  (progn
    (global-undo-tree-mode)
    (setq undo-tree-visualizer-timestamps t)
    (setq undo-tree-visualizer-diff nil)))

(use-package winner
  :init
  (message "loading winner!")
  :ensure t
  )
(winner-mode 1)


(use-package windmove
  :init
  (message "loading windmove!")
  :ensure t
  :bind
  (("C-x <right>" . windmove-right)
   ("C-x <left>" . windmove-left)
   ("C-x <up>" . windmove-up)
   ("C-x <down>" . windmove-down)
   ))
; ---- navigate text and goto 
(use-package avy
  :init
  (message "loading avy!")
  :ensure t
  :bind
  ("C-." . avy-goto-char)
  ("C-c ." . avy-goto-char-2)
  :config
  (avy-setup-default)  
  )
(global-set-key "\C-cg" 'avy-goto-line)
(global-set-key (kbd "C-c C-j") 'avy-resume)

;; https://github.com/browse-kill-ring/browse-kill-ring
(use-package browse-kill-ring
  :init
  (message "loading browse-kill-ring!")
  :ensure t
  :custom
  (setq browse-kill-ring-quit-action 'save-and-restore)
  (browse-kill-ring-highlight-current-entry t)
  )


;; disable automatic infinite recursion
(setq highlight-indent-guides-auto-enabled nil)

(use-package highlight-indent-guides
  :ensure t
  :hook (prog-mode . highlight-indent-guides-mode)
  :config
  (setq highlight-indent-guides-method 'character)
  (add-hook 'after-load-theme-hook #'highlight-indent-guides-auto-set-faces))

;; (use-package highlight-indent-guides
;;   :init
;;   (message "loading hightlight-indent-guides!")
;;   :ensure t
;;   :hook prog-mode-hook highlight-indent-guides-mode
;;   :custom
;;   (setq highlight-indent-guides-character "|")
;;   )

(use-package smartparens
  :init
  (message "loading smartparents")
  (smartparens-global-mode)
  :ensure t
  :diminish smartparens-mode
  )

(use-package autorevert
  :init
  (message "loading autorevert!")
  :ensure t
  :custom
  (auto-revert-use-notify nil)
  :config
  (global-auto-revert-mode 1)
  )

(message"setup dictionaries")
;; ;; DICCTIONARIES
(let ((langs '("american" "francais" "german")))
  (setq lang-ring (make-ring (length langs)))
  (dolist (elem langs) (ring-insert lang-ring elem)))

(defun cycle-ispell-languages ()
  (interactive)
  (let ((lang (ring-ref lang-ring -1)))
    (ring-insert lang-ring lang)
    (ispell-change-dictionary lang)))

(global-set-key [f6] 'cycle-ispell-languages)

(message "disable flycheck and flymake")
(setq flycheck-mode nil)
(setq flymake-mode nil)

(use-package setup-dired
  :init
  (message "Loading setup-dired!")
  :defer t
  )

(use-package multiple-cursors
  :init
  (message "loading multiple-cursors!")
  :ensure t
  :defer 5)

;; =================== windows ===============
(message "Finished loading window_editing")
(provide 'window_editing)
;;; window_editing.el ends here
