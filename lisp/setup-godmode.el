;;; setup-godmode.el --- God-mode integration (subtle Solarized Light UI) -*- lexical-binding: t; -*-

;;; Code:

(require 'god-mode)

(defgroup my-god-mode nil
  "Personal God-mode integration."
  :group 'editing)

;; Solarized Light-ish accents (subtle)
(defcustom my-god-mode-accent-underline "#2aa198" ; cyan
  "Underline color used to indicate God mode is active."
  :type 'string
  :group 'my-god-mode)

(defcustom my-god-mode-accent-foreground "#586e75" ; base01
  "Foreground color used for mode-line text when God mode is active."
  :type 'string
  :group 'my-god-mode)

(defcustom my-god-mode-accent-weight 'semi-bold
  "Font weight for the mode-line when God mode is active."
  :type '(choice (const :tag "Normal" normal)
                 (const :tag "Semi-bold" semi-bold)
                 (const :tag "Bold" bold))
  :group 'my-god-mode)

(defvar-local my-god-mode--modeline-remap-cookie nil
  "Face remap cookie for `mode-line` while God mode is active.")

;;;###autoload
(defun my-god-mode-update-cursor-type ()
  "Update `cursor-type` based on God mode and read-only state."
  (setq cursor-type (if (or god-local-mode buffer-read-only) 'box 'bar)))

(defun my-god-mode--apply-modeline-remap (enabled)
  "Subtly adjust mode-line for current buffer when ENABLED is non-nil."
  ;; Remove any prior remap first (idempotent).
  (when my-god-mode--modeline-remap-cookie
    (face-remap-remove-relative my-god-mode--modeline-remap-cookie)
    (setq my-god-mode--modeline-remap-cookie nil))
  (when enabled
    ;; Keep background untouched; add an underline and a mild fg/weight tweak.
    (setq my-god-mode--modeline-remap-cookie
          (face-remap-add-relative
           'mode-line
           :foreground my-god-mode-accent-foreground
           :weight my-god-mode-accent-weight
           :underline `(:color ,my-god-mode-accent-underline :position -1)))))

;;;###autoload
(defun my-god-mode-update-mode-line ()
  "Update current buffer's mode-line styling according to `god-local-mode`."
  (my-god-mode--apply-modeline-remap god-local-mode)
  (force-mode-line-update))

;;;###autoload
(defun my-god-mode-minibuffer-disable ()
  "Disable God mode in the minibuffer."
  (when (bound-and-true-p god-local-mode)
    (god-local-mode -1)))

;;;###autoload
(define-minor-mode my-god-mode-integration-mode
  "Global integration layer for God mode (keys, cursor, subtle mode-line)."
  :global t
  :lighter ""
  (if my-god-mode-integration-mode
      (progn
        ;; Keys
        (global-set-key (kbd "<escape>") #'god-local-mode)
        (with-eval-after-load 'god-mode
          (define-key god-local-mode-map (kbd "i") #'god-local-mode))

        ;; God-mode settings
        (setq god-mode-enable-function-key-translation nil)

        ;; Hooks
        (add-hook 'minibuffer-setup-hook #'my-god-mode-minibuffer-disable)
        (add-hook 'post-command-hook #'my-god-mode-update-cursor-type)

        (add-hook 'god-mode-enabled-hook  #'my-god-mode-update-mode-line)
        (add-hook 'god-mode-disabled-hook #'my-god-mode-update-mode-line)

        ;; Apply immediately
        (my-god-mode-update-cursor-type)
        (my-god-mode-update-mode-line))
    ;; Disable integration
    (remove-hook 'minibuffer-setup-hook #'my-god-mode-minibuffer-disable)
    (remove-hook 'post-command-hook #'my-god-mode-update-cursor-type)
    (remove-hook 'god-mode-enabled-hook  #'my-god-mode-update-mode-line)
    (remove-hook 'god-mode-disabled-hook #'my-god-mode-update-mode-line)

    ;; Cleanup remap in current buffer (others will clean up when hooks stop firing)
    (my-god-mode--apply-modeline-remap nil)
    (force-mode-line-update)))

;;;###autoload
(defun my-god-mode-setup ()
  "Convenience setup: enable `my-god-mode-integration-mode`."
  (my-god-mode-integration-mode 1))

(provide 'setup-godmode)

;;; setup-godmode.el ends here
