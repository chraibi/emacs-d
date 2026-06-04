;;; early-init.el --- Pre-startup configuration -*- lexical-binding: t; -*-
;;; Commentary:
;; Runs before the package system and GUI are initialized.  Disabling the
;; GUI bars here (rather than in init.el) avoids momentary display of the
;; toolbar/menubar at startup, and raising the GC threshold speeds up init.
;;; Code:

;; We initialize packages manually in init.el.
(setq package-enable-at-startup nil)

;; Raise the GC threshold during startup; reset on `emacs-startup-hook'.
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)

;; Disable GUI elements early to prevent flicker.
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
(setq frame-inhibit-implied-resize t)

;;; early-init.el ends here
