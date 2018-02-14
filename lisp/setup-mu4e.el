(require 'mu4e)
(require 'mu4e-maildirs-extension)
(require 'alert)
(require 'gnus-dired)
(require 'smtpmail)
(require 'org-mu4e)

(mu4e-maildirs-extension)
(define-key global-map (kbd "<f1>") 'mu4e)

(setq mu4e-maildir "~/.Mail/Gmail/")
(setq mu4e-drafts-folder "/[Gmail].Drafts")
(setq mu4e-sent-folder   "/[Gmail].Sent Mail")
;; don't save message to Sent Messages, Gmail/IMAP takes care of this
(setq mu4e-sent-messages-behavior 'delete) ;; not for Juelich

;; allow for updating mail using 'U' in the main view:
(setq mu4e-get-mail-command "offlineimap")

;; shortcuts
(setq mu4e-maildir-shortcuts
    '( ("/INBOX"               . ?i)
       ("/sent"                . ?s)
       ("/[Gmail].Trash"       . ?t)
       ("/Allmail"    . ?a)
       ))

;; something about ourselves
(setq
   user-mail-address "m.chraibi@gmail.com"
   user-full-name  "Mohcine Chraibi"
   mu4e-compose-signature
    (concat
      "Best,\n"
      "Mohcine Chraibi\n"))

;; show images
(setq mu4e-show-images t)

;; use imagemagick, if available
(when (fboundp 'imagemagick-register-types)
  (imagemagick-register-types))

;; convert html emails properly
;; Possible options:
;;   - html2text -utf8 -width 72
;;   - textutil -stdin -format html -convert txt -stdout
;;   - html2markdown | grep -v '&nbsp_place_holder;' (Requires html2text pypi)
;;   - w3m -dump -cols 80 -T text/html
;;   - view in browser (provided below)
(setq mu4e-html2text-command "textutil -stdin -format html -convert txt -stdout")

(setq mu4e-date-format-long "%Y/%m/%d %H:%M:%S")
(setq mu4e-headers-date-format "%y/%m/%d %H:%M:%S")

;; spell check
(add-hook 'mu4e-compose-mode-hook
        (defun my-do-compose-stuff ()
           "My settings for message composition."
           (set-fill-column 72)
           (flyspell-mode)))

;; add option to view html message in a browser
;; `aV` in view to activate
(add-to-list 'mu4e-view-actions
  '("ViewInBrowser" . mu4e-action-view-in-browser) t)

;; fetch mail every 10 mins
(setq mu4e-update-interval 600)

;; Silly mu4e only shows names in From: by default. Of course we also
;; want the addresses.
(setq mu4e-view-show-addresses t)

(setq mu4e-attachment-dir  "~/Downloads/Mail")
(setq mu4e-view-prefer-html t)
(setq mu4e-use-fancy-chars t)
(setq mu4e-headers-skip-duplicates t)
;; Since mu4e cannot help in sending emails, we need to use smtpmail for the same:

;; configuration for sending mail
(setq message-send-mail-function 'smtpmail-send-it
     smtpmail-stream-type 'starttls
     smtpmail-default-smtp-server "smtp.gmail.com"
     smtpmail-smtp-server "smtp.gmail.com"
     smtpmail-smtp-service 587)

;; gpg
(add-hook 'mu4e-compose-mode-hook 'epa-mail-mode)
(add-hook 'mu4e-view-mode-hook 'epa-mail-mode)

;; don't keep message buffers around
(setq message-kill-buffer-on-exit t)

(setq mu4e-hide-index-messages t)


;; Borrowed from http://ionrock.org/emacs-email-and-mu.html
;; Choose account label to feed msmtp -a option based on From header
;; in Message buffer; This function must be added to
;; message-send-mail-hook for on-the-fly change of From address before
;; sending message since message-send-mail-hook is processed right
;; before sending message.
(defun choose-msmtp-account ()
  (if (message-mail-p)
      (save-excursion
        (let*
            ((from (save-restriction
                     (message-narrow-to-headers)
                     (message-fetch-field "from")))
             (account
              (cond
               ((string-match "m.chraibi@gmail.com" from) "gmail")
               ((string-match "m.chraibi@fz-juelich.de" from) "juelich")
               )
              ))
          (setq message-sendmail-extra-arguments (list '"-a" account))))))
(setq message-sendmail-envelope-from 'header)
(add-hook 'message-send-mail-hook 'choose-msmtp-account)


(require 'gnus-dired)
;; make the `gnus-dired-mail-buffers' function also work on
;; message-mode derived modes, such as mu4e-compose-mode
(defun gnus-dired-mail-buffers ()
  "Return a list of active message buffers."
  (let (buffers)
    (save-current-buffer
      (dolist (buffer (buffer-list t))
        (set-buffer buffer)
        (when (and (derived-mode-p 'message-mode)
                   (null message-sent-message-via))
          (push (buffer-name buffer) buffers))))
    (nreverse buffers)))

(setq gnus-dired-mail-mode 'mu4e-user-agent)
(add-hook 'dired-mode-hook 'turn-on-gnus-dired-mode)

;;https://github.com/NicolasPetton/emacs.d/blob/master/hosts/blueberry/init-mu4e.el
(defun nico/notify-new-email ()
  (alert
   "You have unread emails"
   :title "New mail!"
   :icon "~/bin/unread_mail.png"))

(add-hook 'mu4e-index-updated-hook #'nico/notify-new-email)


(defun select-and-insert-contact (&optional start)
  (interactive)
  (let ((mail-abbrev-mode-regexp mu4e~compose-address-fields-regexp)
        (eoh ;; end-of-headers
         (save-excursion
           (goto-char (point-min))
           (search-forward-regexp mail-header-separator nil t))))
    (when (and eoh (> eoh (point)) (mail-abbrev-in-expansion-header-p))
      (let* ((end (point))
             (start
              (or start
                  (save-excursion
                    (re-search-backward "\\(\\`\\|[\n:,]\\)[ \t]*")
                    (goto-char (match-end 0))
                    (point))))
             (contact
              (ido-completing-read "Contact: "
                                   mu4e~contacts-for-completion
                                   nil
                                   nil
                                   (buffer-substring-no-properties start end))))
        (unless (equal contact "")
          (kill-region start end)
          (insert contact))))))

(setq mu4e-refile-folder
  (lambda (msg)
    (cond
      ;; messages to the mu mailing list go to the /mu folder
      ((mu4e-message-contact-field-matches msg :from
         "no-reply@arXiv.org")
       "/arxiv")

      ((mu4e-message-contact-field-matches msg :from
         "Facebook\\|news@linkedin.com\\|LinkedIn\\|Google+\\|alert@chess.com\\|Deezer\\|♟Chess.com\\|noreply@medium.com\\|Intel")
       "/Junk")

      ;; messages sent directly to me go to /archive
      ;; also `mu4e-user-mail-address-regexp' can be used
      ;; ((mu4e-message-contact-field-matches msg :to "m.chraibi@gmail.com")
      ;;   "/Allmail")

      
      ;; messages with football or soccer in the subject go to /football
      ((string-match "facebook\\|chess\\|medium\\|linkedin" (or (mu4e-message-field msg :subject) ""))
        "/Junk")
      ;; everything else goes to /archive
      ;; important to have a catch-all at the end!
      (t "/Allmail"))))


(provide 'setup-mu4e)
;;; setup-mu4e.el ends here
