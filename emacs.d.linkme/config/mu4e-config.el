(req-package mu4e
  :require smtpmail
  :load-path "site-lisp/mu4e"
  :defer t
  :bind ("C-c m" . mu4e)
  :ensure nil
  :config
  ;; default
  (setq mail-user-agent 'mu4e-user-agent)
  (setq mu4e-maildir "~/.mail")
  (setq mu4e-drafts-folder "/drafts")
  (setq mu4e-sent-folder   "/sent")
  (setq mu4e-trash-folder  "/archive")

  (setq mu4e-headers-seen-mark '("S" . "☑")
        mu4e-headers-new-mark '("N" .  "✉")
        mu4e-headers-replied-mark '("R" . "↵")
        mu4e-headers-passed-mark '("P" . "⇉")
        mu4e-headers-encrypted-mark '("x" . "⚷")
        mu4e-headers-signed-mark '("s" . "✍")
        mu4e-headers-empty-parent-prefix '("-" . "◆")
        mu4e-headers-first-child-prefix '("\\" . "▶")
        mu4e-use-fancy-chars t)

                                        ; fix weird html2text is out of range error 'error in process filter: Args out of range: "Email\"", 7, 6'
                                        ; see: https://github.com/djcb/mu/issues/73
  (setq mu4e-html2text-command "html2text -nobs -width 72")
  (setq mu4e-view-prefer-html t)              ;; prefer html
  (setq mu4e-msg2pdf "~/.emacs.d/lib/mu4e/toys/msg2pdf/msg2pdf")

  ;; don't save message to Sent Messages, Gmail/IMAP takes care of this
  (setq mu4e-sent-messages-behavior 'delete)

  ;; setup some handy shortcuts
  ;; you can quickly switch to your Inbox -- press ``ji''
  ;; then, when you want archive some messages, move them to
  ;; the 'All Mail' folder by pressing ``ma''.

  (setq mu4e-maildir-shortcuts
        '( ("/INBOX"   . ?i)
           ("/sent"   . ?s)
           ("/drafts"   . ?d)
           ("/archive"    . ?a)))

  ;; allow for updating mail using 'U' in the main view:
  (setq mu4e-get-mail-command "offlineimap")
  (setq mu4e-update-interval 600)

  (setq mu4e-headers-skip-duplicates t)
  (setq mu4e-confirm-quit nil)

  ;; something about ourselves
  (setq
   user-mail-address "zenguine@gmail.com"
   user-full-name  "Justin Cullen"
   message-signature
   (concat
    "Justin Cullen\n"
    "Email: zenguine@gmail.com\n"
    "\n"))

  ;; sending mail -- replace USERNAME with your gmail username
  ;; also, make sure the gnutls command line utils are installed
  ;; package 'gnutls-bin' in Debian/Ubuntu

  ;; alternatively, for emacs-24 you can use:
  (setq message-send-mail-function 'smtpmail-send-it
        smtpmail-stream-type 'starttls
        smtpmail-default-smtp-server "smtp.gmail.com"
        smtpmail-smtp-server "smtp.gmail.com"
        smtpmail-smtp-service 587)

  ;; don't keep message buffers around
  (setq message-kill-buffer-on-exit t)

  (defun my-mu4e-mode-hook ()
    (setq show-trailing-whitespace nil)))

;; (req-package org-mu4e
;;   :require mu4e
;;   :defer t)

;;----------------------------------------------------------
;; ---- END Email client ----
;;----------------------------------------------------------

(provide 'mu4e-config)
