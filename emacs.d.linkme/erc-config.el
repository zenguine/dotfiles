(require 'erc)
(require 'erc-log)
(require 'erc-notify)

(setq erc-autojoin-channels-alist '(("freenode.net"
				     "#emacs" "#haskell" "#python"
				     "#vim" "##javascript")))

(setq erc-kill-buffer-on-part t)
(setq erc-kill-queries-on-quit t)
(setq erc-kill-server-buffer-on-quit t)
(setq erc-prompt (lambda () (concat (buffer-name) "> ")))

;; exclude boring stuff from tracking
(erc-track-mode t)
(setq erc-track-exclude-types '("JOIN" "NICK" "PART" "QUIT" "MODE"
				"324" "329" "332" "333" "353" "477"))

(setq erc-hide-list '("JOIN" "PART" "NICK"))

;; logging
(setq erc-log-channels-directory "~/.erc/logs/")

(if (not (file-exists-p erc-log-channels-directory))
    (mkdir erc-log-channels-directory t))

(setq erc-save-buffer-on-part t)
(defadvice save-buffers-kill-emacs (before save-logs (arg) activate)
  (save-some-buffers t (lambda () (when (eq major-mode 'erc-mode) t))))

;; truncate long irc buffers
(erc-truncate-mode +1)

;; auto identify
(when (file-exists-p (expand-file-name "~/.ercpass"))
  (load "~/.ercpass")
  (require 'erc-services)
  (erc-services-mode 1)
  (setq erc-prompt-for-nickserv-password nil)
  (setq erc-nickserv-passwords
	`((freenode (("jcullen" . ,jcullen-pass))))))

;; Notifications
(autoload 'erc-nick-notify-mode "erc-nick-notify"
  "Minor mode that calls `erc-nick-notify-cmd' when his nick gets
mentioned in an erc channel" t)
(eval-after-load 'erc '(erc-nick-notify-mode t))

;; private message notification
(defun erc-notify-on-private-msg (proc parsed)
  (let ((nick (car (erc-parse-user (erc-response.sender parsed))))
	(target (car (erc-response.command-args parsed)))
	(msg (erc-response.contents parsed)))
    (when (and (erc-current-nick-p target)
	       (not (erc-is-message-ctcp-and-not-action-p msg)))
      (shell-command-to-string
       (format "notify-send -u critical '%s says:' '%s'" nick msg))
      nil)))

(add-hook 'erc-server-PRIVMSG-functions 'erc-notify-on-private-msg)

(defun start-irc (&optional confirm)
  "Connect to IRC."
  (interactive)
  (let ((confirmed (or confirm (y-or-n-p "Do you want to start IRC? "))))
    (when confirmed
      (erc :server "irc.freenode.net" :port 6667 :nick "jcullen"))))

(defun start-or-switch-irc (&optional confirm)
  "Connect to IRC or switch to last active buffer in existing session"
  (interactive)
  (if (get-buffer "irc.freenode.net:6667")
      (erc-track-switch-buffer 1)
    (start-irc confirm)))

(provide 'erc-config)
