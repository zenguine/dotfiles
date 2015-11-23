(use-package erc
  :ensure t
  :defer t
  :init
  ; Commands to start erc ---------------------
  (defun start-irc (&optional confirm)
    "Connect to IRC."
    (interactive)
    (let ((confirmed (or confirm (y-or-n-p "Do you want to start IRC? "))))
      (when confirmed
        (erc :server "irc.freenode.net" :port 6667 :nick "zenguine"))))

  (defun start-or-switch-irc (&optional confirm)
    "Connect to IRC or switch to last active buffer in existing session"
    (interactive)
    (if (get-buffer "irc.freenode.net:6667")
        (erc-track-switch-buffer 1)
      (start-irc confirm)))
  :commands (start-irc start-or-switch-irc)
  :config
  (use-package erc-nick-colors)
  (use-package erc-log)
  (use-package erc-notify)

  (setq erc-autojoin-channels-alist '(("freenode.net" "#emacs" "#haskell" "#haskell-game" "#darkdynasty"
                                       "#evil-mode" "##machinelearning")))

                                        ; Other interesting ones maybe to add: #angularjs #nodejs #nimrod #go-nuts #security #infra-talk
                                        ;                                      ##math #archlinux #git ##javascript #bash ##networking

  (setq erc-kill-buffer-on-part t)
  (setq erc-kill-queries-on-quit t)
  (setq erc-kill-server-buffer-on-quit t)
  (setq erc-prompt (lambda () (concat (buffer-name) "> ")))

  ;; exclude boring stuff from tracking
  (erc-track-mode t)
  (setq erc-track-exclude-types '("JOIN" "NICK" "PART" "QUIT" "MODE"
                                  "324" "329" "332" "333" "353" "477"))

  (setq erc-hide-list '("JOIN" "PART" "QUIT" "NICK"))

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
          `((freenode (("zenguine" . ,zenguine-pass))))))

                                        ; Notifications ---------------------------------------------------------
  (autoload 'erc-nick-notify-mode "erc-nick-notify"
    "Minor mode that calls `erc-nick-notify-cmd' when his nick gets
mentioned in an erc channel" t)
  (eval-after-load 'erc '(erc-nick-notify-mode t))

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

  (use-package erc-match)
  (setq erc-keywords '("monad"))
  (setq erc-pals '("uint64_t"))

  (erc-match-mode t)

  (defun erc-match-notify (match-type usernickhost msg)
    (let* ((nick (first (split-string usernickhost "!")))
           (channel (erc-default-target)))
      (when (member match-type (list 'keyword 'pal))
        (message (format "%s on %s: %s" nick channel msg))
        (shell-command-to-string
         (format "notify-send -u critical '%s on %s:' '%s'" nick channel msg)))))

  (add-hook 'erc-text-matched-hook 'erc-match-notify)

                                        ; Nick coloring --------------------

)

(provide 'erc-config)
