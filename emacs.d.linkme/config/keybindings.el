(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)
(global-set-key (kbd "RET") 'newline-and-indent)
(global-set-key (kbd "C-c h") help-map)
(define-key help-map (kbd "d") 'helm-descbinds)
(define-key help-map (kbd "C-m") 'discover-my-major)

(global-set-key (kbd "C-x C-b") 'helm-buffers-list)
(global-set-key (kbd "C-x b") 'helm-buffers-list)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-x C-m") 'helm-M-x)
(global-set-key (kbd "C-c h a") 'helm-apropos)
(global-set-key (kbd "C-c o") 'helm-occur)
(global-set-key (kbd "C-c C-o") 'helm-multi-occur-in-this-mode)
(global-set-key (kbd "C-c i") 'helm-imenu)

(global-set-key (kbd "C-c t") 'multi-term-toggle)
(global-set-key (kbd "C-c T") 'multi-term)
(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "C-c e") 'start-or-switch-irc)
(global-set-key (kbd "C-c m") 'mu4e)
(global-set-key (kbd "C-c p s") 'projectile-switch-project)
(global-set-key (kbd "C-c C-n") 'flycheck-next-error)
(global-set-key (kbd "C-c C-p") 'flycheck-previous-error)
(global-set-key (kbd "C-,") popwin:keymap)
(global-set-key (kbd "M-e") 'eval-region)
(global-set-key (kbd "C-c y") 'yas-describe-tables)
(global-set-key (kbd "C-c C-\\") 'erc-track-switch-buffer)
(global-set-key (kbd "C-x k") (lambda ()
				(interactive)
				(kill-buffer (current-buffer))))

(global-set-key (kbd "C-\\") 'universal-argument)
(define-key universal-argument-map (kbd "C-\\") 'universal-argument-more)

;; Org mode bindings
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)
(global-set-key "\C-cc" 'org-capture)
(global-set-key (kbd "C-c C-x C-j") 'org-clock-goto)
(global-set-key (kbd "<f12>") 'visible-mode)
(global-set-key (kbd "<f9> i") 'bh/punch-in)
(global-set-key (kbd "<f9> o") 'bh/punch-out)

(global-set-key (kbd "C-h") (lambda () (interactive) (move-window-or-create 'left)))
(global-set-key (kbd "C-j") (lambda () (interactive) (move-window-or-create 'below)))
(global-set-key (kbd "C-k") (lambda () (interactive) (move-window-or-create 'above)))
(global-set-key (kbd "C-l") (lambda () (interactive) (move-window-or-create 'right)))

(provide 'keybindings)
