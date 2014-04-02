(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "C-x C-m") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)
(global-set-key (kbd "RET") 'newline-and-indent)
(global-set-key (kbd "C-c h") help-map)

(define-key evil-insert-state-map "j" 'cofi/maybe-exit)
(define-key evil-normal-state-map "H" 'evil-first-non-blank)
(define-key evil-normal-state-map  (kbd "C-p") 'fiplr-find-file)
(define-key evil-normal-state-map (kbd "TAB") 'evil-jump-item)
; Mapping for C-i
(define-key evil-normal-state-map (kbd "H-i") 'evil-jump-forward)
(define-key evil-normal-state-map "L" 'evil-end-of-line)
(define-key evil-normal-state-map (kbd "zn")
  (lambda () (interactive) (progn
			     (move-end-of-line nil)
			     (newline-and-indent))))


(define-key evil-normal-state-map (kbd "zp")
  (lambda () (interactive) (progn
			     (move-beginning-of-line nil)
			     (newline-and-indent))))

(define-key evil-normal-state-map (kbd "C-h") (lambda () (interactive) (move-window-or-create 'left)))
(define-key evil-normal-state-map (kbd "C-j") (lambda () (interactive) (move-window-or-create 'below)))
(define-key evil-normal-state-map (kbd "C-k") (lambda () (interactive) (move-window-or-create 'above)))
(define-key evil-normal-state-map (kbd "C-l") (lambda () (interactive) (move-window-or-create 'right)))

; Evil "leader" mappings

(define-key evil-normal-state-map ",wd" 'evil-window-delete)
(define-key evil-normal-state-map ",wo" 'delete-other-windows)
(define-key evil-normal-state-map ",bd" 'kill-this-buffer)
(define-key evil-normal-state-map ",bl" 'bs-show)
(define-key evil-normal-state-map ",ev" (lambda () (interactive)
					  (find-file "~/.vimrc")))
(define-key evil-normal-state-map ",ee" (lambda () (interactive)
					  (find-file "~/.emacs.d/init.el")))

(define-key evil-normal-state-map ",l" 'linum-mode)
(global-set-key (kbd "\\") 'evilnc-comment-or-uncomment-lines)
(define-key evil-normal-state-map ",c" 'evilnc-comment-or-uncomment-lines)
(define-key evil-visual-state-map ",c" 'evilnc-comment-or-uncomment-lines)


(define-key evil-normal-state-map "gb" 'ido-switch-buffer)
(define-key evil-normal-state-map "gs" 'shell)
(define-key evil-normal-state-map "g$" 'eshell)
(define-key evil-normal-state-map "gS" 'ansi-term)

(define-key evil-normal-state-map (kbd ", SPC") (lambda ()
						  (interactive)
						  (switch-to-buffer (other-buffer))))

(provide 'keybindings)
