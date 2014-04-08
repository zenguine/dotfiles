(require 'evil)
(require 'cl)

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

(define-key evil-normal-state-map ",c" 'evilnc-comment-or-uncomment-lines)
(define-key evil-visual-state-map ",c" 'evilnc-comment-or-uncomment-lines)


(define-key evil-normal-state-map "gb" 'ido-switch-buffer)
(define-key evil-normal-state-map "gs" 'eshell-here)
(define-key evil-normal-state-map "g$" 'shell)
(define-key evil-normal-state-map "gS" 'ansi-term)

(define-key evil-normal-state-map (kbd ", SPC") (lambda ()
						  (interactive)
						  (switch-to-buffer (other-buffer))))

(cl-loop for (mode . state) in '((inferior-emacs-lisp-mode . emacs)
				 (pylookup-mode . emacs)
				 (comint-mode . emacs)
				 (ebib-entry-mode . emacs)
				 (ebib-index-mode . emacs)
				 (ebib-log-mode . emacs)
				 (gtags-select-mode . emacs)
				 (shell-mode . emacs)
				 (term-mode . emacs)
				 (bc-menu-mode . emacs)
				 (magit-branch-manager-mode . emacs)
				 (semantic-symref-results-mode . emacs)
				 (rdictcc-buffer-mode . emacs)
				 (erc-mode . normal)
				 (eshell-mode . emacs))
	 do (evil-set-initial-state mode state))

(provide 'evil-config)
