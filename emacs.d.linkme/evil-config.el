(require 'evil)
(require 'cl)

(evil-define-command cofi/maybe-exit ()
  :repeat change
  (interactive)
  (let ((modified (buffer-modified-p)))
    (insert "j")
    (let ((evt (read-event (format "Insert %c to exit insert state" ?k)
			   nil 0.5)))
      (cond
       ((null evt) (message ""))
       ((and (integerp evt) (char-equal evt ?k))
	(delete-char -1)
	(set-buffer-modified-p modified)
	(push 'escape unread-command-events))
       (t (setq unread-command-events (append unread-command-events
					      (list evt))))))))

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

(define-key evil-normal-state-map "]o" 'occur-next)
(define-key evil-normal-state-map "[o" 'occur-prev)
(define-key evil-normal-state-map "]s" 'forward-sexp)
(define-key evil-normal-state-map "[s" 'backward-sexp)
(define-key evil-normal-state-map "]e" 'next-error)
(define-key evil-normal-state-map "[e" 'previous-error)

(define-key evil-normal-state-map (kbd "S-SPC") 'ace-jump-mode)
(define-key evil-normal-state-map (kbd "C-SPC") 'ace-jump-char-mode)
(define-key evil-normal-state-map (kbd "SPC j") 'ace-jump-mode)
(define-key evil-normal-state-map (kbd "SPC J") 'ace-jump-char-mode)

(define-key evil-normal-state-map " c" 'evil-window-delete)
(define-key evil-normal-state-map "  " 'smex)
(define-key evil-visual-state-map "  " 'smex)
(define-key evil-normal-state-map " wo" 'delete-other-windows)
(define-key evil-normal-state-map " 1" 'delete-other-windows)
(define-key evil-normal-state-map " k" 'kill-this-buffer)
(define-key evil-normal-state-map " x" 'delete-single-window)
(define-key evil-normal-state-map " b" 'ido-switch-buffer)
(define-key evil-normal-state-map " f" 'ido-find-file)
(define-key evil-normal-state-map " ev" (lambda () (interactive)
					  (find-file "~/.vimrc")))
(define-key evil-normal-state-map " ee" (lambda () (interactive)
					  (find-file "~/.emacs.d/init.el")))

(define-key evil-normal-state-map " l" 'linum-mode)

(define-key evil-normal-state-map "gb" 'ido-switch-buffer)
(define-key evil-normal-state-map "gs" 'eshell-here)
(define-key evil-normal-state-map "g$" 'shell)
(define-key evil-normal-state-map "gS" 'multi-term-next)

(define-key evil-normal-state-map (kbd ", SPC") 'switch-to-other-buffer)
(define-key evil-normal-state-map (kbd "SPC ,") 'switch-to-other-buffer)

; Key-chord bindings
(key-chord-define evil-emacs-state-map (kbd "SPC SPC") 'smex)
(key-chord-define evil-emacs-state-map (kbd "SPC c") 'evil-window-delete)
(key-chord-define evil-emacs-state-map (kbd "SPC 1") 'delete-other-windows)
(key-chord-define evil-emacs-state-map (kbd "SPC k") 'kill-this-buffer)
(key-chord-define evil-emacs-state-map (kbd "SPC x") 'delete-single-window)
(key-chord-define evil-emacs-state-map (kbd "SPC f") 'ido-find-file)
(key-chord-define evil-emacs-state-map (kbd "SPC b") 'ido-switch-buffer)
(key-chord-define evil-emacs-state-map (kbd "SPC ,") 'switch-to-other-buffer)

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
