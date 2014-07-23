(setq evil-want-C-u-scroll t)
(setq evil-want-C-i-jump nil)
(setq evil-move-cursor-back nil)
(setq evil-overriding-maps nil)
(setq evil-intercept-maps nil)

(require 'evil)
(require 'cl)

(setq evil-emacs-state-cursor '("red" box))
(setq evil-normal-state-cursor '("green" box))
(setq evil-visual-state-cursor '("orange" box))
(setq evil-insert-state-cursor '("red" bar))
(setq evil-replace-state-cursor '("red" bar))
(setq evil-operator-state-cursor '("red" hollow))
(setq evil-search-module 'evil-search)
(setq evil-ex-search-vim-style-regexp t)
(setq evil-magic 'very-magic)

(defadvice evil-ex-search-next (after advice-for-evil-ex-search-next activate)
  (evil-scroll-line-to-center (line-number-at-pos)))

(defadvice evil-ex-search-previous (after advice-for-evil-ex-search-previous activate)
  (evil-scroll-line-to-center (line-number-at-pos)))

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
(define-key evil-insert-state-map (kbd "C-u") 'backward-kill-line)
(define-key evil-insert-state-map (kbd "C-o") 'yas-expand)
(define-key evil-insert-state-map "j" 'cofi/maybe-exit)
(define-key evil-normal-state-map "H" 'evil-first-non-blank)
(define-key evil-normal-state-map  (kbd "C-p") 'helm-for-files)

(define-key evil-operator-state-map (kbd "TAB") 'evil-jump-item)
(define-key evil-motion-state-map (kbd "TAB") 'evil-jump-item)
(define-key evil-normal-state-map (kbd "TAB") 'evil-jump-item)

;; Evil-jumper
(if (require 'evil-jumper nil 'noerror)
    (progn
      (define-key evil-motion-state-map (kbd "M-i") 'evil-jumper/forward)
      (define-key evil-motion-state-map (kbd "M-o") 'evil-jumper/backward)
      (define-key evil-normal-state-map (kbd "M-i") 'evil-jumper/forward)
      (define-key evil-normal-state-map (kbd "M-o") 'evil-jumper/backward))
  (define-key evil-normal-state-map (kbd "M-i") 'evil-jump-forward)
  (define-key evil-normal-state-map (kbd "M-o") 'evil-jump-backward)
  (define-key evil-motion-state-map (kbd "M-i") 'evil-jump-forward)
  (define-key evil-motion-state-map (kbd "M-o") 'evil-jump-backward))

(define-key evil-normal-state-map "L" 'evil-end-of-line)
(define-key evil-normal-state-map (kbd "zn")
  (lambda () (interactive) (progn
			     (move-end-of-line nil)
			     (newline-and-indent))))


(define-key evil-normal-state-map (kbd "zp")
  (lambda () (interactive) (progn
			     (move-beginning-of-line nil)
			     (newline-and-indent))))

(define-key evil-normal-state-map ")" 'forward-sexp)
(define-key evil-operator-state-map")" 'forward-sexp)
(define-key evil-motion-state-map")" 'forward-sexp)

(define-key evil-normal-state-map "(" 'backward-sexp)
(define-key evil-operator-state-map"(" 'backward-sexp)
(define-key evil-motion-state-map"(" 'backward-sexp)

; Evil "leader" mappings

(define-key evil-normal-state-map "]o" 'occur-next)
(define-key evil-normal-state-map "[o" 'occur-prev)
(define-key evil-normal-state-map "]s" 'forward-sexp)
(define-key evil-normal-state-map "[s" 'backward-sexp)
(define-key evil-normal-state-map "]e" 'next-error)
(define-key evil-normal-state-map "[e" 'previous-error)
(define-key evil-normal-state-map "gJ" 'evil-join)

(define-key evil-normal-state-map (kbd "S-SPC") 'ace-jump-mode)
(define-key evil-normal-state-map (kbd "C-SPC") 'ace-jump-char-mode)

(define-key evil-normal-state-map " c" 'evil-window-delete)
(define-key evil-normal-state-map "  " 'helm-M-x)
(define-key evil-visual-state-map "  " 'helm-M-x)
(define-key evil-normal-state-map " 1" 'delete-other-windows)
(define-key evil-normal-state-map " k" 'kill-this-buffer)
(define-key evil-normal-state-map " rb" 'rainbow-blocks-mode)
(define-key evil-normal-state-map " rd" 'rainbow-delimiters-mode)
(define-key evil-normal-state-map " x" 'delete-single-window)
(define-key evil-normal-state-map " b" 'ido-switch-buffer)
(define-key evil-normal-state-map " f" 'ido-find-file)
(define-key evil-normal-state-map " t" 'multi-term-toggle)
(define-key evil-normal-state-map " ev" (lambda () (interactive)
					  (find-file "~/.vimrc")))
(define-key evil-normal-state-map " ee" (lambda () (interactive)
					  (find-file "~/.emacs.d/init.el")))

(define-key evil-normal-state-map " l" 'linum-mode)
(define-key evil-normal-state-map (kbd "SPC TAB") (lambda () (interactive)
						    (switch-to-buffer "*scratch*")))

(define-key evil-normal-state-map "gb" 'ido-switch-buffer)
(define-key evil-normal-state-map "gs" 'eshell-here)
(define-key evil-normal-state-map "g$" 'shell)
(define-key evil-normal-state-map "gS" 'multi-term-next)

(define-key evil-normal-state-map (kbd "SPC ,") 'switch-to-other-buffer)

; Key-chord bindings
(key-chord-define evil-emacs-state-map " c" 'evil-window-delete)
(key-chord-define evil-emacs-state-map " t" 'multi-term-toggle)
(key-chord-define evil-emacs-state-map " 1" 'delete-other-windows)
(key-chord-define evil-emacs-state-map " k" 'kill-this-buffer)
(key-chord-define evil-emacs-state-map " x" 'delete-single-window)
(key-chord-define evil-emacs-state-map " f" 'ido-find-file)
(key-chord-define evil-emacs-state-map " b" 'ido-switch-buffer)
(key-chord-define evil-emacs-state-map " ," 'switch-to-other-buffer)

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
				 (eshell-mode . emacs)
				 (haskell-interactive-mode . emacs)
				 (idris-repl-mode . emacs))
	 do (evil-set-initial-state mode state))

(evil-mode 1)

(when (require 'surround nil 'noerror)
  (global-surround-mode nil))

(when (require 'evil-args nil 'noerror)
  ;; bind evil-args text objects
  (define-key evil-inner-text-objects-map "a" 'evil-inner-arg)
  (define-key evil-outer-text-objects-map "a" 'evil-outer-arg)

  ;; bind evil-forward/backward-args
  (define-key evil-normal-state-map "gl" 'evil-forward-arg)
  (define-key evil-normal-state-map "gh" 'evil-backward-arg)
  (define-key evil-motion-state-map "gl" 'evil-forward-arg)
  (define-key evil-motion-state-map "gh" 'evil-backward-arg)

  ;; bind evil-jump-out-args
  (define-key evil-normal-state-map "gk" 'evil-jump-out-args))

(provide 'evil-config)
