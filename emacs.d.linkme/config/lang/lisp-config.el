(define-key emacs-lisp-mode-map (kbd "C-c C-e") 'pp-macroexpand-last-sexp)

(defun my-elisp-mode-hook ()
  (modify-syntax-entry ?- "w")
  (pretty-mode t)
  (rainbow-delimiters-mode t)
  (elisp-slime-nav-mode t))

(defun my-jump-to-elisp-docs (sym-name)
  "Jump to a pane and do elisp-slime-nav-describe-elisp-thing-at-point"
  (interactive (list (elisp-slime-nav--read-symbol-at-point)))
  (help-xref-interned (intern sym-name))
  (switch-to-buffer-other-window "*Help*" t))

(after 'evil
       (evil-define-key 'normal emacs-lisp-mode-map "K" 'my-jump-to-elisp-docs))


(add-hook 'emacs-lisp-mode-hook 'my-elisp-mode-hook)

(defun my-lisp-interaction-mode-hook ()
  (local-unset-key (kbd "C-j"))
  (define-key emacs-lisp-mode-map (kbd "C-0") 'eval-defun)
  (pretty-mode t)
  (rainbow-delimiters-mode t)
  (elisp-slime-nav-mode t))

(add-hook 'lisp-interaction-mode-hook 'my-lisp-interaction-mode-hook)
(add-hook 'ielm-mode-hook 'my-lisp-interaction-mode-hook)
(add-hook 'ielm-mode-hook 'turn-on-eldoc-mode)
(add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)

(provide 'lisp-config)
