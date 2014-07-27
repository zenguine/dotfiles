(defun my-elisp-mode-hook ()
  (modify-syntax-entry ?- "w")
  (pretty-mode t)
  (rainbow-delimiters-mode t))

(add-hook 'emacs-lisp-mode-hook 'my-elisp-mode-hook)

(defun my-lisp-interaction-mode-hook ()
  (local-unset-key (kbd "C-j"))
  (define-key emacs-lisp-mode-map (kbd "C-0") 'eval-defun)
  (pretty-mode t)
  (rainbow-delimiters-mode t))

(add-hook 'lisp-interaction-mode-hook 'my-lisp-interaction-mode-hook)
(add-hook 'ielm-mode-hook 'my-lisp-interaction-mode-hook)
(add-hook 'ielm-mode-hook 'turn-on-eldoc-mode)
(add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)

(provide 'lisp-config)
