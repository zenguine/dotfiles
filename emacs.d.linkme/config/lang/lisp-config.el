(defun my-elisp-mode-hook ()
  (modify-syntax-entry ?- "w")
  (rainbow-delimiters-mode t))

(add-hook 'emacs-lisp-mode-hook 'my-elisp-mode-hook)

(defun my-lisp-interaction-mode-hook ()
  (local-unset-key (kbd "C-j"))
  (rainbow-delimiters-mode t))

(add-hook 'lisp-interaction-mode-hook 'my-lisp-interaction-mode-hook)
(add-hook 'ielm-mode-hook 'my-lisp-interaction-mode-hook)

(provide 'lisp-config)
