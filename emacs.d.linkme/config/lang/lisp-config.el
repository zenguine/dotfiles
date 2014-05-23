(defun my-elisp-mode-hook ()
  (modify-syntax-entry ?- "w"))

(add-hook 'emacs-lisp-mode-hook 'my-elisp-mode-hook)

(defun my-lisp-interaction-mode-hook ()
  (local-unset-key (kbd "C-j")))

(add-hook 'lisp-interaction-mode-hook 'my-lisp-interaction-mode-hook)

(provide 'lisp-config)
