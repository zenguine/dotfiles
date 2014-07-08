(require 'lisp-config)
(require 'quack)
(setq scheme-program-name "mit-scheme")
(setq quack-fontify-style nil)

(when (require 'evil nil 'noerror)
  (evil-define-key 'insert scheme-mode-map (kbd "SPC") 'just-one-space)
  (evil-define-key 'emacs scheme-mode-map (kbd "SPC") 'just-one-space)
  (evil-define-key 'normal scheme-mode-map (kbd "S-RET")))

(defun my-inferior-scheme-mode-hook ()
  (rainbow-delimiters-mode t))

(add-hook 'inferior-scheme-mode 'my-inferior-scheme-mode-hook)

(provide 'scheme-config)
