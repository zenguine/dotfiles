(require 'lisp-config)
(require 'quack)
(setq scheme-program-name "mit-scheme")
(setq quack-fontify-style nil)

(when (require 'evil nil 'noerror)
  (evil-define-key 'insert scheme-mode-map (kbd "SPC") 'just-one-space)
  (evil-define-key 'emacs scheme-mode-map (kbd "SPC") 'just-one-space)
  (define-key scheme-mode-map (kbd "C-0") 'scheme-send-definition))

;; Start the scheme interpreter with the SICM mechanics library loaded
(defun mechanics ()
  (interactive)
  (run-scheme
   "/usr/local/scmutils/mit-scheme/bin/scheme --library /usr/local/scmutils/mit-scheme/lib"))

(defun my-inferior-scheme-mode-hook ()
  (rainbow-delimiters-mode t))

(add-hook 'inferior-scheme-mode 'my-inferior-scheme-mode-hook)

(provide 'scheme-config)
