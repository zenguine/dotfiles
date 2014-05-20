(require 'paredit)
(require 'evil-paredit)

(define-key paredit-mode-map (kbd "C-k") nil)
(define-key paredit-mode-map (kbd "C-j") nil)

(evil-define-key 'normal evil-paredit-mode-map
  (kbd "M-k") 'paredit-splice-sexp-killing-backward
  (kbd "M-j") 'paredit-splice-sexp-killing-forward
  (kbd "M-l") 'paredit-forward-slurp-sexp
  (kbd "M-h") 'paredit-forward-barf-sexp
  (kbd "M-H") 'paredit-backward-slurp-sexp
  (kbd "M-L") 'paredit-backward-barf-sexp
  (kbd "R") 'paredit-raise-sexp
  (kbd "s") 'paredit-splice-sexp
  (kbd "S") 'paredit-split-sexp
  (kbd "J") 'paredit-join)

(evil-define-key 'operator evil-paredit-mode-map
  (kbd ")") 'paredit-forward
  (kbd "(") 'paredit-backward)

(evil-define-key 'motion evil-paredit-mode-map
  (kbd ")") 'paredit-forward
  (kbd "(") 'paredit-backward)

(evil-define-key 'insert evil-paredit-mode-map
  (kbd "M-k") 'paredit-splice-sexp-killing-backward
  (kbd "M-j") 'paredit-splice-sexp-killing-forward
  (kbd "M-l") 'paredit-forward-slurp-sexp
  (kbd "M-h") 'paredit-forward-barf-sexp
  (kbd "M-H") 'paredit-backward-slurp-sexp
  (kbd "M-L") 'paredit-backward-barf-sexp
  )

(defun my-enable-paredit ()  (interactive)  (paredit-mode t)  (evil-paredit-mode t))

(add-hook 'emacs-lisp-mode-hook 'my-enable-paredit)
(add-hook 'scheme-mode-hook 'my-enable-paredit)

(provide 'paredit-config)
