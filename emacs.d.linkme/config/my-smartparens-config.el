(req-package smartparens
  :require (smartparens-config evil)
  :config
  (let ((map smartparens-mode-map))
    (define-key map (kbd "C-k") nil)
    (define-key map (kbd "C-j") nil)
    (define-key map (kbd "M-?") 'sp-convolute-sexp)

    (define-key map (kbd "M-t") 'sp-transpose-sexp)
    (define-key map (kbd "M-k") 'sp-splice-sexp-killing-backward)
    (define-key map (kbd "M-j") 'sp-splice-sexp-killing-forward)
    (define-key map (kbd "M-l") 'sp-forward-slurp-sexp)
    (define-key map (kbd "M-h") 'sp-forward-barf-sexp)
    (define-key map (kbd "M-H") 'sp-backward-slurp-sexp)
    (define-key map (kbd "M-L") 'sp-backward-barf-sexp)

    (evil-define-key 'normal map
      "gt" 'sp-transpose-sexp
      "gT" 'sp-transpose-hybrid-sexp
      (kbd "M-t") 'sp-transpose-sexp
      (kbd "M-k") 'sp-splice-sexp-killing-backward
      (kbd "M-j") 'sp-splice-sexp-killing-forward
      (kbd "M-l") 'sp-forward-slurp-sexp
      (kbd "M-h") 'sp-forward-barf-sexp
      (kbd "M-H") 'sp-backward-slurp-sexp
      (kbd "M-L") 'sp-backward-barf-sexp
      (kbd "R") 'sp-raise-sexp
      (kbd "s") 'sp-splice-sexp
      (kbd "S") 'sp-split-sexp
      (kbd "J") 'sp-join-sexp
      (kbd ")") 'sp-forward-sexp
      (kbd "(") 'sp-backward-sexp
      (kbd "M-(") 'sp-backward-up-sexp
      (kbd "M-)") 'sp-up-sexp
      (kbd "C-(") 'sp-backward-down-sexp
      (kbd "C-)") 'sp-down-sexp)

    (evil-define-key 'operator map
      (kbd ")") 'sp-forward-sexp
      (kbd "(") 'sp-backward-sexp
      (kbd "M-(") 'sp-backward-up-sexp
      (kbd "M-)") 'sp-up-sexp
      (kbd "C-(") 'sp-backward-down-sexp
      (kbd "C-)") 'sp-down-sexp)

    (evil-define-key 'motion map
      (kbd ")") 'sp-forward-sexp
      (kbd "(") 'sp-backward-sexp
      (kbd "M-(") 'sp-backward-up-sexp
      (kbd "M-)") 'sp-up-sexp
      (kbd "C-(") 'sp-backward-down-sexp
      (kbd "C-)") 'sp-down-sexp)

    (evil-define-key 'insert map
      (kbd "M-t") 'sp-transpose-sexp
      (kbd "M-k") 'sp-splice-sexp-killing-backward
      (kbd "M-j") 'sp-splice-sexp-killing-forward
      (kbd "M-l") 'sp-forward-slurp-sexp
      (kbd "M-h") 'sp-forward-barf-sexp
      (kbd "M-H") 'sp-backward-slurp-sexp
      (kbd "M-L") 'sp-backward-barf-sexp)

    (evil-define-key 'emacs map
      (kbd "M-t") 'sp-transpose-sexp
      (kbd "M-k") 'sp-splice-sexp-killing-backward
      (kbd "M-j") 'sp-splice-sexp-killing-forward
      (kbd "M-l") 'sp-forward-slurp-sexp
      (kbd "M-h") 'sp-forward-barf-sexp
      (kbd "M-H") 'sp-backward-slurp-sexp
      (kbd "M-L") 'sp-backward-barf-sexp))

  (smartparens-global-strict-mode t)
  (show-paren-mode t))

(provide 'my-smartparens-config)

