(require 'company)

(setq company-idle-delay 0)
(setq company-selection-wrap-around t)

(define-key company-active-map (kbd "C-n") 'company-select-next)
(define-key company-active-map (kbd "C-p") 'company-select-previous)
(define-key company-active-map (kbd "SPC") 'company-complete-selection)
(define-key company-active-map (kbd "C-w") 'backward-kill-word)

(add-hook 'after-init-hook 'global-company-mode)
(when (require 'company-ghc nil 'noerror)
  (add-to-list 'company-backends 'company-ghc)
  (custom-set-variables '(company-ghc-show-info t)))

(provide 'company-config)
