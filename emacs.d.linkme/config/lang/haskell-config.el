(require 'haskell-mode)

(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)

(if (require 'shm nil 'noerror)
    (progn
      ;; Faces for solarized light..
      (add-hook 'haskell-mode-hook 'structured-haskell-mode))
  (add-hook 'haskell-mode-hook 'turn-on-haskell-indent))

(provide 'haskell-config)
