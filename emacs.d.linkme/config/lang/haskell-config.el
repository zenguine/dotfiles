(require 'haskell-mode)

(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'inferior-haskell-mode-hook 'turn-on-ghci-completion)

(if (require 'shm nil 'noerror)
    (progn
      ;; Faces for solarized light..
      (set-face-background 'shm-current-face "#eee8d5")
      (set-face-background 'shm-quarantine-face "lemonchiffon")
      (add-hook 'haskell-mode-hook 'structured-haskell-mode))
  (add-hook 'haskell-mode-hook 'turn-on-haskell-indent))

(provide 'haskell-config)
