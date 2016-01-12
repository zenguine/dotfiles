(use-package nix-mode
  :ensure t
  :commands (nix-mode))

(use-package company-nixos-options
  :ensure t)

(use-package helm-nixos-options
  :ensure t
  :bind ("C-c n" . helm-nixos-options))


(provide 'nix-config)
