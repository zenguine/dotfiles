(use-package key-chord
  :ensure t
  :config
  (setq key-chord-two-keys-delay 0.015)
  (setq key-chord-one-key-delay 0.08)
  (key-chord-mode +1))

(provide 'key-chord-config)
