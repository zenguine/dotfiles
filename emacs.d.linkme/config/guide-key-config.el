(use-package guide-key
  :ensure t
  :config
  (setq guide-key/guide-key-sequence '("C-x" "C-c"))
  (setq guide-key/recursive-key-sequence-flag t)
  (guide-key-mode 1))

(provide 'guide-key-config)
