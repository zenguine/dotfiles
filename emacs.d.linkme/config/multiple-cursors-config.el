(use-package multiple-cursors
  :ensure t
  :bind (("C-S-c C-S-c" . mc/edit-lines)
         ("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-c C-<" . mc/mark-all-like-this))
  :config
  ;; for evil compatibility
  (setq mc/unsupported-minor-modes '(company-mode auto-complete-mode flyspell-mode jedi-mode))

  (after 'evil
    (add-hook 'multiple-cursors-mode-enabled-hook 'evil-emacs-state)
    (add-hook 'multiple-cursors-mode-disabled-hook 'evil-normal-state)))

(provide 'multiple-cursors-config)
