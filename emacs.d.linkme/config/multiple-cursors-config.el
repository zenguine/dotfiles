(require 'multiple-cursors)
;; for evil compatibility
(setq mc/unsupported-minor-modes '(company-mode auto-complete-mode flyspell-mode jedi-mode))
(after 'evil
       (add-hook 'multiple-cursors-mode-enabled-hook 'evil-emacs-state)
       (add-hook 'multiple-cursors-mode-disabled-hook 'evil-normal-state))

(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

(provide 'multiple-cursors-config)
