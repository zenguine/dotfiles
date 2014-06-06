(require 'git-gutter)

;; If you enable global minor mode
(global-git-gutter-mode t)

(global-set-key (kbd "C-c g t") 'git-gutter:toggle)
(global-set-key (kbd "C-c g T") 'global-git-gutter-mode)
(global-set-key (kbd "C-c g P") 'git-gutter:popup-hunk)

;; Jump to next/previous hunk
(global-set-key (kbd "C-c g p") 'git-gutter:previous-hunk)
(global-set-key (kbd "C-c g n") 'git-gutter:next-hunk)

;; Stage current hunk
(global-set-key (kbd "C-c g s") 'git-gutter:stage-hunk)

;; Revert current hunk
(global-set-key (kbd "C-c g r") 'git-gutter:revert-hunk)

;; inactivate git-gutter-mode in asm-mode and image-mode

(setq git-gutter:modified-sign "*")

(set-face-foreground 'git-gutter:modified "dodger blue")
(set-face-foreground 'git-gutter:added "#6ac214")
(set-face-foreground 'git-gutter:deleted "tomato")

(provide 'git-gutter-config)
