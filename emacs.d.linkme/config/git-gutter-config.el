(use-package git-gutter
  :ensure t
  :bind (("C-c g t" . git-gutter:toggle)
         ("C-c g T" . global-git-gutter-mode)
         ("C-c g P" . git-gutter:popup-hunk)

         ;; Jump to next/previous hunk
         ("C-c g p" . git-gutter:previous-hunk)
         ("C-c g n" . git-gutter:next-hunk)

         ;; Stage current hunk
         ("C-c g s" . git-gutter:stage-hunk)

         ;; Revert current hunk
         ("C-c g r" . git-gutter:revert-hunk))
  :config
  ;; If you enable global minor mode
  (global-git-gutter-mode t)

  ;; inactivate git-gutter-mode in asm-mode and image-mode

  (setq git-gutter:modified-sign "*")

  (set-face-foreground 'git-gutter:modified "dodger blue")
  (set-face-foreground 'git-gutter:added "#6ac214")
  (set-face-foreground 'git-gutter:deleted "tomato"))

(provide 'git-gutter-config)
