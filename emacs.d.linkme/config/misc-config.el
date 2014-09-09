;; Sublimity
(require 'sublimity)
(recentf-mode t)
(semantic-mode t)

; no backup files
(setq make-backup-files nil
      backup-inhibited t
      auto-save-default nil
      create-lockfiles nil)

(put 'narrow-to-region 'disabled nil)

(setq undo-tree-auto-save-history t)
(setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo")))

(setq ace-jump-mode-scope 'window)
(set-fringe-mode 0)

(provide 'misc-config)
