;; Sublimity
(require 'sublimity)
(require 'sublimity-scroll)

; no backup files
(setq make-backup-files nil
      backup-inhibited t
      auto-save-default nil
      create-lockfiles nil)

(put 'narrow-to-region 'disabled nil)

(setq undo-tree-auto-save-history t)
(setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo")))

(setq ace-jump-mode-scope 'window)
(setq sublimity-scroll-weight 5
      sublimity-scroll-drift-length 1)

(sublimity-mode 1)

(provide 'misc-config)
