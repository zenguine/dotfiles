(require 'uniquify)

;; Evil nerd commenter config
(setq evilnc-hotkey-comment-operator " c")
(require 'evil-nerd-commenter)
(global-set-key (kbd "M-;") 'evilnc-comment-or-uncomment-lines)

(setq uniquify-buffer-name-style 'post-forward-angle-brackets)
(turn-on-eldoc-mode)
(setq project-root-markers '(".git" ".svn"))
(setq tags-revert-without-query t)

(setq auto-save-file-name-transforms `((".*" ,temporary-file-directory t))
      backup-directory-alist `((".*" . ,temporary-file-directory)))

(add-hook 'prog-mode-hook (lambda () (setq show-trailing-whitespace t)))

(setq indent-tabs-mode nil)
(setq tab-width 4)
(setq c-basic-offset 4)

(add-to-list 'auto-mode-alist '("\\.hscm\\'" . scheme-mode))
(add-to-list 'completion-ignored-extensions ".hi")
(add-to-list 'completion-ignored-extensions ".pyc")
(add-to-list 'completion-ignored-extensions ".o")

(provide 'prog-config)
