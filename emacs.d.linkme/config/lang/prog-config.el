(setq project-root-markers '(".git" ".svn"))
(setq tags-revert-without-query t)

(add-hook 'prog-mode-hook (lambda () (setq show-trailing-whitespace t)))

(setq indent-tabs-mode nil)
(setq tab-width 4)
(setq c-basic-offset 4)

(add-to-list 'auto-mode-alist '("\\.hscm\\'" . scheme-mode))
(add-to-list 'completion-ignored-extensions ".hi")
(add-to-list 'completion-ignored-extensions ".pyc")
(add-to-list 'completion-ignored-extensions ".o")

(provide 'prog-config)
