(use-package color-identifiers-mode
    :ensure t
    ;; <spc>ri to toggle color-identifiers-mode
    :commands color-identifiers-mode)

(use-package prog-mode
  :commands prog-mode
  :init
  (add-to-list 'auto-mode-alist '("\\.hscm\\'" . scheme-mode))
  (add-to-list 'completion-ignored-extensions ".hi")
  (add-to-list 'completion-ignored-extensions ".pyc")
  (add-to-list 'completion-ignored-extensions ".o")
  :config
  (require 'uniquify)
  (setq uniquify-buffer-name-style 'post-forward-angle-brackets)

  (turn-on-eldoc-mode)

  (setq project-root-markers '(".git" ".svn"))

  ;; Tags configuration
  (setq tags-revert-without-query t)

  (setq auto-save-file-name-transforms `((".*" ,temporary-file-directory t))
	backup-directory-alist `((".*" . ,temporary-file-directory)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; General programming mode hooks
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (add-hook 'prog-mode-hook (lambda () (setq show-trailing-whitespace t)))
  ;; Start programming files with all folds closed in "outline" mode

  (defun setup-and-close-hs-folds ()
    (interactive)
    (hs-minor-mode t)
    (evil-close-folds))

  (defun my-prog-mode-hook ()
    (interactive)
    (hs-minor-mode t))

  (after 'evil
    (add-hook 'prog-mode-hook 'my-prog-mode-hook))

  ;; Indentation config
  (setq-default indent-tabs-mode nil)
  (setq tab-width 2)
  (setq c-basic-offset 2)


  ;; Compilation mode stuff
  (require 'ansi-color)

  (defun colorize-compilation-buffer ()
    (toggle-read-only)
    (ansi-color-apply-on-region (point-min) (point-max))
    (toggle-read-only))

  (add-hook 'compilation-filter-hook 'colorize-compilation-buffer)
  (setq compilation-scroll-output t))

(provide 'prog-config)
