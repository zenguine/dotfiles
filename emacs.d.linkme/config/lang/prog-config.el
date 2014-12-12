(require 'uniquify)

;; Evil nerd commenter config
(setq evilnc-hotkey-comment-operator "\\")
(require 'evil-nerd-commenter)
(global-set-key (kbd "M-;") 'evilnc-comment-or-uncomment-lines)

(setq uniquify-buffer-name-style 'post-forward-angle-brackets)
(turn-on-eldoc-mode)
(setq project-root-markers '(".git" ".svn"))

;; Tags configuration
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

;; Compilation mode stuff
(require 'ansi-color)

(defun colorize-compilation-buffer ()
  (toggle-read-only)
  (ansi-color-apply-on-region (point-min) (point-max))
  (toggle-read-only))

(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)


(setq compilation-scroll-output t)


(provide 'prog-config)
