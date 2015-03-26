(require 'smart-mode-line)

(setq sml/shorten-directory t)
(setq sml/shorten-modes t)
(setq sml/name-width 40)
(setq sml/mode-width 'full)
(setq sml/no-confirm-load-theme)
(setq sml/theme 'respectful)

(sml/setup)

(add-to-list 'sml/replacer-regexp-list '("^~/.dotfiles/emacs.d.linkme/" ":ED:"))

(provide 'sml-config)
