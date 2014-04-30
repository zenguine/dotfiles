;; Show the current function name in the header line
;; (which-function-mode)
(setq-default header-line-format
	      '((which-func-mode ("" which-func-format " "))))
(setq mode-line-misc-info
	    ;; We remove Which Function Mode from the mode line, because it's mostly
	    ;; invisible here anyway.
	    (assq-delete-all 'which-func-mode mode-line-misc-info))

;; Sublimity
(require 'sublimity)
(require 'sublimity-scroll)

(setq sublimity-scroll-weight 10
      sublimity-scroll-drift-length 3)

(setq sml/theme 'respectful)
(setq sml/shorten-directory t)
(setq sml/shorten-modes t)
(setq sml/name-width 40)
(setq sml/mode-width 'full)
(add-to-list 'sml/replacer-regexp-list '("^~/.dotfiles/emacs.d.linkme/" ":ED:"))
(add-to-list 'sml/replacer-regexp-list '("^~/.dotfiles/" ":DF:"))
(sml/setup)

(provide 'misc-config)
