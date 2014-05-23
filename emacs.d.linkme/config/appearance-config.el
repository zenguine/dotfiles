(show-paren-mode t)
(setq-default highlight-tabs t)
(setq inhibit-startup-message t)
(setq-default show-trailing-whitespace nil)

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; 'y' or 'n' instead of 'yes' or 'no'
(fset 'yes-or-no-p 'y-or-n-p)

(global-hl-line-mode 1)

(when (require 'solarized nil 'noerror)
  (load-theme 'solarized-dark t))

(provide 'appearance-config)
