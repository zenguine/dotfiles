(setq-default highlight-tabs t)
(setq inhibit-startup-message t)
(setq-default show-trailing-whitespace nil)

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; 'y' or 'n' instead of 'yes' or 'no'
(fset 'yes-or-no-p 'y-or-n-p)

(global-hl-line-mode 1)

(load-theme 'solarized-dark t)
(when (require 'nyan-mode nil 'noerror) (nyan-mode))
(when (require 'rainbow-mode nil 'noerror) (rainbow-mode))

;; font size
(set-face-attribute 'default nil :height 120)

(defun set-small-font ()
  (interactive)
  (set-face-attribute 'default nil :height 120))

(defun set-big-font ()
  (interactive)
  (set-face-attribute 'default nil :height 160))

(provide 'appearance-config)
