(setq-default highlight-tabs t)
(setq inhibit-startup-message t)
(setq-default show-trailing-whitespace nil)

;; See http://bzg.fr/emacs-hide-mode-line.html
(defvar-local hidden-mode-line-mode nil)
(defvar-local hide-mode-line nil)
(setq-default line-spacing 2)

(define-minor-mode hidden-mode-line-mode
  "Minor mode to hide the mode-line in the current buffer."
  :init-value nil
  :global nil
  :variable hidden-mode-line-mode
  :group 'editing-basics
  (if hidden-mode-line-mode
      (setq hide-mode-line mode-line-format
            mode-line-format nil)
    (setq mode-line-format hide-mode-line
          hide-mode-line nil))
  (force-mode-line-update)
  ;; Apparently force-mode-line-update is not always enough to
  ;; redisplay the mode-line
  (redraw-display)
  (when (and (called-interactively-p 'interactive)
             hidden-mode-line-mode)
    (run-with-idle-timer
     0 nil 'message
     (concat "Hidden Mode Line Mode enabled.  "
             "Use M-x hidden-mode-line-mode to make the mode-line appear."))))

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; 'y' or 'n' instead of 'yes' or 'no'
(fset 'yes-or-no-p 'y-or-n-p)

(global-hl-line-mode 1)
(column-number-mode 1)

(load-theme 'gruvbox t)
(when (require 'nyan-mode nil 'noerror) (nyan-mode))
(when (require 'rainbow-mode nil 'noerror) (rainbow-mode))

;; Diminish -- stop minor modes from cluttering up my modeline
(require 'diminish)

(-each '(git-gutter-mode
	 smartparens-mode
	 projectile-mode
	 guide-key-mode
	 magit-auto-revert-mode
	 undo-tree-mode
	 haskell-doc-mode
	 ) 'diminish)

(-each '((haskell-indentation-mode . " ind")
	 (structured-haskell-mode . " shm")
	 (flycheck-mode . " flyc"))
  (lambda (x) (diminish (car x) (cdr x))))

;; font size
(set-face-attribute 'default nil :height 130)

(defun set-small-font ()
  (interactive)
  (set-face-attribute 'default nil :height 120))

(defun set-medium-font ()
  (interactive)
  (set-face-attribute 'default nil :height 140))

(defun set-big-font ()
  (interactive)
  (set-face-attribute 'default nil :height 160))

(provide 'appearance-config)
