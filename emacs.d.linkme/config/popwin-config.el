;;; -*- lexical-binding: t -*-
(setq lexical-binding t)

(require 'popwin)

(setq display-buffer-function 'popwin:display-buffer)
(setq popwin:popup-window-position 'bottom)
(setq popwin:popup-window-height 0.3)
(setq popwin:popup-window-width 0.4)

(defun make-buffer-name-matches-regexp-p (re)
  (lambda (buf)
    (string-match re (buffer-name buf))))

(setq popwin/cancel-buffer-kill-funcs '())
(push (const nil) popwin/cancel-buffer-kill-funcs)

;; List of regexps to match buffer name if we dont want to kill it
(dolist (name '("^\\*Messages\\*$" "^\\*Occur\\*$" "^\\*grep\\*$" "^\\*ag.+\\*"))
  (push (make-buffer-name-matches-regexp-p name) popwin/cancel-buffer-kill-funcs))

(defun popwin/maybe-kill-buffer (buf)
  (interactive)
  (when (not (some (lambda (pred) (funcall pred buf)) popwin/cancel-buffer-kill-funcs))
    (kill-buffer popwin:popup-buffer)))

(popwin-mode 1)

(push '("^\\*pytest.+\\*$" :regexp t ::noselect t) popwin:special-display-config)
(push '("*git-messenger*" :noselect t) popwin:special-display-config)
(push '("*GHC Info*" :noselect t) popwin:special-display-config)
(push '("*skewer-error*" :noselect t :position bottom) popwin:special-display-config)
(push '("*company-documentation*" :position bottom :height 15 :noselect t) popwin:special-display-config)
(push '("*Warnings*" :noselect t) popwin:special-display-config)
(push '("^\\*ag.+\\*$" :regexp t :position bottom :height 0.4 :noselect t) popwin:special-display-config)
(push '("*Backtrace*") popwin:special-display-config)
(push '("*Org-Babel Results*" :position bottom :height 0.2) popwin:special-display-config)
(push '("*jedi:doc*") popwin:special-display-config)
(push '("*Help*" :position bottom :height 0.5 :stick t :noselect t) popwin:special-display-config)
(push '("*Org Agenda*" :width 0.5 :position right :stick t) popwin:special-display-config)

(provide 'popwin-config)
