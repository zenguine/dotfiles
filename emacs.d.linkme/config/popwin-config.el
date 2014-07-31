;;; -*- lexical-binding: t -*-
(setq lexical-binding t)

(require 'popwin)

(setq display-buffer-function 'popwin:display-buffer)

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

(push '("^\*pytest.+\*$" :regexp t :height 30 :noselect t) popwin:special-display-config)
(push '("*GHC Info*" :noselect t) popwin:special-display-config)
(push '("*Warnings*" :noselect t) popwin:special-display-config)
(push '("^\*ag.+\*$" :regexp t :height 30 :noselect t) popwin:special-display-config)
(push '("*Backtrace*" :height 30) popwin:special-display-config)
(push '("*Org-Babel Results*" :height 30) popwin:special-display-config)
(push '("*jedi:doc*" :height 30) popwin:special-display-config)
(push '("*Help*" :height 15 :stick t) popwin:special-display-config)
(push '("*Org Agenda*" :width 0.5 :position right :stick t) popwin:special-display-config)

(provide 'popwin-config)
