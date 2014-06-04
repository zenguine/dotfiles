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

(push '("^\*pytest.+\*$" :regexp t :height 20 :noselect t) popwin:special-display-config)
(push '("^\*ag.+\*$" :regexp t :height 20 :noselect t) popwin:special-display-config)
(push '("^\*helm.+\*$" :regexp t :height 20) popwin:special-display-config)
(push '("*Backtrace*" :height 20) popwin:special-display-config)
(push '("*Org-Babel Results*" :height 20) popwin:special-display-config)
(push '("*jedi:doc*" :height 20) popwin:special-display-config)

(defadvice popwin:close-popup-window (before advice-for-before-close-popup-window activate)
  (when popwin:popup-buffer
    (popwin/maybe-kill-buffer popwin:popup-buffer)))

(provide 'popwin-config)
