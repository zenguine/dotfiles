;;; -*- lexical-binding: t -*-
(setq lexical-binding t)

(require 'popwin)

(setq display-buffer-function 'popwin:display-buffer)

(defun make-buffer-name-equal-p (name)
  (lambda (buf)
    (equal (buffer-name buf) name)))

(setq popwin/cancel-buffer-kill-funcs '())
(push (const nil) popwin/cancel-buffer-kill-funcs)

;; Names of buffers which to not kill when popwin window is closed
(dolist (name '("*Messages*" "*Occur*" "*grep*"))
  (push (make-buffer-name-equal-p name) popwin/cancel-buffer-kill-funcs))

(defun popwin/maybe-kill-buffer (buf)
  (interactive)
  (when (not (some (lambda (pred) (funcall pred buf)) popwin/cancel-buffer-kill-funcs))
    (kill-buffer popwin:popup-buffer)))

(popwin-mode 1)

(defadvice popwin:close-popup-window (before advice-for-before-close-popup-window activate)
  (when popwin:popup-buffer
    (popwin/maybe-kill-buffer popwin:popup-buffer)))

(provide 'popwin-config)
