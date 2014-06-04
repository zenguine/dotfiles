(defun comint-delchar-or-eof-or-kill-buffer (arg)
  (interactive "p")
  (if (null (get-buffer-process (current-buffer)))
      (kill-buffer)
    (comint-delchar-or-maybe-eof arg)))

(defun comint-clear ()
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)
    (comint-send-input)))

(defun my-comint-mode-hook ()
  (local-set-key (kbd "C-u") 'comint-kill-input)
  (local-set-key (kbd "C-w") 'backward-kill-word)
  (local-set-key (kbd "C-c C-k") 'kill-line)
  (local-set-key (kbd "C-p") 'comint-previous-input)
  (local-set-key (kbd "C-n") 'comint-next-input)
  (local-set-key (kbd "C-c C-h") 'helm-comint-input-ring)
  (local-set-key (kbd "C-d") 'comint-delchar-or-eof-or-kill-buffer)
  (local-set-key (kbd "C-c C-l") 'comint-clear)
  (setq-local show-trailing-whitespace nil)
  (when (require 'evil nil 'noerror)
    (evil-define-key 'insert comint-mode-map
      (kbd "C-d") 'comint-delchar-or-eof-or-kill-buffer)))

(add-hook 'comint-mode-hook 'my-comint-mode-hook)

(provide 'comint-config)
