(defun eshell/x (&rest args)
  (delete-single-window))

(defun eshell/clear (&rest args)
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)
    (eshell-send-input)))

(defun my-eshell-mode-hook ()
  (local-set-key (kbd "C-u") 'eshell-kill-input)
  (local-set-key (kbd "C-w") 'backward-kill-word)
  (local-set-key (kbd "C-c C-k") 'kill-line)
  (local-set-key (kbd "C-p") 'eshell-previous-input)
  (local-set-key (kbd "C-n") 'eshell-next-input)
  (local-set-key (kbd "C-c C-h") 'helm-eshell-history)
  (local-set-key (kbd "C-c C-l") 'eshell/clear)
  (local-set-key (kbd "C-d") 'eshell-send-eof-to-process)
  (local-unset-key (kbd "C-j"))
  (setq-local show-trailing-whitespace nil)
  (add-to-list 'eshell-visual-commands "watch"))

(eval-after-load 'esh-opt
  '(progn
     (add-hook 'eshell-mode-hook 'my-eshell-mode-hook)))

(provide 'eshell-config)
