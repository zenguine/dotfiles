(defun eshell/x (&rest args)
  (delete-single-window))

(add-hook 'eshell-mode-hook (lambda ()
			      (local-set-key (kbd "C-u") 'eshell-kill-input)
			      (local-set-key (kbd "C-w") 'backward-kill-word)
			      (local-set-key (kbd "C-c C-k") 'kill-line)
			      (local-set-key (kbd "C-p") 'eshell-previous-input)
			      (local-set-key (kbd "C-n") 'eshell-next-input)
			      (local-set-key (kbd "C-c C-h") 'helm-eshell-history)
			      (local-unset-key (kbd "C-j"))))

(provide 'eshell-config)
