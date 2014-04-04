(defun eshell/x (&rest args)
  (delete-single-window))

(add-hook 'eshell-mode-hook (lambda ()
			      (local-set-key (kbd "C-u") 'eshell-kill-input)
			      (local-set-key (kbd "C-w") 'backward-kill-word)
			      (local-set-key (kbd "C-c C-k") 'kill-line)
			      (local-set-key (kbd "C-p") 'eshell-previous-input)
			      (local-set-key (kbd "C-n") 'eshell-next-input)))

(provide 'eshell-config)
