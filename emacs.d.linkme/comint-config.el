(add-hook 'comint-mode-hook (lambda ()
			      (local-set-key (kbd "C-u") 'eshell-kill-input)
			      (local-set-key (kbd "C-w") 'backward-kill-word)
			      (local-set-key (kbd "C-c C-k") 'kill-line)
			      (local-set-key (kbd "C-p") 'comint-previous-input)
			      (local-set-key (kbd "C-n") 'comint-next-input)))


(provide 'comint-config)
