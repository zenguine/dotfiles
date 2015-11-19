(req-package eshell
  :require company
  :defer t
  :config
  (defun eshell/x (&rest args)
    (delete-single-window))

  (defun delete-single-window (&optional window)
    "Remove WINDOW from the display.  Default is `selected-window'."
    (interactive)
    (setq window (or window (selected-window)))
    (select-window window)
    (kill-buffer)
    (if (not (one-window-p t))
	(delete-window (selected-window))))

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
    (add-to-list 'eshell-visual-commands "watch")
    (add-to-list 'eshell-visual-commands "ssh")
    (add-to-list 'eshell-visual-commands "tail")
    (add-to-list 'eshell-output-filter-functions 'eshell-handle-ansi-color)
    (setenv "PAGER" "cat")
    (increase-company-delay-locally 0.7))
  (add-hook 'eshell-mode-hook 'my-eshell-mode-hook)
  ;; (add-hook 'emacs-startup-hook #'(lambda ()
  ;; 				    (let ((default-directory (getenv "HOME")))
  ;; 				      (command-execute 'eshell)
  ;; 				      (kill-buffer))))

  )


(provide 'eshell-config)

