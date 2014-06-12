(require 'haskell-mode)

(defun my-haskell-hook ()
  (interactive)
  (setq show-trailing-whitespace nil))

(add-hook 'haskell-mode-hook 'my-haskell-hook)
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)

(if (require 'shm nil 'noerror)
    (progn
      ;; Faces for solarized light..
      (set-face-background 'shm-current-face "#47434D")
      (set-face-background 'shm-quarantine-face "#47434D")

      (define-key shm-map (kbd "C-j") nil)
      (define-key shm-map (kbd "C-k") nil)

      (when (require 'evil nil 'noerror)
	(evil-define-key 'normal shm-map (kbd "D") 'shm/kill-line)))
  (add-hook 'haskell-mode-hook 'turn-on-haskell-indent))

(provide 'haskell-config)
