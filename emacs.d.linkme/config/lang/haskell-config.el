(require 'haskell-mode)

(defun my-haskell-hook ()
  (interactive)
  (setq show-trailing-whitespace nil)
  (flycheck-mode t)
  (smartparens-mode nil)
  (setq flycheck-display-errors-delay .3)
  (evil-define-key 'normal haskell-mode-map " a" 'haskell/types-file-toggle)
  (when (require 'flycheck nil 'noerror)
    (setq flycheck-ghc-language-extensions '("DeriveFunctor" "DeriveDataTypeable" "DeriveFoldable" "DeriveTraversable" "TemplateHaskell"))))

(defun haskell/types-file-toggle ()
  (interactive)
  (let* ((fp (buffer-file-name))
	 (dir (file-name-directory fp))
	 (base (file-name-base fp)))
    (if (equal base "Types")
	(find-file (concat
		    (substring dir 0 -1)
		    ".hs"))
      (find-file (concat dir base "/Types.hs")))))

(defun my-hoogle-fn (arg)
  (interactive "P")
  (if arg
      (call-interactively 'hoogle)
    (call-interactively 'helm-hoogle)))

(define-key haskell-mode-map (kbd "C-c C-d") 'my-hoogle-fn)
(define-key haskell-mode-map (kbd "C-c C-c C-t") 'haskell/types-file-toggle)

(add-hook 'haskell-mode-hook 'my-haskell-hook)
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)


(defun setup-haskell-interactive-mode-bindings ()
  (interactive)
  (evil-define-key 'insert haskell-interactive-mode-map
    (kbd "C-u") 'haskell-interactive-mode-kill-whole-line)
  (define-key haskell-interactive-mode-map (kbd "C-u") 'haskell-interactive-mode-kill-whole-line)
  (define-key haskell-interactive-mode-map (kbd "C-w") 'backward-kill-word)
  (define-key haskell-interactive-mode-map (kbd "TAB") 'haskell-interactive-mode-tab)
  (define-key haskell-interactive-mode-map (kbd "C-j") nil)
  (define-key haskell-interactive-mode-map
    (kbd "C-p") 'haskell-interactive-mode-history-previous)
  (evil-define-key 'insert haskell-interactive-mode-map
    (kbd "C-p") 'haskell-interactive-mode-history-previous)
  (define-key haskell-interactive-mode-map
    (kbd "C-n") 'haskell-interactive-mode-history-next)
  (evil-define-key 'insert haskell-interactive-mode-map
    (kbd "C-n") 'haskell-interactive-mode-history-next))

(add-hook 'haskell-interactive-mode-hook 'setup-haskell-interactive-mode-bindings)

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
