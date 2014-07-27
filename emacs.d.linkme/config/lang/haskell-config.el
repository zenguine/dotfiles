(require 'haskell-mode)
(require 'shm)

(setq haskell-font-lock-symbols t)

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

(defun shm/kill-whole-line ()
  (interactive "P")
  (evil-beginning-of-line)
  (shm/kill-line))

(defun my-haskell-hook ()
  (interactive)
  (setq show-trailing-whitespace nil)
  (setq haskell-stylish-on-save t)
  (flycheck-mode t)

  (setq flycheck-display-errors-delay .3)
  (evil-define-key 'normal haskell-mode-map " a" 'haskell/types-file-toggle)
  (when (require 'flycheck nil 'noerror)
    (setq flycheck-ghc-language-extensions '("DeriveFunctor" "DeriveDataTypeable" "DeriveFoldable" "DeriveTraversable" "TemplateHaskell"))))

(let ((map shm-map))
  (define-key map (kbd "C-k") nil)
  (define-key map (kbd "C-j") nil)
  (evil-define-key 'normal map (kbd "D") 'shm/kill-line)
  (evil-define-key 'normal map (kbd "R") 'shm/raise)
  (evil-define-key 'insert map (kbd "=") 'shm/=)
  (evil-define-key 'normal map (kbd "P") 'shm/yank)

  (define-key map (kbd "C-j") nil)
  (define-key map (kbd "M-RET") 'shm/newline-indent)
  (define-key map (kbd "RET") 'evil-ret)
  (define-key map (kbd "C-k") nil)

  (evil-define-key 'normal map
    (kbd "M-t") 'sp-transpose-sexp
    (kbd "M-k") 'sp-splice-sexp-killing-backward
    (kbd "M-j") 'sp-splice-sexp-killing-forward
    (kbd "M-l") 'sp-forward-slurp-sexp
    (kbd "M-h") 'sp-forward-barf-sexp
    (kbd "M-H") 'sp-backward-slurp-sexp
    (kbd "M-L") 'sp-backward-barf-sexp
    (kbd "s") 'sp-splice-sexp
    (kbd "S") 'shm/split-list
    (kbd "M-R") 'sp-raise-sexp
    (kbd "J") 'sp-join-sexp
    (kbd ")") 'shm/forward-node
    (kbd "(") 'shm/backward-node
    (kbd "M-(") 'sp-backward-up-sexp
    (kbd "M-)") 'sp-down-sexp
    (kbd "C-(") 'sp-backward-down-sexp
    (kbd "C-)") 'sp-up-sexp)

  (evil-define-key 'operator map
    (kbd ")") 'shm/forward-node
    (kbd "(") 'shm/backward-node
    (kbd "M-(") 'sp-backward-up-sexp
    (kbd "M-)") 'sp-down-sexp
    (kbd "C-(") 'sp-backward-down-sexp
    (kbd "C-)") 'sp-up-sexp)

  (evil-define-key 'motion map
    (kbd ")") 'shm/forward-node
    (kbd "(") 'shm/backward-node
    (kbd "M-(") 'sp-backward-up-sexp
    (kbd "M-)") 'sp-down-sexp
    (kbd "C-(") 'sp-backward-down-sexp
    (kbd "C-)") 'sp-up-sexp)

  (evil-define-key 'insert map
    (kbd "M-t") 'sp-transpose-sexp
    (kbd "M-k") 'sp-splice-sexp-killing-backward
    (kbd "M-j") 'sp-splice-sexp-killing-forward
    (kbd "M-l") 'sp-forward-slurp-sexp
    (kbd "M-h") 'sp-forward-barf-sexp
    (kbd "M-H") 'sp-backward-slurp-sexp
    (kbd "M-L") 'sp-backward-barf-sexp)

  (evil-define-key 'emacs map
    (kbd "M-t") 'sp-transpose-sexp
    (kbd "M-k") 'sp-splice-sexp-killing-backward
    (kbd "M-j") 'sp-splice-sexp-killing-forward
    (kbd "M-l") 'sp-forward-slurp-sexp
    (kbd "M-h") 'sp-forward-barf-sexp
    (kbd "M-H") 'sp-backward-slurp-sexp
    (kbd "M-L") 'sp-backward-barf-sexp))

(defun my-shm-hook ()
  (structured-haskell-mode t)
  (smartparens-mode -1)
  (smartparens-strict-mode -1)
  (set-face-background 'shm-current-face "#47434D")
  (set-face-background 'shm-quarantine-face "#47434D"))

(add-hook 'haskell-mode-hook 'my-haskell-hook)
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'my-shm-hook)
(add-hook 'haskell-interactive-mode-hook 'setup-haskell-interactive-mode-bindings)

(provide 'haskell-config)
