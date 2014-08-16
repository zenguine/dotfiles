(require 'haskell-mode)
(require 'shm)

;; Disabled because it doesn't work with structured-haskell-mode
(setq haskell-font-lock-symbols nil)
;; but a lambda symbol can safely replace '\' because they are the same length
;; and it wont screw up indentation
(defun pretty-lambdas-haskell ()
  (font-lock-add-keywords
   nil `((,(concat "\\(" (regexp-quote "\\") "\\)")
          (0 (progn (compose-region (match-beginning 1) (match-end 1)
                                    ,(make-char 'greek-iso8859-7 107))
                    nil))))))

(add-hook 'haskell-mode-hook 'pretty-lambdas-haskell)

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
(define-key haskell-mode-map (kbd "M-n") 'flycheck-next-error)
(define-key haskell-mode-map (kbd "M-p") 'flycheck-previous-error)
(define-key haskell-mode-map (kbd "C-c C-l") 'haskell-process-load-or-reload)
(define-key haskell-mode-map (kbd "C-c C-r") 'haskell-process-do-info)
(define-key haskell-mode-map (kbd "C-c t") 'haskell-process-do-type)
(define-key haskell-mode-map (kbd "C-c d") 'haskell-process-add-dependency)
(define-key haskell-mode-map (kbd "C-c T") 'haskell/types-file-toggle)

(defun setup-haskell-interactive-mode-bindings ()
  (interactive)
  (evil-define-key 'insert haskell-interactive-mode-map
    (kbd "C-u") 'haskell-interactive-mode-kill-whole-line)
  (define-key haskell-interactive-mode-map (kbd "C-u") 'haskell-interactive-mode-kill-whole-line)
  (define-key haskell-interactive-mode-map (kbd "C-w") 'backward-kill-word)
  (define-key haskell-interactive-mode-map (kbd "TAB") 'haskell-interactive-mode-tab)
  (define-key haskell-interactive-mode-map (kbd "C-c C-l") 'haskell-interactive-mode-clear)
  (define-key haskell-interactive-mode-map (kbd "C-j") nil)
  (define-key haskell-interactive-mode-map
    (kbd "C-p") 'helm-for-files)
  (define-key haskell-interactive-mode-map
    (kbd "C-n") nil))

(defun shm/kill-whole-line ()
  (interactive "P")
  (evil-beginning-of-line)
  (shm/kill-line))

(defun my-haskell-hook ()
  (interactive)
  (setq show-trailing-whitespace nil)
  (rainbow-delimiters-mode t)
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
  (evil-define-key 'normal map (kbd "P") 'shm/yank)

  (define-key map (kbd "C-j") nil)
  (evil-define-key 'insert map (kbd "RET") 'shm/newline-indent)
  (evil-define-key 'normal map (kbd "RET") 'shm/newline-indent)
  (evil-define-key 'insert map (kbd "M-RET") 'evil-ret)
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
  (smartparens-strict-mode -1))

(add-hook 'haskell-mode-hook 'my-haskell-hook)
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'my-shm-hook)
(add-hook 'haskell-interactive-mode-hook 'setup-haskell-interactive-mode-bindings)

(provide 'haskell-config)
