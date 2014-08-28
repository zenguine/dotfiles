(require 'haskell-mode)
(require 'shm)
(require 's)
(require 'dash)

(custom-set-variables
  '(haskell-process-suggest-remove-import-lines t)
  '(haskell-process-auto-import-loaded-modules t)
  '(haskell-process-log t)
  '(haskell-process-type 'cabal-repl))

(setq ghc-ghc-options '("-fno-warn-missing-signatures" "-fno-warn-name-shadowing"))

;; ghc/ghc-mod stuff
(autoload 'ghc-init "ghc" nil t)
(autoload 'ghc-debug "ghc" nil t)

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

(defun shm/current-node-string ()
  "Get the text of the current shm node"
  (shm-node-string (shm-current-node)))

(defun my-haskell-process-do-simple-echo (s)
  (message "Passed to haskell repl:")
  (message s)
  (haskell-process-do-simple-echo s))

(defun haskell/process-eval-string (use-type-p split-lines-p base-string)
  "Send 'eval-string' to the running haskell process to be evaluated.
   If use-type-p is non-nil, the type of eval-string is computed instead."
  (if split-lines-p
      (let* ((eval-strings (s-lines base-string))
	     (eval-strings
	      (if use-type-p (-map (lambda (s) (s-join " " (list ":t" s)))
				   eval-strings)
		eval-strings)))
	(-each eval-strings 'my-haskell-process-do-simple-echo))
    (let* ((lines (s-lines base-string))
	   (newhead (if use-type-p
			(s-join " " (list ":t" (car lines)))
		      (car lines)))
	   (lines (cons newhead (cdr lines)))
	   (eval-string (if (< 1 (length lines))
			    (s-join "\n" (append (cons ":{" lines) '(":}")))
			  (concat (car lines) "\n"))))
      (my-haskell-process-do-simple-echo eval-string))))

(defun haskell/process-send-current (arg start end)
  "Send either the current region if active, or the text in the current shm node
   to the running haskell process to be evaluated.  With prefix argument, the type
   is printed instead."
  (interactive "P\nr")
  (let* ((is-region-active (region-active-p))
	 (eval-string (if is-region-active
			 (buffer-substring start end)
		       (shm/current-node-string))))
    (haskell/process-eval-string arg is-region-active eval-string)))

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

(defun haskell/process-send-current-with-prefix (start end prefix split-lines-p)
  (interactive (let ((prefix (read-string "Function to call: " "" nil "print")))
		 (list (region-beginning) (region-end) prefix (region-active-p))))
  (let ((base-string (if (region-active-p)
			 (buffer-substring
			  (region-beginning)
			  (region-end))
		       (shm/current-node-string))))
    (if split-lines-p
	(let* ((eval-strings (s-lines base-string))
	       (eval-strings
		(-map (lambda (s) (s-join " " (list prefix "$" s)))
		      eval-strings)))
	  (-each eval-strings 'my-haskell-process-do-simple-echo))
      (let* ((lines (s-lines base-string))
	     (full-prefix (concat prefix " $ ( "))A
	     (fp-length (length full-prefix))
	     (newhead (concat full-prefix (car lines)))
	     (new-tail (-map (lambda (line) (concat (s-repeat fp-length " ")
					       line))
			     (cdr lines)))
	     (lines (cons newhead new-tail))
	     (eval-string (if (< 1 (length lines))
			      (s-join "\n" (append (cons ":{" lines) '(")" ":}")))
			    (concat (car lines) ")\n"))))
	(my-haskell-process-do-simple-echo eval-string)))))

(defun my-template-insert-fn (arg)
  (interactive "P")
  (if arg
      (call-interactively 'ghc-initial-code-from-signature)
    (call-interactively 'ghc-insert-template)))


(defun haskell/pprIO-current-with-prefix (arg start end)
  (interactive "P\nr")
  (haskell-interactive-mode-clear)
  (if arg
      (call-interactively 'haskell/process-send-current-with-prefix)
    (haskell/process-send-current-with-prefix start end "pprIO" (region-active-p))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Convenient bindings for evaluating haskell code in the repl from a regular haskell buffer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-key haskell-mode-map (kbd "C-c S") 'haskell/process-send-current-with-prefix)
(evil-define-key 'normal haskell-mode-map (kbd "M-RET") 'haskell/pprIO-current-with-prefix)
(evil-define-key 'visual haskell-mode-map (kbd "M-RET") 'haskell/pprIO-current-with-prefix)

(define-key haskell-mode-map (kbd "C-c s") 'haskell/process-send-current)
(evil-define-key 'normal shm-map (kbd "RET") nil)
(evil-define-key 'normal haskell-mode-map (kbd "RET") 'haskell/process-send-current)
(evil-define-key 'visual haskell-mode-map (kbd "RET") 'haskell/process-send-current)

;; Bindings to check out haskell documentation / browse errors
(define-key haskell-mode-map (kbd "C-c C-d") 'my-hoogle-fn)
(define-key haskell-mode-map (kbd "M-n") 'flycheck-next-error)
(define-key haskell-mode-map (kbd "M-p") 'flycheck-previous-error)
(define-key haskell-mode-map (kbd "C-c C-l") 'haskell-process-load-or-reload)
(define-key haskell-mode-map (kbd "C-c C-z") 'haskell-interactive-switch)

(define-key haskell-mode-map (kbd "C-`") 'haskell-interactive-bring)
(define-key haskell-mode-map (kbd "C-c C-n C-c") 'haskell-process-cabal-build)
(define-key haskell-mode-map (kbd "C-c C-n c") 'haskell-process-cabal)

(define-key haskell-mode-map (kbd "C-c C-r") 'haskell-process-do-info)
(evil-define-key 'normal haskell-mode-map (kbd "K") 'haskell-process-do-info)

(define-key haskell-mode-map (kbd "C-c t") 'haskell-process-do-type)
(define-key haskell-mode-map (kbd "C-c C-t") 'haskell-process-do-type)

(evil-define-key 'normal haskell-mode-map (kbd "g K") 'haskell-process-do-type)
(define-key haskell-mode-map (kbd "C-c d") 'haskell-process-add-dependency)
(define-key haskell-mode-map (kbd "C-c T") 'haskell/types-file-toggle)

(eval-after-load 'haskell-cabal '(progn
  (define-key haskell-cabal-mode-map (kbd "C-`") 'haskell-interactive-bring)
  (define-key haskell-cabal-mode-map (kbd "C-c C-k") 'haskell-interactive-mode-clear)
  (define-key haskell-cabal-mode-map (kbd "C-c C-c") 'haskell-process-cabal-build)
  (define-key haskell-cabal-mode-map (kbd "C-c c") 'haskell-process-cabal)))

(defun setup-haskell-interactive-mode ()
  (interactive)
  (evil-define-key 'insert haskell-interactive-mode-map
    (kbd "C-u") 'haskell-interactive-mode-kill-whole-line)
  (define-key haskell-interactive-mode-map (kbd "C-u") 'haskell-interactive-mode-kill-whole-line)
  (define-key haskell-interactive-mode-map (kbd "C-w") 'backward-kill-word)
  (define-key haskell-interactive-mode-map (kbd "C-c C-z") 'haskell-interactive-switch-back)
  (define-key haskell-interactive-mode-map (kbd "TAB") 'haskell-interactive-mode-tab)
  (define-key haskell-interactive-mode-map (kbd "C-c C-l") 'haskell-interactive-mode-clear)
  (define-key haskell-interactive-mode-map (kbd "C-j") nil)
  (define-key haskell-interactive-mode-map
    (kbd "C-p") 'helm-for-files)
  (define-key haskell-interactive-mode-map
    (kbd "C-n") nil)
  (evil-define-key 'normal haskell-interactive-mode-map (kbd "G") 'end-of-buffer)
  (define-key haskell-interactive-mode-map (kbd "C-c t") 'haskell-process-do-type)
  (define-key haskell-interactive-mode-map (kbd "C-c C-r") 'haskell-process-do-info)
  (define-key haskell-interactive-mode-map (kbd "C-c C-d") 'my-hoogle-fn)
  (modify-syntax-entry ?_ "w"))

(defun shm/kill-whole-line ()
  (interactive "P")
  (evil-beginning-of-line)
  (shm/kill-line))

(defun my-haskell-hook ()
  (interactive)
  (turn-on-haskell-indentation)
  (setq show-trailing-whitespace nil)
  (rainbow-delimiters-mode t)
  (setq haskell-stylish-on-save t)
  (flycheck-mode t)

  (setq flycheck-display-errors-delay .3)
  (evil-define-key 'normal haskell-mode-map " a" 'haskell/types-file-toggle)
  (when (require 'flycheck nil 'noerror)
    (setq flycheck-ghc-language-extensions '("DeriveFunctor" "DeriveDataTypeable" "DeriveFoldable" "DeriveTraversable" "TemplateHaskell")))
  (evil-define-key 'normal haskell-mode-map (kbd "M-t") 'my-template-insert-fn)
  (when (require 'ghc nil 'noerror)
    (ghc-init)))

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
    (kbd "M-k") 'sp-splice-sexp-killing-backward
    (kbd "M-j") 'sp-splice-sexp-killing-forward
    (kbd "M-l") 'sp-forward-slurp-sexp
    (kbd "M-h") 'sp-forward-barf-sexp
    (kbd "M-H") 'sp-backward-slurp-sexp
    (kbd "M-L") 'sp-backward-barf-sexp)

  (evil-define-key 'emacs map
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
  (evil-define-key 'normal shm-map (kbd "RET") nil))

(add-hook 'haskell-mode-hook 'my-haskell-hook)
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'my-shm-hook)
(add-hook 'haskell-interactive-mode-hook 'setup-haskell-interactive-mode)
(add-hook 'haskell-mode-hook 'pretty-lambdas-haskell)

(provide 'haskell-config)
