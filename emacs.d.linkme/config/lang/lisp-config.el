(use-package lisp-mode
  :commands (emacs-lisp-mode lisp-mode lisp-interaction-mode my-eval-expression eval-and-replace)
  :config
  
  ;; This function got removed from the released version, so I snagged it from here:
  ;; https://github.com/purcell/elisp-slime-nav/blob/master/elisp-slime-nav.el#L62
  (defun elisp-slime-nav--read-symbol-at-point ()
    "Return the symbol at point as a string.
If `current-prefix-arg' is not nil, the user is prompted for the symbol."
    (let* ((sym-at-point (symbol-at-point))
	   (at-point (and sym-at-point (symbol-name sym-at-point))))
      (if (or current-prefix-arg (null at-point))
	  (completing-read "Symbol: "
			   (elisp-slime-nav--all-navigable-symbol-names)
			   nil t nil nil at-point)
	at-point)))


  (defun prompt-to-eval ()
    "Yes or no prompt user and if yes, evaluate the current buffer as
  elisp"
    (interactive)
    (let* ((info (if (region-active-p)
		     '("Evaluate the current region as elisp?: " . eval-region)
		   '("Evaluate the buffer as elisp?: " . eval-buffer)))
	   (prompt-string (car info))
	   (eval-func (cdr info)))
      (if (y-or-n-p prompt-string)
	  (progn
	    (call-interactively eval-func)
	    (deactivate-mark)))))

  (defun my-elisp-mode-hook ()
    (modify-syntax-entry ?- "w")
    ;; (pretty-mode t)
    (rainbow-delimiters-mode t)
    (elisp-slime-nav-mode t))

  (defun my-jump-to-elisp-docs (sym-name)
    "Jump to a pane and do elisp-slime-nav-describe-elisp-thing-at-point"
    (interactive (list (elisp-slime-nav--read-symbol-at-point)))
    (help-xref-interned (intern sym-name))
    (switch-to-buffer-other-window "*Help*" t))

  (after 'evil
    (evil-define-key 'normal emacs-lisp-mode-map "K" 'my-jump-to-elisp-docs))

  (add-hook 'emacs-lisp-mode-hook 'my-elisp-mode-hook)

  (evil-define-key 'normal emacs-lisp-mode-map (kbd "C-]") 'elisp-slime-nav-find-elisp-thing-at-point)
  (define-key emacs-lisp-mode-map (kbd "C-0") 'eval-defun)
  (define-key emacs-lisp-mode-map (kbd "C-c C-l") 'prompt-to-eval)
  (define-key lisp-interaction-mode-map (kbd "C-0") 'eval-defun)

  (defun my-lisp-interaction-mode-hook ()
    (local-unset-key (kbd "C-j"))
    ;; (pretty-mode t)
    (rainbow-delimiters-mode t)
    (elisp-slime-nav-mode t))

  (add-hook 'lisp-interaction-mode-hook 'my-lisp-interaction-mode-hook)
  (add-hook 'ielm-mode-hook 'my-lisp-interaction-mode-hook)
  (add-hook 'ielm-mode-hook 'turn-on-eldoc-mode)
  (add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
  (add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)

  (defvar my-read-expression-map
    (let ((map (make-sparse-keymap)))
      (set-keymap-parent map read-expression-map)
      (define-key map [(control ?g)] #'minibuffer-keyboard-quit)
      (define-key map [up]   nil)
      (define-key map [down] nil)
      map))

  (defun my-read--expression (prompt &optional initial-contents)
    (let ((minibuffer-completing-symbol t))
      (minibuffer-with-setup-hook
	  (lambda ()
	    (emacs-lisp-mode)
	    (use-local-map my-read-expression-map)
	    (setq font-lock-mode t)
	    (funcall font-lock-function 1))
	(read-from-minibuffer prompt initial-contents
			      my-read-expression-map nil
			      'read-expression-history))))

  (defun my-eval-expression (expression &optional arg)
    (interactive (list (read (my-read--expression ""))
		       current-prefix-arg))
    (require 'pp)
    (if arg
	(insert (pp-to-string (eval-expression expression lexical-binding)))
      (pp-display-expression (eval-expression expression lexical-binding)
			     "*Pp Eval Output*")))

  (defun eval-and-replace ()
    "Replace the preceding sexp with its value."
    (interactive)
    (backward-kill-sexp)
    (condition-case nil
        (insert (format "%s" (eval (read (current-kill 0)))))
      (error (message "Invalid expression")
             (insert (current-kill 0)))))
  
  (bind-key "C-c C-e" 'pp-macroexpand-last-sexp emacs-lisp-mode-map)
  (bind-key "C-c C-r" 'eval-and-replace emacs-lisp-mode-map))

(provide 'lisp-config)
