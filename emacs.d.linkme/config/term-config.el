(use-package term
  :defer t
  :config

  (defun my-term-mode-hook ()
    (setq show-trailing-whitespace nil)
    (smartparens-mode -1))

  (add-hook 'term-mode-hook
            'my-term-mode-hook)

  (defun term-clear ()
    (interactive)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (term-send-input)))

  (defcustom term-unbind-key-list
    '("C-z" "C-x" "C-c" "C-h" "C-l" "C-j" "C-k" "C-y" "<ESC>")
    "The key list that will need to be unbound."
    :type 'list
    :group 'term)

  (defcustom term-bind-key-alist
    '(
      ("C-c C-c" . term-interrupt-subjob)
      ("C-c C-l" . term-send-raw)
      ("C-p" . previous-line)
      ("C-n" . next-line)
      ("C-s" . isearch-forward)
      ("C-r" . isearch-backward)
      ("C-m" . term-send-raw)
      ("M-r" . term-send-reverse-search-history)
      ("M-," . term-send-input))
    "The key alist that will need to be bind.
If you do not like default setup, modify it, with (KEY . COMMAND) format."
    :type 'alist
    :group 'term))

(provide 'term-config)
