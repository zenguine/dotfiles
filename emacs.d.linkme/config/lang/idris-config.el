(require 'idris-mode)

(defun my-idris-hook ()
  (interactive)
  (setq show-trailing-whitespace nil))

(add-hook 'idris-mode-hook 'my-idris-hook)

(provide 'idris-config)
