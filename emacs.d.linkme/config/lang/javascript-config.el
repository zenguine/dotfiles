(require 'js2-mode)
(require 'js2-refactor)
(require 'skewer-mode)
(require 'tern)

;; Useful when tern forgets to auto-refresh
(defun delete-tern-process ()
  (interactive)
  (delete-process "Tern"))

(defun my-javascript-hook ()
  (modify-syntax-entry ?_ "w")
  (increase-company-delay-locally 0)
  (tern-mode t))

;; Use js2-mode instead of default js-mode
(add-to-list 'auto-mode-alist (cons (rx ".js" eos) 'js2-mode))
(add-hook 'js-mode-hook 'js2-minor-mode)
(add-hook 'js2-minor-mode-hook 'skewer-mode)
(add-hook 'js2-mode-hook 'skewer-mode)

(evil-define-key 'normal tern-mode-keymap (kbd "C-]") 'tern-find-definition)
(evil-define-key 'normal tern-mode-keymap (kbd "C-t") 'tern-pop-find-definition)

;; js2-refactor keybindings
(js2r-add-keybindings-with-prefix "C-c C-m")
(add-hook 'js-mode-hook 'my-javascript-hook)
(add-hook 'js2-mode-hook 'my-javascript-hook)

(provide 'javascript-config)
