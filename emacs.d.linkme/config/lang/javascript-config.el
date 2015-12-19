(use-package skewer-mode
  :ensure t
  :commands skewer-mode
  :config
  (define-key skewer-mode-map (kbd "C-0") 'skewer-eval-defun))

(use-package tern
  :ensure t
  :commands tern-mode
  :config
  ;; Useful when tern forgets to auto-refresh
  (defun delete-tern-process ()
    (interactive)
    (delete-process "Tern")))

(use-package js2-refactor
  :ensure t
  :defer t)

(use-package json-mode
  :ensure t
  :commands (json-mode)
  :config
  (defun my-json-mode-hook ()
    (interactive)
    (js2-minor-mode -1))
  (add-hook 'json-mode-hook 'my-json-mode-hook))

(use-package js2-mode
  :ensure t
  :commands js2-mode
  :init
  ;; Use js2-mode instead of default js-mode
  (add-to-list 'auto-mode-alist (cons (rx ".js" eos) 'js2-mode))
  (add-hook 'js-mode-hook 'js2-minor-mode)
  :config

  (defun my-javascript-hook ()
    (require 'js2-refactor)
    (modify-syntax-entry ?_ "w")
    (increase-company-delay-locally 0)
    (flycheck-mode t)
    (tern-mode t))

  (after 'evil
    (evil-define-key 'normal tern-mode-keymap (kbd "C-]") 'tern-find-definition)
    (evil-define-key 'normal tern-mode-keymap (kbd "C-t") 'tern-pop-find-definition))

  ;; js2-refactor keybindings
  (js2r-add-keybindings-with-prefix "C-c C-m")
  (add-hook 'js-mode-hook 'my-javascript-hook)
  (add-hook 'js2-mode-hook 'my-javascript-hook)

  ;; Skewer configuration
  (add-hook 'js2-minor-mode-hook 'skewer-mode)
  (add-hook 'js2-mode-hook 'skewer-mode))



(provide 'javascript-config)
