(require 'js2-mode)
(require 'js2-refactor)
(require 'skewer-mode)

(defun my-javascript-hook ()
  (increase-company-delay-locally 2))

;; Use js2-mode instead of default js-mode
(add-to-list 'auto-mode-alist (cons (rx ".js" eos) 'js2-mode))
(add-hook 'js-mode-hook 'js2-minor-mode)
(add-hook 'js2-minor-mode-hook 'skewer-mode)
(add-hook 'js2-mode-hook 'skewer-mode)

;; js2-refactor keybindings
(js2r-add-keybindings-with-prefix "C-c C-m")

(provide 'javascript-config)
