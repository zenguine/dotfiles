(req-package flycheck
  :defer t
  :config
  (use-package flycheck-pos-tip)
  (custom-set-variables
   '(flycheck-display-errors-function  #'flycheck-pos-tip-error-messages)))

(provide 'flycheck-config)
