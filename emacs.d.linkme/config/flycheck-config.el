(require 'flycheck)
(require 'flycheck-pos-tip)

(eval-after-load 'flycheck
  '(custom-set-variables
    '(flycheck-display-errors-function  #'flycheck-pos-tip-error-messages)))

(provide 'flycheck-config)
