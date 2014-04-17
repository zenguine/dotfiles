;; Show the current function name in the header line
;; (which-function-mode)
(setq-default header-line-format
	      '((which-func-mode ("" which-func-format " "))))
(setq mode-line-misc-info
	    ;; We remove Which Function Mode from the mode line, because it's mostly
	    ;; invisible here anyway.
	    (assq-delete-all 'which-func-mode mode-line-misc-info))

;; Sublimity
(require 'sublimity)
(require 'sublimity-scroll)

(setq sublimity-scroll-weight 10
      sublimity-scroll-drift-length 5)

(provide 'misc-config)
