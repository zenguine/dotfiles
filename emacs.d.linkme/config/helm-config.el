(setq helm-buffers-fuzzy-matching t)
(require 'helm)


(defun helm-multi-occur-in-this-mode ()
  "Show all lines matching REGEXP in buffers with this major mode."
  (interactive)
  (helm-multi-occur
   (get-buffers-matching-mode major-mode)))

(provide 'helm-config)
