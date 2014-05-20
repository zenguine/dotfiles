(setq helm-buffers-fuzzy-matching t)
(require 'helm)
(require 'helm-files)
(require 'helm-projectile)

(setq helm-idle-delay 0.1
      helm-input-idle-delay 0.1
      helm-locate-command "locate %s -e %s")

(dolist (ext '("\\.elc$" "\\.pyc$"))
  (add-to-list 'helm-boring-file-regexp-list ext))

(global-set-key (kbd "M-t") 'helm-for-files)

(setq helm-for-files-preferred-list
      '(helm-source-buffers-list
	helm-source-recentf
	helm-source-files-in-current-dir
	helm-source-projectile-files-list
	helm-source-locate))

(defun helm-multi-occur-in-this-mode ()
  "Show all lines matching REGEXP in buffers with this major mode."
  (interactive)
  (helm-multi-occur
   (get-buffers-matching-mode major-mode)))

(provide 'my-helm-config)
