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
(setq helm-quick-update t)

(setq helm-for-files-preferred-list
      '(helm-source-buffers-list
	helm-source-projectile-files-list
	helm-source-recentf
	helm-source-files-in-current-dir
	helm-source-locate))

(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action)
(define-key helm-map (kbd "C-z") 'helm-select-action)

(defun helm-multi-occur-in-this-mode ()
  "Show all lines matching REGEXP in buffers with this major mode."
  (interactive)
  (helm-multi-occur
   (get-buffers-matching-mode major-mode)))

;; Helm-swoop config

(when (require 'helm-swoop nil 'noerrors)
  (define-key helm-swoop-map (kbd "M-i") 'helm-multi-swoop-all-from-helm-swoop)
  (when (require 'evil nil 'noerrors)
    (define-key evil-motion-state-map (kbd "M-i") 'helm-swoop-from-evil-search))
  (setq helm-multi-swoop-edit-save t))

(provide 'my-helm-config)
