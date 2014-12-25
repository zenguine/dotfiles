;; Fuzzy matching
(setq helm-buffers-fuzzy-matching t)
(setq helm-M-x-fuzzy-match t)
(setq helm-apropos-fuzzy-match t)

(require 'helm)
(require 'helm-files)
(require 'helm-projectile)

(setq helm-idle-delay 0.1
      helm-input-idle-delay 0.1
      helm-locate-command "locate %s -e %s")

(dolist (ext '("\\.elc$" "\\.pyc$"))
  (add-to-list 'helm-boring-file-regexp-list ext))

;; General helm settings
(setq helm-quick-update t)
(setq helm-bookmark-show-location t)
(setq helm-split-window-in-side-p t)
(setq helm-ff-search-library-in-sexp t) ;; TODO: What does this do?
(setq helm-ff-file-name-history-use-recentf t)

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helm-projectile configuration stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(helm-projectile-on)

(define-key projectile-mode-map (kbd "C-c p p") 'helm-projectile-switch-project)
(define-key projectile-mode-map (kbd "C-p") nil)

(defun helm-projectile-custom ()
  (interactive)
  (let ((src '(helm-source-projectile-buffers-list
	       helm-source-projectile-recentf-list
	       helm-source-projectile-files-list
	       helm-source-projectile-projects)))
    (helm :sources src
	  :buffer "*helm projectile: custom*"
	  :prompt (projectile-prepend-project-name "pattern: "))))

(setq helm-projectile-sources-list '(
				     helm-source-projectile-projects
				     helm-source-projectile-buffers-list
				     helm-source-projectile-recentf-list
				     helm-source-projectile-files-list
				     ))

;; helm-projectile-switch-project doesn't correctly save window
;; configuration like regular projectile-switch-project this advice is
;; a hack to fix the problem for now..
(defun my-projectile-maybe-save-window-config (&rest args)
  (interactive)
  (when (and projectile-remember-window-configs
             (projectile-project-p))
    (projectile-save-window-config (projectile-project-name))))

(advice-add 'helm-projectile-switch-project :before #'my-projectile-maybe-save-window-config)

(setq projectile-switch-project-action 'helm-projectile)

;; Misc helm search keybindings
(ifhave 'helm-ag
  (global-set-key (kbd "M-s a") 'helm-ag))

(provide 'my-helm-config)
