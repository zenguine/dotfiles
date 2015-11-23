(use-package cc-mode
  :commands c-mode
  :config
  (require 'semantic)
  ;; Use CEDET/semantic with company mode for completion

  (global-semanticdb-minor-mode 1)
  (global-semantic-idle-scheduler-mode 1)

  ;; Use below function to add libraries to include path for semantic
  ;; (semantic-add-system-include "/usr/include/boost" 'c++-mode)

  ;; Basic configuration
  (setq-default c-basic-offset 2 c-default-style "gnu")
  (setq-default tab-width 2)

  (defun my-c-mode-hook ()
    (interactive)
    (semantic-mode t))

  (add-hook 'c-mode-hook 'my-c-mode-hook)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Use helm-gtags for tag jumping..
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (setq helm-gtags-ignore-case t
	helm-gtags-auto-update t
	helm-gtags-use-input-at-cursor t
	helm-gtags-pulse-at-cursor t
	helm-gtags-prefix-key "\C-cg"
	helm-gtags-suggested-key-mapping t
	))

  ;; (require 'helm-gtags)
  ;; ;; Enable helm-gtags-mode
  ;; (add-hook 'dired-mode-hook 'helm-gtags-mode)
  ;; (add-hook 'eshell-mode-hook 'helm-gtags-mode)
  ;; (add-hook 'c-mode-hook 'helm-gtags-mode)
  ;; (add-hook 'c++-mode-hook 'helm-gtags-mode)
  ;; (add-hook 'asm-mode-hook 'helm-gtags-mode)

  ;; (define-key helm-gtags-mode-map (kbd "C-c g a") 'helm-gtags-tags-in-this-function)
  ;; ;;(define-key helm-gtags-mode-map (kbd "C-j") 'helm-gtags-select)
  ;; (define-key helm-gtags-mode-map (kbd "C-c <") 'helm-gtags-previous-history)
  ;; (define-key helm-gtags-mode-map (kbd "C-c >") 'helm-gtags-next-history)
  ;; (evil-define-key 'normal helm-gtags-mode-map (kbd "C-]") 'helm-gtags-dwim)
;; (evil-define-key 'normal helm-gtags-mode-map (kbd "C-t") 'helm-gtags-pop-stack))

(provide 'c-config)
