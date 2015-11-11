;; Sublimity
(require 'sublimity)
(recentf-mode t)
(semantic-mode t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; minibuffer config..
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-hook 'minibuffer-setup-hook 'conditionally-enable-minibuffer-features)

(after 'helm-eval
  (define-key helm-eval-expression-map (kbd "TAB") 'helm-lisp-completion-at-point)
  (define-key helm-eval-expression-map (kbd "RET") 'helm-eval-new-line-and-indent)
  (define-key helm-eval-expression-map (kbd "M-RET") 'helm-maybe-exit-minibuffer)
  (define-key helm-eval-expression-map (kbd "C-RET") 'helm-maybe-exit-minibuffer)
  (define-key helm-eval-expression-map [(control ?g)] #'abort-recursive-edit)
  (define-key helm-eval-expression-map [tab] 'helm-lisp-completion-at-point))

(defun conditionally-enable-minibuffer-features ()
  "enable paredit-mode during eval-expression"
  (if (or inside-helm-eval-expression-p (eq this-command 'eval-expression))
      (smartparens-mode 1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Info manual config
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(after 'ace-link (ace-link-setup-default))
(after 'info (require 'info+))
(define-key Info-mode-map (kbd "C-d") 'evil-scroll-down)
(define-key Info-mode-map (kbd "C-u") 'evil-scroll-up)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Other
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; no backup files
(setq make-backup-files nil
      backup-inhibited t
      auto-save-default nil
      create-lockfiles nil)

;; Save bookmarks after every change
(setq bookmark-save-flag 1)

(setq save-place-file (expand-file-name ".places" user-emacs-directory))
(setq-default save-place t)
(require 'saveplace)

(setq enable-recursive-minibuffers t)
(put 'narrow-to-region 'disabled nil)

(setq undo-tree-auto-save-history t)
(setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo")))

(set-fringe-mode 0)

;; Paradox github token configuration: temporary!!! dont commit this
(setq paradox-github-token "7e2bac3eae766006574ccd74e35ed3907947bbf1")

(ifhave 'beacon (beacon-mode 1))

(provide 'misc-config)
