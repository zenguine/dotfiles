;; Load ELPA  
;;; -*- lexical-binding: t -*-

(require 'package)
(setq package-enable-at-startup nil)
(package-initialize)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/"))
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'load-path "~/.emacs.d/elpa")
(require 'cl)
;; Guarantee all packages are installed on start
(defvar packages-list
  '(evil
    evil-nerd-commenter
    evil-leader
    evil-matchit
    color-theme-solarized 
    smex
    helm
    auto-complete
    python-mode
    flycheck
    flycheck-haskell
    fiplr)
  "List of packages needs to be installed at launch")

(defun has-package-not-installed ()
  (loop for p in packages-list
        when (not (package-installed-p p)) do (return t)
        finally (return nil)))
(when (has-package-not-installed)
  ;; Check for new packages (package versions)
  (message "%s" "Get latest versions of all packages...")
  (package-refresh-contents)
  (message "%s" " done.")
  ;; Install the missing packages
  (dolist (p packages-list)
    (when (not (package-installed-p p))
      (package-install p))))

; General customization

(global-linum-mode t)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)
(global-set-key (kbd "RET") 'newline-and-indent)

; Enable auto-complete
(require 'auto-complete-config)
(ac-config-default)

; Evil mode customization
(evil-mode 1)
; (helm-mode 1)

(setq evil-want-C-u-scroll t)
(define-key evil-insert-state-map "j" 'cofi/maybe-exit)
(define-key evil-normal-state-map "H" 'evil-digit-argument-or-evil-beginning-of-line)
(define-key evil-normal-state-map "L" 'evil-end-of-line)
(define-key evil-normal-state-map (kbd "TAB") 'evil-jump-item)
;; (define-key evil-normal-state-map "\\" 'evilnc-comment-or-uncomment-lines)
(define-key evil-normal-state-map  (kbd "C-p") 'fiplr-find-file)
(define-key evil-normal-state-map  (kbd "C-z") 'suspend-emacs)
(define-key evil-normal-state-map  (kbd "C-c C-z") 'evil-emacs-state)
(define-key evil-insert-state-map  (kbd "C-z") 'suspend-emacs)
(define-key evil-insert-state-map  (kbd "C-c C-z") 'evil-emacs-state)
(define-key evil-emacs-state-map  (kbd "C-z") 'suspend-emacs)
(define-key evil-emacs-state-map  (kbd "C-c C-z") 'evil-emacs-state)

(define-key evil-normal-state-map  (kbd "M-h") 'evil-window-left)
(define-key evil-normal-state-map  (kbd "M-j") 'evil-window-down)
(define-key evil-normal-state-map  (kbd "M-k") 'evil-window-up)
(define-key evil-normal-state-map  (kbd "M-l") 'evil-window-right)
 
(evil-define-command cofi/maybe-exit ()
  :repeat change
  (interactive)
  (let ((modified (buffer-modified-p)))
    (insert "j")
    (let ((evt (read-event (format "Insert %c to exit insert state" ?k)
			   nil 0.5)))
      (cond
       ((null evt) (message ""))
       ((and (integerp evt) (char-equal evt ?k))
	(delete-char -1)
	(set-buffer-modified-p modified)
	(push 'escape unread-command-events))
       (t (setq unread-command-events (append unread-command-events
					      (list evt))))))))

; From solarized color scheme apparently?
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes (quote ("fc5fcb6f1f1c1bc01305694c59a1a861b008c534cae8d0e48e4d5e81ad718bc6" default))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'narrow-to-region 'disabled nil)

(load-theme 'solarized-dark)
