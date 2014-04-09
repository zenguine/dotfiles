;; Load ELPA
;;; -*- lexical-binding: t -*-

(require 'package)
(setq package-enable-at-startup nil)
(setq evil-want-C-u-scroll t)
(package-initialize)
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.milkbox.net/packages/"))
(add-to-list 'package-archives
	     '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'load-path "~/.emacs.d")
(add-to-list 'load-path "~/.emacs.d/elpa")
(load "surround.el")
(require 'surround)
(global-surround-mode 1)
(require 'cl)

(defadvice terminal-init-xterm (around map-S-up-escape-sequence activate)
  (define-key input-decode-map (kbd "C-i") (kbd "H-i")))

;; Guarantee all packages are installed on start
(defvar packages-list
  '(
    ace-jump-mode
    auto-complete
    color-theme-solarized
    evil
    evil-leader
    evil-matchit
    evil-nerd-commenter
    fiplr
    flx
    flx-ido
    flycheck
    flycheck-haskell
    helm
    ido-vertical-mode
    ipython
    jedi
    key-chord
    magit
    projectile
    python-mode
    smartparens
    smex
    zenburn-theme
    )
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

;; 'y' or 'n' instead of 'yes' or 'no'
(fset 'yes-or-no-p 'y-or-n-p)
; highlight corresponding paren
(show-paren-mode t)
(setq-default highlight-tabs t)
(setq inhibit-startup-message t)
(setq-default show-trailing-whitespace t)

(add-hook 'before-save-hook 'whitespace-cleanup)
(add-hook 'before-save-hook (lambda () (delete-trailing-whitespace)))

; no backup files
(setq make-backup-files nil
      backup-inhibited t
      auto-save-default nil)

;; (global-linum-mode t)
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(ido-vertical-mode)
(global-hl-line-mode 1)

(autoload 'project-root "util" nil t)
(autoload 'find-project-root "util" nil t)
(autoload 'anyp "util" nil t)
(autoload 'root-p "util" nil t)
(autoload 'move-window-or-create "util" nil t)
(autoload 'pytest-test-all "util" nil t)
(autoload 'pytest-test-current-file "util" nil t)
(autoload 'pytest-test-specific-test "util" nil t)
(autoload 'delete-single-window "util" nil t)
(autoload 'eshell-here "util" nil t)
(autoload 'get-buffers-matching-mode "util" nil t)

; Enable auto-complete
(require 'auto-complete-config)
(ac-config-default)

(evil-mode 1)
(ido-mode 1)
(flx-ido-mode 1)
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(projectile-global-mode 1)
(setq evil-overriding-maps nil)
(setq evil-intercept-maps nil)
(setq evil-want-C-u-scroll t)

; Utility stuff

; Find project root based on .git file or something

(setq project-root-markers '(".git" ".svn"))

;; Mode configuratoin
;python

(setq jedi:complete-on-dot t)
(setq jedi:get-in-function-call-delay 100)

(add-hook 'python-mode-hook (lambda ()
			      ; (flycheck-mode 0)
			      (local-set-key (kbd "C-c !") 'python-shell-switch-to-shell)
			      (local-set-key (kbd "C-c t a") 'pytest-test-all)
			      (local-set-key (kbd "C-c t f") 'pytest-test-current-file)
			      (local-set-key (kbd "C-c t t") 'pytest-test-specific-test)
			      (jedi:setup)
			      ))

(add-hook 'comint-mode-hook 'evil-emacs-state)


(setq
 python-shell-interpreter "ipython"
 python-shell-interpreter-args ""
 python-shell-prompt-regexp "In \\[[0-9]+\\]: "
 python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: "
 python-shell-completion-setup-code
 "from IPython.core.completerlib import module_completion"
 python-shell-completion-module-string-code
 "';'.join(module_completion('''%s'''))\n"
 python-shell-completion-string-code
 "';'.join(get_ipython().Completer.all_completions('''%s'''))\n")


; From solarized color scheme apparently?
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes (quote ("a53714de04cd4fdb92ed711ae479f6a1d7d5f093880bfd161467c3f589725453" "1e7e097ec8cb1f8c3a912d7e1e0331caeed49fef6cff220be63bd2a6ba4cc365" "fc5fcb6f1f1c1bc01305694c59a1a861b008c534cae8d0e48e4d5e81ad718bc6" default))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(put 'narrow-to-region 'disabled nil)

(load-theme 'zenburn)

(require 'comint-config)
(require 'eshell-config)
(require 'evil-config)
(require 'helm-config)
(require 'misc-config)
(require 'my-smartparens-config)

(require 'keybindings)
