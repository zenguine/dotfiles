; Load ELPA
;;; -*- lexical-binding: t -*-

(require 'package)
(setq package-enable-at-startup nil)
(setq evil-want-C-u-scroll t)
(package-initialize)
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.milkbox.net/packages/"))
(add-to-list 'load-path "~/.emacs.d")
(add-to-list 'load-path "~/.emacs.d/elpa")
(add-to-list 'load-path "~/.emacs.d/mu4e")
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
    guide-key
    helm
    ido-vertical-mode
    ipython
    jedi
    key-chord
    magit
    multi-term
    projectile
    python-mode
    smart-mode-line
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

(add-hook 'before-save-hook 'delete-trailing-whitespace)
(add-hook 'before-save-hook 'whitespace-cleanup)

(setq indent-tabs-mode nil)
(setq tab-width 4)
(setq c-basic-offset 4)

; no backup files
(setq make-backup-files nil
      backup-inhibited t
      auto-save-default nil)

;; (global-linum-mode t)
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(ido-vertical-mode)
(sublimity-mode t)
(global-hl-line-mode 1)
(key-chord-mode +1)
(setq key-chord-two-keys-delay 0.015)
(setq key-chord-one-key-delay 0.08)

(autoload 'project-root "util" nil t)
(autoload 'erc "erc" nil t)
(autoload 'start-irc "erc-config" nil t)
(autoload 'start-or-switch-irc "erc-config" nil t)
(autoload 'switch-to-other-buffer "util" nil t)
(autoload 'multi-term-toggle "util" nil t)
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

(require 'guide-key)
(setq guide-key/guide-key-sequence '("C-x" "C-c"))
(setq guide-key/recursive-key-sequence-flag t)
(guide-key-mode 1)

(evil-mode 1)
(ido-mode 1)
(flx-ido-mode 1)
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(projectile-global-mode 1)
(setq evil-overriding-maps nil)
(setq evil-intercept-maps nil)
(setq evil-want-C-u-scroll t)
(setq undo-tree-auto-save-history t)
(setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo")))


; Utility stuff

; Find project root based on .git file or something

(setq project-root-markers '(".git" ".svn"))

;; Mode configuratoin
;python

(setq jedi:complete-on-dot t)
(setq jedi:install-imenu t)
(setq jedi:get-in-function-call-delay 100)

(defun my-python-mode-hook ()
  (local-set-key (kbd "C-c !") 'python-shell-switch-to-shell)
  (local-set-key (kbd "C-c T a") 'pytest-test-all)
  (local-set-key (kbd "C-c T f") 'pytest-test-current-file)
  (local-set-key (kbd "C-c T t") 'pytest-test-specific-test)
  (jedi:setup)
  (modify-syntax-entry ?_ "w"))

(add-hook 'python-mode-hook 'my-python-mode-hook)

(defun my-lisp-mode-hook ()
  (modify-syntax-entry ?- "w"))

(add-hook 'lisp-mode-hook 'my-lisp-mode-hook)

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

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

(put 'narrow-to-region 'disabled nil)

(load-theme 'zenburn)

(require 'comint-config)
(require 'eshell-config)
(require 'evil-config)
(require 'helm-config)
(require 'misc-config)
(require 'term-config)
(require 'org-config)
(require 'evil-org)
(require 'mu4e-config)
(require 'org-mu4e)
(require 'my-smartparens-config)

(require 'keybindings)

(start-or-switch-irc t)
