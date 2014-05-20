; Load ELPA
;;; -*- lexical-binding: t -*-
(add-to-list 'load-path "~/.emacs.d")
(require 'package)
(require 'my-autoloads)
(setq package-enable-at-startup nil)
(setq evil-want-C-u-scroll t)
(setq evil-move-cursor-back nil)
(package-initialize)
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.milkbox.net/packages/"))
(add-to-theme-path-recursive "~/.emacs.d/themes")
(add-to-load-path-recursive "~/.emacs.d/config")
(add-to-load-path-recursive "~/.emacs.d/site-lisp")

(when (require 'evil nil 'noerror)
  (require 'surround)
  (global-surround-mode nil))

(require 'cl)

(defadvice terminal-init-xterm (around map-S-up-escape-sequence activate)
  (define-key input-decode-map (kbd "C-i") (kbd "H-i")))

;; Guarantee all packages are installed on start
(defvar packages-list
  '(
    ace-jump-mode
    ag
    auto-complete
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
    helm-projectile
    ido-vertical-mode
    jedi
    key-chord
    magit
    multi-term
    paredit
    popwin
    projectile
    smart-mode-line
    smartparens
    smex
    sublimity
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
(winner-mode t)
(show-paren-mode t)
(setq-default highlight-tabs t)
(setq inhibit-startup-message t)
(setq-default show-trailing-whitespace nil)

(add-hook 'prog-mode-hook (lambda () (setq show-trailing-whitespace t)))

(setq indent-tabs-mode nil)
(setq tab-width 4)
(setq c-basic-offset 4)

(add-to-list 'auto-mode-alist '("\\.hscm\\'" . scheme-mode))

; no backup files
(setq make-backup-files nil
      backup-inhibited t
      auto-save-default nil)

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(ido-vertical-mode)
(sublimity-mode t)
(global-hl-line-mode 1)
(key-chord-mode +1)
(setq key-chord-two-keys-delay 0.015)
(setq key-chord-one-key-delay 0.08)

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

(defun my-elisp-mode-hook ()
  (modify-syntax-entry ?- "w"))

(add-hook 'emacs-lisp-mode-hook 'my-elisp-mode-hook)

(defun my-lisp-interaction-mode-hook ()
  (local-unset-key (kbd "C-j")))

(add-hook 'lisp-interaction-mode-hook 'my-lisp-interaction-mode-hook)

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

(put 'narrow-to-region 'disabled nil)

(load-theme 'solarized-dark t)

(require 'comint-config)
(require 'eshell-config)
(require 'evil-config)
(require 'my-helm-config)
(require 'misc-config)
(require 'term-config)
(require 'org-config)
(require 'evil-org)
(require 'mu4e-config)
(require 'org-mu4e)
(require 'popwin-config)
(require 'my-smartparens-config)
(require 'sml-config)
(require 'python-config)
(require 'paredit-config)

(require 'keybindings)

; (start-or-switch-irc t)
