;; Load ELPA
;;;; -*- lexical-binding: t -*-
(add-to-list 'load-path "~/.emacs.d")
(require 'package)
(require 'my-autoloads)
(setq package-enable-at-startup nil)
(package-initialize)
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.milkbox.net/packages/"))
(add-to-theme-path-recursive "~/.emacs.d/themes")
(add-to-load-path-recursive "~/.emacs.d/config")
(add-to-load-path-recursive "~/.emacs.d/site-lisp")

(require 'cl)
(defadvice terminal-init-xterm (around map-S-up-escape-sequence activate)
  (define-key input-decode-map (kbd "C-i") (kbd "H-i")))

;;; Guarantee all packages are installed on start
(defvar packages-list
  '(
    ace-jump-mode
    ag
    auto-complete
    elpy
    evil
    evil-args
    evil-leader
    evil-matchit
    evil-nerd-commenter
    fiplr
    flx
    flx-ido
    flycheck
    git-gutter
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

;; Utility stuff

;; Find project root based on .git file or something

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

;;; Load these first..
(require 'misc-config)
(require 'appearance-config)
(require 'evil-config)

(require 'comint-config)
(require 'eshell-config)
(require 'evil-org)
(require 'git-gutter-config)
(require 'guide-key-config)
(require 'ido-config)
(require 'key-chord-config)
(require 'mu4e-config)
(require 'my-helm-config)
(require 'my-smartparens-config)
(require 'org-config)
(require 'org-mu4e)
(require 'paredit-config)
(require 'popwin-config)
(require 'projectile-config)
(require 'sml-config)
(require 'sublimity-config)
(require 'term-config)

;;; Yasnippet must come before autocomplete config for both to work together
(require 'yasnippet-config)
(require 'my-ac-config)

;;; Language specific configuration
(require 'prog-config)
(require 'python-config)
(require 'lisp-config)
(require 'haskell-config)

(require 'keybindings)

(start-or-switch-irc t)
