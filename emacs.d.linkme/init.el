;; Load ELPA
;;;; -*- lexical-binding: t -*-
(add-to-list 'load-path "~/.emacs.d")
(require 'package)
(require 'my-autoloads)
(require 'cl)

(add-to-list 'package-archives
	     '("melpa" . "http://melpa.milkbox.net/packages/"))

(setq package-enable-at-startup nil)
(package-initialize)
(require 'dash)
(require 's)
(require 'f)


(add-to-theme-path-recursive "~/.emacs.d/themes")
(add-to-load-path-recursive "~/.emacs.d/config")
(add-to-load-path-recursive "~/.emacs.d/site-lisp")

;; ---------------------------------------------
;; ELPA Initialization
;; ---------------------------------------------

;;; Elpa packages list to initialize on start
(defvar packages-list
  '(
    ace-jump-mode
    ag
    company-ghc
    dash
    dash-functional
    diminish
    discover-my-major
    elisp-slime-nav
    elpy
    evil
    evil-args
    evil-leader
    evil-matchit
    evil-nerd-commenter
    expand-region
    f
    fiplr
    flx
    flx-ido
    flycheck
    flycheck-pos-tip
    ghc
    git-gutter
    git-timemachine
    gruvbox-theme
    guide-key
    haskell-mode
    helm
    helm-dash
    helm-descbinds
    helm-git
    helm-google
    helm-proc
    helm-projectile
    helm-pydoc
    helm-swoop
    helm-themes
    ibuffer-git
    ibuffer-vc
    ido-ubiquitous
    ido-vertical-mode
    idris-mode
    jedi
    key-chord
    magit
    molokai-theme
    multiple-cursors
    multi-term
    nyan-mode
    paredit
    popwin
    projectile
    quack
    rainbow-blocks
    rainbow-delimiters
    rainbow-mode
    s
    shm
    smartparens
    smex
    spacegray-theme
    sublime-themes
    sublimity
    zenburn-theme
    )
  "List of packages needs to be installed at launch")

(defun has-package-not-installed ()
  (loop for p in packages-list
	when (not (package-installed-p p)) do (return t)
	finally (return nil)))

;; Install all elpa packages listed above
(when (has-package-not-installed)
  ;; Check for new packages (package versions)
  (message "%s" "Get latest versions of all packages...")
  (package-refresh-contents)
  (message "%s" " done.")
  ;; Install the missing packages
  (dolist (p packages-list)
    (when (not (package-installed-p p))
      (package-install p))))

;; --------------------------------
;; My personal configuration stuff
;; --------------------------------
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Require autoloads files
;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'pastebin-autoloads)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Require configuration files
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
(require 'expand-region-config)
(require 'multiple-cursors-config)
(require 'ibuffer-config)
(require 'winner-config)
(require 'flycheck-config)

;;; Yasnippet must come before autocomplete config for both to work together
(require 'yasnippet-config)

;;; Language specific configuration
(require 'prog-config)
(require 'python-config)
(require 'lisp-config)
(require 'scheme-config)
(require 'idris-config)
(require 'haskell-config)

(require 'keybindings)
