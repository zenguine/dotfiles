(setq custom-file "~/.emacs.d/custom.el")
(setq custom-faces-file "~/.emacs.d/custom-faces.el")
(load custom-file 'noerror)

;; Load ELPA
;;;; -*- lexical-binding: t -*-
(add-to-list 'load-path "~/.emacs.d")
(require 'package)
(require 'my-autoloads)
(require 'cl)
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.milkbox.net/packages/"))
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
(setq package-enable-at-startup nil)
(package-initialize)

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
    ace-link
    ac-js2
    afternoon-theme
    ag
    ample-theme
    async
    birds-of-paradise-plus-theme
    color-theme-sanityinc-tomorrow
    company-ghc
    company-tern
    dash
    dash-functional
    diminish
    discover-my-major
    elisp-slime-nav
    elpy
    etags-select
    evil
    evil-args
    evil-leader
    evil-matchit
    evil-nerd-commenter
    evil-visualstar
    expand-region
    f
    fiplr
    flatland-theme
    flx
    flx-ido
    flycheck
    flycheck-pos-tip
    ghc
    git-gutter
    git-timemachine
    gotham-theme
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
    hi2
    ibuffer-git
    ibuffer-vc
    ido-ubiquitous
    ido-vertical-mode
    idris-mode
    info+
    jedi
    js2-mode
    js2-refactor
    key-chord
    load-theme-buffer-local
    magit
    molokai-theme
    monokai-theme
    multiple-cursors
    nyan-mode
    org-plus-contrib
    paredit
    popwin
    pretty-mode
    projectile
    quack
    rainbow-blocks
    rainbow-delimiters
    rainbow-mode
    s
    skewer-mode
    smart-mode-line
    smartparens
    smex
    soothe-theme
    spacegray-theme
    subatomic-theme
    sublime-themes
    sublimity
    tern
    tern-auto-complete
    web-beautify
    web-mode
    zenburn-theme)
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
(require 'dash)
(require 's)

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
(require 'company-config)

;;; Yasnippet must come before autocomplete config for both to work together
(require 'yasnippet-config)

;;; Language specific configuration
(require 'haskell-config)
(require 'idris-config)
(require 'javascript-config)
(require 'lisp-config)
(require 'prog-config)
(require 'python-config)
(require 'scheme-config)
(require 'web-config)

(require 'keybindings)

;; Start the server from here rather than with emacs --daemon since
;; some appearance configuration appears to not work correctly when
;; run like that.
(server-start)
