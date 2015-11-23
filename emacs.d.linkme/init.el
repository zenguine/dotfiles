(setq custom-file "~/.emacs.d/custom.el")

(setq debug-on-error t)
(setq custom-faces-file "~/.emacs.d/custom-faces.el")
(load custom-file 'noerror)

;; Load ELPA
;;;; -*- lexical-binding: t -*-
(require 'package)
(add-to-list 'load-path (concat user-emacs-directory "elisp"))
(require 'my-autoloads)
(require 'cl)

(add-to-list 'package-archives
	     '("melpa" . "http://melpa.milkbox.net/packages/"))
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
(setq package-enable-at-startup nil)
(package-initialize)

(add-to-theme-path-recursive (concat user-emacs-directory "themes"))
(add-to-load-path-recursive (concat user-emacs-directory "config")) ; configuration for packages
(add-to-load-path-recursive (concat user-emacs-directory "site-lisp")) ; snippets / misc that I didn't completely write
(add-to-load-path-recursive (concat user-emacs-directory "elisp")) ; my own custom library / utility code

;; ---------------------------------------------
;; ELPA Initialization
;; ---------------------------------------------

;;; Elpa packages list to initialize on start
(defvar packages-list
  '(
    ac-js2
    afternoon-theme
    ag
    ample-theme
    anzu
    async
    avy
    beacon
    bind-key
    birds-of-paradise-plus-theme
    busybee-theme
    color-identifiers-mode
    color-theme-sanityinc-tomorrow
    company-ghc
    company-tern
    dash
    dash-functional
    diminish
    discover-my-major
    ein
    elisp-slime-nav
    elpy
    etags-select
    evil
    evil-anzu
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
    ggtags
    ghc
    git-gutter
    git-messenger
    git-timemachine
    gotham-theme
    gruvbox-theme
    guide-key
    haskell-mode
    helm
    helm-ag
    helm-company
    helm-dash
    helm-descbinds
    helm-git
    helm-google
    helm-gtags
    helm-proc
    helm-projectile
    helm-pydoc
    helm-swoop
    helm-themes
    hi2
    hydra
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
    mustang-theme
    nyan-mode
    obsidian-theme
    org-plus-contrib
    paradox
    paredit
    perspective
    persp-projectile
    popwin
    powerline
    pretty-mode
    projectile
    quack
    rainbow-blocks
    rainbow-delimiters
    rainbow-mode
    req-package
    s
    seq
    skewer-mode
    smart-mode-line
    smartparens
    smex
    soft-charcoal-theme
    soothe-theme
    spacegray-theme
    subatomic-theme
    sublime-themes
    sublimity
    tern
    tern-auto-complete
    twilight-anti-bright-theme
    use-package
    warm-night-theme
    web-beautify
    web-mode
    ws-butler
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

;; (setq use-package-always-ensure t)
(setq use-package-verbose t)
;; (setq req-package-log-level 'debug)
(require 'use-package)
;; (require 'req-package)

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
(require 'appearance-config)
(require 'evil-config)
(require 'comint-config)
(require 'term-config)
(require 'company-config)
(require 'eshell-config)
;; (require 'evil-org)

(require 'git-gutter-config)
(require 'guide-key-config)
(require 'ido-config)
(require 'key-chord-config)
(require 'mu4e-config)
(require 'projectile-config)
(require 'my-helm-config)
(require 'misc-config)
(require 'my-smartparens-config)
(require 'org-config)
(message "Fuck this 1")

;; (require 'org-mu4e)
;; (require 'paredit-config)
(message "Fuck this 2")
(require 'popwin-config)
(message "Fuck this 3")
;; (require 'my-modeline-config)
(message "Fuck this 4")
(require 'sublimity-config)

(message "Fuck this 5")
(message "Fuck this 6")
(require 'expand-region-config)
(message "Fuck this 7")
(require 'multiple-cursors-config)
(message "Fuck this 8")
(require 'ibuffer-config)
(message "Fuck this 9")
(require 'winner-config)
(message "Fuck this 10")
(require 'flycheck-config)
(message "Fuck this 11")
(message "Fuck this 12")
(require 'hydra-config)
(message "Fuck this 13")
(require 'erc-config)
(message "Fuck this 14")

;;; Yasnippet must come before autocomplete config for both to work together
(require 'yasnippet-config)
(message "Fuck this 15")

;;; Language specific configuration
;; (require 'haskell-config)
;; (message "Fuck this 16")
;; (require 'idris-config)
;; (message "Fuck this 17")
;; (require 'javascript-config)
;; (message "Fuck this 18")
;; (require 'lisp-config)
;; (message "Fuck this 20")
;; (require 'prog-config)
;; (message "Fuck this 21")
;; (require 'python-config)
;; (message "Fuck this 22")
;; (require 'scheme-config)
;; (message "Fuck this 23")
;; (require 'c-config)
;; (message "Fuck this 24")
;; (require 'web-config)
;; (message "Fuck this 25")

(require 'keybindings)
(message "Fuck this 26")

;; Start the server from here rather than with emacs --daemon since
;; some appearance configuration appears to not work correctly when
;; run like that.
(server-start)
