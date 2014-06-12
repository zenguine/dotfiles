;; Load ELPA
;;;; -*- lexical-binding: t -*-
(add-to-list 'load-path "~/.emacs.d")
(require 'package)
(require 'my-autoloads)
(require 'cl)

(setq package-enable-at-startup nil)
(package-initialize)

(add-to-list 'package-archives
	     '("melpa" . "http://melpa.milkbox.net/packages/"))

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

;; ---------------------------------------------
;; El-get initialization
;; ---------------------------------------------

;; EL-GET
(add-to-list 'load-path (locate-user-emacs-file "el-get/el-get"))

(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
    (goto-char (point-max))
    (eval-print-last-sexp)))

(defun el-get-sync-recipes (overlay)
  (let* ((recipe-glob (locate-user-emacs-file (concat overlay "/recipes/*.rcp")))
         (recipe-files (file-expand-wildcards recipe-glob))
         (recipes (mapcar 'el-get-read-recipe-file recipe-files)))
    (mapcar (lambda (r) (add-to-list 'el-get-sources r)) recipes)
    (el-get 'sync (mapcar 'el-get-source-name recipes))))
(setq el-get-user-package-directory user-emacs-directory)

;; EL-GET SYNC OVERLAYS
(el-get-sync-recipes "el-get-haskell")
(el-get-sync-recipes "el-get-user")

;; --------------------------------
;; My personal configuration stuff
;; --------------------------------
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)

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
