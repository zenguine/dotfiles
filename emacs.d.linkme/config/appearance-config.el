(setq-default highlight-tabs t)
(setq inhibit-startup-message t)
(setq-default show-trailing-whitespace nil)

;; See http://bzg.fr/emacs-hide-mode-line.html
(defvar-local hidden-mode-line-mode nil)
(defvar-local hide-mode-line nil)
(setq-default line-spacing 0)

(define-minor-mode hidden-mode-line-mode
  "Minor mode to hide the mode-line in the current buffer."
  :init-value nil
  :global nil
  :variable hidden-mode-line-mode
  :group 'editing-basics
  (if hidden-mode-line-mode
      (setq hide-mode-line mode-line-format
            mode-line-format nil)
    (setq mode-line-format hide-mode-line
          hide-mode-line nil))
  (force-mode-line-update)
  ;; Apparently force-mode-line-update is not always enough to
  ;; redisplay the mode-line
  (redraw-display)
  (when (and (called-interactively-p 'interactive)
           hidden-mode-line-mode)
    (run-with-idle-timer
     0 nil 'message
     (concat "Hidden Mode Line Mode enabled.  "
             "Use M-x hidden-mode-line-mode to make the mode-line appear."))))

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; Sane scrolling
(setq redisplay-dont-pause t
      scroll-margin 3
      scroll-step 1
      scroll-conservatively 10000
      scroll-preserve-screen-position 1)

;; Set mouse wheel / touchpad scrolling behavior
(setq mouse-wheel-follow-mouse 't)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 5) ((control) . nil)))

;; 'y' or 'n' instead of 'yes' or 'no'
(fset 'yes-or-no-p 'y-or-n-p)

(global-hl-line-mode 1)
(column-number-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Theme stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun guess-company-face-defaults ()
  (let ((bg (face-attribute 'default :background)))
    (custom-set-faces
     `(company-tooltip ((t (:inherit default :background ,(color-lighten-name bg 2)))))
     `(company-scrollbar-bg ((t (:background ,(color-lighten-name bg 10)))))
     `(company-scrollbar-fg ((t (:background ,(color-lighten-name bg 5)))))
     `(company-tooltip-selection ((t (:inherit font-lock-function-name-face))))
     `(company-tooltip-common ((t (:inherit font-lock-constant-face)))))))

(defun load-theme-with-extras (theme)
  "Load theme while also enabling extra special configuration
on a per-theme basis."
  (interactive (list (intern (read-string "Load which theme?: "))))
  (case theme
    (gruvbox
     (progn
       ;; (load-theme 'zenburn)
       (guess-company-face-defaults)))
    (t nil))
  (load-theme theme))

(defvar random-theme-candidates
  '(
    zenburn
    solarized-dark
    monokai
    gruvbox
    spacegray
    firebelly
    )
  "List of themes I like, one of which will be randomly selected to
   load at emacs initialization time.")

(defun load-theme-randomly (&optional candidates)
  "Randomly pick a theme from CANDIDATES (if non-nil, otherwise use
`random-theme-candidates'), and load it."
  (interactive)
  (let ((chosen (random-element (or candidates random-theme-candidates))))
    (load-theme chosen t)
    chosen))

(load-theme 'zenburn t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Diminish -- stop minor modes from cluttering up my modeline
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when (require 'nyan-mode nil 'noerror) (nyan-mode))
(when (require 'rainbow-mode nil 'noerror) (rainbow-mode))
(require 'diminish)
(-each '((git-gutter . git-gutter-mode)
         (smartparens . smartparens-mode)
         (projectile . projectile-mode)
         (guide-key . guide-key-mode)
         (undo-tree . undo-tree-mode)
         (elisp-slime-nav . elisp-slime-nav-mode)
         (haskell-doc . haskell-doc-mode)
         ) (lambda (x)
             (eval-after-load (car x)
               `(diminish ',(cdr x)))))

(-each '((shm . (structured-haskell-mode . " shm"))
         (flycheck . (flycheck-mode . " flyc")))
  (lambda (x)
    (eval-after-load (car x)
      `(diminish ',(cadr x) ,(cddr x)))))

(defun change-font-size (size)
  (interactive
   (list (string-to-int (read-string "Font size? (default 110):" nil nil "110"))))
  (set-face-attribute 'default nil :height size))


(defun set-small-font ()
  (interactive)
  (change-font-size 90))

(defun set-medium-font ()
  (interactive)
  (change-font-size ))

(defun set-screen-font ()
  (interactive)
  (change-font-size 110))

(defun set-big-font ()
  (interactive)
  (change-font-size 120))

(provide 'appearance-config)
