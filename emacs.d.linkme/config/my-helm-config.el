;;; XXX: Move the stuff below somewhere sensical.  was taken from keybindings.el
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Personal global map
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-prefix-command 'personal-global-map)
(global-set-key (kbd "C-x x") 'personal-global-map)
(bind-key "t" 'term personal-global-map)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tags bindings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-prefix-command 'my-tags-map)
(global-set-key (kbd "C-x t") 'my-tags-map)
(bind-key "v" 'visit-tags-table my-tags-map)
(def-key my-tags-map (kbd "d") 'message tags-file-name)
(def-key my-tags-map (kbd "D") 'message (string-join tags-table-list " | "))
(bind-key "f" 'find-tag my-tags-map)
(bind-key "F" 'find-tag-regexp my-tags-map)
(bind-key "s" 'tags-search my-tags-map)
(bind-key "a" 'tags-apropos my-tags-map)
(bind-key "r" 'tags-reset-tags-tables my-tags-map)
(bind-key "e" 'etags-select-find-tag my-tags-map)

(make-variable-buffer-local
 (defvar inside-helm-eval-expression-p
   nil
   "hackish way to check if I'm in a helm-eval-expression.. yay dynamic binding.."))

(defun my-helm-eval-expression-with-eldoc ()
  (interactive)
  (let ((inside-helm-eval-expression-p t))
    (call-interactively 'helm-eval-expression-with-eldoc)))

(use-package helm
  :ensure t
  :bind (("M-:" . my-helm-eval-expression-with-eldoc)
         ("M-x" . helm-M-x)
         ("C-x b" . helm-buffers-list)
         ("C-x C-f" . helm-find-files)
         ("C-x C-m" . helm-M-x)
         ("C-c h a" . helm-apropos)
         ("C-c i" . helm-semantic-or-imenu)
         ("M-y" . helm-show-kill-ring))
  :init
  (setq helm-buffers-fuzzy-matching t)
  (setq helm-M-x-fuzzy-match t)
  (setq helm-apropos-fuzzy-match t)
  (setq helm-semantic-fuzzy-match t
        helm-imenu-fuzzy-match    t)
  (bind-key "d" 'helm-descbinds help-map)
  (bind-key "b" 'helm-bookmarks personal-global-map)
  (bind-key "h" 'helm-etags-select my-tags-map)
  :config
  (require 'hydra)
  (require 'helm-files)
  (setq helm-idle-delay 0.1
        helm-input-idle-delay 0.1
        helm-locate-command "locate %s -e %s")

  (dolist (ext '("\\.elc$" "\\.pyc$"))
    (add-to-list 'helm-boring-file-regexp-list ext))

  ;; General helm settings
  (setq helm-quick-update t)
  (setq helm-bookmark-show-location t)
  (setq helm-split-window-in-side-p t)
  (setq helm-ff-search-library-in-sexp t) ;; TODO: What does this do?
  (setq helm-ff-file-name-history-use-recentf t)

  (setq helm-for-files-preferred-list
        '(helm-source-buffers-list
          helm-source-projectile-files-list
          helm-source-recentf
          helm-source-files-in-current-dir
          helm-source-locate))

  (bind-key "<tab>" 'helm-execute-persistent-action helm-map)
  (bind-key "C-i" 'helm-execute-persistent-action helm-map)
  (bind-key "C-z" 'helm-select-action helm-map)

  (defun my-helm-eval-expression-with-eldoc ()
    (interactive)
    (let ((inside-helm-eval-expression-p t))
      (call-interactively 'helm-eval-expression-with-eldoc)))

  (defun helm-multi-occur-in-this-mode ()
    "Show all lines matching REGEXP in buffers with this major mode."
    (interactive)
    (helm-multi-occur
     (get-buffers-matching-mode major-mode)))

  ;; Change helm appearance to look more like ido-verticle or grizzl..

  (setq helm-display-header-line nil) ;; t by default
  (set-face-attribute 'helm-source-header nil :height 0.1)
  (helm-autoresize-mode 1)
  (setq helm-autoresize-max-height 30)
  (setq helm-autoresize-min-height 30)
  (setq helm-split-window-in-side-p t)

  ;; add this function to helm-before-initialize-hook if you want source headers
  ;; for helm commands with multiple sources. i.e:
  ;; (remove-hook 'helm-before-initialize-hook 'helm-toggle-header-line)
  (defun helm-toggle-header-line ()
    (if (= (length helm-sources) 1)
        (set-face-attribute 'helm-source-header nil :height 0.1)
      (set-face-attribute 'helm-source-header nil :height 1.0)))

  ;; Cool unite/vim style helm hydra
  (defhydra helm-like-unite (:hint nil
                                   :color pink)
    "
Nav ^^^^^^^^^        Mark ^^          Other ^^       Quit
^^^^^^^^^^------------^^----------------^^----------------------
_K_ ^ ^ _k_ ^ ^     _m_ark           _v_iew         _i_: cancel
^↕^ _h_ ^✜^ _l_     _t_oggle mark    _H_elp         _o_: quit
_J_ ^ ^ _j_ ^ ^     _U_nmark all     _d_elete
^^^^^^^^^^                           _f_ollow: %(helm-attr 'follow)
"
    ;; arrows
    ("h" helm-beginning-of-buffer)
    ("j" helm-next-line)
    ("k" helm-previous-line)
    ("l" helm-end-of-buffer)
    ;; beginning/end
    ("g" helm-beginning-of-buffer)
    ("G" helm-end-of-buffer)
    ;; scroll
    ("K" helm-scroll-other-window-down)
    ("J" helm-scroll-other-window)
    ;; mark
    ("m" helm-toggle-visible-mark)
    ("t" helm-toggle-all-marks)
    ("U" helm-unmark-all)
    ;; exit
    ("<escape>" keyboard-escape-quit "" :exit t)
    ("o" keyboard-escape-quit :exit t)
    ("i" nil)
    ;; sources
    ("}" helm-next-source)
    ("{" helm-previous-source)
    ;; rest
    ("H" helm-help)
    ("v" helm-execute-persistent-action)
    ("d" helm-persistent-delete-marked)
    ("f" helm-follow-mode))

  (bind-key "<escape>" 'helm-like-unite/body helm-map)
  (bind-key "C-[" 'helm-like-unite/body helm-map)
  (helm-mode 1))


(use-package helm-projectile
  :ensure t
  :commands (jc/helm-projectile-switch-project helm-projectile-custom)
  :init
  (bind-key "C-c p p" 'jc/helm-projectile-switch-project projectile-mode-map)
  :config
  (helm-projectile-on)
  ;; whoa snazzy.  rebind projectile-switch-project-by-name to
  ;; use projectile-persp-switch-project.. but not recursively.
  ;; hence the nested "letf"
  (defun jc/helm-projectile-switch-project (&rest args)
    (interactive)
    "Like helm-projectile-switch project but also switches perspectives
   just like how projectile-persp-switch-project does."
    (let ((oldf (symbol-function 'projectile-switch-project-by-name)))
      (letf (((symbol-function-nonrec 'projectile-switch-project-by-name)
              (symbol-function 'projectile-persp-switch-project)))
        (apply 'helm-projectile-switch-project args))))

  (helm-projectile-on)
  (bind-key "C-p" nil projectile-mode-map)

  (defun helm-projectile-custom ()
    (interactive)
    (let ((src '(helm-source-projectile-buffers-list
                 helm-source-projectile-files-list
                 helm-source-projectile-recentf-list
                 helm-source-projectile-projects)))
      (helm :sources src
            :buffer "*helm projectile: custom*"
            :prompt (projectile-prepend-project-name "pattern: "))))

  (setq helm-projectile-sources-list '(
                                       helm-source-projectile-projects
                                       helm-source-projectile-buffers-list
                                       helm-source-projectile-recentf-list
                                       helm-source-projectile-files-list
                                       ))

  (setq projectile-switch-project-action 'helm-projectile)
)

(use-package helm-swoop
  :ensure t
  :bind (("C-c o" . helm-swoop)
         ("C-c C-o" . my-helm-multi-swoop))
  :commands helm-multi-swoop-all-from-helm-swoop helm-swoop-from-evil-search
  :init
  (require 'evil)
  (bind-key "M-i" 'helm-swoop-from-evil-search evil-motion-state-map)
  :config
  (defun my-helm-multi-swoop (arg)
    (interactive "P")
    (if arg
        (call-interactively 'helm-multi-swoop-all)
      (call-interactively 'helm-multi-swoop)))

  (bind-key "M-i" 'helm-multi-swoop-all-from-helm-swoop helm-swoop-map)
  (setq helm-multi-swoop-edit-save t))

;; Misc helm search keybindings
(use-package helm-ag
  :ensure t
  :bind ("M-s a" . helm-ag))

;; helm-company config
(use-package helm-company
  :ensure t
  :commands helm-company
  :defer t
  :init
  (bind-key "C-:" 'helm-company company-mode-map)
  (bind-key "C-:" 'helm-company company-active-map))

(provide 'my-helm-config)
