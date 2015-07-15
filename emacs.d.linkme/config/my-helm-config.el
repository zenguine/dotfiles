;; Fuzzy matching
(setq helm-buffers-fuzzy-matching t)
(setq helm-M-x-fuzzy-match t)
(setq helm-apropos-fuzzy-match t)
(setq helm-semantic-fuzzy-match t
      helm-imenu-fuzzy-match    t)

(require 'helm)
(require 'helm-files)
(require 'helm-projectile)

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

(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action)
(define-key helm-map (kbd "C-z") 'helm-select-action)

(defun helm-multi-occur-in-this-mode ()
  "Show all lines matching REGEXP in buffers with this major mode."
  (interactive)
  (helm-multi-occur
   (get-buffers-matching-mode major-mode)))

;; Helm-swoop config

(when (require 'helm-swoop nil 'noerrors)
  (define-key helm-swoop-map (kbd "M-i") 'helm-multi-swoop-all-from-helm-swoop)
  (when (require 'evil nil 'noerrors)
    (define-key evil-motion-state-map (kbd "M-i") 'helm-swoop-from-evil-search))
  (setq helm-multi-swoop-edit-save t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helm-projectile configuration stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
(define-key projectile-mode-map (kbd "C-c p p") 'jc/helm-projectile-switch-project)
(define-key projectile-mode-map (kbd "C-p") nil)

(defun helm-projectile-custom ()
  (interactive)
  (let ((src '(helm-source-projectile-buffers-list
	       helm-source-projectile-recentf-list
	       helm-source-projectile-files-list
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

;; Misc helm search keybindings
(ifhave 'helm-ag
  (global-set-key (kbd "M-s a") 'helm-ag))

;; helm-company config
(require 'helm-company)
(eval-after-load 'company
  '(progn
     (define-key company-mode-map (kbd "C-:") 'helm-company)
     (define-key company-active-map (kbd "C-:") 'helm-company)))

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

(define-key helm-map (kbd "<escape>") 'helm-like-unite/body)
(define-key helm-map (kbd "C-[") 'helm-like-unite/body)

(provide 'my-helm-config)
