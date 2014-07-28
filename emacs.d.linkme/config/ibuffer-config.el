(require 'ibuffer)
(require 'ibuffer-vc)
(require 'ibuffer-git)

(setq-default ibuffer-default-sorting-mode 'major-mode)

(defun ibuffer-set-up-preferred-filters ()
  (ibuffer-vc-set-filter-groups-by-vc-root)
  (unless (eq ibuffer-sorting-mode 'filename/process)
    (ibuffer-do-sort-by-filename/process)))

(add-hook 'ibuffer-hook 'ibuffer-set-up-preferred-filters)

(define-ibuffer-column size-h
  (:name "Size" :inline t)
  (cond
   ((> (buffer-size) 1000000) (format "%7.1fM" (/ (buffer-size) 1000000.0)))
   ((> (buffer-size) 1000) (format "%7.1fk" (/ (buffer-size) 1000.0)))
   (t (format "%8d" (buffer-size)))))

(setq ibuffer-saved-filter-groups
      '(("default"
	 ("org" ;; all org-related buffers
	  (mode . org-mode))
	 ("darkdynasty-haskell"
	  (filename . "code/darkdynasty-haskell"))
	 ("darkdynasty"
	  (filename . "code/darkdynasty"))
	 ("emacs"
	  (or
	   (filename . ".dotfiles/emacs.d.linkme")
	   (filename . ".emacs.d/")))
	 ("dotfiles"
	  (filename . ".dotfiles"))
	 ("Programming" ;; prog stuff not already in MyProjectX
	  (or
	   (mode . c-mode)
	   (mode . perl-mode)
	   (mode . python-mode)
	   (mode . emacs-lisp-mode)
	   ;; etc
	   ))
	 ("ERC"   (mode . erc-mode)))))

(setq ibuffer-formats
      '((mark modified read-only vc-status-mini " "
              (name 18 18 :left :elide)
              " "
              (size-h 9 -1 :right)
              " "
              (mode 16 16 :left :elide)
              " "
              filename-and-process)
	(mark modified read-only git-status-mini " "
              (name 18 18 :left :elide)
              " "
              (size-h 9 -1 :right)
              " "
              (mode 16 16 :left :elide)
              " "
              (git-status 8 8 :left)
              " "
              filename-and-process)))

(add-hook 'ibuffer-mode-hook
  (lambda ()
    (ibuffer-switch-to-saved-filter-groups "default")))

(provide 'ibuffer-config)
