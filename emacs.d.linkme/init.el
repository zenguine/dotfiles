;; Load ELPA  
;;; -*- lexical-binding: t -*-

(require 'package)
(setq package-enable-at-startup nil)
(setq evil-want-C-u-scroll t)
(package-initialize)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/"))
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'load-path "~/.emacs.d")
(add-to-list 'load-path "~/.emacs.d/elpa")
(load "surround.el")
(require 'surround)
(global-surround-mode 1)
(require 'cl)

; Separate tab from C-i
(define-key input-decode-map (kbd "C-i") (kbd "H-i"))

;; Guarantee all packages are installed on start
(defvar packages-list
  '(
    auto-complete
    autopair
    color-theme-solarized 
    evil
    evil-leader
    evil-matchit
    evil-nerd-commenter
    fiplr
    flycheck
    flycheck-haskell
    helm
    ipython
    magit
    python-mode
    smex
    key-chord
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

;; (global-linum-mode t)
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)
(global-set-key (kbd "RET") 'newline-and-indent)
(global-set-key (kbd "C-c h") help-map)

(autoload 'autopair-global-mode "autopair" nil t)
(autopair-global-mode)

; Enable auto-complete
(require 'auto-complete-config)
(ac-config-default)

; Evil mode customization
(evil-mode 1)
(ido-mode 1)
(setq ido-enable-flex-matching t)
(setq evil-overriding-maps nil)
(setq evil-intercept-maps nil)
; (helm-mode 1)

(setq evil-want-C-u-scroll t)

; (define-key evil-emacs-state-map  (kbd "C-c C-z") 'suspend-emacs)

(define-key evil-insert-state-map "j" 'cofi/maybe-exit)
; (define-key evil-insert-state-map  (kbd "C-c C-z") 'suspend-emacs)

(define-key evil-normal-state-map "H" 'evil-first-non-blank)
(define-key evil-normal-state-map  (kbd "C-p") 'fiplr-find-file)
; (define-key evil-normal-state-map  (kbd "C-c C-z") 'suspend-emacs)
(define-key evil-normal-state-map (kbd "TAB") 'evil-jump-item)
; Mapping for C-i
(define-key evil-normal-state-map (kbd "H-i") 'evil-jump-forward)

(define-key evil-normal-state-map "L" 'evil-end-of-line)

(define-key evil-normal-state-map (kbd "zn")
  (lambda () (interactive) (progn
			     (move-end-of-line nil)
			     (newline-and-indent))))


(define-key evil-normal-state-map (kbd "zp")
  (lambda () (interactive) (progn
			     (move-beginning-of-line nil)
			     (newline-and-indent))))

; Utility stuff

; Find project root based on .git file or something

(setq project-root-markers '(".git" ".svn"))

(defun project-root ()
  "Locate the root of the project by walking up the directory tree.
The first directory containing one of project-root-markers is the root.
If no root marker is found, the current working directory is used."
  (let ((cwd (if (buffer-file-name)
                 (directory-file-name
                  (file-name-directory (buffer-file-name)))
               (file-truename "."))))
    (or (find-project-root cwd project-root-markers)
        cwd)))

(defun find-project-root (path root-markers)
  "Tail-recursive part of project-root."
  (let* ((this-dir (file-name-as-directory (file-truename path)))
         (parent-dir (expand-file-name (concat this-dir "..")))
         (system-root-dir (expand-file-name "/")))
    (cond
     ((root-p path root-markers) this-dir)
     ((equal system-root-dir this-dir) nil)
     (t (find-project-root parent-dir root-markers)))))

(defun anyp (pred seq)
  "True if any value in SEQ matches PRED."
  (catch 'found
    (cl-map nil (lambda (v)
                  (when (funcall pred v)
                    (throw 'found v)))
            seq)))

(defun root-p (path root-markers)
  "Predicate to check if the given directory is a project root."
  (let ((dir (file-name-as-directory path)))
    (anyp (lambda (marker)
                  (file-exists-p (concat dir marker)))
                root-markers)))

(defun move-window-or-create (dir)
  "Move to window in a direction or create if it doesnt exist"
  (let ((move-func (pcase dir
		     (`right 'evil-window-right)
		     (`left 'evil-window-left)
		     (`above 'evil-window-up)
		     (`below 'evil-window-down))))
    (condition-case nil
	(funcall move-func 1)
      (error (progn
	       (split-window nil nil dir)
	       (funcall move-func 1))))))

(define-key evil-normal-state-map (kbd "C-h") (lambda () (interactive) (move-window-or-create 'left)))
(define-key evil-normal-state-map (kbd "C-j") (lambda () (interactive) (move-window-or-create 'below)))
(define-key evil-normal-state-map (kbd "C-k") (lambda () (interactive) (move-window-or-create 'above)))
(define-key evil-normal-state-map (kbd "C-l") (lambda () (interactive) (move-window-or-create 'right)))

; Evil "leader" mappings

(define-key evil-normal-state-map ",wd" 'evil-window-delete)
(define-key evil-normal-state-map ",wo" 'delete-other-windows)
(define-key evil-normal-state-map ",bd" 'kill-this-buffer)
(define-key evil-normal-state-map ",bl" 'bs-show)
(define-key evil-normal-state-map ",ev" (lambda () (interactive)
					  (find-file "~/.vimrc")))
(define-key evil-normal-state-map ",ee" (lambda () (interactive)
					  (find-file "~/.emacs.d/init.el")))

(define-key evil-normal-state-map ",l" 'linum-mode)
(global-set-key (kbd "\\") 'evilnc-comment-or-uncomment-lines)
(define-key evil-normal-state-map ",c" 'evilnc-comment-or-uncomment-lines)
(define-key evil-visual-state-map ",c" 'evilnc-comment-or-uncomment-lines)


(define-key evil-normal-state-map "gb" 'ido-switch-buffer)
(define-key evil-normal-state-map "gs" 'shell)
(define-key evil-normal-state-map "gS" 'ansi-term)

(define-key evil-normal-state-map (kbd ", SPC") (lambda ()
						  (interactive)
						  (switch-to-buffer (other-buffer))))


(evil-define-command cofi/maybe-exit ()
  :repeat change
  (interactive)
  (let ((modified (buffer-modified-p)))
    (insert "j")
    (let ((evt (read-event (format "Insert %c to exit insert state" ?k)
			   nil 0.5)))
      (cond
       ((null evt) (message ""))
       ((and (integerp evt) (char-equal evt ?k))
	(delete-char -1)
	(set-buffer-modified-p modified)
	(push 'escape unread-command-events))
       (t (setq unread-command-events (append unread-command-events
					      (list evt))))))))

;; Mode configuratoin
;python
; py.test stuff

(defun pytest-test-all ()
  "Run py.test from current directory running all tests"
  (interactive)
  (let ((results-buffer-name "*pytest-results:all*"))
    (if (get-buffer results-buffer-name)
	(save-excursion
	  (set-buffer results-buffer-name)
	  (erase-buffer)))
    (let ((default-directory (project-root)))
      (start-process "py-test:all"
		     results-buffer-name
		     "py.test"))
    (display-buffer results-buffer-name)))

(defun pytest-test-current-file ()
  "Run py.test on the current file"
  (interactive)
  (let* ((test-file-full (buffer-file-name))
	 (test-file (file-name-nondirectory test-file-full))
	 (process-name (format "pytest:%s" test-file))
	 (results-buffer-name (format "*pytest-results:%s*" test-file)))
    (if (get-buffer results-buffer-name)
	(save-excursion
	  (set-buffer results-buffer-name)
	  (erase-buffer)))
    (start-process process-name
		   results-buffer-name
		   "py.test"
		   test-file-full)
    (display-buffer results-buffer-name)))

(defun pytest-test-specific-test (testname)
  "Run py.test on a specific test"
  (interactive
   (list (read-string
	  (format "Run test (default %s): " (symbol-at-point))
	  nil nil
	  (symbol-at-point))))
  (let* ((test-file-full (buffer-file-name))
	 (test-file (file-name-nondirectory test-file-full))
	 (process-name (format "pytest:%s" testname))
	 (results-buffer-name (format "*pytest-results:%s*" testname)))
    (if (get-buffer results-buffer-name)
	(save-excursion
	  (set-buffer results-buffer-name)
	  (erase-buffer)))
    (start-process process-name
 		   results-buffer-name
 		   "py.test"
 		   test-file-full
 		   "-k"
 		   (format "%s" testname))
    (display-buffer results-buffer-name)))

(add-hook 'python-mode-hook (lambda ()
			      ; (flycheck-mode 0)
			      (local-set-key (kbd "C-c !") 'python-shell-switch-to-shell)
			      (local-set-key (kbd "C-c t a") 'pytest-test-all)
			      (local-set-key (kbd "C-c t f") 'pytest-test-current-file)
			      (local-set-key (kbd "C-c t t") 'pytest-test-specific-test)
			      ))


(setq
 python-shell-interpreter "ipython"
 python-shell-interpreter-args ""
 python-shell-prompt-regexp "In \\[[0-9]+\\]: "
 python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: "
 python-shell-completion-setup-code
 "from IPython.core.completerlib import module_completion"
 python-shell-completion-module-string-code
 "';'.join(module_completion('''%s'''))\n"
 python-shell-completion-string-code
 "';'.join(get_ipython().Completer.all_completions('''%s'''))\n")

 ;; pdbtrack constants
 (defconst py-pdbtrack-stack-entry-regexp
  "^> \\(.*\\)(\\([0-9]+\\))\\([?a-zA-Z0-9_<>]+\\)()"; this is new
 ;  "^> \\([^(]+\\)(\\([0-9]+\\))\\([?a-zA-Z0-9_]+\\)()"
 ;  "^> \\(.*\\)(\\([0-9]+\\))\\([?a-zA-Z0-9_]+\\)()"
   "Regular expression pdbtrack uses to find a stack trace entry.")


;(defconst py-pdbtrack-input-prompt "\n[(<]*[Pp]db[>)]+ "
(defconst py-pdbtrack-input-prompt "\n[(<]*[Ii]?[Pp]db[>)]+ "; this is new
   "Regular expression pdbtrack uses to recognize a pdb prompt.")

;; (py-pdbtrack-overlay-arrow nil)
;; (setq block (ansi-color-filter-apply block)) ; this is new
;; (setq target (py-pdbtrack-get-source-buffer block))

; From solarized color scheme apparently?
(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
  '(custom-safe-themes (quote ("fc5fcb6f1f1c1bc01305694c59a1a861b008c534cae8d0e48e4d5e81ad718bc6" default))))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
  )

(put 'narrow-to-region 'disabled nil)

(load-theme 'solarized-dark)
