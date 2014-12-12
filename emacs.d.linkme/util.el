;;; -*- lexical-binding: t -*-

;;;;;;;;;;;;;;;;;;;;;
;; Convenience macros
;;;;;;;;;;;;;;;;;;;;;

(defmacro ifhave (x &rest body)
  "Shorthand for (when (require X)) BODY)"
  (declare (indent defun))
  `(when (require ,x nil 'noerror)
     ,@body))

(defmacro after (feature &rest body)
  "Shorthand for (eval-after-load <x> <body>)"
  (declare (indent defun))
  `(eval-after-load ,feature
     '(progn ,@body)))

(defmacro def-key (keymap key &rest definfo)
  "Like `define-key' but automagically creates a wrapping lambda if
   you pass it multiple arguments as the binding.  The first is
   assumed to be a symbol for the function to be called, and the rest
   are assumed to be the parameters to that function."
  (if (> (length definfo) 1)
      `(define-key
	 ,keymap
	 ,key
	 (lambda () (interactive)
	   (funcall
	    ,(car definfo)
	    ,@(cdr definfo)))
	 )
    `(define-key ,keymap ,key ,(car definfo))))

(defmacro global-def-key (key &rest definfo)
  "The equivalent of `def-key' for `global-set-key'."
  (if (> (length definfo) 1)
      `(global-set-key
	 ,key
	 (lambda () (interactive)
	   (funcall
	    ,(car definfo)
	    ,@(cdr definfo))))
    `(global-set-key ,key ,(car definfo))))

(after 'evil
       (defmacro evil-def-key (keymap state key &rest definfo)
	 "The equivalent of `def-key' for `evil-define-key'.  The
          other difference from `evil-define-key' is that evil-def-key
          does not allow you to specify multiple key binding pairs in
          a single call to it. "
	 (if (> (length definfo) 1)
	     `(evil-define-key
		,keymap
		,state
		,key
		(lambda () (interactive)
		  (funcall
		   ,(car definfo)
		   ,@(cdr definfo)))
		)
	   `(evil-define-key ,keymap ,state ,key ,(car definfo)))))

(defun const (val) (lambda (x) val))

(defun delete-window-and-balance (&optional window)
  "Delete given window (or current window) and then re-balance
   the remaining child windows of its parent."
  (interactive)
  (let ((window-par (window-parent (selected-window))))
    (delete-window window)
    (balance-windows window-par)))

(defun add-to-load-path-recursive (basepath)
  (let ((base (expand-file-name basepath)))
    (add-to-list 'load-path base)
    (dolist (f (directory-files base))
      (let ((name (concat base "/" f)))
	(when (and (file-directory-p name)
		   (not (equal f ".."))
		   (not (equal f ".git"))
		   (not (equal f ".")))
	  (add-to-load-path-recursive name))))))

(defun add-to-theme-path-recursive (basepath)
  (let ((base (expand-file-name basepath)))
    (add-to-list 'custom-theme-load-path base)
    (dolist (f (directory-files base))
      (let ((name (concat base "/" f)))
	(when (and (file-directory-p name)
		   (not (equal f ".."))
		   (not (equal f ".git"))
		   (not (equal f ".")))
	  (add-to-theme-path-recursive name))))))

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

(defun switch-to-other-buffer () (interactive)
  (switch-to-buffer (other-buffer)))

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
      (error (let ((window-par (window-parent (selected-window))))
	       (split-window nil nil dir)
	       (funcall move-func 1)
	       (balance-windows window-par))))))

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
		     "jetpack"
		     "test"))
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
		   "jetpack"
		   "test"
		   "-s"
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
		   "jetpack"
		   "test"
		   "-s"
		   test-file-full
		   "-k"
		   (format "%s" testname))
    (display-buffer results-buffer-name)))

(defun eshell-here ()
  "Opens up a new shell in the directory associated with the current buffer's file."
  (interactive)
  (let* ((parent (file-name-directory (buffer-file-name)))
	 (name   (car
		  (last
		   (split-string parent "/" t))))
	 (eshell-buffer-name (concat "*eshell: " name "*"))
	 (eshell-buffer nil)
	 (eshell-window nil))

    (unless (setq eshell-buffer (get-buffer eshell-buffer-name))
      (save-window-excursion (setq eshell-buffer (eshell)))
      (with-current-buffer eshell-buffer
	(goto-char (point-max))
	(insert (concat "ls"))
	(eshell-send-input)))
    
    (setq eshell-window (display-buffer eshell-buffer))
    (select-window eshell-window)))

(defun delete-single-window (&optional window)
  "Remove WINDOW from the display.  Default is `selected-window'."
  (interactive)
  (save-current-buffer
    (setq window (or window (selected-window)))
    (select-window window)
    (kill-buffer)
    (if (not (one-window-p t))
	(delete-window (selected-window)))))

(defun get-buffers-matching-mode (mode)
  "Returns a list of buffers where their major-mode is equal to MODE"
  (let ((buffer-mode-matches '()))
   (dolist (buf (buffer-list))
     (with-current-buffer buf
       (if (eq mode major-mode)
	   (add-to-list 'buffer-mode-matches buf))))
   buffer-mode-matches))

(defun backward-kill-line (arg)
  (interactive "p")
  (kill-line (- 1 arg)))

(defun trim-string (string)
  "Remove white spaces in beginning and ending of STRING.
White space here is any of: space, tab, emacs newline (line feed, ASCII 10)."
  (replace-regexp-in-string "\\`[ \t\n]*" "" (replace-regexp-in-string "[ \t\n]*\\'" "" string)))

(defun lookup-mode-config-file-exceptions (mode-symbol)
  "Override default behavior for edit-mode-config-file for certain
   major modes"
  (case mode-symbol
    (emacs-lisp-mode "lisp-config.el")
    (js2-mode "javascript-config.el")
    (inferior-emacs-lisp-mode "lisp-config.el")))

(defun random-element (choices)
  "Return a pseudo-randomly selected element of `choices'."
  (let ((n (random (length choices))))
    (elt choices n)))

(after 'f (defun edit-mode-config-file (&optional config-file-name)
  "Edit the configuration file corresponding to the current major
   mode.  being in mode <modename>-mode corresponds to (find-file
   ~/.emacs.d/config/<modename>-config.el) or (find-file
   ~/.emacs.d/config/lang/<modename>-config.el) if the first does not
   exist."
  (interactive)
  (let* ((mm-string (symbol-name major-mode))
	 (mm-name (car (split-string mm-string "-mode")))
	 (mm-file-name (or config-file-name
			  (lookup-mode-config-file-exceptions major-mode)
			  (concat mm-name "-config.el")))
	 (mm-config-file-path (f-join
			       user-emacs-directory
			       "config"
			       mm-file-name))
	 (mm-config-file-path-with-lang (f-join
					 user-emacs-directory
					 "config"
					 "lang"
					 mm-file-name)))
    (if (f-exists?  mm-config-file-path-with-lang)
	(find-file mm-config-file-path-with-lang)
      (find-file mm-config-file-path)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Custom eval expression code
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar my-read-expression-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map read-expression-map)
    (define-key map [(control ?g)] #'abort-recursive-edit)
    (define-key map [up]   nil)
    (define-key map [down] nil)
    (define-key map [down] nil)
    map))

(defun my-read--expression (prompt &optional initial-contents)
  (let ((minibuffer-completing-symbol t))
    (minibuffer-with-setup-hook
	(lambda ()
	  (emacs-lisp-mode)
	  (use-local-map my-read-expression-map)
	  (setq font-lock-mode t)
	  (funcall font-lock-function 1))
      (read-from-minibuffer prompt initial-contents
			    my-read-expression-map nil
			    'read-expression-history))))

(defun my-eval-expression (expression &optional arg)
  (interactive (list (read (my-read--expression "Eval: "))
		     current-prefix-arg))
  (if arg
      (insert (pp-to-string (eval expression lexical-binding)))
    (pp-display-expression (eval expression lexical-binding)
			   "*Pp Eval Output*")))
