;;; -*- lexical-binding: t -*-

(defun const (val) (lambda (x) val))

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

(defun multi-term-toggle ()
  "If there is only one multi-term window.. this should toggle between a term
window and a non-term window"
  (interactive)
  (let ((start-buffer (current-buffer)))
    (multi-term-next)
    (if (eq start-buffer (current-buffer))
	(switch-to-buffer (other-buffer)))))

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
		   (split-string parent "/" t)))))
    (split-window-horizontally)
    (other-window 1)
    (eshell "new")
    (rename-buffer (concat "*eshell: " name "*"))

    (insert (concat "ls"))
    (eshell-send-input)))

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
