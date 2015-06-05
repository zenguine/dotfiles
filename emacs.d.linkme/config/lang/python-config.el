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

(setq-default python-indent 2
							python-indent-offset 2)

(setq jedi:complete-on-dot t)
(setq jedi:install-imenu t)
(setq jedi:get-in-function-call-delay 100)
(setq jedi:server-args
      '("--sys-path" "~/code/jetpack/src"))

(evil-define-key 'normal python-mode-map
  "K" 'jedi:show-doc
  (kbd "C-]") 'jedi:goto-definition
  (kbd "C-t") 'jedi:goto-definition-pop-marker)

(defun my-ipython-shell-get-or-create-process ()
  "Get or create an inferior Python process for current buffer and return it."
  (let* ((dedicated-proc-name (python-shell-get-process-name t))
         (dedicated-proc-buffer-name (format "*%s*" dedicated-proc-name))
         (global-proc-name  (python-shell-get-process-name nil))
         (global-proc-buffer-name (format "*%s*" global-proc-name))
         (dedicated-running (comint-check-proc dedicated-proc-buffer-name))
         (global-running (comint-check-proc global-proc-buffer-name))
         (current-prefix-arg 16))
    (when (and (not dedicated-running) (not global-running))
      (if (run-python (executable-find "ipython") t t)
          (setq dedicated-running t)
        (setq global-running t)))
    ;; Always prefer dedicated
    (get-buffer-process (if dedicated-running
                            dedicated-proc-buffer-name
                          global-proc-buffer-name))))


(defun python-package-dir-p (dir)
  (file-exists-p (concat dir "__init__.py")))

(defun python-package-root (&optional silent)
  (interactive)
  (let ((buffer-file-dir (file-name-directory
			  (expand-file-name
			   (buffer-file-name (current-buffer))))))
    (if (python-package-dir-p buffer-file-dir)
	(expand-file-name (vc-find-root
	 buffer-file-dir
	 (lambda (dir) (not (python-package-dir-p dir)))))
      (if silent
	  nil
	(error "Current directory is not a python package")))))

(defun file-python-package-name ()
  (interactive)
  (let* ((file-path (expand-file-name
		     (buffer-file-name (current-buffer))))
	 (package-root (python-package-root))
	 (fp-rel-to-root (file-relative-name file-path package-root)))
    (if (equal (file-name-extension fp-rel-to-root) "py")
	(mapconcat
	 'identity
	 (split-string (file-name-sans-extension fp-rel-to-root) "/")
	 ".")
      (error "Only .py files may be a python module"))))

(defun my-ipython-shell-load-file-as-module ()
  (interactive)
  (let ((containing-dir (python-package-root))
	(file-module-name (file-python-package-name)))
    (my-ipython-shell-get-or-create-process)
    (python-shell-send-string "import sys")
    (python-shell-send-string
     (format "if \"%s\" not in sys.path: sys.path.insert(1, \"%s\")"
	     containing-dir containing-dir))
    (python-shell-send-string
     (format "from %s import *" file-module-name))))


(defun my-ipython-shell-reload-file-module ()
  (interactive)
  (let ((containing-dir (python-package-root))
	(file-module-name (file-python-package-name)))
    (my-ipython-shell-get-or-create-process)
    (python-shell-send-string
     (format "reload(%s)" file-module-name))))

(defun my-ipython-shell-switch-to-shell ()
  "Switch to inferior Python process buffer."
  (interactive)
  (pop-to-buffer (process-buffer (my-ipython-shell-get-or-create-process)) t))

(defun my-python-send-region-or-buffer (&optional show)
  (interactive)
  (my-ipython-shell-get-or-create-process)
  (if (region-active-p)
      (python-shell-send-region (region-beginning) (region-end))
    (python-shell-send-buffer))
  (if show
      (my-ipython-shell-switch-to-shell)))

(defun my-python-mode-hook ()
  (local-set-key (kbd "C-c !") 'python-shell-switch-to-shell)
  (local-set-key (kbd "C-c T a") 'pytest-test-all)
  (local-set-key (kbd "C-c T f") 'pytest-test-current-file)
  (local-set-key (kbd "C-c T t") 'pytest-test-specific-test)
  (local-set-key (kbd "C-c C-l") 'my-ipython-shell-load-file-as-module)
  (hs-minor-mode t)
  (modify-syntax-entry ?_ "w")
  (flymake-mode -1)
  (auto-complete-mode -1))

(defun my-jedi-setup ()
  (defmacro add-args (arg-list arg-name arg-value)
    `(setq ,arg-list (append ,arg-list (list ,arg-name ,arg-value))))
  (make-local-variable 'jedi:server-args)
  (let ((proj-package-root (python-package-root t)))
    (if proj-package-root
	(add-args jedi:server-args "--sys-path" proj-package-root)))
  (jedi:setup)
  (jedi:create-nested-imenu-index))

(add-hook 'python-mode-hook 'my-python-mode-hook)
(add-hook 'python-mode-hook 'my-jedi-setup)
(setq elpy-rpc-backend "jedi")

(elpy-enable)
(setq elpy-modules (delete 'elpy-module-flymake elpy-modules))
(elpy-use-ipython)

(define-key elpy-mode-map (kbd "C-c C-z") 'my-ipython-shell-switch-to-shell)
(define-key elpy-mode-map (kbd "C-c C-r") 'my-ipython-shell-reload-file-module)
(def-key elpy-mode-map (kbd "C-c C-c") 'my-python-send-region-or-buffer t)
(define-key python-mode-map (kbd "C-c C-d") 'helm-pydoc)
(define-key elpy-mode-map (kbd "C-c C-d") 'my-pydoc-fn)

(defun my-pydoc-fn (arg)
  (interactive "P")
  (if arg
      (call-interactively 'elpy-pydoc)
    (call-interactively 'helm-pydoc)))

(provide 'python-config)
