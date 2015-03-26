(require 'powerline)
(defun spacemacs/powerline-nyan-cat ()
	"Construct a powerline segment for nyan cat."
	(let* ((active (powerline-selected-window-active))
				 (l (1+ (truncate (powerline-width lhs))))
				 (r (1+ (truncate (powerline-width rhs))))
				 (face1 (if active 'powerline-active1 'powerline-inactive1))
				 (face2 (if active 'powerline-active2 'powerline-inactive2))
				 (separator-left
					(intern (format "powerline-%s-%s"
													powerline-default-separator
													(car powerline-default-separator-dir))))
				 (separator-right
					(intern (format "powerline-%s-%s"
													powerline-default-separator
													(cdr powerline-default-separator-dir)))))
		(setq nyan-bar-length (min 32 (/ (- (window-total-width) l r) 2)))
		(list
		 (funcall separator-right face2 face1)
		 (powerline-raw (nyan-create) face1)
		 (funcall separator-left face1 face2))))

(setq-default dotspacemacs-mode-line-unicode-symbols t)
(defvar spacemacs-evil-cursor-colors '((normal . "DarkGoldenrod2")
																			 (insert . "chartreuse3")
																			 (emacs . "SkyBlue2")
																			 (visual . "gray")
																			 (motion . "plum3")
																			 (lisp . "HotPink1")
																			 (iedit . "firebrick1")
																			 (iedit-insert . "firebrick1"))
	"Colors assigned to evil states.")
(defun spacemacs/state-color-face (state)
	"Return the symbol of the face for the given STATE."
	(intern (format "spacemacs-%s-face" (symbol-name state))))

(defun spacemacs/current-state-face ()
	"Return the face associated to the current state."
	(let ((state (if (eq evil-state 'operator)
									 evil-previous-state
								 evil-state)))
		(spacemacs/state-color-face state)))

(defun spacemacs/defface-state-color (state color)
	"Define a face for the given STATE and background COLOR."
	(eval `(defface ,(spacemacs/state-color-face state) '((t ()))
					 ,(format "%s state face." (symbol-name state))
					 :group 'spacemacs))
	(set-face-attribute (spacemacs/state-color-face state) nil
											:background color
											:foreground (face-background 'mode-line)
											:box (face-attribute 'mode-line :box)
											:inherit 'mode-line))
(defun spacemacs/state-color (state)
	"Return the color string associated to STATE."
	(face-background (spacemacs/state-color-face state)))
(defun spacemacs/current-state-color ()
	"Return the color string associated to the current state."
	(face-background (spacemacs/state-color-face evil-state)))
(defun spacemacs/state-face (state)
	"Return the face associated to the STATE."
	(spacemacs/state-color-face state))
(defun spacemacs/current-state-face ()
	"Return the face associated to the current state."
	(let ((state (if (eq evil-state 'operator)
									 evil-previous-state
								 evil-state)))
		(spacemacs/state-color-face state)))
(defun spacemacs/set-state-faces ()
	"Define or set the state faces."
	(mapcar (lambda (x) (spacemacs/defface-state-color (car x) (cdr x)))
					spacemacs-evil-cursor-colors))
(spacemacs/set-state-faces)

;; Custom format of minor mode lighters, they are separated by a pipe.
(defpowerline spacemacs-powerline-minor-modes
	(mapconcat (lambda (mm)
							 (propertize
								mm
								'mouse-face 'mode-line-highlight
								'help-echo "Minor mode\n mouse-1: Display minor mode menu\n mouse-2: Show help for minor mode\n mouse-3: Toggle minor modes"
								'local-map (let ((map (make-sparse-keymap)))
														 (define-key map
															 [mode-line down-mouse-1]
															 (powerline-mouse 'minor 'menu mm))
														 (define-key map
															 [mode-line mouse-2]
															 (powerline-mouse 'minor 'help mm))
														 (define-key map
															 [mode-line down-mouse-3]
															 (powerline-mouse 'minor 'menu mm))
														 (define-key map
															 [header-line down-mouse-3]
															 (powerline-mouse 'minor 'menu mm))
														 map)))
						 (split-string (format-mode-line minor-mode-alist))
						 (concat (propertize
											(if dotspacemacs-mode-line-unicode-symbols " " "") 'face face)
										 (unless dotspacemacs-mode-line-unicode-symbols "|"))))

(defvar spacemacs-mode-line-minor-modesp nil
	"If not nil, minor modes lighter are displayed in the mode-line.")

(defun spacemacs/mode-line-minor-modes-toggle ()
	"Toggle display of minor modes."
	(interactive)
	(if spacemacs-mode-line-minor-modesp
			(setq spacemacs-mode-line-minor-modesp nil)
		(setq spacemacs-mode-line-minor-modesp t)))

(defvar spacemacs-mode-line-display-point-p nil
	"If not nil, display point alongside row/column in the mode-line.")

(defun spacemacs/mode-line-display-point-toggle ()
	(interactive)
	(if spacemacs-mode-line-display-point-p
			(setq spacemacs-mode-line-display-point-p nil)
		(setq spacemacs-mode-line-display-point-p t)))

(defvar spacemacs-mode-line-org-clock-current-taskp t
	"If not nil, the currently clocked org-mode task will be
displayed in the mode-line.")

(defvar spacemacs-mode-line-org-clock-format-function
	'org-clock-get-clock-string
	"Function used to render the currently clocked org-mode task.")

(defun spacemacs/mode-line-org-clock-current-task-toggle ()
	(interactive)
	(if spacemacs-mode-line-org-clock-current-taskp
			(setq spacemacs-mode-line-org-clock-current-taskp nil)
		(setq spacemacs-mode-line-org-clock-current-taskp t)))

(if (display-graphic-p)
		(setq-default powerline-default-separator 'contour)
	(setq-default powerline-default-separator 'utf-8))

(defun spacemacs/mode-line-prepare-left ()
	(let* ((active (powerline-selected-window-active))
				 (line-face (if active 'mode-line 'mode-line-inactive))
				 (face1 (if active 'powerline-active1 'powerline-inactive1))
				 (face2 (if active 'powerline-active2 'powerline-inactive2))
				 (state-face (if active (spacemacs/current-state-face) face2))
				 (window-numberingp (and (boundp 'window-numbering-mode)
																 (symbol-value window-numbering-mode)))
				 (anzup (and (boundp 'anzu--state) anzu--state))
				 (flycheckp (and (boundp 'flycheck-mode)
												 (symbol-value flycheck-mode)
												 (or flycheck-current-errors
														 (eq 'running flycheck-last-status-change))))
				 (vc-face (if (or flycheckp spacemacs-mode-line-minor-modesp)
											face1 line-face))
				 (separator-left (intern (format "powerline-%s-%s"
																				 powerline-default-separator
																				 (car powerline-default-separator-dir))))
				 (separator-right (intern (format "powerline-%s-%s"
																					powerline-default-separator
																					(cdr powerline-default-separator-dir)))))
		(append
		 ;; window number
		 (if (and window-numberingp (spacemacs/window-number))
				 (list (powerline-raw (spacemacs/window-number) state-face))
			 (list (powerline-raw (evil-state-property evil-state :tag t) state-face)))
		 ;; (if (and active anzup)
		 ;; 		(list (funcall separator-right state-face face1)
		 ;; 					(powerline-raw (anzu--update-mode-line) face1)
		 ;; 					(funcall separator-right face1 line-face))
		 ;; 	(list (funcall separator-right state-face line-face)))
		 ;; evil state
		 ;; (powerline-raw evil-mode-line-tag state-face)
		 ;; (funcall separator-right state-face line-face)
		 ;; buffer name
		 (list
			(powerline-raw "%*" line-face 'l)
			(powerline-buffer-size line-face 'l)
			(powerline-buffer-id line-face 'l)
			(powerline-raw " " line-face)
			;; major mode
			(funcall separator-left line-face face1)
			(powerline-major-mode face1 'l)
			(powerline-raw " " face1)
			(when active
				(funcall separator-right face1 line-face)))
		 ;; flycheck
		 (when (and active flycheckp)
			 (list (powerline-raw " " line-face)
						 (powerline-raw (spacemacs|custom-flycheck-lighter error)
														'spacemacs-mode-line-flycheck-error-face)
						 (powerline-raw (spacemacs|custom-flycheck-lighter warning)
														'spacemacs-mode-line-flycheck-warning-face)
						 (powerline-raw (spacemacs|custom-flycheck-lighter info)
														'spacemacs-mode-line-flycheck-info-face)))
		 ;; separator between flycheck and minor modes
		 (when (and active flycheckp spacemacs-mode-line-minor-modesp)
			 (list (funcall separator-left line-face face1)
						 (powerline-raw " " face1)
						 (funcall separator-right face1 line-face)))
		 ;; minor modes
		 (when (and active spacemacs-mode-line-minor-modesp)
			 (list (spacemacs-powerline-minor-modes line-face 'l)
						 (powerline-raw mode-line-process line-face 'l)
						 (powerline-raw " " line-face)))
		 ;; version control
		 (when (and active (or flycheckp spacemacs-mode-line-minor-modesp))
			 (list (funcall separator-left (if vc-face line-face face1) vc-face)))
		 (if active
				 (list (powerline-vc vc-face)
							 (powerline-raw " " vc-face)
							 (funcall separator-right vc-face face2))
			 (list (funcall separator-right face1 face2)))
		 ;; org clocked task
		 (when (and active
								spacemacs-mode-line-org-clock-current-taskp
								(fboundp 'org-clocking-p)
								(org-clocking-p))
			 (list (powerline-raw " " face2)
						 (funcall spacemacs-mode-line-org-clock-format-function)
						 (powerline-raw " " face2))))))

(defun column-number-at-pos (pos)
	"Analog to line-number-at-pos."
	(save-excursion (goto-char pos) (current-column)))

(defun selection-info ()
	"String holding the number of columns in the selection
if it covers only one line, else number of lines in the selection"
	(let* ((lines (count-lines (region-beginning) (1+ (region-end))))
				 (chars (- (1+ (region-end)) (region-beginning))))
		(if (> lines 1) (format "%s lines" (number-to-string lines))
			(format "%s chars" (number-to-string chars)))))

(defun spacemacs/mode-line-prepare-right ()
	(let* ((active (powerline-selected-window-active))
				 (line-face (if active 'mode-line 'mode-line-inactive))
				 (face1 (if active 'powerline-active1 'powerline-inactive1))
				 (face2 (if active 'powerline-active2 'powerline-inactive2))
				 (state-face (if active (spacemacs/current-state-face) face2))
				 (nyancatp (and (boundp 'nyan-mode) nyan-mode))
				 (batteryp (and (boundp 'fancy-battery-mode)
												(symbol-value fancy-battery-mode)))
				 (battery-face (if batteryp (fancy-battery-powerline-face)))
				 (separator-left (intern (format "powerline-%s-%s"
																				 powerline-default-separator
																				 (car powerline-default-separator-dir))))
				 (separator-right (intern (format "powerline-%s-%s"
																					powerline-default-separator
																					(cdr powerline-default-separator-dir)))))
		(append
		 ;; battery
		 (if (and active batteryp)
				 (list (funcall separator-left face2 battery-face)
							 (powerline-raw (fancy-battery-default-mode-line)
															battery-face 'r)
							 (funcall separator-right battery-face face1))
			 (list (funcall separator-right face2 face1)))
		 (if (use-region-p)
				 ;; selection info, if there is a selection.
				 (list
					(powerline-raw " " face1)
					(powerline-raw (selection-info) face1)
					(powerline-raw " " face1)
					(funcall separator-left face1 face2)
					(powerline-raw " " face2)
					(funcall separator-right face2 face1)))
		 (list
			;; row:column
			(powerline-raw " " face1)
			(powerline-raw (if spacemacs-mode-line-display-point-p
												 (concat (format "%d | " (point)) "%l:%2c" )
											 "%l:%2c")
										 face1 'r)
			(funcall separator-left face1 line-face)
			(powerline-raw " " line-face))
		 (list
			;; global-mode
			(unless (equal '("") global-mode-string)
				(powerline-raw global-mode-string)
				(powerline-raw " " line-face)))
		 (let ((progress (format-mode-line "%p")))
			 (list
				;; percentage in the file
				(powerline-raw "%p" line-face 'r)
				;; display hud
				(powerline-chamfer-left line-face face1)
				(if (string-match "\%" progress)
						(powerline-hud state-face face1)))))))

(defun spacemacs/mode-line-prepare ()
	(let* ((active (powerline-selected-window-active))
				 (face2 (if active 'powerline-active2 'powerline-inactive2))
				 (lhs (spacemacs/mode-line-prepare-left))
				 (rhs (spacemacs/mode-line-prepare-right))
				 (nyancatp (and (boundp 'nyan-mode) nyan-mode)))
		(concat (powerline-render lhs)
						(when (and active nyancatp)
							(powerline-render (spacemacs/powerline-nyan-cat)))
						(powerline-fill face2 (powerline-width rhs))
						(powerline-render rhs))))

(setq-default mode-line-format
							'("%e" (:eval (spacemacs/mode-line-prepare))))

(defun spacemacs//restore-powerline (buffer)
	"Restore the powerline in buffer"
	(with-current-buffer buffer
		(setq-local mode-line-format
								'("%e" (:eval (spacemacs/mode-line-prepare))))
		(powerline-set-selected-window)
		(powerline-reset)))

(defun spacemacs//set-powerline-for-startup-buffers ()
	"Set the powerline for buffers created when Emacs starts."
	(dolist (buffer '("*Messages*" "*spacemacs*" "*Compile-Log*"))
		(when (get-buffer buffer)
			(spacemacs//restore-powerline buffer))))

(set-face-background 'org-mode-line-clock (face-attribute 'powerline-active2 :background))
(add-hook 'after-init-hook
					'spacemacs//set-powerline-for-startup-buffers)


(provide 'my-modeline-config)
