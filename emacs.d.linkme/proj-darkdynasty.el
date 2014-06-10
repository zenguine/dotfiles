(require 'jetpack-mode)

(defvar darkdynasty/root-path (expand-file-name "~/code/darkdynasty")
  "Path to root directory of darkdynasty source tree")


(defvar darkdynasty/cli-rel-path "bin/cli.py"
  "Path to command-line executable for darkdynasty")

(defvar darkdynasty/cli-args '()
  "Commandline arguments to pass to cli.py")

(defvar darkdynasty-cli-mode-map
  (let ((map (nconc (make-sparse-keymap) comint-mode-map)))
    (define-key map (kbd "TAB") 'completion-at-point)
    map))

(define-minor-mode darkdynasty-mode
  "Mode for darkdynasty project files"
  :lighter " darkdynasty"
  :keymap (let ((map (make-sparse-keymap)))
	    (define-key map (kbd "C-c \\ c") 'darkdynasty/run-cli)
	    map))

(defun darkdynasty/run-cli ()
  "Run an inferior instance of `darkdynasty/bin/cli.py' inside Emacs."
  (interactive)
  (let* ((buffer (comint-check-proc "DarkdynastyCli")))
    ;; pop to the "*DarkdynastyCli*" buffer if the process is dead, the
    ;; buffer is missing or it's got the wrong mode.
    (pop-to-buffer-same-window
     (if (or buffer (not (derived-mode-p 'darkdynasty-mode))
             (comint-check-proc (current-buffer)))
         (get-buffer-create (or buffer "*DarkdynastyCli*"))
       (current-buffer)))
    ;; create the comint process if there is no buffer.
    (unless buffer
      (apply 'make-comint-in-buffer "DarkdynastyCli" buffer
             "jetpack" nil "shell" darkdynasty/cli-rel-path
	     "--project" darkdynasty/root-path '())
      (darkdynasty-mode))))

(provide 'proj-darkdynasty)
