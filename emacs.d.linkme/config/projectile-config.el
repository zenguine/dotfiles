(require 'projectile)

(setq projectile/dont-recur-tags-projects '(".dotfiles"))

(defun projectile/maybe-regenerate-tags ()
  (interactive)
  (when (not (member (projectile-project-name) projectile/dont-recur-tags-projects))
    (message "Regenerating tags..")
    (projectile-regenerate-tags)))

(remove-hook 'projectile-idle-timer-hook 'projectile-regenerate-tags)
(add-hook 'projectile-idle-timer-hook 'projectile/maybe-regenerate-tags)

(define-key projectile-mode-map (kbd "C-c p a") 'projectile-ag)
(define-key projectile-mode-map (kbd "C-c p A") 'projectile-ack)

(projectile-global-mode 1)

;;; Setup org-mode behavior to synergize with projectile projets
;;; code borrowed and modified from:
;;; https://github.com/glynnforrest/emacs.d/blob/master/site-lisp/setup-projects.el

(defvar org-projects-dir (expand-file-name "~/org/projects"))

(defun gf/create-org-path (path)
  "Create a name suitable for an org file from the last part of a file
path."
  (let ((last (car (last (split-string (if (equal (substring path -1) "/")
					   (substring path 0 -1) path) "/")))))
    (concat org-projects-dir "/"
	    (downcase
	     (replace-regexp-in-string
	      "\\." "-" (if (equal (substring last 0 1) ".")
			    (substring last 1) last)))
	    ".org")))

(defun gf/project-org-file ()
  "Get the path of the org file for the current project."
  (gf/create-org-path (projectile-project-root)))

(defun gf/switch-to-project-org-file ()
  "Switch to the org file for the current project."
  (interactive)
  (find-file (gf/project-org-file)))

(defvar gf/previous-project-buffers (make-hash-table :test 'equal))

(defun gf/toggle-switch-to-project-org-file ()
  "Alternate between the current buffer and the org file for the
current project."
  (interactive)
  (if (and
       (string-equal "org-mode" (symbol-name major-mode))
       (s-contains-p "/org/" (buffer-file-name)))
      (if (gethash (buffer-file-name) gf/previous-project-buffers)
	  (switch-to-buffer (gethash (buffer-file-name) gf/previous-project-buffers))
	(error "Previous project buffer not found"))
    (let ((file (gf/project-org-file)))
      (puthash file (current-buffer) gf/previous-project-buffers)
      (find-file file)
      )))

(define-key projectile-mode-map (kbd "C-c p o") 'gf/toggle-switch-to-project-org-file)
(define-key projectile-mode-map (kbd "C-c p O") 'projectile-multi-occur)

(provide 'projectile-config)
