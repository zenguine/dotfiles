(require 'projectile)

(setq projectile/dont-recur-tags-projects '(".dotfiles"))

(defun projectile/maybe-regenerate-tags ()
  (interactive)
  (when (not (member (projectile-project-name) projectile/dont-recur-tags-projects))
    (message "Regenerating tags..")
    (projectile-regenerate-tags)))

(remove-hook 'projectile-idle-timer-hook 'projectile-regenerate-tags)
(add-hook 'projectile-idle-timer-hook 'projectile/maybe-regenerate-tags)

(define-key projectile-mode-map (kbd "C-c p p") 'projectile-switch-project)
(define-key projectile-mode-map (kbd "C-c p a") 'projectile-ag)
(define-key projectile-mode-map (kbd "C-c p A") 'projectile-ack)

(projectile-global-mode 1)

(provide 'projectile-config)
