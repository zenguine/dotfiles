(require 'projectile)

(setq projectile/dont-recur-tags-projects '(".dotfiles"))

(defun projectile/maybe-regenerate-tags ()
  (interactive)
  (when (not (member (projectile-project-name) projectile/dont-recur-tags-projects))
    (message "Regenerating tags..")
    (projectile-regenerate-tags)))

(remove-hook 'projectile-idle-timer-hook 'projectile-regenerate-tags)
(add-hook 'projectile-idle-timer-hook 'projectile/maybe-regenerate-tags)

(provide 'projectile-config)
