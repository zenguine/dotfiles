(require 'org)

(setq org-log-done t)
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)
(global-set-key "\C-cc" 'org-capture)

(add-hook 'org-mode-hook 'turn-on-font-lock) ; not needed when global-font-lock-mode is on
(add-hook 'org-mode-hook 'org-indent-mode)

(setq org-agenda-files (list (concat org-directory "/work.org")
			     (concat org-directory "/personal.org")
			     (concat org-directory "/refile.org")))

(setq org-default-notes-file (concat org-directory "/refile.org"))

					; Targets include this file and any file contributing to the agenda - up to 9 levels deep
(setq org-refile-targets (quote ((nil :maxlevel . 9)
				 (org-agenda-files :maxlevel . 9))))

; Use full outline paths for refile targets - we file directly with IDO
(setq org-refile-use-outline-path t)

; Targets complete directly with IDO
(setq org-outline-path-complete-in-steps nil)

					; Allow refile to create parent tasks with confirmation
(setq org-refile-allow-creating-parent-nodes (quote confirm))

; Use IDO for both buffer and file completion and ido-everywhere to t
(setq org-completion-use-ido t)

(defun my-org-mode-hook ()
  (local-unset-key (kbd "C-j")) ;
  (local-unset-key (kbd "C-k"))
  (smartparens-mode nil))

(add-hook 'org-mode-hook 'my-org-mode-hook)

(provide 'org-config)
