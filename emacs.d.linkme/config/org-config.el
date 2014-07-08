(require 'org)

(setq org-treat-S-cursor-todo-selection-as-state-change nil
      org-default-notes-file (concat org-directory "/refile.org")
      org-refile-use-outline-path t
      org-outline-path-complete-in-steps nil
      org-refile-allow-creating-parent-nodes 'confirm
      org-completion-use-ido t)

(setq org-refile-targets '((nil :maxlevel . 3)
			   (org-agenda-files :maxlevel . 4)))

;; Org-babel config..
(setq org-edit-src-content-indentation 0
      org-src-tab-acts-natively t
      org-src-fontify-natively t
      org-confirm-babel-evaluate nil)

(setq org-log-done t
      org-log-into-drawer t
      org-use-property-inheritance '("STYLE"))

;; Clocking
(setq org-clock-out-remove-zero-time-clocks t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Org mode TODO workflow settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq org-todo-keywords
      (quote ((sequence "TODO(t)" "NEXT(n)" "STARTED(s@/!)" "|" "DONE(d@/!)")
              (sequence "WAIT(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)")
	      (sequence "READ(r)" "EVENTUALLY(e)" "|" "ABSORBED(a@/!)"))))

(setq org-todo-keyword-faces
      (quote (("TODO" :foreground "orange" :weight bold)
              ("NEXT" :foreground "white" :weight bold)
              ("READ" :foreground "white" :weight bold)
              ("DONE" :foreground "forest green" :weight bold)
              ("ABSORBED" :foreground "grey" :weight bold)
              ("WAIT" :foreground "forest green" :weight bold)
              ("EVENTUALLY" :foreground "grey" :weight bold)
              ("CANCELLED" :foreground "grey" :weight bold))))

(setq org-todo-state-tags-triggers
      (quote (("CANCELLED" ("cancelled" . t))
              ("WAIT" ("waiting" . t))
              ("HOLD" ("waiting") ("hold" . t))
              (done ("waiting") ("hold"))
              ("TODO" ("waiting") ("cancelled") ("hold"))
              ("NEXT" ("waiting") ("cancelled") ("hold"))
              ("DONE" ("waiting") ("cancelled") ("hold"))
	      ("ABSORBED" ("reading" . t) ("waiting") ("cancelled") ("hold"))
	      ("READ" ("reading" . t) ("waiting") ("cancelled") ("hold"))
	      ("EVENTUALLY" ("waiting") ("cancelled") ("hold")))))

;; Capture templates
(setq org-capture-templates
      (quote (("t" "todo" entry (file "~/org/refile.org")
               "* TODO %?\n%U\n%a\n" :clock-in t :clock-resume t)
	      ("c" "Clocked subtask" entry (clock)
               "* STARTED %?\n%U\n%a\n" :clock-in t :clock-keep t)
              ("r" "respond" entry (file "~/org/refile.org")
               "* NEXT Respond to %:from on %:subject\nSCHEDULED: %t\n%U\n%a\n" :clock-in t :clock-resume t :immediate-finish t)
              ("n" "note" entry (file "~/org/refile.org")
               "* %? :NOTE:\n%U\n%a\n" :clock-in t :clock-resume t)
	      ("l" "Link" entry (file+olp "~/org/personal.org" "Links" "Unsorted")
	       "* %a\n %?\n %i")
              ("h" "Habit" entry (file "~/org/personal.org" "Habits")
               "* NEXT %?\n%U\n%a\nSCHEDULED: %(format-time-string \"<%Y-%m-%d %a .+1d/3d>\")\n:PROPERTIES:\n:STYLE: habit\n:REPEAT_TO_STATE: NEXT\n:END:\n"))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Org Custom Agenda Setup
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Do not dim blocked tasks
(setq org-agenda-dim-blocked-tasks nil)

;; Compact the block agenda view
(setq org-agenda-compact-blocks t)

;; Custom agenda command definitions
(setq org-agenda-custom-commands
      '(("n" "Agenda and all TODO's"
	 ((agenda "")
	  (alltodo)))))



(defun my-org-mode-hook ()
  (local-unset-key (kbd "C-j")) ;
  (local-unset-key (kbd "C-k"))
  (smartparens-mode nil))

(add-hook 'org-mode-hook 'my-org-mode-hook)
(add-hook 'org-mode-hook 'turn-on-font-lock) ; not needed when global-font-lock-mode is on
(add-hook 'org-mode-hook 'org-indent-mode)


(require 'org-protocol)
(setq org-protocol-default-template-key "l")

(provide 'org-config)
