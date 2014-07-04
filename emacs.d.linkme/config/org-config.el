(require 'org)

(setq org-log-done t)

;; Org-babel config..
(setq org-edit-src-content-indentation 0
      org-src-tab-acts-natively t
      org-src-fontify-natively t
      org-confirm-babel-evaluate nil)

(add-hook 'org-mode-hook 'turn-on-font-lock) ; not needed when global-font-lock-mode is on
(add-hook 'org-mode-hook 'org-indent-mode)

(setq org-treat-S-cursor-todo-selection-as-state-change nil)

(setq org-todo-keywords
      (quote ((sequence "TODO(t)" "NEXT(n)" "STARTED(s@/!)" "|" "DONE(d@/!)")
              (sequence "WAIT(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)")
	      (sequence "READ(r)" "EVENTUALLY(e)" "|" "ABSORBED(a@/!)"))))

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

(setq org-capture-templates
      (quote (("t" "todo" entry (file "~/org/refile.org")
               "* TODO %?\n%U\n%a\n" :clock-in t :clock-resume t)
              ("r" "respond" entry (file "~/org/refile.org")
               "* NEXT Respond to %:from on %:subject\nSCHEDULED: %t\n%U\n%a\n" :clock-in t :clock-resume t :immediate-finish t)
              ("n" "note" entry (file "~/org/refile.org")
               "* %? :NOTE:\n%U\n%a\n" :clock-in t :clock-resume t)
              ("j" "Journal" entry (file+datetree "~/org/diary.org")
               "* %?\n%U\n" :clock-in t :clock-resume t)
              ("w" "org-protocol" entry (file "~/org/refile.org")
               "* TODO Review %c\n%U\n" :immediate-finish t)
	      ("l" "Link" entry (file+olp "~/org/personal.org" "Web Links")
	       "* %a\n %?\n %i")
              ("m" "Meeting" entry (file "~/org/refile.org")
               "* MEETING with %? :MEETING:\n%U" :clock-in t :clock-resume t)
              ("p" "Phone call" entry (file "~/org/refile.org")
               "* PHONE %? :PHONE:\n%U" :clock-in t :clock-resume t)
              ("h" "Habit" entry (file "~/org/refile.org")
               "* NEXT %?\n%U\n%a\nSCHEDULED: %(format-time-string \"<%Y-%m-%d %a .+1d/3d>\")\n:PROPERTIES:\n:STYLE: habit\n:REPEAT_TO_STATE: NEXT\n:END:\n"))))

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

(require 'org-protocol)
(setq org-protocol-default-template-key "l")

(provide 'org-config)
