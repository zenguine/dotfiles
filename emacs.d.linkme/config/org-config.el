(require 'org)
(require 'my-org-util)

(setq org-treat-S-cursor-todo-selection-as-state-change nil
      org-default-notes-file (concat org-directory "/refile.org")
      org-refile-use-outline-path t
      org-outline-path-complete-in-steps nil
      org-refile-allow-creating-parent-nodes 'confirm
      org-completion-use-ido t)

(setq org-refile-targets '((org-agenda-files :maxlevel . 4)
			   (nil :maxlevel . 3)))

;; Org-babel config..
(setq org-edit-src-content-indentation 0
      org-src-tab-acts-natively t
      org-src-fontify-natively t
      org-confirm-babel-evaluate nil)

(setq org-log-done t
      org-log-into-drawer t
      org-use-property-inheritance '("STYLE")
      org-enforce-todo-dependencies t
      org-enforce-todo-checkbox-dependencies t)

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
              ("READ" :foreground "purple" :weight bold)
              ("DONE" :foreground "forest green" :weight bold)
              ("ABSORBED" :foreground "forest green" :weight bold)
              ("WAIT" :foreground "forest green" :weight bold)
              ("EVENTUALLY" :foreground "forest green" :weight bold)
              ("CANCELLED" :foreground "forest green" :weight bold))))

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
	      ("n" "NEXT task" entry (file "~/org/refile.org")
               "* NEXT %?\n%U\n%a\n" :clock-in t :clock-resume t)
	      ("C" "Misc clocked task" entry (file "~/org/refile.org")
               "* NEXT %?\n%U\n%a\n" :clock-in t :clock-keep t)
	      ("c" "Clocked subtask" entry (clock)
               "* NEXT %?\n%U\n%a\n" :clock-in t :clock-keep t)
              ("R" "respond" entry (file "~/org/refile.org")
               "* NEXT Respond to %:from on %:subject\nSCHEDULED: %t\n%U\n%a\n" :clock-in t :clock-resume t :immediate-finish t)
	      ("r" "Add to reading list" entry (file+olp "~/org/personal.org" "Learning" "Uncategorized")
	       "* %x%? :reading:\n%U\n %i" :clock-in t :clock-resume t)
              ("N" "note" entry (file "~/org/refile.org")
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
(setq org-agenda-span 'day)
(setq org-agenda-show-inherited-tags '(search))

;; Compact the block agenda view
(setq org-agenda-compact-blocks t)

;; Custom agenda command definitions
(setq org-agenda-custom-commands
      '(("n" "Agenda and all TODO's"
	 ((agenda "")
	  (alltodo)))
	("r" "All reading list items" tags "reading")
	("u" "Unread reading list items" tags "reading-TODO=\"ABSORBED\"")))

(setq org-agenda-custom-commands
      (quote (("N" "Notes" tags "NOTE"
               ((org-agenda-overriding-header "Notes")
                (org-tags-match-list-sublevels t)))
              ("h" "Habits" tags-todo "STYLE=\"habit\""
               ((org-agenda-overriding-header "Habits")
                (org-agenda-sorting-strategy
                 '(todo-state-down effort-up category-keep))))
              ("n" "All NEXT items" tags-todo "TODO=\"NEXT\""
               ((org-agenda-overriding-header "Next")))
	      ("r" . "Reading List")
              ("ru" "Unread reading items" tags "+reading/-ABSORBED"
               ((org-agenda-overriding-header "Reading List: unread")))
              ("ra" "All reading items" tags "+reading"
               ((org-agenda-overriding-header "Reading List: all")))
              ("rr" "Read reading items" tags "+reading/+ABSORBED"
               ((org-agenda-overriding-header "Reading List: read")))
              ("n" "All NEXT items" tags-todo "TODO=\"NEXT\""
               ((org-agenda-overriding-header "Next")))
              (" " "Agenda"
               ((agenda "" nil)
                (tags "refile"
                      ((org-agenda-overriding-header "Tasks to Refile")
                       (org-tags-match-list-sublevels nil)))
                (tags-todo "-cancelled/!"
                           ((org-agenda-overriding-header "Stuck Projects")
                            (org-agenda-skip-function 'bh/skip-non-stuck-projects)
                            (org-agenda-sorting-strategy
                             '(category-keep))))
                (tags-todo "-eventually-hold-cancelled/!"
                           ((org-agenda-overriding-header "Projects")
                            (org-agenda-skip-function 'bh/skip-non-projects)
                            (org-tags-match-list-sublevels 'indented)
                            (org-agenda-sorting-strategy
                             '(category-keep))))
                (tags-todo "-cancelled/!NEXT"
                           ((org-agenda-overriding-header (concat "Project Next Tasks"
                                                                  (if bh/hide-scheduled-and-waiting-next-tasks
                                                                      ""
                                                                    " (including WAITING and SCHEDULED tasks)")))
                            (org-agenda-skip-function 'bh/skip-projects-and-habits-and-single-tasks)
                            (org-tags-match-list-sublevels t)
                            (org-agenda-todo-ignore-scheduled bh/hide-scheduled-and-waiting-next-tasks)
                            (org-agenda-todo-ignore-deadlines bh/hide-scheduled-and-waiting-next-tasks)
                            (org-agenda-todo-ignore-with-date bh/hide-scheduled-and-waiting-next-tasks)
                            (org-agenda-sorting-strategy
                             '(todo-state-down effort-up category-keep))))
                (tags-todo "-refile-cancelled-waiting-hold/!"
                           ((org-agenda-overriding-header (concat "Project Subtasks"
                                                                  (if bh/hide-scheduled-and-waiting-next-tasks
                                                                      ""
                                                                    " (including WAITING and SCHEDULED tasks)")))
                            (org-agenda-skip-function 'bh/skip-non-project-tasks)
                            (org-agenda-todo-ignore-scheduled bh/hide-scheduled-and-waiting-next-tasks)
                            (org-agenda-todo-ignore-deadlines bh/hide-scheduled-and-waiting-next-tasks)
                            (org-agenda-todo-ignore-with-date bh/hide-scheduled-and-waiting-next-tasks)
                            (org-agenda-sorting-strategy
                             '(category-keep))))
                (tags-todo "-refile-cancelled-waiting-hold/!-EVENTUALLY-READ"
                           ((org-agenda-overriding-header (concat "Standalone Tasks"
                                                                  (if bh/hide-scheduled-and-waiting-next-tasks
                                                                      ""
                                                                    " (including WAITING and SCHEDULED tasks)")))
                            (org-agenda-skip-function 'bh/skip-project-tasks)
                            (org-agenda-todo-ignore-scheduled bh/hide-scheduled-and-waiting-next-tasks)
                            (org-agenda-todo-ignore-deadlines bh/hide-scheduled-and-waiting-next-tasks)
                            (org-agenda-todo-ignore-with-date bh/hide-scheduled-and-waiting-next-tasks)
                            (org-agenda-sorting-strategy
                             '(category-keep))))
                (tags-todo "-cancelled+waiting|HOLD/!-EVENTUALLY"
                           ((org-agenda-overriding-header (concat "Waiting and Postponed Tasks"
                                                                  (if bh/hide-scheduled-and-waiting-next-tasks
                                                                      ""
                                                                    " (including WAITING and SCHEDULED tasks)")))
                            (org-agenda-skip-function 'bh/skip-non-tasks)
                            (org-tags-match-list-sublevels nil)
                            (org-agenda-todo-ignore-scheduled bh/hide-scheduled-and-waiting-next-tasks)
                            (org-agenda-todo-ignore-deadlines bh/hide-scheduled-and-waiting-next-tasks)))
                (tags "-refile/-ABSORBED"
                      ((org-agenda-overriding-header "Tasks to Archive")
                       (org-agenda-skip-function 'bh/skip-non-archivable-tasks)
                       (org-tags-match-list-sublevels nil))))
               nil))))

;;;;;;;;;;;;;;;;;;;
;; Org habit config
;;;;;;;;;;;;;;;;;;;
(setq org-habit-show-habits-only-for-today t)
(setq org-habit-show-all-today nil)

;;;;;;;;;;;;;;;;;;
;;; Clocking stuff
;;;;;;;;;;;;;;;;;;
(setq org-clock-out-remove-zero-time-clocks t)

;; Resume clocking task when emacs is restarted
(org-clock-persistence-insinuate)
;; Show lot of clocking history so it's easy to pick items off the C-F11 list
(setq org-clock-history-length 23)
;; Resume clocking task on clock-in if the clock is open
(setq org-clock-in-resume t)
;; Change tasks to NEXT when clocking in
(setq org-clock-in-switch-to-state 'bh/clock-in-to-next)
;; Separate drawers for clocking and logs
(setq org-drawers (quote ("PROPERTIES" "LOGBOOK")))
;; Save clock data and state changes and notes in the LOGBOOK drawer
(setq org-clock-into-drawer t)
;; Sometimes I change tasks I'm clocking quickly - this removes clocked tasks with 0:00 duration
(setq org-clock-out-remove-zero-time-clocks t)
;; Clock out when moving task to a done state
(setq org-clock-out-when-done t)
;; Save the running clock and all clock history when exiting Emacs, load it on startup
(setq org-clock-persist t)
;; Do not prompt to resume an active clock
(setq org-clock-persist-query-resume nil)
;; Enable auto clock resolution for finding open clocks
(setq org-clock-auto-clock-resolution (quote when-no-clock-is-running))
;; Include current clocking task in clock reports
(setq org-clock-report-include-clocking-task t)

;;; Hooks

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
