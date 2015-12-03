(setq org-clock-sound t)

(use-package my-org-util
  :load-path "site-lisp"
  :defer t
  :ensure nil)

(use-package org
  :ensure t
  :commands (org-mode bh/punch-in jump-to-org-agenda)
  :defer t
  :init
  (setq org-agenda-idle-display-timeout 5)

  ;; Auto display org agenda on idling

  (defun jump-to-org-agenda ()
    (interactive)
    (let ((buf (get-buffer "*Org Agenda*"))
	  wind)
      (if buf
	  (if (setq wind (get-buffer-window buf))
	      (select-window wind)
	    (if (called-interactively-p)
		(progn
		  (select-window (display-buffer buf t t))
		  (org-fit-window-to-buffer)
		  ;; (org-agenda-redo)
		  )
	      (with-selected-window (display-buffer buf)
		(org-fit-window-to-buffer)
		;; (org-agenda-redo)
		)))
	(call-interactively 'org-agenda-list)))
    ;;(let ((buf (get-buffer "*Calendar*")))
    ;;  (unless (get-buffer-window buf)
    ;;    (org-agenda-goto-calendar)))
    )
  
  (defun jump-to-org-agenda-trigger ()
    (jump-to-org-agenda)
    (if org-agenda-idle-display-timeout
	(run-with-idle-timer org-agenda-idle-display-timeout nil 'jump-to-org-agenda-trigger)))

  (run-with-idle-timer org-agenda-idle-display-timeout nil 'jump-to-org-agenda-trigger)
  :config
  (require 'org-id)
  (require 'my-org-util)
  (require 'f)
  (require 's)
  (require 'ido)
  (setq org-files-home "~/org")
  (setq org-agenda-diary-file (f-join org-files-home "diary.org"))

  (setq org-agenda-files '("~/org/personal.org" "~/org/refile.org" "~/org/work.org" "~/org/projects"))

  (setq org-treat-S-cursor-todo-selection-as-state-change nil
	org-default-notes-file (concat org-directory "/refile.org")
	org-startup-indented t
	org-refile-use-outline-path t
	org-outline-path-complete-in-steps nil
	org-refile-allow-creating-parent-nodes 'confirm
	org-completion-use-ido nil)

  (setq org-refile-targets '((org-agenda-files :maxlevel . 5)
			     (nil :maxlevel . 5)))

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
		(sequence "READ(r)" "NEXT(n)" "STARTED(s@/!)" "EVENTUALLY(e)" "|" "ABSORBED(a@/!)"))))

  (setq org-todo-keyword-faces
	(quote (("TODO" :foreground "orange" :weight bold)
		("NEXT" :foreground "white" :weight bold)
		("READ" :foreground "orange" :weight bold)
		("STARTED" :foreground "tomato" :weight bold)
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
		("STARTED" ("waiting") ("cancelled") ("hold"))
		("DONE" ("waiting") ("cancelled") ("hold"))
		("ABSORBED" ("reading" . t) ("waiting") ("cancelled") ("hold"))
		("READ" ("reading" . t) ("waiting") ("cancelled") ("hold"))
		("EVENTUALLY" ("waiting") ("cancelled") ("hold")))))

  (defun org-clocked-parent ()
    "Move to the parent of the task currently being clocked. If
   the currently clocked task is a top level heading, then move to the end of
   the file that it is in."
    (org-clock-goto)
    (interactive))

  ;; Capture templates
  (setq org-capture-templates
	(quote (("t" "Create new (non-clocked) task")
		("tr" "refile todo task" entry (file "~/org/refile.org")
		 "* TODO %?\n%U\n%a\n" :clock-in t :clock-resume t)
		("tt" "sub-task of current clocked task" entry (clock)
		 "* TODO %?\n%U\n%a\n" :clock-in t :clock-resume t)
		("ts" "sibling of current clocked task" entry (function org-clocked-parent)
		 "* TODO %?\n%U\n%a\n" :clock-in t :clock-resume t)
		("c" "Create and clock-in to new task")
		("cr" "(refile) todo task (and clock-in)" entry (file "~/org/refile.org")
		 "* NEXT %?\n%U\n%a\n" :clock-in t :clock-keep t)
		("ct" "sub-task of current clocked (and clock-in)" entry (clock)
		 "* NEXT %?\n%U\n%a\n" :clock-in t :clock-keep t)
		("cs" "sibling of current clocked (and clock-in)" entry (function org-clocked-parent)
		 "* NEXT %?\n%U\n%a\n" :clock-in t :clock-keep t)
		("n" "NEXT task" entry (file "~/org/refile.org")
		 "* NEXT %?\n%U\n%a\n" :clock-in t :clock-resume t)
		("i" "Interruption (clock returns to where it was after C-c C-c)" entry (file+datetree "~/org/diary.org")
		 "* %? :interruption:\n%U\n" :clock-in t :clock-resume t)
		("d" "Diary entry (clock remains active after C-c C-c)" entry (file+datetree "~/org/diary.org")
		 "* %?\n%U\n" :clock-in t :clock-keep t)
		("j" "Journal" entry (file+datetree "~/org/journal.org")
		 "* %?\n%U\n" :clock-in t :clock-resume t)
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
		("rc" "Currently reading" tags "+reading/+STARTED"
		 ((org-agenda-overriding-header "Reading List: current")))
		("rn" "Next to read" tags "+reading/+NEXT"
		 ((org-agenda-overriding-header "Reading List: next up")))
		("rt" "Todo: read eventually" tags "+reading/+READ"
		 ((org-agenda-overriding-header "Reading List: todo")))
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
    (smartparens-mode -1)
    (smartparens-strict-mode -1))

  (add-hook 'org-mode-hook 'my-org-mode-hook)
  (add-hook 'org-mode-hook 'turn-on-font-lock) ; not needed when global-font-lock-mode is on
  (add-hook 'org-mode-hook 'org-indent-mode)

  ;; Advice for saving all org buffers.. when clocking in/out, captureing, or refiling,
  ;; I want to guarantee that the changes are saved even if I forget to do it.
  ;; XXX: There are some uses that dont actually change buffers when these functions
  ;;      are given prefix arguments.. but the advice will still save all org-buffers
  ;;      even for these uses.. this isn't ideal but it also isn't a big deal and this is
  ;;      just a quick hack.
  (defadvice org-clock-in (after advice-for-after-org-clock-in activate)
    (org-save-all-org-buffers))

  (defadvice org-clock-out (after advice-for-after-org-clock-out activate)
    (org-save-all-org-buffers))

  (defadvice org-capture (after advice-for-after-org-capture activate)
    (org-save-all-org-buffers))

  (defadvice org-refile (after advice-for-after-org-refile activate)
    (org-save-all-org-buffers))

  ;; Add effort estimate on the fly when clocking in on org-mode tasks
  (add-hook 'org-clock-in-prepare-hook
	    'my-org-mode-ask-effort)

  (defun my-org-mode-ask-effort ()
    "Ask for an effort estimate when clocking in."
    (unless (org-entry-get (point) "Effort")
      (let ((effort
	     (completing-read
	      "Effort: "
	      (org-entry-get-multivalued-property (point) "Effort"))))
	(unless (equal effort "")
	  (org-set-property "Effort" effort)))))

  (require 'org-protocol)
  (setq org-protocol-default-template-key "l")

  (add-hook 'org-clock-out-hook 'bh/clock-out-maybe)

  ;; Org-mode encryption settings
  (require 'org-crypt)
  (org-crypt-use-before-save-magic)
  (setq org-tags-exclude-from-inheritance '("crypt"))
  (setq org-crypt-key nil)

  (defun my/org-insert-quote-block (&optional quote-type)
    "Insert a `QUOTE-TYPE' quoted block of text in org-mode.
   `QUOTE-TYPE' can be one of 'center, 'quote, or 'verse.
    See http://orgmode.org/manual/Paragraphs.html for the
    different semantics."
    (interactive
     (let ((quote-types-list
	    '("quote" "verse" "center")))
       (list (ido-completing-read "Quoted text block type: " quote-types-list))))
    (let* ((quote-type (or quote-type "quote"))
	   (quote-type-cap (s-upcase quote-type)))
      (progn
	(newline-and-indent)
	(insert (format "#+BEGIN_%s\n" quote-type-cap))
	(newline-and-indent)
	(insert (format "#+END_%s\n" quote-type-cap))
	(previous-line 2))))

  ;; originally in my-org-util

  (add-hook 'org-agenda-mode-hook
	    '(lambda () (org-defkey org-agenda-mode-map "W" (lambda () (interactive) (setq bh/hide-scheduled-and-waiting-next-tasks t) (bh/widen))))
	    'append)

  (add-hook 'org-agenda-mode-hook
	    '(lambda () (org-defkey org-agenda-mode-map "N" 'bh/narrow-to-subtree))
	    'append)

  (add-hook 'org-agenda-mode-hook
	    '(lambda () (org-defkey org-agenda-mode-map "U" 'bh/narrow-up-one-level))
	    'append)

  (add-hook 'org-agenda-mode-hook
	    '(lambda () (org-defkey org-agenda-mode-map "P" 'bh/narrow-to-project))
	    'append)

  (add-hook 'org-agenda-mode-hook
	    '(lambda () (org-defkey org-agenda-mode-map "V" 'bh/view-next-project))
	    'append)

  (define-key org-mode-map (kbd "C-c s") 'org-insert-src-block)

  ;; Org-mode specific keybinings
  (define-key org-mode-map (kbd "C-c q") 'my/org-insert-quote-block)
  
  (after 'evil
    (use-package evil-org
      :load-path "elisp/"))
  
  (after 'mu4e
    (require 'org-mu4e)))

(provide 'org-config)
