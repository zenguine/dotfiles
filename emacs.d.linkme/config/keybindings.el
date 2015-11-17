;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Universal argument related
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(global-set-key (kbd "C-\\") 'universal-argument)
(define-key universal-argument-map (kbd "C-\\") 'universal-argument-more)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Commands manipulating buffer lines matching a regexp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq regexp-manip-funcs-map (make-sparse-keymap))
(define-key regexp-manip-funcs-map (kbd "k") 'keep-lines) ;; alias: delete-non-matching-lines
(define-key regexp-manip-funcs-map (kbd "d") 'flush-lines) ;; alias: delete-matching-lines
(define-key regexp-manip-funcs-map (kbd "c") 'count-matches) ;; alias: how-many
(define-key regexp-manip-funcs-map (kbd "o") 'occur) ;; alias: list-matching-lines
(define-key regexp-manip-funcs-map (kbd "h") 'highlight-lines-matching-regexp) ;; alias: list-matching-lines
(global-set-key (kbd "C-c r") regexp-manip-funcs-map)
(global-set-key (kbd "M-\\") 'hippie-expand)
(global-set-key (kbd "C-s-/") 'evil-ex-nohighlight)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Miscellaneous  / unsorted commands
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(global-set-key (kbd "RET") 'newline-and-indent)

(global-def-key (kbd "C-c C-s") 'switch-to-buffer "*scratch*")
(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "C-c e") 'start-or-switch-irc)
(global-set-key (kbd "C-c m") 'mu4e)
(global-set-key (kbd "C-c C-n") 'flycheck-next-error)
(global-set-key (kbd "C-c C-p") 'flycheck-previous-error)
(global-set-key (kbd "C-,") popwin:keymap)
(global-set-key (kbd "C-c y") 'yas-describe-tables)
(global-set-key (kbd "C-c C-\\") 'erc-track-switch-buffer)
(global-set-key (kbd "C-c B") 'erc-iswitchb)
(global-def-key (kbd "C-x k") 'kill-buffer (current-buffer))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Help / documentation  commands
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(global-set-key (kbd "C-c h") help-map)
(define-key help-map (kbd "C-l") 'find-library)
(define-key help-map (kbd "C-f") 'find-function)
(define-key help-map (kbd "C-k") 'find-function-on-key)
(define-key help-map (kbd "C-v") 'find-variable)
(define-key help-map (kbd "C-m") 'discover-my-major)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Version control
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(global-set-key (kbd "C-x v p") 'git-messenger:popup-message)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Commands to evaluate elisp expressions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(global-set-key (kbd "M-e") 'eval-region)
(global-set-key (kbd "C-c :") 'my-eval-expression)
(global-set-key (kbd "C-c C-j") 'eval-print-last-sexp)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helm / commands that list things
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)
(global-set-key (kbd "C-x p") 'proced)
(global-set-key (kbd "C-x C-p") 'paradox-list-packages)
(global-set-key (kbd "C-x C-b") 'ibuffer-other-window)
(global-set-key (kbd "C-c R") 'revert-buffer)

;;;;;;;;;;;;;;;;;;;;
;; Org mode bindings
;;;;;;;;;;;;;;;;;;;;
(global-set-key (kbd "C-c x d h") 'org-decrypt-entry)
(global-set-key (kbd "C-c x d b") 'org-decrypt-entries)
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)
(global-set-key "\C-cc" 'org-capture)
(global-set-key (kbd "C-c C-x C-j") 'org-clock-goto)
(global-set-key (kbd "C-c C-x C-o") 'org-clock-out)
(global-set-key (kbd "<f12>") 'visible-mode)
(global-set-key (kbd "<f9> i") 'bh/punch-in)
(global-set-key (kbd "<f9> o") 'bh/punch-out)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Window moving/resizing keybindings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(global-set-key (kbd "C-x 0") 'delete-window-and-balance)

(global-set-key (kbd "M-[") 'winner-undo)
(global-set-key (kbd "M-]") 'winner-redo)

(global-def-key (kbd "C-h") 'move-window-or-create 'left)
(global-def-key (kbd "C-j") 'move-window-or-create 'below)
(global-def-key (kbd "C-k") 'move-window-or-create 'above)
(global-def-key (kbd "C-l") 'move-window-or-create 'right)

(global-def-key (kbd "S-<up>") 'enlarge-window 3)
(global-def-key (kbd "S-<down>") 'shrink-window 3)
(global-def-key (kbd "S-<right>") 'enlarge-window-horizontally 3)
(global-def-key (kbd "S-<left>") 'shrink-window-horizontally 3)




(provide 'keybindings)
