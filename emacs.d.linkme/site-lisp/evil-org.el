(require 'evil)
(require 'org)

(defun always-insert-item ()
  "Force insertion of org item"
  (if (not (org-in-item-p))
      (insert "\n- ")
    (org-insert-item)))

(defun evil-org-eol-call (fun)
  "Go to end of line and call provided function"
  (end-of-line)
  (funcall fun)
  (evil-append nil))

;; Define hydra for convenient moving around between headers with vim
;; keybindings in org mode files
(after 'hydra
	;; to be backward compatible with older org version
	(defhydra hydra-org-move (:color red)
		"Goto org heading:"
		("p" outline-up-heading "parent")
		("h" outline-previous-visible-heading "next visible")
		("l" outline-next-visible-heading "prev visible")
		("j" org-forward-heading-same-level "next sibling")
		("k" org-backward-heading-same-level "prev sibling")
		("q" nil "cancel" :color blue))
	(evil-define-key 'normal org-mode-map
		"gp" 'hydra-org-move/outline-up-heading
		"gh" 'hydra-org-move/outline-previous-visible-heading
		"gj" 'hydra-org-move/org-forward-heading-same-level
		"gk" 'hydra-org-move/org-backward-heading-same-level
		"gl" 'hydra-org-move/outline-next-visible-heading))

    ;; normal state shortcuts
(evil-define-key 'normal org-mode-map
  (kbd "RET") 'org-open-at-point
  "H" 'org-beginning-of-line
  "L" 'org-end-of-line
  "o" '(lambda () (interactive) (evil-org-eol-call 'org-insert-heading))
  "$" 'org-end-of-line
  "^" 'org-beginning-of-line
  "<" 'org-metaleft
  ">" 'org-metaright
  "-" 'org-cycle-list-bullet)

;; normal & insert state shortcuts.
(mapc (lambda (state)
        (evil-define-key state org-mode-map
          (kbd "M-l") 'org-metaright
          (kbd "M-h") 'org-metaleft
          (kbd "M-k") 'org-metaup
          (kbd "M-j") 'org-metadown
          (kbd "M-L") 'org-shiftmetaright
          (kbd "M-H") 'org-shiftmetaleft
          (kbd "M-K") 'org-shiftmetaup
          (kbd "M-J") 'org-shiftmetadown
          (kbd "M-o") '(lambda () (interactive)
                         (evil-org-eol-call
                          '(lambda()
                             (org-insert-heading)
                             (org-metaright))))
          (kbd "M-t") '(lambda () (interactive)
                         (evil-org-eol-call
                          '(lambda()
                             (org-insert-todo-heading nil)
                             (org-metaright))))
          ))
      '(normal insert))

(evil-define-key 'normal org-mode-map (kbd "C-t") 'org-mark-ring-goto)
(provide 'evil-org)
