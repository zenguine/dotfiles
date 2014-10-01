(require 'company)

;; nil causes company to always be there.  I'd like this except it
;; doesn't work well in haskell mode..  This gets changed to 0.7 in
;; the haskell mode hook and other hooks that have slow auto-complete
(setq company-idle-delay 0)

;; add this fucntion to any mode hook where company completion is
;; annoyingly slow to be happening with no delay
(defun increase-company-delay-locally (&optional val)
  (interactive)
  (setq-local company-idle-delay (or val 0.7)))

(setq tab-always-indent 'complete)

(define-key company-mode-map [remap indent-for-tab-command]
  'company-indent-for-tab-command)


(defvar completion-at-point-functions-saved nil)

(defun company-indent-for-tab-command (&optional arg)
  (interactive "P")
  (let ((completion-at-point-functions-saved completion-at-point-functions)
        (completion-at-point-functions '(company-complete-common-wrapper)))
    (indent-for-tab-command arg)))

(defun company-complete-common-wrapper ()
  (let ((completion-at-point-functions completion-at-point-functions-saved))
    (company-complete-common)))

(setq tab-always-indent 'complete)
(setq company-selection-wrap-around t)

(define-key company-active-map (kbd "C-w") 'backward-kill-word)
(define-key company-active-map (kbd "TAB") 'company-complete)
(define-key company-active-map [tab] 'company-complete)
(define-key company-active-map (kbd "C-w") 'backward-kill-word)

(add-hook 'after-init-hook 'global-company-mode)

(when (require 'company-ghc nil 'noerror)
  (add-to-list 'company-backends 'company-ghc)
  (custom-set-variables '(company-ghc-show-info t)))

(provide 'company-config)
