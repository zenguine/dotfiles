(require 'company)

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
(setq company-idle-delay 0.7)
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
