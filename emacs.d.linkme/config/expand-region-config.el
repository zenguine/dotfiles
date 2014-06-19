(when (require 'evil nil 'noerror)
  (define-key evil-normal-state-map (kbd "+") 'er/expand-region)
  (define-key evil-normal-state-map (kbd "RET") 'er/expand-region)
  (define-key evil-visual-state-map (kbd "x") 'er/expand-region)
  (define-key evil-visual-state-map (kbd "X") 'er/contract-region)
  (define-key evil-visual-state-map (kbd "+") 'er/expand-region)
  (define-key evil-visual-state-map (kbd "-") 'er/contract-region))

(provide 'expand-region-config)
