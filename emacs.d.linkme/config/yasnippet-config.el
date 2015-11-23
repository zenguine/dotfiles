(use-package yasnippet
  :ensure t
  :defer t
  :config
  (defun active-snippet-names ()
    "Return the names of currently active snippets."
    (let (names)
      (mapc (lambda (hash-table)
              (maphash (lambda (key _) (push key names)) hash-table))
            (mapcar #'yas--table-hash
                    (yas--get-snippet-tables)))
      names))

  ;;(define-key yas-minor-mode-map (kbd "C-RET") 'yas-next-field)
  ;;(define-key yas-minor-mode-map (kbd "<C-return>") 'yas-next-field)
  ;;(define-key yas-minor-mode-map [(control return)] 'yas-next-field)
  ;; (define-key yas-minor-mode-map [(tab)] nil)
  ;; (define-key yas-minor-mode-map (kbd "TAB") nil)

  (after 'evil
    (evil-define-key 'insert yas-minor-mode-map (kbd "C-o") 'yas-expand-from-trigger-key))

  (yas-global-mode 1))

(provide 'yasnippet-config)
