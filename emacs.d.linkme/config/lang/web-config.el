(use-package web-mode
  :ensure t
  :commands web-mode
  :init
  (add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.[gj]sp\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
  :config
  (setq web-mode-engines-alist '(("php" . "\\.phtml\\'")
				 ("blade" . "\\.blade\\.")))
  (defun my-web-mode-hook ()
    "Hooks for web-mode."
    (setq web-mode-markup-indent-offset 2))

  (add-hook 'web-mode-hook 'my-web-mode-hook))

(provide 'web-config)
