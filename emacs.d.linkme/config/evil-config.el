(require 'cl)
(setq evil-want-C-u-scroll t)

(use-package avy
  :ensure t
  :defer t)

(use-package evil
  :ensure t
  :init
  (setq evil-want-fine-undo 'no)
  (setq evil-want-C-i-jump nil)
  (setq evil-move-cursor-back nil)
  (setq evil-overriding-maps nil)
  (setq evil-intercept-maps nil)

  (setq evil-shift-width 2)
  (setq evil-auto-balance-windows t)
  :config
  (use-package f :ensure t)
  ;; Tag navigation
  (define-key evil-normal-state-map (kbd "M-.") 'find-tag)
  (define-key evil-normal-state-map (kbd "C-M-.") 'find-tag-regexp)
  (define-key evil-normal-state-map (kbd "g ]") 'etags-select-find-tag-at-point)

  ;; Define RET behavior in various modes
  (evil-define-key 'normal Custom-mode (kbd "RET") 'Custom-newline)
  (evil-define-key 'normal Custom-mode (kbd "q") 'Custom-buffer-done)
  (evil-define-key 'normal Info-mode (kbd "RET") 'Info-follow-nearest-node)
  (evil-define-key 'normal Info-mode (kbd "l") 'evil-forward-char)
  (evil-define-key 'normal Info-mode (kbd "h") 'evil-backward-char)

  (setq evil-emacs-state-cursor '("#fb4934" box))
  (setq evil-normal-state-cursor '("#d3869b" box))
  (setq evil-visual-state-cursor '("orange" box))
  (setq evil-insert-state-cursor '("maroon" bar))
  (setq evil-replace-state-cursor '("maroon" bar))
  (setq evil-operator-state-cursor '("maroon" hollow))

  (setq evil-search-module 'isearch)
  (setq evil-ex-search-vim-style-regexp t)
  (setq evil-magic 'magic)

  (defadvice evil-ex-search-next (after advice-for-evil-ex-search-next activate)
    (evil-scroll-line-to-center (line-number-at-pos)))

  (defadvice evil-ex-search-previous (after advice-for-evil-ex-search-previous activate)
    (evil-scroll-line-to-center (line-number-at-pos)))

  (evil-define-command cofi/maybe-exit ()
    :repeat change
    (interactive)
    (let ((modified (buffer-modified-p)))
      (insert "j")
      (let ((evt (read-event (format "Insert %c to exit insert state" ?k)
                             nil 0.5)))
        (cond
         ((null evt) (message ""))
         ((and (integerp evt) (char-equal evt ?k))
          (delete-char -1)
          (set-buffer-modified-p modified)
          (push 'escape unread-command-events))
         (t (setq unread-command-events (append unread-command-events
                                                (list evt))))))))
  (define-key evil-insert-state-map (kbd "C-u") 'backward-kill-line)
  (define-key evil-emacs-state-map (kbd "C-u") 'backward-kill-line)
  (evil-define-key 'insert yas-minor-mode-map (kbd "C-o") 'yas-expand)
  (define-key evil-insert-state-map "j" 'cofi/maybe-exit)
  (define-key evil-normal-state-map "H" 'evil-first-non-blank)

  (define-key evil-normal-state-map (kbd "C-p") 'helm-projectile-custom)

  (define-key evil-normal-state-map (kbd "j") 'evil-next-visual-line)
  (define-key evil-normal-state-map (kbd "k") 'evil-previous-visual-line)

  (define-key evil-normal-state-map  (kbd "'") 'evil-goto-mark)
  (define-key evil-normal-state-map  (kbd "`") 'evil-goto-mark-line)
  (define-key evil-motion-state-map  (kbd "'") 'evil-goto-mark)
  (define-key evil-motion-state-map  (kbd "`") 'evil-goto-mark-line)
  (define-key evil-operator-state-map  (kbd "'") 'evil-goto-mark)
  (define-key evil-operator-state-map  (kbd "`") 'evil-goto-mark-line)

  (define-key evil-operator-state-map (kbd "TAB") 'evil-jump-item)
  (define-key evil-motion-state-map (kbd "TAB") 'evil-jump-item)
  (define-key evil-normal-state-map (kbd "TAB") 'evil-jump-item)
  (define-key evil-normal-state-map "L" 'evil-end-of-line)
  (define-key evil-normal-state-map (kbd "zn")
    (lambda () (interactive)
      (progn
        (move-end-of-line nil)
        (newline-and-indent))))
  
  (define-key evil-normal-state-map (kbd "zp")
    (lambda () (interactive)
      (progn
        (move-beginning-of-line nil)
        (newline-and-indent))))

  (define-key evil-normal-state-map ")" 'forward-sexp)
  (define-key evil-operator-state-map")" 'forward-sexp)
  (define-key evil-motion-state-map")" 'forward-sexp)

  (define-key evil-normal-state-map "(" 'backward-sexp)
  (define-key evil-operator-state-map"(" 'backward-sexp)
  (define-key evil-motion-state-map"(" 'backward-sexp)

                                        ; Folding commands
  (define-key evil-normal-state-map "zo" 'evil-open-folds)
  (define-key evil-normal-state-map "zr" 'evil-open-fold)

                                        ; Evil "leader" mappings

  (define-key evil-normal-state-map "]o" 'occur-next)
  (define-key evil-normal-state-map "[o" 'occur-prev)
  (define-key evil-normal-state-map "]s" 'forward-sexp)
  (define-key evil-normal-state-map "[s" 'backward-sexp)
  (define-key evil-normal-state-map "]e" 'next-error)
  (define-key evil-normal-state-map "[e" 'previous-error)
  (define-key evil-normal-state-map "gJ" 'evil-join)

  (setf avy-all-windows nil)
  (define-key evil-motion-state-map (kbd "S-SPC") 'avy-goto-subword-1)
  (define-key evil-motion-state-map (kbd "C-SPC") 'avy-goto-char-2)


  ;; Switch eval-expresion and evil-ex bindings since eval-expression is
  ;; more useful to me now
  (define-key evil-normal-state-map (kbd ":") 'my-eval-expression)
  (define-key evil-normal-state-map (kbd "C-:") 'evil-ex)

  (define-key evil-normal-state-map "  " 'helm-M-x)
  (define-key evil-normal-state-map " c" 'evil-window-delete)
  (define-key evil-visual-state-map "  " 'helm-M-x)
  (define-key evil-normal-state-map " 1" 'delete-other-windows)
  (define-key evil-normal-state-map " o" 'delete-other-windows)
  (define-key evil-normal-state-map " k" 'kill-this-buffer)
  (define-key evil-normal-state-map " rb" 'rainbow-blocks-mode)
  (define-key evil-normal-state-map " rd" 'rainbow-delimiters-mode)
  (define-key evil-normal-state-map " ri" 'color-identifiers-mode)
  (define-key evil-normal-state-map " x" 'delete-single-window)
  (define-key evil-normal-state-map " b" 'ido-switch-buffer)
  (define-key evil-normal-state-map " f" 'ido-find-file)
  (define-key evil-normal-state-map " g" 'helm-bookmarks)

  ;; keybindings to edit specific files
  (define-key evil-normal-state-map " em" 'edit-mode-config-file)
  (def-key evil-normal-state-map " ea" 'find-file (f-join user-emacs-directory "config" "appearance-config.el"))
  (def-key evil-normal-state-map " ee" 'find-file (f-join user-emacs-directory "config" "evil-config.el"))
  (def-key evil-normal-state-map " ei" 'find-file user-init-file)
  (def-key evil-normal-state-map " ek" 'find-file (f-join user-emacs-directory "config" "keybindings.el"))
  (def-key evil-normal-state-map " eo" 'find-file (f-join org-files-home "personal.org"))
  (def-key evil-normal-state-map " eu" 'find-file (f-join user-emacs-directory "elisp" "util.el"))
  (def-key evil-normal-state-map " ev" 'find-file (f-join user-emacs-directory "config" "evil-config.el"))
  (def-key evil-normal-state-map " en" 'find-file "/sudo::/etc/nixos/configuration.nix" )


  (define-key evil-normal-state-map " l" 'linum-mode)
  (def-key evil-normal-state-map (kbd "SPC TAB") 'switch-to-buffer "*scratch*")

  (define-key evil-normal-state-map "gb" 'ido-switch-buffer)
  (define-key evil-normal-state-map "gs" 'eshell-here)
  (define-key evil-normal-state-map "g$" 'shell)

  (define-key evil-normal-state-map (kbd "SPC ,") 'switch-to-other-buffer)

  ; Key-chord bindings
  (key-chord-define evil-emacs-state-map " 1" 'delete-other-windows)
  (key-chord-define evil-emacs-state-map " c" 'evil-window-delete)
  (key-chord-define evil-emacs-state-map " k" 'kill-this-buffer)
  (key-chord-define evil-emacs-state-map " x" 'delete-single-window)
  (key-chord-define evil-emacs-state-map " f" 'ido-find-file)
  (key-chord-define evil-emacs-state-map " b" 'ido-switch-buffer)
  (key-chord-define evil-emacs-state-map " ," 'switch-to-other-buffer)

  (cl-loop for (mode . state) in '((bc-menu-mode . emacs)
                                   (comint-mode . emacs)
                                   (Custom-mode . normal)
                                   (ebib-entry-mode . emacs)
                                   (ebib-index-mode . emacs)
                                   (ebib-log-mode . emacs)
                                   (erc-mode . normal)
                                   (diff-mode . emacs)
                                   (git-rebase-mode . emacs)
                                   (ein:notebook-multilang-mode . emacs)
                                   (eshell-mode . emacs)
                                   (gtags-select-mode . emacs)
                                   (haskell-interactive-mode . emacs)
                                   (idris-repl-mode . emacs)
                                   (inferior-emacs-lisp-mode . emacs)
                                   (magit-branch-manager-mode . emacs)
                                   (occur-mode . normal)
                                   (package-menu-mode . emacs)
                                   (paradox-menu-mode . emacs)
                                   (pylookup-mode . emacs)
                                   (rdictcc-buffer-mode . emacs)
                                   (semantic-symref-results-mode . emacs)
                                   (shell-mode . emacs)
                                   (term-mode . emacs))
           do (evil-set-initial-state mode state))
  
  ; Enable evil mode globally
  (evil-mode 1)

  )

;; Evil-jumper
(use-package evil-jumper
  :ensure t
  :commands (evil-jumper/forward evil-jumper/backward)
  :config
  (define-key evil-motion-state-map (kbd "M-i") 'evil-jumper/forward)
  (define-key evil-motion-state-map (kbd "M-o") 'evil-jumper/backward)
  (define-key evil-normal-state-map (kbd "M-i") 'evil-jumper/forward)
  (define-key evil-normal-state-map (kbd "M-o") 'evil-jumper/backward)
  (define-key evil-normal-state-map (kbd "M-i") 'evil-jump-forward)
  (define-key evil-normal-state-map (kbd "M-o") 'evil-jump-backward)
  (define-key evil-motion-state-map (kbd "M-i") 'evil-jump-forward)
  (define-key evil-motion-state-map (kbd "M-o") 'evil-jump-backward))

;; Evil nerd commenter config
(use-package evil-nerd-commenter
  :ensure t
  :commands evilnc-comment-operator evilnc-comment-or-uncomment-lines
  :init
  (require 'evil)
  (define-key evil-normal-state-map (kbd "\\") 'evilnc-comment-operator)
  (define-key evil-visual-state-map (kbd "\\") 'evilnc-comment-operator)
  (global-set-key (kbd "M-;") 'evilnc-comment-or-uncomment-lines))

(use-package surround
  :load-path "site-lisp/"
  :ensure nil)

(use-package evil-args
  :ensure t
  :commands evil-inner-arg evil-outer-arg evil-forward-arg evil-backward-arg evil-jump-out-args
  ;; bind evil-args text objects
  :init
  (require 'evil)
  (define-key evil-inner-text-objects-map "a" 'evil-inner-arg)
  (define-key evil-outer-text-objects-map "a" 'evil-outer-arg)

  ;; bind evil-forward/backward-args
  (define-key evil-normal-state-map "gl" 'evil-forward-arg)
  (define-key evil-normal-state-map "gh" 'evil-backward-arg)
  (define-key evil-motion-state-map "gl" 'evil-forward-arg)
  (define-key evil-motion-state-map "gh" 'evil-backward-arg)

  ;; bind evil-jump-out-args
  (define-key evil-normal-state-map "gk" 'evil-jump-out-args))

(use-package evil-visualstar
  :ensure t)

(use-package evil-anzu
  :ensure t)

(provide 'evil-config)
