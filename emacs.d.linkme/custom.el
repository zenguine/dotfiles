(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["color-237" "#d75f5f" "#afaf00" "#ffaf00" "#87afaf" "#d787af" "#87af87" "color-223"])
 '(column-number-mode t)
 '(company-ghc-show-info t)
 '(company-minimum-prefix-length 2)
 '(compilation-message-face (quote default))
 '(cua-global-mark-cursor-color "#2aa198")
 '(cua-normal-cursor-color "#657b83")
 '(cua-overwrite-cursor-color "#b58900")
 '(cua-read-only-cursor-color "#859900")
 '(custom-safe-themes
   (quote
    ("c5a044ba03d43a725bd79700087dea813abcb6beb6be08c7eb3303ed90782482" "3b819bba57a676edf6e4881bd38c777f96d1aa3b3b5bc21d8266fa5b0d0f1ebf" "6a37be365d1d95fad2f4d185e51928c789ef7a4ccf17e7ca13ad63a8bf5b922f" "077bb64958d1b2effe53ff01ce2faac7abfda24484c4422d0a8b90195261dffe" "454dc6f3a1e9e062f34c0f988bcef5d898146edc5df4aa666bf5c30bed2ada2e" "e16a771a13a202ee6e276d06098bc77f008b73bbac4d526f160faa2d76c1dd0e" "60f04e478dedc16397353fb9f33f0d895ea3dab4f581307fbf0aa2f07e658a40" "0e121ff9bef6937edad8dfcff7d88ac9219b5b4f1570fd1702e546a80dba0832" "cdf488af2fbc0735c3eeff42e77bc62cb14bd869a89c6a27a854e2c4a50c9ad2" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "146d24de1bb61ddfa64062c29b5ff57065552a7c4019bee5d869e938782dfc2a" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "c2cfe2f1440d9ef4bfd3ef4cf15bfe35ff40e6d431264b1e24af64f145cffb11" "3a727bdc09a7a141e58925258b6e873c65ccf393b2240c51553098ca93957723" "cd70962b469931807533f5ab78293e901253f5eeb133a46c2965359f23bfb2ea" "a53714de04cd4fdb92ed711ae479f6a1d7d5f093880bfd161467c3f589725453" "1e7e097ec8cb1f8c3a912d7e1e0331caeed49fef6cff220be63bd2a6ba4cc365" "fc5fcb6f1f1c1bc01305694c59a1a861b008c534cae8d0e48e4d5e81ad718bc6" default)))
 '(elpy-default-minor-modes (quote (eldoc-mode flymake-mode yas-minor-mode)))
 '(erc-modules
   (quote
    (autojoin button completion fill irccontrols list match menu move-to-prompt netsplit networks noncommands readonly ring services stamp track)))
 '(erc-nick "jcullen")
 '(erc-port 6667)
 '(erc-server "irc.freenode.net")
 '(evil-search-module (quote evil-search))
 '(flycheck-display-errors-function (function flycheck-pos-tip-error-messages))
 '(git-gutter:disabled-modes (quote (org-mode)))
 '(git-gutter:hide-gutter t)
 '(haskell-interactive-popup-errors nil)
 '(haskell-process-auto-import-loaded-modules t)
 '(haskell-process-log t)
 '(haskell-process-suggest-remove-import-lines t)
 '(haskell-process-type (quote cabal-repl))
 '(haskell-stylish-on-save nil)
 '(haskell-tags-on-save t)
 '(helm-buffer-details-flag t)
 '(helm-split-window-default-side (quote same))
 '(helm-split-window-in-side-p nil)
 '(highlight-changes-colors (quote ("#d33682" "#6c71c4")))
 '(highlight-symbol-colors
   (--map
    (solarized-color-blend it "#fdf6e3" 0.25)
    (quote
     ("#b58900" "#2aa198" "#dc322f" "#6c71c4" "#859900" "#cb4b16" "#268bd2"))))
 '(highlight-symbol-foreground-color "#586e75")
 '(highlight-tail-colors
   (quote
    (("#eee8d5" . 0)
     ("#B4C342" . 20)
     ("#69CABF" . 30)
     ("#69B7F0" . 50)
     ("#DEB542" . 60)
     ("#F2804F" . 70)
     ("#F771AC" . 85)
     ("#eee8d5" . 100))))
 '(magit-diff-use-overlays nil)
 '(magit-item-highlight-face (quote bold))
 '(magit-use-overlays nil)
 '(org-agenda-files
   (quote
    ("~/org/personal.org" "~/org/work.org" "~/org/refile.org")))
 '(org-modules
   (quote
    (org-bbdb org-bibtex org-docview org-gnus org-info org-jsinfo org-habit org-irc org-mew org-mhe org-rmail org-vm org-wl org-w3m)))
 '(projectile-enable-idle-timer t)
 '(projectile-remember-window-configs t)
 '(projectile-tags-command "ctags --python-kinds=-iv -Re %s")
 '(quack-programs
   (quote
    ("mechanics" "bigloo" "csi" "csi -hygienic" "gosh" "gracket" "gsi" "gsi ~~/syntax-case.scm -" "guile" "kawa" "mit-scheme" "racket" "racket -il typed/racket" "rs" "scheme" "scheme48" "scsh" "sisc" "stklos" "sxi")))
 '(safe-local-variable-values
   (quote
    ((eval when
	   (require
	    (quote proj-darkdynasty)
	    nil
	    (quote noerror))
	   (darkdynasty-mode t)))))
 '(show-paren-mode t)
 '(smartrep-mode-line-active-bg (solarized-color-blend "#859900" "#eee8d5" 0.2))
 '(term-default-bg-color "#fdf6e3")
 '(term-default-fg-color "#657b83")
 '(tool-bar-mode nil)
 '(weechat-color-list
   (quote
    (unspecified "#fdf6e3" "#eee8d5" "#990A1B" "#dc322f" "#546E00" "#859900" "#7B6000" "#b58900" "#00629D" "#268bd2" "#93115C" "#d33682" "#00736F" "#2aa198" "#657b83" "#839496"))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:background nil :family "Fantasque Sans Mono" :foundry "unknown" :slant normal :weight normal :height 113 :width normal))))
 '(company-scrollbar-bg ((t (:background "#414141"))))
 '(company-scrollbar-fg ((t (:background "#343434"))))
 '(company-tooltip ((t (:inherit default :background "#2d2d2d"))))
 '(company-tooltip-common ((t (:inherit font-lock-constant-face))))
 '(company-tooltip-selection ((t (:inherit font-lock-function-name-face))))
 '(shm-current-face ((t (:background "#295864")))))
