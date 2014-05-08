(require 'popwin)

(popwin-mode 1)
(setq display-buffer-function 'popwin:display-buffer)
(push '("^\*pytest.+\*$" :regexp t :height 20 :noselect t) popwin:special-display-config)

(defadvice popwin:close-popup-window (before advice-for-before-close-popup-window activate)
  (when popwin:popup-buffer
    (kill-buffer popwin:popup-buffer)))

(provide 'popwin-config)