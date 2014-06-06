(require 'comint)

(defvar jetpack/root-path (expand-file-name "~/code/jetpack")
  "Path to root directory of jetpack source tree")

(defvar jetpack/executable-name "jetpack"
  "Name of the jetpack executable")

(provide 'jetpack-mode)
