;; install-vterm.el --- -*- lexical-binding: t -*-

(load-file (concat (file-name-directory load-file-name) "init/settings.el"))

(require 'treesit) ; to silence an autoload warning, seems like emacs bug
(require 'package)
(package-initialize)

;; vterm will fire off a prompt upon `require` if it does not see vterm-module
;; yet, and we don't want it to do that, so shim `vterm-module`
(provide 'vterm-module)
(vterm-module-compile)

(provide 'install-vterm)
;;; install-vterm.el ends here
