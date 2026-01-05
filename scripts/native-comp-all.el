;; byte-compile-local.el --- -*- lexical-binding: t -*-

(setq my-native-comp-enable t)
(setq my-server-start-p nil)
(setq my-emacs-path (expand-file-name
                     (concat (file-name-directory load-file-name) "../")))

(message "Native-compiling all files...")
(load-file (concat my-emacs-path "init/early-shared-init.el"))
(load-file (concat my-emacs-path "init/shared-init.el"))
(my-run-deferred-tasks)

(provide 'native-comp-all)
;;; byte-compile-local.el ends here
