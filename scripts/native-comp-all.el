;;; native-comp-all.el --- -*- lexical-binding: t -*-

(setq my-native-comp-enable t)
(setq my-server-start-p nil)
(setq my-emacs-path (expand-file-name
                     (concat (file-name-directory load-file-name) "../")))

(require 'package)
(package-initialize)
(advice-add 'package-vc-install :override #'ignore)

(let ((config-files '("init/settings.el"
                       "init/early-shared-init.el"
                       "init/shared-init.el")))

  ;; Phase 1: Load config and run deferred tasks so packages are available
  (dolist (lib config-files)
    (load-file lib))
  (condition-case err
      (my-run-deferred-tasks)
    (error (message "Warning during deferred tasks: %S" err)))

  ;; Phase 2: Compile
  (message "Native-compiling all files...")
  (dolist (lib config-files)
    (native-compile lib)))

(provide 'native-comp-all)
;;; native-comp-all.el ends here
