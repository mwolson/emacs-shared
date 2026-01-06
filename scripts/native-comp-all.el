;; byte-compile-local.el --- -*- lexical-binding: t -*-

(setq my-native-comp-enable t)
(setq my-server-start-p nil)
(setq my-emacs-path (expand-file-name
                     (concat (file-name-directory load-file-name) "../")))

(defun my-load-and-compile (lib)
  (let ((file (format "%sinit/%s.el" my-emacs-path lib)))
    (load-file lib)
    (native-compile lib)))

(message "Native-compiling all files...")
(my-load-and-compile "init/settings.el")
(my-load-and-compile "init/early-shared-init.el")
(my-load-and-compile "init/shared-init.el")
(my-run-deferred-tasks)

(provide 'native-comp-all)
;;; byte-compile-local.el ends here
