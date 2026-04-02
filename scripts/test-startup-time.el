;;; test-startup-time.el --- -*- lexical-binding: t -*-

(setq my-server-start-p nil)
(setq my-emacs-path (expand-file-name
                     (concat (file-name-directory load-file-name) "../")))

(require 'package)
(package-initialize)
(advice-add 'package-vc-install :override #'ignore)

(message "\n=== Startup Time Test ===\n")

(let ((t1 (float-time)))
  (load-file (concat my-emacs-path "init/shared-init.el"))
  (let ((t2 (float-time)))
    (condition-case err
        (my-run-deferred-tasks)
      (error (message "Warning during deferred tasks: %S" err)))
    (let ((t3 (float-time)))
      (message "Config load time:         %.3fs" (- t2 t1))
      (message "Deferred tasks time:      %.3fs" (- t3 t2))
      (message "Total (load + deferred):  %.3fs" (- t3 t1)))))

(message "")

;;; test-startup-time.el ends here
