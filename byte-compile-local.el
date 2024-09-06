(load-file (concat default-directory "init/transient-overrides.el"))
(load-file (concat default-directory "init/settings.el"))

(require 'package)
(package-initialize)

(defun my-byte-compile-local-package (lib-name lib-path)
  (let* ((lib-sym (intern lib-name))
         (pkg (cadr (assq lib-sym (package--alist)))))
    (when pkg
      (package-delete pkg t)))
  (package-install-file (concat default-directory lib-path)))

(apply #'my-byte-compile-local-package argv)
