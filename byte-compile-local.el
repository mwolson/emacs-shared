(load-file (concat default-directory "init/transient-overrides.el"))
(load-file (concat default-directory "init/settings.el"))

(require 'package)
(package-initialize)

(defun my-byte-compile-local-package (lib-name lib-path)
  (let* ((lib-sym (intern lib-name))
         (pkg (cadr (assq lib-sym (package--alist))))
         (dir-or-file (file-name-concat default-directory lib-path)))
    (when pkg
      (package-delete pkg t))
    (if (string-match-p "\\.el\\'" dir-or-file)
        (progn
          (message "Compiling %s..." (expand-file-name dir-or-file))
          (byte-compile-file dir-or-file))
      (package-install-file dir-or-file))))

(apply #'my-byte-compile-local-package argv)
