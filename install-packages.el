(load-file (concat default-directory "init/settings.el"))

(require 'package)
(package-initialize t)
(package-refresh-contents)

(mapc #'package-install package-selected-packages)

(require 'package-utils)
(package-utils-upgrade-all-no-fetch)

(defun my-package-autoremove ()
  (interactive)
  (let ((removable (package--removable-packages)))
    (if removable
        (mapc (lambda (p)
                (package-delete (cadr (assq p package-alist)) t))
              removable)
      (message "Nothing to autoremove"))))

(my-package-autoremove)
