(load-file (concat default-directory "init/settings.el"))

(require 'package)
(package-initialize t)
(package-refresh-contents)

(mapc #'package-install package-selected-packages)

(add-to-list 'load-path (concat default-directory "elisp/package-utils"))
(provide 'async) ; hack: we're not using async code, so just stub out that library
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
