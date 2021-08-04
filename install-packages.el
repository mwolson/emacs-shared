(add-to-list 'load-path (concat default-directory "elisp/libegit2") t)

(if (eq system-type 'windows-nt)
    (setq libgit--module-file (concat default-directory "elisp/libegit2/build/libegit2.dll"))
  (setq libgit--module-file (concat default-directory "elisp/libegit2/build/libegit2.so")))

(load-file (concat default-directory "init/settings.el"))

(require 'package)
(package-initialize)
(package-refresh-contents)

(mapc #'package-install package-selected-packages)

(require 'package-utils)
(package-utils-upgrade-all-no-fetch)

(if (file-exists-p libgit--module-file)
    (byte-compile-file (concat default-directory "elisp/magit-libgit.el"))
  (delete-file (concat default-directory "elisp/magit-libgit.elc")))

(defun my-package-autoremove ()
  (interactive)
  (let ((removable (package--removable-packages)))
    (if removable
        (mapc (lambda (p)
                (package-delete (cadr (assq p package-alist)) t))
              removable)
      (message "Nothing to autoremove"))))

(my-package-autoremove)
