;; install-packages.el --- -*- lexical-binding: t -*-

(load-file (concat (file-name-directory load-file-name) "../init/settings.el"))

(require 'treesit) ; to silence an autoload warning, seems like emacs bug
(require 'package)
(setq package-native-compile t)
(package-initialize)
(package-refresh-contents)

(mapc #'package-install package-selected-packages)

(require 'package-utils)
(package-utils-upgrade-all-no-fetch)

(defun my-package-autoremove ()
  (interactive)
  (let ((removable (package--removable-packages)))
    (if removable
        (mapc (lambda (p)
                (package-delete (cadr (assq p (package--alist))) t))
              removable)
      (message "Nothing to autoremove"))))

(my-package-autoremove)

(require 'kind-icon)
(kind-icon-reset-cache)
(kind-icon-preview-all)

(provide 'install-packages)
;;; install-packages.el ends here
