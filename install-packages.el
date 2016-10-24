
(load-file (concat default-directory "init/settings.el"))

(require 'package)
(package-initialize t)
(package-refresh-contents)

(mapc #'package-install package-selected-packages)

(package-autoremove)
