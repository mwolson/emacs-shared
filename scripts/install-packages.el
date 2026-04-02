;;; install-packages.el --- -*- lexical-binding: t -*-

(require 'treesit) ; to silence an autoload warning, seems like emacs bug

(require 'package)
(package-initialize)

(require 'use-package)
(setq use-package-vc-prefer-newest t)

(advice-add 'project-remember-projects-under :override #'ignore)

(use-package vcupp
  :vc (:url "https://github.com/mwolson/vcupp")
  :demand t)

(require 'vcupp-install-packages)

(vcupp-install-packages
 `(:root ,(expand-file-name
           (concat (file-name-directory load-file-name) "../"))
   :load-files ("init/early-shared-init.el" "init/shared-init.el")
   :setup-forms ((setq my-server-start-p nil)
                 (setq my-emacs-path
                       ,(expand-file-name
                         (concat (file-name-directory load-file-name)
                                 "../"))))
   :post-install-forms
   ((vcupp-activate-package 'kind-icon)
    (require 'kind-icon)
    (kind-icon-reset-cache)
    (kind-icon-preview-all))))

(provide 'install-packages)
;;; install-packages.el ends here
