;;; native-comp-all.el --- -*- lexical-binding: t -*-

(require 'package)
(setq package-install-upgrade-built-in t)
(package-initialize)

(require 'use-package)
(setq use-package-vc-prefer-newest t)

(advice-add 'project-remember-projects-under :override #'ignore)

(use-package vcupp
  :vc (:url "https://github.com/mwolson/vcupp")
  :demand t)
(require 'vcupp-native-comp)

(vcupp-native-comp-all
 `(:root ,(expand-file-name
           (concat (file-name-directory load-file-name) "../"))
   :load-files ("init/early-shared-init.el" "init/shared-init.el")
   :setup-forms ((setq my-server-start-p nil)
                 (setq my-emacs-path
                       ,(expand-file-name
                         (concat (file-name-directory load-file-name)
                                 "../"))))
   :post-load-forms ((my-run-deferred-tasks))))

(provide 'native-comp-all)
;;; native-comp-all.el ends here
