;;; native-comp-all.el --- -*- lexical-binding: t -*-

(require 'package)
(setq package-user-dir (locate-user-emacs-file "elpa"))
(package-initialize)

(require 'use-package)
(setq use-package-vc-prefer-newest t)

(use-package vcupp
  :vc (:url "https://github.com/mwolson/vcupp")
  :demand t)
(require 'vcupp-native-comp)

(setq vcupp-batch-args
      `(:root ,(expand-file-name
                (concat (file-name-directory load-file-name) "../"))
        :load-files ("init/early-shared-init.el" "init/shared-init.el")
        :setup-forms ((setq my-native-comp-enable nil)
                      (setq my-server-start-p nil)
                      (setq my-emacs-path
                            ,(expand-file-name
                              (concat (file-name-directory load-file-name) "../"))))
        :post-load-function my-run-deferred-tasks))

(vcupp-native-comp-all vcupp-batch-args)

(provide 'native-comp-all)
;;; native-comp-all.el ends here
