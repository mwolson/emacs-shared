;;; install-packages.el --- -*- lexical-binding: t -*-

(require 'treesit) ; to silence an autoload warning, seems like emacs bug

(require 'package)
(setq package-user-dir (locate-user-emacs-file "elpa"))
(package-initialize)

(require 'use-package)
(setq use-package-vc-prefer-newest t)

(use-package vcupp
  :vc (:url "https://github.com/mwolson/vcupp")
  :demand t)
(require 'vcupp-install-packages)

(setq vcupp-batch-args
      `(:root ,(expand-file-name
                (concat (file-name-directory load-file-name) "../"))
        :load-files ("init/early-shared-init.el" "init/shared-init.el")
        :setup-forms ((setq my-install-packages t)
                      (setq my-native-comp-enable nil)
                      (setq my-server-start-p nil)
                      (setq my-emacs-path
                            ,(expand-file-name
                              (concat (file-name-directory load-file-name) "../"))))
        :delete-elc-globs ("init/*.elc")
        :post-load-function my-run-deferred-tasks
        :post-install-functions (kind-icon-reset-cache kind-icon-preview-all)))

(vcupp-install-packages vcupp-batch-args)

(provide 'install-packages)
;;; install-packages.el ends here
