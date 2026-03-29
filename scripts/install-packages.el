;;; install-packages.el --- -*- lexical-binding: t -*-

(setq my-native-comp-enable nil)
(setq my-server-start-p nil)
(setq my-emacs-path (expand-file-name
                     (concat (file-name-directory load-file-name) "../")))

(require 'treesit) ; to silence an autoload warning, seems like emacs bug
(require 'package)
(setq my-install-packages t)
(setq package-native-compile t)
(package-initialize)
(package-refresh-contents)

(dolist (elc (file-expand-wildcards (concat my-emacs-path "init/*.elc") t))
  (delete-file elc))

(advice-add 'project-remember-projects-under :override #'ignore)
(advice-add 'yes-or-no-p :override (lambda (&rest _) t))

(load-file (concat my-emacs-path "init/shared-init.el"))
(condition-case err
    (my-run-deferred-tasks)
  (error (message "Warning during deferred tasks (OK during install): %S" err)))

;; Note: package-autoremove is not called here because :vc packages aren't
;; tracked in package-selected-packages and would be incorrectly removed.

;; Ensure all VC packages are on a tracking branch (not detached HEAD) so that
;; package-vc-upgrade-all can pull.  This is needed when packages were
;; previously installed with :last-release (which detaches HEAD at a tag).
(message "Checking VC packages for detached HEAD...")
(dolist (pkg-alist-entry package-alist)
  (dolist (pkg-desc (cdr pkg-alist-entry))
    (when (package-vc-p pkg-desc)
      (let ((pkg-dir (package-desc-dir pkg-desc)))
        (when (and pkg-dir (file-directory-p (expand-file-name ".git" pkg-dir)))
          (let ((default-directory pkg-dir))
            (unless (zerop (process-file "git" nil nil nil
                                         "symbolic-ref" "--quiet" "HEAD"))
              (let ((branch
                     (string-trim
                      (with-output-to-string
                        (with-current-buffer standard-output
                          (process-file "git" nil t nil
                                        "rev-parse" "--abbrev-ref"
                                        "origin/HEAD"))))))
                (when (string-prefix-p "origin/" branch)
                  (setq branch (substring branch 7)))
                (when (or (string= branch "") (string= branch "HEAD"))
                  (setq branch
                        (cl-loop for b in '("main" "master" "trunk")
                                 when (zerop (process-file
                                              "git" nil nil nil
                                              "rev-parse" "--verify"
                                              (concat "origin/" b)))
                                 return b)))
                (when branch
                  (message "  %s: checking out %s"
                           (package-desc-name pkg-desc) branch)
                  (process-file "git" nil nil nil
                                "checkout" "-f" branch))))))))))

(message "Upgrading VC packages to latest commits...")
(package-vc-upgrade-all)

;; Remove stale .elc files from VC packages after upgrade, since the source
;; may be newer than the byte-compiled version.
(message "Cleaning stale .elc files from VC packages...")
(dolist (pkg-alist-entry package-alist)
  (dolist (pkg-desc (cdr pkg-alist-entry))
    (when (package-vc-p pkg-desc)
      (let ((pkg-dir (package-desc-dir pkg-desc)))
        (when pkg-dir
          (dolist (elc (directory-files-recursively pkg-dir "\\.elc\\'"))
            (let ((el (concat (file-name-sans-extension elc) ".el")))
              (when (and (file-exists-p el)
                         (file-newer-than-file-p el elc))
                (delete-file elc)))))))))

(require 'kind-icon)
(kind-icon-reset-cache)
(kind-icon-preview-all)

(provide 'install-packages)
;;; install-packages.el ends here
