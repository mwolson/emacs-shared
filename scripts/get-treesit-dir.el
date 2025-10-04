;; get-treesit-dir.el --- -*- lexical-binding: t -*-

(load-file (concat (file-name-directory load-file-name) "../init/settings.el"))

(require 'treesit)
(require 'package)
(package-initialize)

(defun my-get-treesit-dir ()
  (let ((out-dir (locate-user-emacs-file "tree-sitter")))
    (unless (file-exists-p out-dir)
      (make-directory out-dir t))
    (message "%s" (expand-file-name out-dir))))

(apply #'my-get-treesit-dir argv)

(provide 'get-treesit-dir)
;;; get-treesit-dir.el ends here
