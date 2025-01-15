(load-file (concat (file-name-directory load-file-name) "init/settings.el"))

(require 'package)
(require 'treesit)
(package-initialize)

(defun my-install-treesit-grammar (lang-name url &optional revision source-dir)
  (let ((lang-sym (intern lang-name)))
    (add-to-list 'treesit-language-source-alist
                 `(,lang-sym ,url ,revision ,source-dir))
    (message "")
    (treesit-install-language-grammar lang-sym)
    (message "")))

(apply #'my-install-treesit-grammar argv)
