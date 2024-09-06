(require 'cl-seq)

;; Tell package.el that transient is already provided as a built-in with a specific version
;; Note: May need to update this when Emacs 29.4 is updated
(with-eval-after-load "finder-inf"
  (push '(transient 0 4 3) package--builtin-versions)
  (aset (cdr (assq 'transient package--builtins)) 0 '(0 4 3)))

(eval-when-compile (require 'finder-inf))

(provide 'transient-overrides)
;;; transient-overrides.el ends here
