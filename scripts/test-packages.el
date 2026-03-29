;;; test-packages.el --- -*- lexical-binding: t -*-

(setq my-native-comp-enable nil)
(setq my-server-start-p nil)
(setq my-emacs-path (expand-file-name
                     (concat (file-name-directory load-file-name) "../")))

(require 'package)
(package-initialize)

(load-file (concat my-emacs-path "init/shared-init.el"))
(my-run-deferred-tasks)

(defvar my-test-failures '())
(defvar my-test-pass-count 0)

(defun my-test-package-installed (pkg)
  (if (package-installed-p pkg)
      (progn
        (cl-incf my-test-pass-count)
        t)
    (push (format "NOT INSTALLED: %s" pkg) my-test-failures)
    nil))

(defun my-test-feature-loadable (feature)
  (condition-case err
      (progn
        (require feature)
        (cl-incf my-test-pass-count)
        t)
    (error
     (push (format "CANNOT REQUIRE: %s (%s)" feature (error-message-string err))
           my-test-failures)
     nil)))

(defun my-test-has-elc (pkg)
  (let* ((name (symbol-name pkg))
         (elpa-dir (expand-file-name "elpa" user-emacs-directory))
         (pkg-dir (or (let ((vc-dir (expand-file-name name elpa-dir)))
                        (and (file-directory-p vc-dir) vc-dir))
                      (car (directory-files elpa-dir t
                                            (format "^%s-" name))))))
    (if (and pkg-dir
             (or (directory-files pkg-dir nil "\\.elc\\'")
                 (cl-some (lambda (sub)
                            (and (file-directory-p sub)
                                 (directory-files sub nil "\\.elc\\'")))
                          (directory-files pkg-dir t "\\`[^.]"))))
        (progn
          (cl-incf my-test-pass-count)
          t)
      (push (format "NO .elc FILES: %s" pkg) my-test-failures)
      nil)))

(message "\n=== Package Migration Test ===\n")

;; GNU/NonGNU ELPA packages
(dolist (pkg '(vcl-mode))
  (my-test-package-installed pkg))

;; VC packages
(dolist (pkg '(add-node-modules-path apheleia archive-rpm astro-ts-mode
               atomic-chrome basic-mode browse-kill-ring cape cider
               clojure-mode clojure-ts-mode color-theme-sanityinc-tomorrow
               compile-angel cond-let consult corfu corfu-prescient
               corfu-terminal diff-hl diminish dumb-jump edit-indirect
               eglot-python-preset eglot-typescript-preset el-mock embark
               embark-consult fish-mode flx flymake-stylelint git-modes gptel
               gptel-fn-complete graphql-ts-mode js-comint jtsx kdl-mode
               kind-icon kotlin-ts-mode ligature llama lua-mode macrostep
               magit magit-section majutsu marginalia markdown-mode maxframe
               mermaid-ts-mode minions minuet mise modus-themes
               nerd-icons-completion nix-ts-mode nsis-mode orderless parseclj
               parseedn plz popon pulsar rainbow-delimiters reformatter rg
               sesman slime spinner svg-lib svelte-ts-mode swift-ts-mode
               terraform-mode tmux-mode toc-org transient vertico
               vertico-prescient vue-ts-mode web-mode websocket wgrep
               with-editor zig-ts-mode))
  (my-test-package-installed pkg))

;; Key features loadable
(dolist (feature '(use-package eglot magit consult vertico corfu gptel
                   dired tramp diff-hl pulsar))
  (my-test-feature-loadable feature))

;; Byte-compilation check for a few key packages
(dolist (pkg '(magit consult vertico corfu gptel))
  (my-test-has-elc pkg))

(message "\n=== Results ===")
(message "Passed: %d" my-test-pass-count)
(message "Failed: %d" (length my-test-failures))

(when my-test-failures
  (message "\nFailures:")
  (dolist (f (nreverse my-test-failures))
    (message "  - %s" f)))

(message "")

(if my-test-failures
    (progn
      (message "TESTS FAILED")
      (kill-emacs 1))
  (message "ALL TESTS PASSED")
  (kill-emacs 0))

;;; test-packages.el ends here
