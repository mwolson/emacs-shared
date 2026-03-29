;;; shared-init.el --- -*- lexical-binding: t -*-
;;
;; Description: Emacs initialization settings common to multiple computers
;; Author: Michael Olson
;;
;; The contents of this file may be used, distributed, and modified
;; without restriction.

;;; Initialization

;; Early init
(unless (featurep 'early-shared-init)
  (load (concat my-emacs-path "init/early-shared-init") nil nil nil t))

(eval-and-compile
  (package-initialize))

;; Workarounds for Emacs bug#76325: package-vc has two issues with monorepos
;; that use :main-file.
(require 'package-vc nil t)
(with-eval-after-load 'package-vc
  ;; 1. package-vc--unpack saves the spec to package-vc-selected-packages
  ;;    AFTER cloning, but package-vc--release-rev needs it DURING cloning to
  ;;    resolve :main-file. Save it earlier so the spec is visible.
  (define-advice package-vc--unpack (:before (pkg-desc pkg-spec &optional _rev) save-spec-early)
    (when-let* ((name (package-desc-name pkg-desc))
                ((not (alist-get name package-vc-selected-packages nil nil #'string=))))
      (push (cons name pkg-spec) package-vc-selected-packages)))

  ;; 2. package-vc--unpack-1 scans ALL .el files for Package-Requires, even
  ;;    when :main-file is specified. This causes circular dependencies in
  ;;    monorepos like prescient.el. Restrict scanning to just the main file.
  (define-advice package-vc--unpack-1 (:around (orig-fn pkg-desc pkg-dir) main-file-deps)
    (let* ((pkg-spec (package-vc--desc->spec pkg-desc))
           (main-file (plist-get pkg-spec :main-file)))
      (if (not main-file)
          (funcall orig-fn pkg-desc pkg-dir)
        (cl-letf* ((orig-directory-files (symbol-function 'directory-files))
                   ((symbol-function 'directory-files)
                    (lambda (dir &optional full match nosort count)
                      (if (and full (equal match "\\.el\\'"))
                          (let ((mf (expand-file-name main-file dir)))
                            (if (file-exists-p mf) (list mf)
                              (funcall orig-directory-files dir full match nosort count)))
                        (funcall orig-directory-files dir full match nosort count)))))
          (funcall orig-fn pkg-desc pkg-dir)))))

  ;; 3. package-vc--clone calls project-remember-projects-under on every
  ;;    checkout, polluting the project list with elpa directories.
  (define-advice project-remember-projects-under (:around (orig-fn dir &rest args) skip-elpa)
    (unless (string-prefix-p (expand-file-name package-user-dir) (expand-file-name dir))
      (apply orig-fn dir args)))

  ;; 4. package-strip-rcs-id rejects pre-release versions like "0.3.3-DEV".
  ;;    Strip common pre-release suffixes so VC packages with such headers
  ;;    still get a usable version instead of falling back to "0".
  (define-advice package-strip-rcs-id (:around (orig-fn str) handle-pre-release)
    (or (condition-case nil (funcall orig-fn str) (error nil))
        (when str
          (condition-case nil
              (funcall orig-fn (replace-regexp-in-string
                                "-\\(?:DEV\\|SNAPSHOT\\|alpha\\|beta\\|rc\\)[^.]*\\'" "" str))
            (error nil)))))

  ;; 5. package--compile and package--native-compile-async byte-compile the
  ;;    entire package directory.  In monorepos with :main-file, this compiles
  ;;    unrelated files whose dependencies aren't installed yet.  Restrict to
  ;;    just the main file in that case.
  (define-advice package--compile (:around (orig-fn pkg-desc) main-file-only)
    (let* ((pkg-spec (and (package-vc-p pkg-desc)
                          (package-vc--desc->spec pkg-desc)))
           (main-file (and pkg-spec (plist-get pkg-spec :main-file))))
      (if (not main-file)
          (funcall orig-fn pkg-desc)
        (let* ((lisp-dir (plist-get pkg-spec :lisp-dir))
               (dir (package-desc-dir pkg-desc))
               (full-dir (if lisp-dir (expand-file-name lisp-dir dir) dir))
               (main-path (expand-file-name main-file full-dir)))
          (when (file-exists-p main-path)
            (let ((warning-minimum-level :error))
              (byte-compile-file main-path)))))))

  (define-advice package--native-compile-async (:around (orig-fn pkg-desc) main-file-only)
    (when (native-comp-available-p)
      (let* ((pkg-spec (and (package-vc-p pkg-desc)
                            (package-vc--desc->spec pkg-desc)))
             (main-file (and pkg-spec (plist-get pkg-spec :main-file))))
        (if (not main-file)
            (funcall orig-fn pkg-desc)
          (let* ((lisp-dir (plist-get pkg-spec :lisp-dir))
                 (dir (package-desc-dir pkg-desc))
                 (full-dir (if lisp-dir (expand-file-name lisp-dir dir) dir))
                 (main-path (expand-file-name main-file full-dir)))
            (when (file-exists-p main-path)
              (let ((warning-minimum-level :error))
                (native-compile-async main-path)))))))))

(eval-and-compile
  (require 'use-package))

;; Add shared elisp directory (but prefer system libs)
(eval-and-compile
  (add-to-list 'load-path (concat my-emacs-path "elisp") t))

;; Load settings at compile time so package customizations are available
(eval-when-compile
  (load (concat my-emacs-path "init/settings") nil t nil t))

;; Display initial screen quickly
(my-init-client-display)
(pop-to-buffer-same-window (scratch-buffer))

;; Tasks that are run after initial startup for appearance of speed
(defvar my-deferred-startup-hook '(display-startup-echo-area-message))

(defun my-defer-startup (func &optional depth)
  "Defer running a task until sometime after Emacs has started.

When `depth' is provided, pass it to `add-hook'."
  (add-hook 'my-deferred-startup-hook func depth))

(defun my-run-deferred-tasks ()
  (unless (eq system-type 'windows-nt)
    (run-hooks 'my-deferred-startup-hook))
  (setq my-deferred-startup-hook nil))

(run-with-idle-timer 0.2 nil #'my-run-deferred-tasks)

;; Garbage collection settings
(defun my-restore-gc-settings ()
  (setq gc-cons-threshold  (* 67108864 2)) ; 128MB
  (setq gc-cons-percentage 0.1)
  (garbage-collect))

(my-defer-startup #'my-restore-gc-settings t)

;;; OS Setup

(setq source-directory "~/emacs-shared/extra/emacs")
(setq find-function-C-source-directory
      (expand-file-name "src" source-directory))

;; Make it easier to use find-library to get to this file
(add-to-list 'load-path (concat my-emacs-path "init"))

;; Install additional system paths
(defvar my-original-exec-path exec-path)
(defvar my-original-env-path (getenv "PATH"))

(defun my-update-system-paths (&optional extra-paths)
  (setq exec-path (append extra-paths my-system-paths my-original-exec-path))
  (setenv "PATH" (mapconcat (lambda (path)
                              (if (eq system-type 'windows-nt)
                                  (replace-regexp-in-string "/" "\\\\" path)
                                path))
                            (append extra-paths my-system-paths
                                    (list my-original-env-path))
                            (if (eq system-type 'windows-nt) ";" ":"))))

(my-update-system-paths)

;; Setup mise, a version manager for node.js and other software
(defun my-mise-exclude ()
  "Return non-nil if current buffer should not obey `global-mise-mode'."
  (let* ((filename (buffer-file-name)))
    (or (not filename)
        (when-let* ((lst my-mise-exclude-file-regexps))
          (string-match-p
           (string-join (mapcar (lambda (x)
                                  (concat "\\(?:" x "\\)")) lst) "\\|")
           filename))
        (mise-default-exclude))))

(use-package inheritenv
  :vc (:url "https://github.com/purcell/inheritenv")
  :defer t)

(use-package mise
  :vc (:url "https://github.com/eki3z/mise.el")
  :defer t
  :config
  (setopt mise-exclude-predicate #'my-mise-exclude
          mise-trust t))

(my-defer-startup #'global-mise-mode)

;; Transitive dependencies -- declared here so package-vc fetches latest
;; commits during bootstrap instead of using ELPA release versions.
;; Ordered by dependency chain: packages that are deps of later entries
;; must come first, otherwise package.el installs the ELPA version.
(use-package clojure-mode
  :vc (:url "https://github.com/clojure-emacs/clojure-mode"
       :main-file "clojure-mode.el")
  :defer t)
(use-package cond-let
  :vc (:url "https://github.com/tarsius/cond-let")
  :defer t)
(use-package lv
  :vc (:url "https://github.com/abo-abo/hydra"
       :main-file "lv.el")
  :defer t)
(use-package llama
  :vc (:url "https://github.com/tarsius/llama")
  :defer t)
(use-package macrostep
  :vc (:url "https://github.com/emacsorphanage/macrostep")
  :defer t)
(use-package magit-section
  :vc (:url "https://github.com/magit/magit"
       :main-file "magit-section.el" :lisp-dir "lisp")
  :defer t)
(use-package parseclj
  :vc (:url "https://github.com/clojure-emacs/parseclj")
  :defer t)
(use-package parseedn
  :vc (:url "https://github.com/clojure-emacs/parseedn")
  :defer t)
(use-package plz
  :vc (:url "https://github.com/alphapapa/plz.el")
  :defer t)
(use-package popon
  :vc (:url "https://codeberg.org/akib/emacs-popon")
  :defer t)
(use-package sesman
  :vc (:url "https://github.com/vspinu/sesman")
  :defer t)
(use-package spinner
  :vc (:url "https://github.com/Malabarba/spinner.el")
  :defer t)
(use-package svg-lib
  :vc (:url "https://github.com/rougier/svg-lib")
  :defer t)
(use-package transient
  :vc (:url "https://github.com/magit/transient")
  :defer t)
(use-package websocket
  :vc (:url "https://github.com/ahyatt/emacs-websocket")
  :defer t)
(use-package with-editor
  :vc (:url "https://github.com/magit/with-editor")
  :defer t)
;; gptel depends on transient + plz; magit depends on transient + with-editor
(use-package gptel
  :vc (:url "https://github.com/karthink/gptel")
  :defer t)
(use-package magit
  :vc (:url "https://github.com/magit/magit")
  :defer t)

;; Setup manpage browsing
(cond ((eq system-type 'darwin)
       (setenv "INFOPATH" (concat "/opt/homebrew/share/info:"
                                  (or (getenv "INFOPATH") "")))
       (setenv "MANPATH" (concat "/opt/homebrew/share/man:"
                                 (or (getenv "MANPATH") ""))))
      ((eq system-type 'windows-nt)
       (setenv "MANPATH" (concat (expand-file-name "~/emacs-shared/share/man") ";"
                                 "C:\\msys64\\usr\\share\\man;"
                                 "C:\\msys64\\ucrt64\\share\\man;"
                                 "C:\\Program Files\\Emacs\\emacs-" emacs-version "\\share\\man"))
       (require 'woman)
       (defalias 'man #'woman)))

;;; Customizations

;; Load customizations
(setq custom-file (if my-settings-shared-p
                      (concat my-emacs-path "init/settings.el")
                    (locate-user-emacs-file "settings.el")))
(load custom-file nil nil nil t)

;;; Functions

(defmacro my-match-data-changed (&rest body)
  "Determine whether the match data has been modified by BODY."
  (let ((mdata (make-symbol "temp-buffer")))
    `(let ((,mdata (match-data)))
       (prog1 ,@body
         (if (equal ,mdata (match-data))
             (message "Match data has not been changed")
           (message "Match data has been changed!"))))))

(put 'my-match-data-changed 'lisp-indent-function 0)
(put 'my-match-data-changed 'edebug-form-spec '(body))

(defun my-byte-compile-this-file-temporarily ()
  (interactive)
  (let ((file buffer-file-name))
    (byte-compile-file file)
    (save-match-data
      (when (string-match "\\.el\\'" file)
        (delete-file (concat file "c"))))))

(defun my-byte-compile-this-file ()
  (interactive)
  (let ((file buffer-file-name))
    (byte-compile-file file)))

(defun my-fetch-url (url)
  "Fetch the given URL into a buffer and switch to it."
  (interactive (list (read-string "URL: ")))
  (require 'url-handlers)
  (let ((outer (generate-new-buffer (format "*URL: %s*"
                                            (substring url 0 40)))))
    (message "Fetching URL ...")
    (url-retrieve
     url
     `(lambda (status)
        (let ((results (current-buffer))
              size-and-charset)
          (with-current-buffer ,outer
            (setq success (url-insert results))
            (kill-buffer results)
            (unless (cadr size-and-charset)
              (decode-coding-inserted-region
               (point-min) (point-max) (buffer-name ,outer)))
            (goto-char (point-min)))
          (switch-to-buffer ,outer))))))

(defun my-replace-cdrs-in-alist (old-el new-el alist)
  "Replace cdr instances of OLD-EL with NEW-EL in ALIST."
  (mapc #'(lambda (el)
            (when (eq (cdr el) old-el)
              (setcdr el new-el)))
        (symbol-value alist)))

;;; Base Programs and Features

;; Enable some commands
(put 'downcase-region 'disabled nil)
(put 'scroll-left 'disabled nil)
(put 'upcase-region 'disabled nil)

;; Personal info
(defun my-update-personal-info ()
  (interactive)
  ;; full name
  (setq debian-changelog-full-name my-full-name)
  (setopt user-full-name my-full-name)
  ;; changelog email addresses
  (when (featurep 'add-log)
    (setopt add-log-mailing-address my-changelog-address))
  (setq debian-changelog-mailing-address my-changelog-address)
  ;; email addresses
  (setq post-email-address my-email-address)
  (setopt user-mail-address my-email-address))
(my-update-personal-info)

(use-package add-log
  :ensure nil
  :defer t
  :custom
  (add-log-keep-changes-together t)
  :config
  (setopt add-log-mailing-address my-changelog-address))

;; Load `dired' itself, with `tramp' extension
(use-package dired
  :ensure nil
  :demand t
  :custom
  (dired-dwim-target t)
  (dired-recursive-copies 'always)
  (dired-recursive-deletes 'always))

(use-package dired-x
  :ensure nil
  :demand t
  :after dired)

;; Enable wdired on "r"
(use-package wdired
  :ensure nil
  :demand t
  :after dired
  :bind (:map dired-mode-map ("r" . wdired-change-to-wdired-mode)))

(use-package ffap
  :ensure nil
  :demand t)

;; gptel-fn-complete: complete current function with AI
;;
;; for local development and testing, replace the use-package block with:
;; (add-to-list 'load-path (expand-file-name "~/devel/projects/gptel-fn-complete"))
;; (require 'gptel-fn-complete)
(use-package gptel-fn-complete
  :vc (:url "https://github.com/mwolson/gptel-fn-complete")
  :commands (gptel-fn-complete--mark-function-treesit)
  :defer t)

;; Load tramp
(use-package tramp
  :ensure nil
  :demand t
  :custom
  (tramp-auto-save-directory "~/.emacs.d/.autosave.d")
  (tramp-backup-directory-alist '(("." . "~/.emacs.d/backup"))))

;; List directories first in dired
(use-package ls-lisp
  :ensure nil
  :demand t
  :custom
  (ls-lisp-dirs-first t)
  (ls-lisp-ignore-case t)
  (ls-lisp-support-shell-wildcards nil)
  (ls-lisp-use-insert-directory-program nil)
  (ls-lisp-verbosity '(uid gid)))

(use-package cperl-mode
  :ensure nil
  :defer t
  :custom
  (cperl-close-paren-offset -4)
  (cperl-indent-level 4)
  (cperl-indent-parens-as-block t)
  (cperl-merge-trailing-else nil))

(use-package css-mode
  :ensure nil
  :defer t
  :custom
  (css-indent-offset 2))

(use-package eldoc
  :ensure nil
  :defer t
  :custom
  (eldoc-documentation-strategy 'eldoc-documentation-compose)
  (eldoc-echo-area-use-multiline-p nil))

(use-package sql
  :ensure nil
  :defer t
  :custom
  (sql-product 'postgres))

(use-package woman
  :ensure nil
  :defer t
  :custom
  (woman-fill-column 95)
  (woman-fontify t)
  (woman-use-own-frame nil)
  :custom-face
  (woman-italic ((t (:underline t :slant italic)))))

;; Long lines support
(global-so-long-mode 1)
(add-to-list 'so-long-target-modes 'fundamental-mode)

;; Don't slow down ls and don't make dired output too wide on w32 systems
(setq w32-get-true-file-attributes nil)

;; Make shell commands run in unique buffer so we can have multiple at once, and run all shell
;; asynchronously.  Taken in part from EmacsWiki: ExecuteExternalCommand page.

(defvar my-erase-buffer-noop nil
  "When non-nil, `erase-buffer' becomes a no-op.")

(define-advice erase-buffer (:around (orig-fn) my-noop)
  "Make erase-buffer do nothing when `my-erase-buffer-noop' is non-nil."
  (unless my-erase-buffer-noop
    (funcall orig-fn)))

(define-advice shell-command (:around (orig-fn command &optional output-buffer error-buffer) my-unique-buffer)
  "Run shell commands in unique buffers asynchronously."
  (if (or current-prefix-arg output-buffer)
      ;; if this is used programmatically, allow it to be synchronous
      (funcall orig-fn command output-buffer error-buffer)

    (save-match-data
      (let ((lisp-exp (and (string-match "\\`[[:blank:]]*([^&|]+)[[:blank:]]*\\'" command)
                           (condition-case nil (read command) (error nil)))))
        ;; if we accidentally entered a readable lisp expression, eval it
        (if lisp-exp
            (eval-expression lisp-exp nil)

          (unless (string-match "&[ \t]*\\'" command)
            (setq command (concat command " &")))

          ;; set match data for buffer name
          (string-match "[ \t]*&[ \t]*\\'" command)

          (let* ((command-name (substring command 0 (min 40 (match-beginning 0))))
                 (command-name (car (split-string command-name "\n" t)))
                 (command-buffer-name (format "*Shell Command: %s*" command-name))
                 (command-buffer (get-buffer command-buffer-name)))

            ;; if the buffer exists and has a live process, rename it uniquely
            (if (and command-buffer (get-buffer-process command-buffer))
                (with-current-buffer command-buffer
                  (rename-uniquely))
              (when (buffer-live-p command-buffer)
                (kill-buffer command-buffer)))
            (setq output-buffer command-buffer-name)

            ;; insert command at top of buffer
            (switch-to-buffer-other-window output-buffer)
            (insert "Running command: " command "\n"
                    (make-string (- (window-width) 1) ?\~)
                    "\n\n")

            ;; temporarily disable erase-buffer while doing it, to avoid erasing the above
            (let ((my-erase-buffer-noop t)
                  (process-environment (cons "PAGER=" process-environment)))
              (funcall orig-fn command output-buffer error-buffer))))))))

;; Docker support
(defun my-docker-machine-env ()
  (interactive)
  (let* ((machine "default")
         (out (if (file-exists-p "~/.docker-env")
                  (with-temp-buffer
                    (insert-file-contents-literally "~/.docker-env")
                    (buffer-substring (point-min) (point-max)))
                (shell-command-to-string (concat "docker-machine env " machine))))
         (changes 0))
    (save-match-data
      (dolist (line (split-string out "\n" t))
        (when (string-match "\\`export \\([^=]+\\)=\"\\(.+\\)\"\\'" line)
          (let ((env-var (match-string 1 line))
                (env-setting (match-string 2 line)))
            (cl-incf changes)
            (setenv env-var env-setting)))))
    (if (= changes 0)
        (message "Could not load docker changes, output:\n%s" out)
      (message "Loaded Docker env for machine: %s" machine))))

(add-to-list 'auto-mode-alist
             '("\\(?:Dockerfile\\(?:\\..*\\)?\\|\\.[Dd]ockerfile\\)\\'"
               . dockerfile-ts-mode))

(add-to-list 'auto-mode-alist '("/.dockerignore\\'" . gitignore-mode))

;; Support for s6-overlay containers: https://github.com/just-containers/s6-overlay
(setq auto-mode-interpreter-regexp
      (replace-regexp-in-string "/bin/env" "/\\(?:usr/\\)?bin/\\(?:with-cont\\)?env"
                                auto-mode-interpreter-regexp t t))

;;; Programming Modes and Features

(defvar my-md-code-aliases '())

(defun my-around-advice (fun advice)
  "Apply around ADVICE to FUN.
If FUN is a list, apply ADVICE to each element of it."
  (cond ((listp fun)
         (dolist (el fun) (my-around-advice el advice)))
        ((and (symbolp fun)
              (not (advice-member-p advice fun)))
         (advice-add fun :around advice))))

(defun my-in-indirect-md-buffer-p ()
  "Return non-nil if the current buffer is an indirect buffer created from a markdown buffer."
  (when-let* ((buf (buffer-base-buffer))
              ((buffer-live-p buf)))
    (with-current-buffer buf
      (derived-mode-p 'markdown-mode))))

(defun my-inhibit-in-indirect-md-buffers (orig-fun &rest args)
  "Don't run ORIG-FUN (with ARGS) in indirect markdown buffers.
Use this to advise functions that could be problematic."
  (unless (my-in-indirect-md-buffer-p)
    (apply orig-fun args)))

;; Apheleia for automatic code formatting
(defun my-detect-biome ()
  "Detect whether to use Biome based on project configuration.

Returns the config filename if one is found, `t' if found in package.json"
  (let ((root (or (my-project-root) default-directory)))
    (or (file-exists-p (expand-file-name "biome.json" root))
        (file-exists-p (expand-file-name "biome.jsonc" root))
        (and (file-exists-p (expand-file-name "package.json" root))
             (with-temp-buffer
               (insert-file-contents (expand-file-name "package.json" root))
               (re-search-forward "\"biome\"" nil t))
             t))))

(defun my-detect-eslint ()
  "Detect whether to use ESLint based on project configuration."
  (let ((root (or (my-project-root) default-directory)))
    (or (file-exists-p (expand-file-name "eslint.config.js" root))
        (file-exists-p (expand-file-name "eslint.config.mjs" root))
        (file-exists-p (expand-file-name "eslint.config.cjs" root))
        (file-exists-p (expand-file-name ".eslintrc.js" root))
        (file-exists-p (expand-file-name ".eslintrc.cjs" root))
        (file-exists-p (expand-file-name ".eslintrc.json" root))
        (file-exists-p (expand-file-name ".eslintrc" root))
        (and (file-exists-p (expand-file-name "package.json" root))
             (with-temp-buffer
               (insert-file-contents (expand-file-name "package.json" root))
               (re-search-forward "\"eslintConfig\"" nil t))
             t))))

(defun my-detect-oxfmt ()
  "Detect whether to use Oxfmt based on project configuration.

Returns the config filename if one is found, `t' if found in package.json"
  (let ((root (or (my-project-root) default-directory)))
    (or (file-exists-p (expand-file-name ".oxfmtrc.json" root))
        (file-exists-p (expand-file-name ".oxfmtrc.jsonc" root))
        (and (file-exists-p (expand-file-name "package.json" root))
             (with-temp-buffer
               (insert-file-contents (expand-file-name "package.json" root))
               (re-search-forward "\"oxfmt\"" nil t))
             t))))

(defun my-detect-oxlint ()
  "Detect whether to use Oxlint based on project configuration.

Returns the config filename if one is found, `t' if found in package.json"
  (let ((root (or (my-project-root) default-directory)))
    (or (file-exists-p (expand-file-name ".oxlintrc.json" root))
        (and (file-exists-p (expand-file-name "package.json" root))
             (with-temp-buffer
               (insert-file-contents (expand-file-name "package.json" root))
               (re-search-forward "\"oxlint\"" nil t))
             t))))

(use-package apheleia
  :vc (:url "https://github.com/radian-software/apheleia")
  :defer t
  :config
  ;; Note: oxfmt does not support stdin/stdout formatting. Apheleia's `inplace'
  ;; integration uses a temp file with the correct extension.
  (setf (alist-get 'oxfmt apheleia-formatters)
        '("apheleia-npx" "oxfmt" inplace)))

(defun my-detect-prettier ()
  "Detect whether to use Prettier based on project configuration.

Returns the config filename if one is found, `t' if found in package.json"
  (let ((root (or (my-project-root) default-directory)))
    (or (file-exists-p (expand-file-name ".prettierrc.js" root))
        (file-exists-p (expand-file-name ".prettierrc.json" root))
        (file-exists-p (expand-file-name ".prettierrc" root))
        (file-exists-p (expand-file-name ".prettierrc.yaml" root))
        (file-exists-p (expand-file-name ".prettierrc.yml" root))
        (and (file-exists-p (expand-file-name "package.json" root))
             (with-temp-buffer
               (insert-file-contents (expand-file-name "package.json" root))
               (re-search-forward "\"prettier\"" nil t))
             t))))

(defun my-detect-js-formatter ()
  "Detect whether to use Oxfmt, Biome, or Prettier based on project configuration."
  (cond
   ((my-detect-oxfmt) 'oxfmt)
   ((my-detect-biome) 'biome)
   ((my-detect-prettier) 'prettier)
   (t nil)))

(defun my-detect-markdown-formatter ()
  "Detect whether to use Prettier based on project configuration."
  (cond
   ((my-detect-prettier) 'prettier)
   (t nil)))

(defun my-detect-yaml-formatter ()
  "Detect whether to use Prettier based on project configuration."
  (cond
   ((my-detect-prettier) 'prettier)
   (t nil)))

(defun my-apheleia-set-js-formatter ()
  "Set apheleia JS formatter based on project configuration."
  (setq-local apheleia-formatter (my-detect-js-formatter)))

(defun my-apheleia-set-markdown-formatter ()
  "Set apheleia markdown formatter based on project configuration."
  (setq-local apheleia-formatter (my-detect-markdown-formatter)))

(defun my-apheleia-set-yaml-formatter ()
  "Set apheleia yaml formatter based on project configuration."
  (setq-local apheleia-formatter (my-detect-yaml-formatter)))

(apheleia-global-mode 1)

;; Atomic Chrome: Edit Server support for launching Emacs from browsers
(use-package atomic-chrome
  :vc (:url "https://github.com/alpha22jp/atomic-chrome")
  :defer t
  :init
  (when my-server-start-p
    (my-defer-startup #'atomic-chrome-start-server)))

;; Compile buffers
(use-package compile
  :ensure nil
  :defer t
  :hook (compilation-filter . ansi-color-compilation-filter)
  :custom
  (compilation-scroll-output 'first-error)
  :config
  (keymap-set compilation-mode-map "M-g" #'recompile)
  (keymap-set compilation-shell-minor-mode-map "M-g" #'recompile))

;; Editorconfig support
(use-package editorconfig
  :ensure nil
  :demand t
  :config
  (put 'editorconfig-lisp-use-default-indent 'safe-local-variable #'always)
  (my-around-advice #'editorconfig-major-mode-hook
                    #'my-inhibit-in-indirect-md-buffers)
  (editorconfig-mode 1))

;; Set up eglot for LSP features
;; to debug eglot:
;; (setq my-debug-jsonrpc t)
(defvar my-debug-jsonrpc nil
  "Whether to enable log messages for jsonrpc.")

(defun my-jsonrpc--log-event-real (&rest _args)
  "Placeholder for `jsonrpc--log-event'."
  nil)

(defun my-jsonrpc--log-event (&rest args)
  "Control whether jsonrpc events are logged."
  (when my-debug-jsonrpc
    (apply #'my-jsonrpc--log-event-real args)))

(use-package eglot
  :ensure nil
  :demand t
  :custom
  (eglot-extend-to-xref t)
  (eglot-send-changes-idle-time 0.2)
  (eglot-sync-connect nil)
  :config
  (setq eglot-diagnostics-map
        (let ((map (make-sparse-keymap)))
          (keymap-set map "<mouse-3>" #'eglot-code-actions-at-mouse)
          map))

  (my-around-advice #'eglot-ensure #'my-inhibit-in-indirect-md-buffers)
  (keymap-set eglot-mode-map "<f2>" #'eglot-rename)
  (fset #'my-jsonrpc--log-event-real (symbol-function 'jsonrpc--log-event))
  (fset #'jsonrpc--log-event #'my-jsonrpc--log-event))

;; Flymake
(defvar my-flymake-mode-map
  (let ((map (make-sparse-keymap)))
    (keymap-set map "c" #'flymake-start)
    (keymap-set map "l" #'flymake-show-buffer-diagnostics)
    (keymap-set map "L" #'flymake-switch-to-log-buffer)
    (keymap-set map "n" #'flymake-goto-next-error)
    (keymap-set map "p" #'flymake-goto-prev-error)
    map)
  "My key customizations for flymake.")

(use-package flymake
  :ensure nil
  :defer t
  :config
  (keymap-set flymake-mode-map "C-c f" my-flymake-mode-map)
  (keymap-set flymake-mode-map "C-x f" my-flymake-mode-map))

;; NodeJS REPL
(use-package js-comint
  :vc (:url "https://github.com/redguardtoo/js-comint")
  :commands (js-comint-send-string)
  :defer t)
(defun my-js-comint-send-defun (start end)
  "Send the function at point to the inferior Javascript process."
  (interactive "r")
  (unless (region-active-p)
    (require 'gptel-fn-complete)
    (save-mark-and-excursion
      (gptel-fn-complete--mark-function-treesit)
      (setq start (point)
            end (mark))))
  (let ((text (buffer-substring-no-properties start end)))
    (js-comint-start-or-switch-to-repl)
    (js-comint-send-string text)))

(defun my-js-comint-send-last-sexp ()
  "Send the previous sexp to the inferior Javascript process."
  (interactive)
  (let* ((b (save-excursion
              (backward-sexp)
              (move-beginning-of-line nil)
              (point)))
         (e (point))
         (str (buffer-substring-no-properties b e)))
    (js-comint-start-or-switch-to-repl)
    (js-comint-send-string str)))

(defun my-js-comint-send-line ()
  "Send the buffer to the inferior Javascript process."
  (interactive)
  (let ((text (buffer-substring-no-properties (line-beginning-position)
                                              (line-end-position))))
    (js-comint-start-or-switch-to-repl)
    (js-comint-send-string text)))

(defun my-js-comint-send-region (start end)
  "Send the region to the inferior Javascript process."
  (interactive "r")
  (let ((text (buffer-substring-no-properties start end)))
    (js-comint-start-or-switch-to-repl)
    (js-comint-send-string text)))

(defvar node-repl-interaction-mode-map
  (let ((map (make-sparse-keymap)))
    (keymap-set map "C-x C-e" 'my-js-comint-send-last-sexp)
    (keymap-set map "C-c C-l" 'my-js-comint-send-line)
    (keymap-set map "C-c C-r" 'my-js-comint-send-region)
    (keymap-set map "C-c C-z" 'js-comint-start-or-switch-to-repl)
    (keymap-set map "C-M-x" 'my-js-comint-send-defun)
    (keymap-set map "C-s-x" 'my-js-comint-send-defun)
    map)
  "Keymap for node-repl-interaction-mode.")

(define-minor-mode node-repl-interaction-mode
  "Minor mode to interact with a NodeJS REPL in combination with the `nodejs-repl' package.

When called interactively, toggle `node-repl-interaction-mode'.
With prefix ARG, enable `node-repl-interaction-mode' if ARG is
positive, otherwise disable it.

When called from Lisp, enable `node-repl-interaction-mode' if ARG
is omitted, nil or positive.  If ARG is `toggle', toggle
`node-repl-interaction-mode'.  Otherwise behave as if called
interactively.

\\{node-repl-interaction-mode-map}"
  :keymap node-repl-interaction-mode-map
  (cond
   (node-repl-interaction-mode
    (require 'js-comint))
   (t nil)))

(defun my-node-repl-setup ()
  (node-repl-interaction-mode 1))

(my-around-advice #'my-node-repl-setup #'my-inhibit-in-indirect-md-buffers)

;; Highlight current line
(use-package hl-line-plus
  :ensure nil
  :demand t
  :config
  (hl-line-when-idle-interval 0.3)
  (toggle-hl-line-when-idle 1))

;; Highlight changed lines
(use-package diff-hl
  :vc (:url "https://github.com/dgutov/diff-hl")
  :defer t
  :hook ((dired-mode . diff-hl-dired-mode)
         (prog-mode . turn-on-diff-hl-mode)
         (magit-post-refresh . diff-hl-magit-post-refresh))
  :config
  (diff-hl-margin-mode 1)

  (dolist (el diff-hl-margin-symbols-alist)
    (setcdr el " "))

  (setopt diff-hl-draw-borders nil
          diff-hl-margin-symbols-alist diff-hl-margin-symbols-alist
          diff-hl-update-async t)
  (my-around-advice #'turn-on-diff-hl-mode #'my-inhibit-in-indirect-md-buffers))

(defface my-pulsar-face
  '((default :extend t)
    (t :inherit xref-match))
  "Face for pulsar."
  :group 'pulsar-faces)

;; Pulsar, for highlighting current line after a jump-style keybind
(use-package pulsar
  :vc (:url "https://github.com/protesilaos/pulsar")
  :defer t
  :custom
  (pulsar-delay 0.03)
  (pulsar-face 'my-pulsar-face)
  :config
  ;; Use setq instead of setopt: pulsar's defcustom type is (repeat function),
  ;; but its default includes symbols for packages not always loaded (evil, logos).
  ;; The type check fails for these, causing a spurious warning.
  (setq pulsar-pulse-functions
        (append '(diff-hl-next-hunk
                  diff-hl-previous-hunk
                  flymake-goto-next-error
                  flymake-goto-prev-error)
                (cl-set-difference pulsar-pulse-functions
                                   '(scroll-down-command scroll-up-command))))

  (with-eval-after-load "consult"
    (add-hook 'consult-after-jump-hook #'pulsar-recenter-top)
    (add-hook 'consult-after-jump-hook #'pulsar-reveal-entry)))

(my-defer-startup #'pulsar-global-mode)

;; SMerge mode, for editing files with inline diffs
(add-hook 'prog-mode-hook #'smerge-mode t)
(my-around-advice #'smerge-mode #'my-inhibit-in-indirect-md-buffers)

;; Transient
(use-package transient
  :defer t
  :config
  (transient-bind-q-to-quit))

;; Tree-sitter
(use-package treesit
  :ensure nil
  :defer t
  :custom
  (treesit-font-lock-level 4))

;; Set up gptel
(defvar my-gptel--backends-defined nil)
(defvar my-gptel--claude nil)
(defvar my-gptel--claude-thinking nil)
(defvar my-gptel--gemini nil)
(defvar my-gptel--gemini-lite nil)
(defvar my-gptel--groq nil)
(defvar my-gptel--local-ai nil)
(defvar my-gptel--mistral nil)
(defvar my-gptel--openai nil)
(defvar my-gptel--opencode-zen nil)
(defvar my-gptel--openrouter-kimi-k2 nil)
(defvar my-gptel--xai nil)
(defvar my-gptel-local-models
  '((Qwen3.5-35B-A3B-UD-Q8_K_XL
     :description "Local default: Qwen3.5-35B-A3B-UD-Q8_K_XL"
     :capabilities (media tool json url)
     :context-window 262144
     :request-params
     (:temperature 0.6
      :top_k 20
      :top_p 0.95
      :min_p 0.0
      :presence_penalty 0.0
      :frequency_penalty 0.0
      :repetition_penalty 1.0))
    (Qwen3.5-122B-A10B-UD-Q5_K_XL
     :description "Local alternate: Qwen3.5-122B-A10B-UD-Q5_K_XL"
     :capabilities (media tool json url)
     :context-window 262144
     :request-params
     (:temperature 0.6
      :top_k 20
      :top_p 0.95
      :min_p 0.0
      :presence_penalty 0.0
      :frequency_penalty 0.0
      :repetition_penalty 1.0))))

(defun my-auth-source-get-api-key (host &optional user)
  (if-let* ((secret (plist-get
                     (car (auth-source-search
                           :host host
                           :user (or user "apikey")
                           :require '(:secret)))
                     :secret)))
      (if (functionp secret)
          (encode-coding-string (funcall secret) 'utf-8)
        secret)
    (user-error "No `apikey' found in the auth source")))

(defvar my-gptel-ensure-backends-hook '()
  "Additional functions to call when running `my-gptel-ensure-backends'.")

(eval-when-compile
  (require 'gptel nil t))
(defun my-gptel-ensure-backends ()
  (unless my-gptel--backends-defined
    (setq my-gptel--backends-defined t)

    (require 'gptel-anthropic)
    (setq my-gptel--claude
          (gptel-make-anthropic "Claude"
            :stream t
            :key #'gptel-api-key-from-auth-source))

    (setq my-gptel--claude-thinking
          (gptel-make-anthropic "Claude-Thinking"
            :stream t
            :key #'gptel-api-key-from-auth-source
            :header (lambda () (when-let* ((key (gptel--get-api-key)))
                                 `(("x-api-key" . ,key)
                                   ("anthropic-version" . "2023-06-01")
                                   ("anthropic-beta" . "pdfs-2024-09-25")
                                   ("anthropic-beta" . "output-128k-2025-02-19")
                                   ("anthropic-beta" . "prompt-caching-2024-07-31"))))
            :request-params
            `(:thinking
              (:type "enabled"
               :budget_tokens ,my-gptel-claude-thinking-budget)
              :temperature 1
              :max_tokens 4096)))

    (require 'gptel-gemini)
    (unless (alist-get 'gemini-2.5-pro gptel--gemini-models)
      (setf (alist-get 'gemini-2.5-pro gptel--gemini-models)
            '(gemini-2.5-pro
              :description "Most powerful Gemini thinking model with maximum response accuracy and state-of-the-art performance"
              :capabilities (tool-use json media)
              :mime-types
              ("image/png" "image/jpeg" "image/webp" "image/heic" "image/heif"
               "application/pdf" "text/plain" "text/csv" "text/html")
              :context-window 1048 ; 65536 output token limit
              :input-cost 1.25 ; 2.50 for >200k tokens
              :output-cost 10.00 ; 15 for >200k tokens
              :cutoff-date "2025-01"))
      (setopt gptel--gemini-models gptel--gemini-models))

    (unless (alist-get 'gemini-2.5-flash gptel--gemini-models)
      (setf (alist-get 'gemini-2.5-flash gptel--gemini-models)
            `(:description "Best Gemini model in terms of price-performance, offering well-rounded capabilities"
              :capabilities (tool-use json media)
              :mime-types
              ("image/png" "image/jpeg" "image/webp" "image/heic" "image/heif"
               "application/pdf" "text/plain" "text/csv" "text/html")
              :context-window 1000
              :input-cost 0.15
              :output-cost 0.60 ; 3.50 for thinking
              :cutoff-date "2025-04"))
      (setopt gptel--gemini-models gptel--gemini-models))

    (setq my-gptel--gemini
          (gptel-make-gemini "Gemini"
            :stream t
            :key #'gptel-api-key-from-auth-source
            ;; don't let gptel set temperature
            :request-params '(:generationConfig)))

    (setq my-gptel--gemini-lite
          (gptel-make-gemini "Gemini (Lite Thinking)"
            :stream t
            :key #'gptel-api-key-from-auth-source
            :request-params
            `(:generationConfig
              (:thinkingConfig
               (:thinkingBudget ,my-gptel-gemini-lite-thinking-budget)))))

    (setq my-gptel--groq
          (gptel-make-openai "Groq"
            :host "api.groq.com"
            :endpoint "/openai/v1/chat/completions"
            :stream t
            :key #'gptel-api-key-from-auth-source
            :models '(deepseek-r1-distill-llama-70b
                      llama-3.3-70b-versatile)))

    (setq my-gptel--local-ai
          (gptel-make-openai "Local AI Server"
            :host "localhost:1337"
            :protocol "http"
            :stream t
            :models my-gptel-local-models))

    (setq my-gptel--mistral
          (gptel-make-openai "Mistral"
            :host "api.mistral.ai"
            :stream t
            :key #'gptel-api-key-from-auth-source
            :models '((codestral-latest
                       :description "Official codestral Mistral AI model"
                       :capabilities (tool json url)
                       :context-window 256)
                      (open-codestral-mamba
                       :description "Official codestral-mamba Mistral AI model"
                       :capabilities (tool json url)
                       :context-window 256)
                      (open-mistral-nemo
                       :description "Official open-mistral-nemo Mistral AI model"
                       :capabilities (tool json url)
                       :context-window 131))))

    (require 'gptel-openai)
    (unless (alist-get 'gpt-5.2 gptel--openai-models)
      (setf (alist-get 'gpt-5.2 gptel--openai-models)
            '(gpt-5.2
              :description "Flagship model for coding, reasoning, and agentic tasks across domains"
              :capabilities (media tool-use json url)
              :mime-types ("image/jpeg" "image/png" "image/gif" "image/webp")
              :context-window 400
              :input-cost 1.75
              :output-cost 14.0
              :cutoff-date "2025-08"))
      (setopt gptel--openai-models gptel--openai-models))

    (setq my-gptel--openai
          (gptel-make-openai "ChatGPT"
            :key #'gptel-api-key-from-auth-source
            :stream t
            :models gptel--openai-models))
    (setq my-gptel--openrouter-kimi-k2
          (gptel-make-openai "OpenRouter"
            :host "openrouter.ai"
            :endpoint "/api/v1/chat/completions"
            :stream t
            :key #'gptel-api-key-from-auth-source
            :request-params
            '(:provider (:only ["deepinfra"])
              :temperature 0.6)
            :models '(moonshotai/kimi-k2
                      :description "Kimi K2")))

    (require 'gptel-openai-extras)
    (setq my-gptel--xai
          (gptel-make-xai "xAI"
            :key #'gptel-api-key-from-auth-source
            :stream t
            :models '((grok-4-fast
                       :description "Fast reasoning model"
                       :capabilities '(tool-use json reasoning)
                       :context-window 256
                       :input-cost 0.2
                       :output-cost 1.5))))

    (setq my-gptel--opencode-zen
          (gptel-make-openai "OpenCode Zen"
            :host "opencode.ai"
            :endpoint "/zen/v1/chat/completions"
            :stream t
            :key #'gptel-api-key-from-auth-source
            :models '((big-pickle
                       :description "Big Pickle model - currently free"
                       :capabilities (tool-use json)
                       :context-window 200
                       :input-cost 0.0
                       :output-cost 0.0)
                      (claude-haiku-4-5
                       :description "Claude Haiku 4.5 model"
                       :capabilities (tool-use json)
                       :context-window 200
                       :input-cost 1.0
                       :output-cost 5.0
                       :cutoff-date "2025-01")
                      (claude-opus-4-5
                       :description "Claude Opus 4.5 model - most powerful Claude model"
                       :capabilities (tool-use json media)
                       :mime-types ("image/jpeg" "image/png" "image/gif" "image/webp")
                       :context-window 200
                       :input-cost 15.0
                       :output-cost 75.0
                       :cutoff-date "2025-02")
                      (claude-sonnet-4-5
                       :description "Claude Sonnet 4.5 model with enhanced capabilities"
                       :capabilities (tool-use json media)
                       :mime-types ("image/jpeg" "image/png" "image/gif" "image/webp")
                       :context-window 200
                       :input-cost 3.0
                       :output-cost 15.0
                       :cutoff-date "2024-09")
                      (gemini-3-pro
                       :description "Gemini 3.0 Pro model - most intelligent model with state-of-the-art reasoning and multimodal capabilities"
                       :capabilities (tool-use json media)
                       :mime-types ("image/jpeg" "image/png" "image/gif" "image/webp" "image/heic" "image/heif" "application/pdf" "text/plain" "text/csv" "text/html" "video/mp4" "video/webm")
                       :context-window 1000
                       :input-cost 1.25
                       :output-cost 10.0
                       :cutoff-date "2025-01")
                      (glm-4.7-free
                       :description "GLM 4.7 model (free)"
                       :capabilities (tool-use json)
                       :context-window 200
                       :input-cost 0.6
                       :output-cost 2.2)
                      (gpt-5.2
                       :description "GPT 5.2 model - latest GPT model"
                       :capabilities (media tool-use json url)
                       :mime-types ("image/jpeg" "image/png" "image/gif" "image/webp")
                       :context-window 200
                       :input-cost 1.75
                       :output-cost 14.0
                       :cutoff-date "2025-08")
                      (grok-code
                       :description "Grok Code Fast 1 model - currently free"
                       :capabilities (tool-use json)
                       :context-window 200
                       :input-cost 0.0
                       :output-cost 0.0)
                      (kimi-k2
                       :description "Kimi K2 model"
                       :capabilities (tool-use json)
                       :context-window 200
                       :input-cost 0.6
                       :output-cost 2.5)
                      (qwen3-coder
                       :description "Qwen3 Coder 480B model"
                       :capabilities (tool-use json)
                       :context-window 200
                       :input-cost 0.45
                       :output-cost 1.5))))

    (run-hooks 'my-gptel-ensure-backends-hook))

  (setq gptel-backend (symbol-value my-gptel-backend))
  (setopt gptel-model (or my-gptel-model (car (gptel-backend-models gptel-backend)))
          gptel-expert-commands t
          gptel-rewrite-default-action 'accept
          gptel-temperature my-gptel-temperature)

  ;; uncomment to debug gptel:
  ;; (setq gptel-log-level 'debug)

  (when my-gptel-system-prompt
    (setq gptel--system-message my-gptel-system-prompt)))

(use-package gptel
  :defer t
  :custom
  (gptel-default-mode #'gfm-mode)
  :config
  (my-gptel-ensure-backends)
  (add-to-list 'gptel-prompt-prefix-alist '(gfm-mode . "")))

;; Separate block so :commands generates autoloads targeting "gptel-context",
;; which also suppresses byte-compiler warnings for these functions.
(use-package gptel-context
  :ensure nil
  :commands (gptel-context-add-file gptel-context--buffer-setup
                                    gptel-context-confirm gptel-context-remove-all)
  :config
  (let ((map gptel-context-buffer-mode-map))
    (keymap-set map "q" #'my-gptel-context-save-and-quit)))

(defun my-gptel-start ()
  "Start gptel with a default buffer name."
  (interactive)
  (require 'gptel)
  (let ((backend-name (format "*%s*" (gptel-backend-name gptel-backend))))
    (switch-to-buffer (gptel backend-name nil ""))))

(defun my-gptel-toggle-local ()
  "Toggle between local AI and remote AI."
  (interactive)
  (require 'gptel)
  (let* ((use-local (cond ((eq gptel-backend (symbol-value my-gptel-backend-local))
                           nil)
                          ((eq gptel-backend (symbol-value my-gptel-backend-remote))
                           t)
                          (t t)))
         (backend-sym
          (if use-local my-gptel-backend-local my-gptel-backend-remote))
         (backend (symbol-value backend-sym))
         (model (or (if use-local my-gptel-model-local my-gptel-model-remote)
                    (car (gptel-backend-models backend)))))
    (setq gptel-backend backend
          my-gptel-backend backend-sym
          gptel-model model
          my-gptel-model model)
    (message "gptel backend is now %s" backend-sym)))

(defun my-gptel-toggle-model (gptel-backend-sym gptel-model-sym)
  "Switch to a specific AI model."
  (interactive)
  (require 'gptel)
  (let* ((use-preferred (not (eq my-gptel-preferred-provider 'default)))
         (backend-sym (if use-preferred
                          my-gptel-preferred-provider
                        gptel-backend-sym))
         (model-sym (if use-preferred
                        (pcase gptel-model-sym
                          ('claude-opus-4-5-20251101 'claude-opus-4-5)
                          ('claude-sonnet-4-5-20250929 'claude-sonnet-4-5)
                          ('grok-code-fast-1 'grok-code)
                          ('moonshotai/kimi-k2 'kimi-k2)
                          (_ gptel-model-sym))
                      gptel-model-sym)))
    (setq gptel-backend (symbol-value my-gptel-backend-local)
          my-gptel-backend-remote backend-sym
          my-gptel-model-remote model-sym)
    (my-gptel-toggle-local)))

(defun my-gptel-toggle-gemini-flash ()
  (interactive)
  (my-gptel-toggle-model 'my-gptel--gemini-lite
                         'gemini-2.5-flash))

(defun my-gptel-toggle-gemini-pro ()
  (interactive)
  (my-gptel-toggle-model 'my-gptel--gemini
                         'gemini-3-pro))

(defun my-gptel-toggle-glm ()
  (interactive)
  (my-gptel-toggle-model 'my-gptel--opencode-zen
                         'glm-4.7-free))

(defun my-gptel-toggle-gpt ()
  (interactive)
  (my-gptel-toggle-model 'my-gptel--openai
                         'gpt-5.2))

(defun my-gptel-toggle-grok-code-fast ()
  (interactive)
  (my-gptel-toggle-model 'my-gptel--xai
                         'grok-code-fast-1))

(defun my-gptel-toggle-kimi-k2 ()
  (interactive)
  (my-gptel-toggle-model 'my-gptel--openrouter-kimi-k2
                         'moonshotai/kimi-k2))

(defun my-gptel-toggle-opus ()
  (interactive)
  (my-gptel-toggle-model 'my-gptel--claude
                         'claude-opus-4-5-20251101))

(defun my-gptel-toggle-opus-thinking ()
  (interactive)
  (my-gptel-toggle-model 'my-gptel--claude-thinking
                         'claude-opus-4-5-20251101))

(defun my-gptel-toggle-sonnet ()
  (interactive)
  (my-gptel-toggle-model 'my-gptel--claude
                         'claude-sonnet-4-5-20250929))

(defun my-gptel-toggle-sonnet-thinking ()
  (interactive)
  (my-gptel-toggle-model 'my-gptel--claude-thinking
                         'claude-sonnet-4-5-20250929))

(defun my-gptel-context-save-and-quit ()
  "Apply gptel context changes and quit."
  (interactive)
  (cl-letf (((symbol-function 'gptel-context-quit) (lambda () (quit-window))))
    (gptel-context-confirm)))

(defun my-gptel-add-function ()
  "Add the current function to the LLM context.

Use the region instead if one is selected."
  (interactive)
  (unless (use-region-p)
    (gptel-fn-complete-mark-function))
  (call-interactively #'gptel-add))

(defun my-gptel-add-current-file ()
  "Add the current file to the LLM context."
  (interactive)
  (gptel-context-add-file (buffer-file-name)))

(defun my-gptel-view-context ()
  (interactive)
  (gptel-context--buffer-setup))

(defun my-gptel-rewrite-function ()
  "Rewrite or refactor the current function using an LLM.

Rewrite the region instead if one is selected."
  (interactive)
  (unless (use-region-p)
    (gptel-fn-complete-mark-function))
  (let ((gptel-include-reasoning nil))
    (call-interactively #'gptel-rewrite)))

(defun my-gptel-query-function ()
  "Add the current function to gptel context and query an LLM.

Use the region instead if one is selected."
  (interactive)
  (my-gptel-add-function)
  (split-window-right)
  (other-window 1)
  (call-interactively #'my-gptel-start))

(defun my-gptel-context-remove-all ()
  (interactive)
  (gptel-context-remove-all))

;; Minuet for AI completion
(defun my-minuet-exclude ()
  (let* ((filename (buffer-file-name)))
    (or (not filename)
        (when-let* ((lst my-minuet-exclude-file-regexps))
          (string-match-p
           (string-join (mapcar (lambda (x)
                                  (concat "\\(?:" x "\\)")) lst) "\\|")
           filename)))))

(defun my-minuet-maybe-turn-on-auto-suggest ()
  (when (and my-minuet-auto-suggest-p (not (my-minuet-exclude)))
    (minuet-auto-suggestion-mode 1)))

(eval-when-compile
  (require 'minuet nil t))
(defun my-minuet-get-api-key (backend)
  (my-auth-source-get-api-key (gptel-backend-host backend)))

(defun my-minuet-block-suggestions ()
  "Return nil if we should show suggestions, t (blocked) otherwise.

Criteria:
- File must be writable
- Cursor must not be at beginning of line
- Cursor must be at the end of line (ignoring whitespace)."
  (not (and (not buffer-read-only)
            (not (bolp))
            (looking-at-p "\s*$"))))

(defun my-minuet-sync-options-from-gptel (m-backend g-backend &optional g-model)
  "Synchronize Minuet provider options from the current gptel backend.

M-BACKEND is the Minuet backend symbol.
G-BACKEND is the gptel backend instance.
optional G-MODEL is the gptel model symbol to use."
  (let* ((options-name (format "minuet-%s-options" (symbol-name m-backend)))
         (options (symbol-value (intern options-name)))
         (model (or g-model (car (gptel-backend-models g-backend)))))
    (when (memq m-backend '(openai-compatible openai-fim-compatible))
      (plist-put options :name (gptel-backend-name g-backend))
      (setf (plist-get options :optional)
            (copy-sequence (gptel--model-request-params gptel-model)))
      (plist-put options :model (symbol-name model)))
    (cond (my-minuet-provider-local-p
           (minuet-set-optional-options options :max_tokens 128)
           (plist-put options :api-key #'(lambda () "local-ai")))
          (t
           (plist-put options :api-key
                      `(lambda () (my-minuet-get-api-key ,g-backend)))))
    (cond ((eq m-backend 'openai-fim-compatible)
           (plist-put options :end-point
                      (format "%s://%s%s"
                              (gptel-backend-protocol g-backend)
                              (gptel-backend-host g-backend)
                              "/v1/completions")))
          ((eq m-backend 'openai-compatible)
           (plist-put options :end-point
                      (format "%s://%s%s"
                              (gptel-backend-protocol g-backend)
                              (gptel-backend-host g-backend)
                              (gptel-backend-endpoint g-backend)))))))

(defun my-minuet-init-provider ()
  "Initialize Minuet provider settings based on current gptel backend."
  (interactive)
  (my-gptel-ensure-backends)
  (let* ((g-provider (pcase my-minuet-provider
                       ('claude my-gptel--claude)
                       ('openai my-gptel--openai)
                       ((or 'openai-compatible 'openai-compatible-fim)
                        (symbol-value my-gptel-preferred-provider)))))
    (my-minuet-sync-options-from-gptel my-minuet-provider
                                       g-provider
                                       my-minuet-model)
    (setq minuet-provider my-minuet-provider)))

(use-package minuet
  :vc (:url "https://github.com/milanglacier/minuet-ai.el")
  :defer t
  :hook (prog-mode . my-minuet-maybe-turn-on-auto-suggest)
  :custom
  (minuet-add-single-line-entry nil)
  (minuet-auto-suggestion-debounce-delay 0.3)
  (minuet-n-completions 1)
  :config
  (my-minuet-init-provider)

  (add-hook 'minuet-auto-suggestion-block-predicates
            #'my-minuet-block-suggestions -100)

  (keymap-set minuet-active-mode-map "C-c C-c" #'minuet-accept-suggestion)
  (keymap-set minuet-active-mode-map "C-c C-n" #'minuet-next-suggestion)
  (keymap-set minuet-active-mode-map "C-c C-p" #'minuet-previous-suggestion)
  (keymap-set minuet-active-mode-map "C-g" #'minuet-dismiss-suggestion)
  (keymap-set minuet-active-mode-map "<tab>" #'minuet-accept-suggestion-line))

(my-around-advice #'my-minuet-maybe-turn-on-auto-suggest
                  #'my-inhibit-in-indirect-md-buffers)

;; Enable dumb-jump, which makes `C-c . .' jump to a function's definition
(use-package dumb-jump
  :vc (:url "https://github.com/jacktasia/dumb-jump")
  :demand t
  :custom
  (dumb-jump-selector 'completing-read)
  :config
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate))

(defvar my-xref-map
  (let ((map (make-sparse-keymap)))
    (keymap-set map "a a" #'my-gptel-add-function)
    (keymap-set map "a f" #'my-gptel-add-current-file)
    (keymap-set map "c" #'minuet-show-suggestion)
    (keymap-set map "f" #'gptel-fn-complete)
    (keymap-set map "k" #'my-gptel-context-remove-all)
    (keymap-set map "l" #'my-gptel-toggle-local)
    (keymap-set map "q" #'my-gptel-query-function)
    (keymap-set map "r" #'my-gptel-rewrite-function)
    (keymap-set map "v" #'my-gptel-view-context)
    (keymap-set map "." #'xref-find-definitions)
    (keymap-set map "," #'xref-go-back)
    (keymap-set map "/" #'xref-find-references)
    (keymap-set map "RET" #'embark-act)
    map)
  "My key customizations for AI and xref.")

(keymap-global-set "C-c ." my-xref-map)
(keymap-global-set "C-c C-." my-xref-map)
(keymap-global-set "C-x ." my-xref-map)
(keymap-global-set "C-x C-." my-xref-map)

(defvar my-xref-minor-mode-map
  (let ((map (make-sparse-keymap)))
    (keymap-set map "C-c ." my-xref-map)
    map))

(define-minor-mode my-xref-minor-mode
  "Minor mode for jumping to variable and function definitions"
  :keymap my-xref-minor-mode-map)

(my-around-advice #'my-xref-minor-mode #'my-inhibit-in-indirect-md-buffers)

;; Bash shell script and .env support
(use-package sh-script
  :ensure nil
  :demand t
  :mode ("\\.env\\(\\..*\\)?\\'" . bash-ts-mode)
  :custom
  (sh-shell-file "/bin/bash")
  :init
  (add-to-list 'major-mode-remap-alist '(sh-mode . bash-ts-mode))
  :config
  (fset #'my-real-sh-mode #'sh-mode)
  (add-to-list 'my-md-code-aliases '(bash . my-real-sh-mode))
  (add-to-list 'my-md-code-aliases '(sh . my-real-sh-mode))
  (add-to-list 'my-md-code-aliases '(shell . my-real-sh-mode)))

;; C/C++
(add-to-list 'major-mode-remap-alist '(c-mode . c-ts-mode))
(add-to-list 'major-mode-remap-alist '(c++-mode . c++-ts-mode))
(add-to-list 'major-mode-remap-alist '(c-or-c++-mode . c-or-c++-ts-mode))
(add-hook 'c-ts-base-mode-hook #'eglot-ensure)
(add-hook 'c-ts-base-mode-hook #'my-xref-minor-mode t)

;; C# - requires exactly v0.20.0 of its treesit grammar
;; `C-c . .` doesn't currently work, see this for ideas:
;; https://github.com/theschmocker/dotfiles/blob/33944638a5a59ddba01b64066daf50d46e5f0c3a/emacs/.doom.d/config.el#L807
(use-package csharp-mode
  :ensure nil
  :defer t
  :init
  (add-to-list 'major-mode-remap-alist '(csharp-mode . csharp-ts-mode))
  :hook (csharp-ts-mode . my-xref-minor-mode)
  :config
  (keymap-set csharp-mode-map "C-c ." nil))

(when (executable-find "omnisharp")
  (add-hook 'csharp-ts-mode-hook #'eglot-ensure))

;; Caddy conf files
(define-derived-mode my-caddyfile-mode conf-space-mode "Caddy"
  "Major mode for highlighting caddy config files."
  (setq-local indent-tabs-mode t)
  (setq-local tab-width 4))

(add-to-list 'auto-mode-alist '("/caddy\\.conf\\'" . my-caddyfile-mode))
(add-to-list 'auto-mode-alist '("/Caddyfile\\'" . my-caddyfile-mode))

;; Clojure
(use-package cider
  :vc (:url "https://github.com/clojure-emacs/cider"
       :lisp-dir "lisp")
  :defer t)

(eval-when-compile
  (require 'cider-repl nil t))
(use-package cider-repl
  :ensure nil
  :defer t
  :config
  (keymap-set cider-repl-mode-map "C-d" #'cider-quit))

(defvar my-clojure-modes
  '(clojure-ts-mode clojure-ts-clojurec-mode clojure-ts-clojurescript-mode))

(use-package apheleia-formatters
  :ensure nil
  :defer t
  :after apheleia
  :config
  (setf (alist-get 'zprint apheleia-formatters)
        '("zprint" "{:style [:how-to-ns] :search-config? true}"))
  (dolist (mode my-clojure-modes)
    (setf (alist-get mode apheleia-mode-alist)
          'zprint)))

(add-to-list 'major-mode-remap-alist '(clojure-mode . clojure-ts-mode))
(add-to-list 'major-mode-remap-alist
             '(clojurec-mode . clojure-ts-clojurec-mode))
(add-to-list 'major-mode-remap-alist
             '(clojurescript-mode . clojure-ts-clojurescript-mode))

(dolist (mode my-clojure-modes)
  (let ((hook (intern (concat (symbol-name mode) "-hook"))))
    (add-hook hook #'cider-mode t)
    (add-hook hook #'eglot-ensure t)))

(my-around-advice #'cider-mode #'my-inhibit-in-indirect-md-buffers)

(add-to-list 'eglot-server-programs
             `(,my-clojure-modes . ("clojure-lsp")))

;; Conf files

;; ini files in markdown code blocks
(add-to-list 'my-md-code-aliases '("ini" . conf-mode))
;; MariaDB/MySQL conf files
(add-to-list 'auto-mode-alist '("\\.cnf\\'" . conf-mode))
;; SystemD conf files
(add-to-list 'auto-mode-alist '("\\.service\\'" . conf-mode))
;; SSH conf files
(add-to-list 'auto-mode-alist '("_config\\'" . conf-mode))
;; /private/etc/ files on macOS
(add-to-list 'auto-mode-alist '("\\`/private/etc/\\(?:DIR_COLORS\\|ethers\\|.?fstab\\|.*hosts\\|lesskey\\|login\\.?de\\(?:fs\\|vperm\\)\\|magic\\|mtab\\|pam\\.d/.*\\|permissions\\(?:\\.d/.+\\)?\\|protocols\\|rpc\\|services\\)\\'"
                                . conf-space-mode))

;; CSS
(add-to-list 'major-mode-remap-alist '(css-mode . css-ts-mode))

(add-hook 'css-ts-mode-hook #'add-node-modules-path t)
(add-hook 'css-ts-mode-hook #'eglot-ensure t)
(add-hook 'css-ts-mode-hook #'my-setup-web-ligatures t)
;; (uses eglot-typescript-preset for rass: vscode-css + tailwindcss)

;; Emacs Lisp
(eval-when-compile
  (require 'ielm nil t))
(use-package plist-lisp-indent
  :ensure nil
  :commands plist-lisp-indent-install
  :hook (emacs-lisp-mode . plist-lisp-indent-install))
(defun my-ielm-setup ()
  (let ((map (copy-keymap inferior-emacs-lisp-mode-map)))
    (keymap-set map "C-d" #'kill-buffer-and-window)
    (push `(inferior-emacs-lisp-mode-map . ,map)
          minor-mode-overriding-map-alist)))

(add-to-list 'display-buffer-alist
             '("*ielm*"
               (display-buffer-in-side-window)
               (side . bottom)
               (window-height . 7)))

(setq ielm-header "")
(setq ielm-prompt "Eval: ")
(add-hook 'ielm-indirect-setup-hook #'rainbow-delimiters-mode t)
(add-hook 'ielm-mode-hook #'my-ielm-setup t)
(keymap-global-set "C-M-;" #'ielm)

;; Go
(defun my-project-find-go-module (dir)
  (when-let* ((root (locate-dominating-file dir "go.mod")))
    (cons 'go-module root)))

(add-to-list 'auto-mode-alist '("\\.go\\'" . go-ts-mode))
(add-to-list 'auto-mode-alist '("/go\\.mod\\'" . go-mod-ts-mode))
(add-to-list 'my-md-code-aliases '(go . go-ts-mode))
(add-hook 'go-ts-mode-hook #'eglot-ensure)
(add-hook 'go-mod-ts-mode-hook #'eglot-ensure)

;; GraphQL
(add-to-list 'auto-mode-alist '("\\.\\(graphql\\|gql\\)\\'" . graphql-ts-mode))
(add-hook 'graphql-ts-mode-hook #'my-apheleia-set-js-formatter)

;; HTML
(add-to-list 'major-mode-remap-alist '(html-mode . html-ts-mode))

(add-hook 'html-ts-mode-hook #'eglot-ensure)
(add-hook 'html-ts-mode-hook #'my-setup-web-ligatures t)

;; Java
(add-to-list 'major-mode-remap-alist '(java-mode . java-ts-mode))
(add-hook 'java-ts-mode-hook #'my-xref-minor-mode t)

(when (executable-find "jdtls")
  ;; see https://gist.github.com/rosholger/e519c04243ae7ccb5bbf7ebef3f1cec2
  ;; and the eglot-java package for more options / alternatives
  (add-to-list 'eglot-server-programs
               '((java-ts-mode java-mode)
                 . ("jdtls"
                    :initializationOptions
                    (:extendedClientCapabilities
                     (:classFileContentsSupport t
                      :skipProjectConfiguration t)))))
  (add-hook 'java-ts-mode-hook #'eglot-ensure))

;; JSON
(add-to-list 'major-mode-remap-alist '(json-mode . json-ts-mode))
(add-to-list 'auto-mode-alist '("\\.jsonc?\\'" . json-ts-mode))
(add-hook 'json-ts-mode-hook #'add-node-modules-path t)
(add-hook 'json-ts-mode-hook #'eglot-ensure)
(add-hook 'json-ts-mode-hook #'my-setup-web-ligatures t)
(add-hook 'json-ts-mode-hook #'my-apheleia-set-js-formatter)

;; JTSX (Astro, Javascript, JSX, Typescript, and TSX support)
(defvar my-jtsx-major-modes
  '(astro-ts-mode jtsx-jsx-mode jtsx-tsx-mode jtsx-typescript-mode))
(defvar my-jtsx-ts-major-modes '(jtsx-tsx-mode jtsx-typescript-mode))

(use-package js
  :ensure nil
  :demand t
  :custom
  (js-indent-level 2)
  :config
  (fset #'my-real-js-mode #'js-mode))

;; load this early (astro-ts-mode dependency) so that we can override the
;; auto-mode-alist entries that it makes
(use-package typescript-ts-mode
  :ensure nil
  :demand t)

(add-to-list 'auto-mode-alist '("\\.astro\\'" . astro-ts-mode))
(add-to-list 'auto-mode-alist '("\\.[cm]js\\'" . jtsx-jsx-mode))
(add-to-list 'auto-mode-alist '("\\.jsx?\\'" . jtsx-jsx-mode))
(add-to-list 'auto-mode-alist '("\\.ts\\'" . jtsx-typescript-mode))
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . jtsx-tsx-mode))
(add-to-list 'auto-mode-alist '("/\\.babelrc\\'" . json-ts-mode))
(add-to-list 'auto-mode-alist '("/.eslintignore\\'" . gitignore-mode))
(add-to-list 'auto-mode-alist '("/\\.graphqlconfig\\'" . json-ts-mode))
(add-to-list 'auto-mode-alist '("\\.npmrc\\'" . conf-mode))
(add-to-list 'auto-mode-alist '("/\\.prettierrc\\'" . json-ts-mode))
(add-to-list 'auto-mode-alist '("/\\.prettierignore\\'" . gitignore-mode))
(add-to-list 'auto-mode-alist '("/\\yarn.lock\\'" . conf-mode))
(add-to-list 'auto-mode-alist '("\\.yarnrc\\'" . yaml-ts-mode))
(add-to-list 'my-md-code-aliases '(javascript . my-real-js-mode))
(add-to-list 'my-md-code-aliases '(js . my-real-js-mode))
(add-to-list 'my-md-code-aliases '(ts . my-real-js-mode))
(add-to-list 'my-md-code-aliases '(typescript . my-real-js-mode))
(add-to-list 'major-mode-remap-alist '(js-mode . jtsx-jsx-mode))
(add-to-list 'major-mode-remap-alist '(ts-mode . jtsx-tsx-mode))

(defun my-apheleia-skip-bun ()
  (when (string-match-p "/bun\\.lock\\'" (or buffer-file-name ""))
    (setq apheleia-inhibit t)))

(add-to-list 'auto-mode-alist '("/bun\\.lock\\'" . json-ts-mode))
(add-hook 'json-ts-mode-hook #'my-apheleia-skip-bun -100)

;; - eglot-typescript-preset: TS/JS/Astro/Svelte/Vue project detection and
;; eglot setup
;;
;; for local development and testing, replace the use-package block with:
;; (add-to-list 'load-path (expand-file-name "~/devel/projects/eglot-typescript-preset"))
;; (require 'eglot-typescript-preset)
(use-package eglot-typescript-preset
  :vc (:url "https://github.com/mwolson/eglot-typescript-preset")
  :custom
  (eglot-typescript-preset-tsdk (concat my-emacs-path "node_modules/typescript/lib"))
  (eglot-typescript-preset-astro-lsp-server 'rass)
  (eglot-typescript-preset-lsp-server 'rass)
  (eglot-typescript-preset-rass-tools '(typescript-language-server biome)))

(dolist (mode my-jtsx-major-modes)
  (let ((hook (intern (concat (symbol-name mode) "-hook"))))
    (add-hook hook #'add-node-modules-path t)
    (add-hook hook #'eglot-ensure t)
    (add-hook hook #'my-node-repl-setup t)
    (add-hook hook #'my-setup-web-ligatures t)
    (add-hook hook #'my-apheleia-set-js-formatter)))

(my-around-advice
 '(add-node-modules-path
   my-node-repl-setup
   my-apheleia-set-js-formatter
   my-apheleia-set-markdown-formatter
   my-apheleia-set-yaml-formatter)
 #'my-inhibit-in-indirect-md-buffers)

;; KDL
(defun my-setup-kdl-mode ()
  (setq-local tab-width 4))

(add-hook 'kdl-mode-hook #'my-setup-kdl-mode)

;; Kotlin
(add-to-list 'auto-mode-alist '("\\.kts?\\'" . kotlin-ts-mode))
(add-to-list 'my-md-code-aliases '(kotlin . kotlin-ts-mode))

;; Lisp
(use-package slime
  :vc (:url "https://github.com/slime/slime")
  :demand t
  :custom
  (slime-auto-connect 'always)
  (slime-kill-without-query-p t)
  (slime-protocol-version 'ignore)
  :config
  (slime-setup '(slime-repl)))

;; Don't warn me when opening some Common Lisp files
(put 'package 'safe-local-variable 'symbolp)
(put 'Package 'safe-local-variable 'symbolp)
(put 'syntax 'safe-local-variable 'symbolp)
(put 'Syntax 'safe-local-variable 'symbolp)
(put 'Base 'safe-local-variable 'integerp)
(put 'base 'safe-local-variable 'integerp)

;; Markdown support
(add-to-list 'major-mode-remap-alist '(markdown-mode . gfm-mode))

(eval-when-compile
  (require 'markdown-mode nil t))
(defun my-replace-mode-in-symbol (mode-sym)
  (intern
   (replace-regexp-in-string "-mode\\'" ""
                             (symbol-name mode-sym))))

(defun my-markdown-install-aliases ()
  (dolist (to-remap major-mode-remap-alist)
    (let ((from (my-replace-mode-in-symbol (car to-remap)))
          (to (cdr to-remap)))
      (unless (eq to 'gfm-mode)
        (add-to-list 'markdown-code-lang-modes (cons from to)))))
  (dolist (alias my-md-code-aliases)
    (add-to-list 'markdown-code-lang-modes alias)))

(defun my-markdown-yank-chunk ()
  (interactive)
  (save-excursion
    (if-let* ((bounds (markdown-get-enclosing-fenced-block-construct))
              (fence-begin (nth 0 bounds))
              (fence-end (nth 1 bounds)))
        (let* ((begin (progn (goto-char fence-begin)
                             (line-beginning-position 2)))
               (indentation (current-indentation))
               (end (progn (goto-char fence-end)
                           (line-beginning-position 1)))
               (contents (buffer-substring-no-properties begin end)))
          (with-temp-buffer
            (insert contents)
            (when (> indentation 0)
              (indent-rigidly (point-min) (point-max) (- indentation)))
            (copy-region-as-kill (point-min) (point-max)))
          (message "Yanked fenced code block"))
      (user-error "Not inside a GFM or tilde fenced code block"))))

(quail-define-package
 "Arrows" "UTF-8" "→" nil
 "Arrow input mode"
 nil t t nil nil nil nil nil nil nil t)

(quail-define-rules
 ("->" ?→))

(defun my-turn-on-arrow-input ()
  "Turn on arrow input mode."
  (interactive)
  (set-input-method 'Arrows))

(defun my-turn-off-arrow-input ()
  "Turn off arrow input mode."
  (interactive)
  (deactivate-input-method))

(use-package markdown-mode
  :vc (:url "https://github.com/jrblevin/markdown-mode")
  :defer t
  :hook ((markdown-mode . add-node-modules-path)
         (markdown-mode . my-setup-web-ligatures)
         (markdown-mode . my-turn-on-arrow-input)
         (markdown-mode . my-apheleia-set-markdown-formatter))
  :custom
  (markdown-command "npx marked")
  (markdown-enable-wiki-links t)
  (markdown-fontify-code-blocks-natively t)
  :config
  (my-markdown-install-aliases)
  (my-replace-cdrs-in-alist 'sh-mode 'bash-ts-mode
                            'markdown-code-lang-modes)
  (my-replace-cdrs-in-alist 'shell-script-mode 'bash-ts-mode
                            'markdown-code-lang-modes)
  (keymap-set markdown-mode-map "C-c C-w" #'my-markdown-yank-chunk)
  (easy-menu-add-item markdown-mode-menu
                      nil
                      '["Yank Code Block" my-markdown-yank-chunk]
                      "Kill Element"))

(my-around-advice #'my-turn-on-arrow-input
                  #'my-inhibit-in-indirect-md-buffers)

(add-to-list
 'eglot-server-programs
 `(my-mdx-mode
   . ("mdx-language-server" "--stdio"
      :initializationOptions
      (:typescript
       (:enabled t
        :tsdk ,(concat my-emacs-path "node_modules/typescript/lib"))))))

;; MDX
(define-derived-mode my-mdx-mode gfm-mode "MDX"
  "Major mode for highlighting MDX files.")

(add-hook 'my-mdx-mode-hook #'eglot-ensure)

(add-to-list 'auto-mode-alist '("\\.mdx\\'" . my-mdx-mode))
(add-to-list 'my-md-code-aliases '(mdx . my-mdx-mode))

;; Mermaid diagrams
(use-package mermaid-ts-mode
  :vc (:url "https://github.com/kiennq/mermaid-ts-mode")
  :mode "\\.mmd\\'")

;; Nix
(add-to-list 'auto-mode-alist '("\\.nix\\'" . nix-ts-mode))
(add-to-list 'my-md-code-aliases '(nix . nix-ts-mode))

;; Node.js
(defvar my-nodejs-compilation-regexp
  '("^[ \t]+at +\\(?:.+(\\)?\\([^()\n]+\\):\\([0-9]+\\):\\([0-9]+\\))?$" 1 2 3)
  "Highlight node.js stacktraces in *compile* buffers.")

(with-eval-after-load "compile"
  (add-to-list 'compilation-error-regexp-alist-alist
               (cons 'nodejs my-nodejs-compilation-regexp))
  (add-to-list 'compilation-error-regexp-alist 'nodejs))

;; NSIS (.nsh files)
(add-to-list 'auto-mode-alist '("\\.[Nn][Ss][HhIi]\\'" . nsis-mode))

;; .plist files: from https://www.emacswiki.org/emacs/MacOSXPlist
(add-to-list 'jka-compr-compression-info-list
             ["\\.plist$"
              "converting text XML to binary plist"
              "plutil"
              ("-convert" "binary1" "-o" "-" "-")
              "converting binary plist to text XML"
              "plutil"
              ("-convert" "xml1" "-o" "-" "-")
              nil nil "bplist"])

;; Prisma support (a JS DB framework)
(add-to-list 'auto-mode-alist '("\\.prisma\\'" . prisma-ts-mode))

;; Python
(add-to-list 'auto-mode-alist '("/uv\\.lock\\'" . toml-ts-mode))
(add-to-list 'major-mode-remap-alist '(python-mode . python-ts-mode))

;; - eglot-python-preset: Python project detection and PEP-723 support
;;
;; for local development and testing, replace the use-package block with:
;; (add-to-list 'load-path (expand-file-name "~/devel/projects/eglot-python-preset"))
;; (require 'eglot-python-preset)
(use-package eglot-python-preset
  :vc (:url "https://github.com/mwolson/eglot-python-preset")
  :custom
  (eglot-python-preset-lsp-server 'ty))
;; (setopt eglot-python-preset-lsp-server 'basedpyright)
;; (setopt eglot-workspace-configuration
;;         (plist-put eglot-workspace-configuration
;;                    :basedpyright.analysis
;;                    '(:autoImportCompletions :json-false
;;                      :typeCheckingMode "basic")))

;; Rust
(add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-ts-mode))
(add-to-list 'my-md-code-aliases '(rust . rust-ts-mode))
(add-to-list 'eglot-server-programs
             '((rust-ts-mode rust-mode)
               . ("rust-analyzer"
                  :initializationOptions
                  (:check (:command "clippy")))))
(add-hook 'rust-ts-mode-hook #'eglot-ensure)

;; SCSS
(use-package flymake-stylelint
  :vc (:url "https://github.com/orzechowskid/flymake-stylelint")
  :defer t)

(add-hook 'scss-mode-hook #'add-node-modules-path t)
(add-hook 'scss-mode-hook #'flymake-stylelint-enable t)
(my-around-advice #'flymake-stylelint-enable
                  #'my-inhibit-in-indirect-md-buffers)

(add-to-list 'eglot-server-programs
             '((scss-mode)
               . ("vscode-css-language-server" "--stdio")))
(add-hook 'scss-mode-hook #'eglot-ensure)

;; Svelte
(use-package svelte-ts-mode
  :vc (:url "https://github.com/leafOfTree/svelte-ts-mode")
  :mode "\\.svelte\\'")

(add-hook 'svelte-ts-mode-hook #'add-node-modules-path t)
(add-hook 'svelte-ts-mode-hook #'eglot-ensure t)
(add-hook 'svelte-ts-mode-hook #'my-setup-web-ligatures t)
(add-hook 'svelte-ts-mode-hook #'my-apheleia-set-js-formatter)

;; Swift
(add-to-list 'auto-mode-alist '("\\.swift\\(interface\\)?\\'" . swift-ts-mode))
(add-to-list 'my-md-code-aliases '(swift . swift-ts-mode))

;; TOML
(add-to-list 'auto-mode-alist '("\\.toml\\'" . toml-ts-mode))

;; Vue
(use-package vue-ts-mode
  :vc (:url "https://github.com/8uff3r/vue-ts-mode")
  :mode "\\.vue\\'")

(add-hook 'vue-ts-mode-hook #'add-node-modules-path t)
(add-hook 'vue-ts-mode-hook #'eglot-ensure t)
(add-hook 'vue-ts-mode-hook #'my-setup-web-ligatures t)
(add-hook 'vue-ts-mode-hook #'my-apheleia-set-js-formatter)

(eval-when-compile
  (require 'css-mode nil t))
(defun my-vue-ts-set-fontify-css-colors ()
  (require 'css-mode)
  (setq-local font-lock-fontify-region-function #'css--fontify-region))

(with-eval-after-load "vue-ts-mode"
  (add-hook 'vue-ts-mode-hook #'my-vue-ts-set-fontify-css-colors))

;; Web Mode
(use-package web-mode
  :vc (:url "https://github.com/fxbois/web-mode")
  :mode "\\.hbs\\'"
  :hook ((web-mode . add-node-modules-path)
         (web-mode . my-setup-web-ligatures))
  :custom
  (web-mode-code-indent-offset 2)
  (web-mode-enable-auto-indentation nil)
  (web-mode-enable-auto-quoting nil)
  (web-mode-markup-indent-offset 2)
  :custom-face
  (web-mode-html-tag-face ((t (:inherit font-lock-constant-face))))
  (web-mode-json-key-face ((t (:foreground unspecified :inherit font-lock-variable-name-face)))))

;; YAML
(defun my-run-prog-mode-hooks ()
  (run-hooks 'prog-mode-hook))

(add-hook 'yaml-ts-mode-hook #'my-run-prog-mode-hooks t)
(add-hook 'yaml-ts-mode-hook #'my-apheleia-set-yaml-formatter)
(add-to-list 'auto-mode-alist '("\\.ya?ml\\'" . yaml-ts-mode))

;; Zig
(add-to-list 'auto-mode-alist '("\\.zig\\'" . zig-ts-mode))
(add-to-list 'my-md-code-aliases '(zig . zig-ts-mode))
(add-to-list 'eglot-server-programs
             '((zig-ts-mode)
               . ("zls" :initializationOptions ())))
(add-hook 'zig-ts-mode-hook #'eglot-ensure)

;; Consult, Embark, Marginalia, Orderless, Prescient, Vertico
(defvar my-minibuffer-from-consult-line nil)

(defun my-consult-line ()
  "Start incremental search from current line."
  (interactive)
  (let ((reg (if (region-active-p)
                 (prog1
                     (buffer-substring-no-properties (point) (mark))
                   (deactivate-mark))
               (if-let* ((sym (and current-prefix-arg
                                   (symbol-at-point))))
                   (symbol-name sym))))
        (my-minibuffer-from-consult-line t))
    (consult-line reg nil)))

(eval-when-compile
  (require 'consult nil t))
(defvar my-default-ripgrep-args "--hidden -i --no-ignore-vcs --ignore-file=.gitignore --glob=!.git/")

(defun my-consult-ripgrep (regexp rg-args)
  "Run a Consult Ripgrep search with `REGEXP' rooted at the current project root.

With \\[universal-argument], also prompt for extra rg arguments and set into RG-ARGS."
  (interactive
   (list (and (use-region-p)
              (buffer-substring-no-properties
               (region-beginning) (region-end)))
         (let ((default-args (progn
                               (require 'consult)
                               (concat consult-ripgrep-args " "
                                       my-default-ripgrep-args))))
           (if current-prefix-arg
               (read-from-minibuffer
                "Additional rg args: " default-args nil nil nil default-args)
             default-args))))
  (let ((consult-ripgrep-args rg-args))
    (consult-ripgrep nil regexp)))

(use-package consult
  :vc (:url "https://github.com/minad/consult")
  :defer t
  :config
  (with-eval-after-load "minuet"
    (consult-customize minuet-complete-with-minibuffer)))

(use-package embark
  :vc (:url "https://github.com/oantolin/embark"
       :main-file "embark.el")
  :defer t)

(use-package embark-consult
  :vc (:url "https://github.com/oantolin/embark"
       :main-file "embark-consult.el")
  :defer t
  :after (embark consult))

(use-package marginalia
  :vc (:url "https://github.com/minad/marginalia")
  :defer t
  :config
  (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup t))

(use-package nerd-icons
  :vc (:url "https://github.com/rainstormstudio/nerd-icons.el")
  :defer t)

(use-package nerd-icons-completion
  :vc (:url "https://github.com/rainstormstudio/nerd-icons-completion")
  :defer t)

(eval-when-compile
  (require 'orderless nil t)
  (require 'vertico-directory nil t)
  (require 'vertico-repeat nil t)
  (require 'vertico-suspend nil t))

(defun my-vertico-insert-like-ivy ()
  (interactive)
  (let* ((mb (minibuffer-contents-no-properties))
         (lc (if (string= mb "") mb (substring mb -1))))
    (cond ((string-match-p "^[/~:]" lc) (self-insert-command 1 ?/))
          ((file-directory-p (vertico--candidate)) (vertico-insert))
          (t (self-insert-command 1 ?/)))))

(use-package vertico
  :vc (:url "https://github.com/minad/vertico")
  :defer t
  :init
  (let ((ext-dir (expand-file-name "vertico/extensions" package-user-dir)))
    (when (file-directory-p ext-dir)
      (add-to-list 'load-path ext-dir)))
  :custom
  (vertico-count 10)
  (vertico-mouse-mode t)
  (vertico-resize nil)
  :config
  (require 'vertico-directory)
  (require 'vertico-repeat)
  (require 'vertico-suspend)
  (keymap-set occur-mode-map "r" #'occur-edit-mode)
  (keymap-set vertico-map "?" #'minibuffer-completion-help)
  (keymap-set vertico-map "/" #'my-vertico-insert-like-ivy)
  (keymap-set vertico-map "C-c C-c" #'vertico-repeat)
  (keymap-set vertico-map "C-c C-o" #'embark-export)
  (keymap-set vertico-map "C-c C-s" #'vertico-suspend)
  (keymap-set vertico-map "C-k" #'kill-line)
  (keymap-set vertico-map "C-r" #'vertico-previous)
  (keymap-set vertico-map "C-s" #'vertico-next)
  (keymap-set vertico-map "DEL" #'vertico-directory-delete-char)
  (keymap-set vertico-map "M-DEL" #'vertico-directory-delete-word)
  (keymap-set vertico-map "RET" #'vertico-directory-enter)
  (keymap-set vertico-map "<prior>" #'vertico-scroll-down)
  (keymap-set vertico-map "<next>" #'vertico-scroll-up)

  (add-hook 'minibuffer-setup-hook #'vertico-repeat-save)
  (add-hook 'rfn-eshadow-update-overlay-hook #'vertico-directory-tidy))

(use-package prescient
  :vc (:url "https://github.com/radian-software/prescient.el"
       :main-file "prescient.el")
  :defer t)

(use-package vertico-prescient
  :vc (:url "https://github.com/radian-software/prescient.el"
       :main-file "vertico-prescient.el")
  :defer t
  :after vertico
  :custom
  (prescient-sort-full-matches-first t)
  :config
  ;; disable prescient for consult-line since history doesn't make sense there
  (setopt vertico-prescient-completion-category-overrides
          (append '((consult-location (styles orderless basic)))
                  vertico-prescient-completion-category-overrides))
  (vertico-prescient-mode)
  (prescient-persist-mode))

(defun my-extended-command-predicate (symbol buffer)
  (and (command-completion-default-include-p symbol buffer)
       (transient-command-completion-not-suffix-only-p symbol buffer)))

(setq completion-category-defaults nil
      completion-category-overrides '((file (styles basic partial-completion)))
      completion-styles '(orderless basic)
      consult-async-min-input 2
      consult-async-input-debounce 0.1
      consult-async-input-throttle 0.2
      consult-async-refresh-delay 0.15
      prefix-help-command #'embark-prefix-help-command
      read-extended-command-predicate #'my-extended-command-predicate
      xref-search-program 'ripgrep
      xref-show-definitions-function #'consult-xref
      xref-show-xrefs-function #'consult-xref)

(use-package wgrep
  :vc (:url "https://github.com/mhayashi1120/Emacs-wgrep"
       :main-file "wgrep.el")
  :defer t)

(eval-when-compile
  (require 'grep nil t)
  (require 'wgrep nil t))
(use-package grep
  :ensure nil
  :defer t
  :config
  (keymap-set grep-mode-map "r" #'wgrep-change-to-wgrep-mode))
(eval-when-compile
  (require 'crm nil t))
(defun my-crm-indicator (args)
  "Work around issue in completing-read-multiple.

Not needed in Emacs 31 or higher."
  (cons (format "[CRM%s] %s"
                (replace-regexp-in-string
                 "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                 crm-separator)
                (car args))
        (cdr args)))

(advice-add #'completing-read-multiple :filter-args #'my-crm-indicator)

(defvar my-minibuffer-restore-pos nil)

(defun my-minibuffer-move-window-contents-up ()
  "Ensure that the window is moved to leave room for minibuffer under cursor.

This prevents the window from later moving back once the minibuffer is done showing."
  (require 'vertico)
  (let* ((prior-win (minibuffer-selected-window))
         (prior-buf (and prior-win (window-buffer prior-win))))
    (when (and prior-win (= (minibuffer-depth) 1))
      (with-selected-window prior-win
        (let* ((rows-dec (window-screen-lines))
               (rows (if (> (mod rows-dec 1.0) 0.0)
                         (round (+ rows-dec 0.5))
                       (round rows-dec)))
               (ppos (posn-at-point))
	       (cursor-row (cdr (posn-actual-col-row ppos)))
               ;; 4 = 1 minibuffer input line + 3 rows of context
               (max-allowed-row (max 0 (- rows vertico-count 4)))
               (scroll-up-amount (min (max 0 cursor-row)
                                      (- cursor-row max-allowed-row))))
          (when (and (> cursor-row max-allowed-row)
                     (> scroll-up-amount 0))
            (setq my-minibuffer-restore-pos
                  (vector prior-win prior-buf (line-beginning-position)
                          scroll-up-amount))
            (scroll-up scroll-up-amount)))))))

(defun my-minibuffer-restore-after-exit ()
  (when (= (minibuffer-depth) 1)
    (if my-minibuffer-from-consult-line
        (progn
          (setq my-minibuffer-restore-pos nil)
          ;; 5 ~= (1 minibuffer input line + 10) / 2
          (run-with-timer 0.0 nil (lambda ()
                                    (recenter)
                                    (scroll-up 5))))
      (when (and my-minibuffer-restore-pos
                 (null (transient-active-prefix)))
        (let ((prior-win (aref my-minibuffer-restore-pos 0))
              (prior-buf (aref my-minibuffer-restore-pos 1))
              (prior-line-begin (aref my-minibuffer-restore-pos 2))
              (scroll-up-amount (aref my-minibuffer-restore-pos 3)))
          (setq my-minibuffer-restore-pos nil)
          (run-with-timer
           0.0 nil
           (lambda ()
             (when (and (window-live-p prior-win)
                        (eq (window-buffer prior-win) prior-buf))
               (with-selected-window prior-win
                 (when (eq (line-beginning-position)
                           prior-line-begin)
                   (scroll-up scroll-up-amount)))))))))))

(plist-put minibuffer-prompt-properties 'cursor-intangible t)
(setopt minibuffer-prompt-properties minibuffer-prompt-properties)

(add-hook 'minibuffer-setup-hook #'my-minibuffer-move-window-contents-up -100)
(add-hook 'minibuffer-setup-hook #'cursor-intangible-mode +100)
(add-hook 'minibuffer-exit-hook #'my-minibuffer-restore-after-exit 100)

(my-defer-startup #'marginalia-mode)
(my-defer-startup #'vertico-mode)

(dolist (map (list minibuffer-local-map read-expression-map))
  (keymap-set map "C-k" #'kill-line)
  (keymap-set map "M-s" #'consult-history)
  (keymap-set map "M-r" #'consult-history))

(defvar my-consult-M-g-map
  (let ((map (make-sparse-keymap)))
    (keymap-set map "a" #'find-face-definition)
    (keymap-set map "e" #'consult-compile-error)
    (keymap-set map "f" #'consult-flymake)
    (keymap-set map "g" #'consult-goto-line)
    (keymap-set map "i" #'consult-imenu)
    (keymap-set map "I" #'consult-imenu-multi)
    (keymap-set map "k" #'consult-global-mark)
    (keymap-set map "m" #'consult-mark)
    (keymap-set map "M-g" #'consult-goto-line)
    (keymap-set map "s-g" #'consult-goto-line)
    map)
  "My key customizations for consult with `M-g' prefix.")

(keymap-global-set "C-c C-r" #'vertico-suspend)
(keymap-global-set "C-c M-x" #'consult-mode-command)
(keymap-global-set "C-h b" #'embark-bindings)
(keymap-global-set "C-r" #'my-consult-line)
(keymap-global-set "C-s" #'my-consult-line)
(keymap-global-set "C-x b" #'consult-buffer)
(keymap-global-set "M-g" my-consult-M-g-map)
(keymap-global-set "M-y" #'consult-yank-pop)

;; Corfu, Cape, Dabbrev for auto-completion
(defvar my-orderless-done-p nil)

(defun my-setup-orderless ()
  (unless my-orderless-done-p
    (setq my-orderless-done-p t)
    (require 'orderless)
    (orderless-define-completion-style orderless-literal-only
      (orderless-style-dispatchers nil)
      (orderless-matching-styles '(orderless-literal)))))

(defun my-setup-corfu-mode ()
  (my-setup-orderless)
  (setq-local completion-styles '(orderless-literal-only basic)
              completion-category-overrides nil
              completion-category-defaults nil))

(defun my-eglot-capf ()
  (setq-local completion-at-point-functions
              (list (cape-capf-super
                     #'eglot-completion-at-point
                     #'cape-file))))

(defun my-corfu-elisp ()
  "Configure Corfu completion for Elisp using CAPE backends locally."
  (add-hook 'completion-at-point-functions #'cape-elisp-block nil t)
  (add-hook 'completion-at-point-functions #'cape-elisp-symbol nil t))

(defun my-corfu-terminal-start ()
  (unless window-system (corfu-terminal-mode 1)))

(defun my-load-corfu ()
  (with-eval-after-load "elisp-mode"
    (add-hook 'emacs-lisp-mode-hook #'my-corfu-elisp t))

  (require 'corfu)
  (dolist (hook '(prog-mode-hook shell-mode-hook))
    (add-hook hook #'corfu-mode t))

  (my-around-advice #'corfu-mode #'my-inhibit-in-indirect-md-buffers)
  (add-hook 'corfu-mode-hook #'my-setup-corfu-mode)
  (add-hook 'eglot-managed-mode-hook #'my-eglot-capf)

  (require 'kind-icon)
  (plist-put kind-icon-default-style :height 0.35)
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter)

  (keymap-set corfu-map "<remap> <move-beginning-of-line>" nil)
  (keymap-set corfu-map "<remap> <move-end-of-line>" nil)
  (keymap-set corfu-map "<remap> <scroll-down-command>" nil)
  (keymap-set corfu-map "<remap> <scroll-up-command>" nil)
  (keymap-set corfu-map "RET" nil)

  (require 'corfu-auto)
  (require 'corfu-popupinfo)
  (setopt corfu-auto t
          corfu-popupinfo-delay '(0.3 . 0.01)
          corfu-popupinfo-hide nil
          corfu-quit-no-match 'separator
          global-corfu-modes t
          text-mode-ispell-word-completion nil)

  (dolist (el '("delete-backward-char\\'" "\\`backward-delete-char"))
    (setq corfu-auto-commands (delete el corfu-auto-commands)))

  (my-corfu-terminal-start)
  (add-hook 'my-init-client-display-hook #'my-corfu-terminal-start t)
  (corfu-popupinfo-mode)
  (corfu-prescient-mode)
  (global-corfu-mode))

(my-defer-startup #'my-load-corfu)

(add-hook 'completion-at-point-functions #'cape-dabbrev)
(add-hook 'completion-at-point-functions #'cape-file)
(add-hook 'completion-at-point-functions #'cape-keyword)

(use-package orderless
  :vc (:url "https://github.com/oantolin/orderless")
  :defer t)

(use-package corfu
  :vc (:url "https://github.com/minad/corfu")
  :defer t
  :init
  (let ((ext-dir (expand-file-name "corfu/extensions" package-user-dir)))
    (when (file-directory-p ext-dir)
      (add-to-list 'load-path ext-dir))))

(use-package corfu-prescient
  :vc (:url "https://github.com/radian-software/prescient.el"
       :main-file "corfu-prescient.el")
  :commands (corfu-prescient-mode)
  :defer t)

(use-package corfu-terminal
  :vc (:url "https://codeberg.org/akib/emacs-corfu-terminal")
  :defer t)

(use-package cape
  :vc (:url "https://github.com/minad/cape")
  :defer t)

(use-package kind-icon
  :vc (:url "https://github.com/jdtsmith/kind-icon")
  :defer t)

(use-package dabbrev
  :ensure nil
  :defer t
  :config
  (add-to-list 'dabbrev-ignored-buffer-regexps "\\` ")
  (add-to-list 'dabbrev-ignored-buffer-modes 'doc-view-mode)
  (add-to-list 'dabbrev-ignored-buffer-modes 'pdf-view-mode)
  (add-to-list 'dabbrev-ignored-buffer-modes 'tags-table-mode))

;; Set up majutsu
(use-package majutsu
  :vc (:url "https://github.com/0WD0/majutsu")
  :defer t
  :custom
  (majutsu-display-buffer-function 'same-window-except-diff))

(defun my-majutsu-log ()
  (interactive)
  (majutsu-log (or (my-project-root) default-directory)))

(defun my-remove-project-switch-bindings (to-remove)
  (dolist (item (cdr project-prefix-map))
    (let* ((raw-key (car item))
           (cmd (cdr item)))
      (when (and (memq cmd to-remove) (integerp raw-key))
        (keymap-set project-prefix-map (string raw-key) nil))))
  (setq project-switch-commands
        (cl-remove-if #'(lambda (item) (memq (car item) to-remove))
                      project-switch-commands)))

;; Set up project.el
(use-package project
  :ensure nil
  :defer t
  :config
  ;; from https://github.com/golang/tools/blob/master/gopls/doc/emacs.md#configuring-project-for-go-modules-in-emacs
  (cl-defmethod project-root ((project (head go-module)))
    (cdr project))

  (add-hook 'project-find-functions #'my-project-find-go-module)
  (my-remove-project-switch-bindings '(project-eshell
                                       project-find-dir
                                       project-find-regexp
                                       project-query-replace-regexp
                                       project-vc-dir))
  (add-to-list 'project-switch-commands '(project-dired "Dired") t)
  (keymap-set project-prefix-map "b" #'consult-project-buffer)
  (keymap-set project-prefix-map "d" #'project-dired)
  (add-to-list 'project-switch-commands '(my-consult-ripgrep "Ripgrep") t)
  (keymap-set project-prefix-map "j" #'my-majutsu-log)
  (add-to-list 'project-switch-commands '(my-majutsu-log "jj log") t)
  (keymap-set project-prefix-map "r" #'my-consult-ripgrep)
  (keymap-set project-prefix-map "s" #'my-consult-ripgrep)
  (add-to-list 'project-switch-commands '(magit-project-status "Magit") t)
  (keymap-set project-prefix-map "RET" #'magit-project-status)
  (keymap-set project-prefix-map "m" #'magit-project-status))

(defun my-project-root (&optional maybe-prompt)
  "Return root directory of the current project."
  (when-let* ((proj (project-current maybe-prompt)))
    (project-root proj)))

;; Support copying paths relative to the current buffer
(defun my-path-of-current-buffer ()
  (expand-file-name (or (buffer-file-name) default-directory)))

(defun my-copy-path-of-current-buffer ()
  (interactive)
  (let ((filepath (my-path-of-current-buffer)))
    (kill-new filepath)
    (message "Copied '%s' to clipboard" filepath)))

(defun my-copy-project-relative-path-of-current-buffer ()
  (interactive)
  (let ((filepath (file-relative-name
                   (my-path-of-current-buffer) (my-project-root))))
    (kill-new filepath)
    (message "Copied '%s' to clipboard" filepath)))

;; Insinuate with ripgrep
(defun my-rg-command-line-flags (&optional flags)
  (append flags (split-string-shell-command my-default-ripgrep-args)))

(setq rg-command-line-flags-function #'my-rg-command-line-flags)

(eval-when-compile
  (require 'rg nil t))
(use-package rg
  :vc (:url "https://github.com/dajva/rg.el")
  :defer t
  :config
  (keymap-set rg-mode-map "e" #'rg-rerun-change-regexp)
  (keymap-set rg-mode-map "r" #'wgrep-change-to-wgrep-mode))

;; Bind N and P in ediff so that I don't leave the control buffer
(defun my-ediff-next-difference (&rest _args)
  (interactive)
  (save-selected-window
    (call-interactively 'ediff-next-difference)))

(defun my-ediff-previous-difference (&rest _args)
  (interactive)
  (save-selected-window
    (call-interactively 'ediff-previous-difference)))

(defun my-ediff-extra-keys ()
  (keymap-set ediff-mode-map "N" #'my-ediff-next-difference)
  (keymap-set ediff-mode-map "P" #'my-ediff-previous-difference))

(use-package ediff
  :ensure nil
  :defer t
  :custom
  (ediff-window-setup-function 'ediff-setup-windows-plain)
  :hook (ediff-keymap-setup . my-ediff-extra-keys))

(eval-when-compile
  (require 'texinfo nil t))
;; Make TexInfo easier to work with
(defun my-texinfo-view-file ()
  "View the published version of the current file."
  (interactive)
  (let ((file (buffer-file-name)))
    (when (string-match "\\.tex\\(i\\|info\\)?\\'" file)
      (setq file (replace-match ".info" t t file))
      (when (buffer-live-p (get-buffer "*info*"))
        (kill-buffer "*info*"))
      (info file))))

(defun my-texinfo-extra-keys ()
  "Make texinfo stuff easier to work with."
  (keymap-set texinfo-mode-map "C-c C-p" #'makeinfo-buffer)
  (keymap-set texinfo-mode-map "C-c C-v" #'my-texinfo-view-file))
(add-hook 'texinfo-mode-hook #'my-texinfo-extra-keys t)

;; Make tramp's backup directories the same as the normal ones
(setopt tramp-backup-directory-alist backup-directory-alist)

;; Navigate the kill ring when doing M-y
(use-package browse-kill-ring
  :vc (:url "https://github.com/browse-kill-ring/browse-kill-ring")
  :demand t
  :config
  (browse-kill-ring-default-keybindings))

;; extension of mine to make list editing easy
(use-package edit-list
  :ensure nil
  :demand t
  :config
  (defalias 'my-edit-list 'edit-list))

;; Packages that need :ensure but have no complex config
(use-package s
  :vc (:url "https://github.com/magnars/s.el")
  :defer t)

(use-package add-node-modules-path
  :vc (:url "https://github.com/codesuki/add-node-modules-path")
  :defer t)
(use-package archive-rpm
  :vc (:url "https://github.com/nbarrientos/archive-rpm")
  :defer t)
(use-package astro-ts-mode
  :vc (:url "https://github.com/Sorixelle/astro-ts-mode")
  :defer t)
(use-package basic-mode
  :vc (:url "https://github.com/dykstrom/basic-mode"
       :lisp-dir "src")
  :defer t)
(use-package clojure-ts-mode
  :vc (:url "https://github.com/clojure-emacs/clojure-ts-mode")
  :defer t)
(use-package color-theme-sanityinc-tomorrow
  :vc (:url "https://github.com/purcell/color-theme-sanityinc-tomorrow")
  :defer t)
(use-package diminish
  :vc (:url "https://github.com/myrjola/diminish.el")
  :defer t)
(use-package edit-indirect
  :vc (:url "https://github.com/Fanael/edit-indirect")
  :defer t)
(use-package el-mock
  :vc (:url "https://github.com/rejeep/el-mock.el")
  :defer t)
(use-package fish-mode
  :vc (:url "https://github.com/wwwjfy/emacs-fish")
  :defer t)
(use-package flx
  :vc (:url "https://github.com/lewang/flx")
  :defer t)
(use-package git-modes
  :vc (:url "https://github.com/magit/git-modes")
  :defer t)
(use-package graphql-ts-mode
  :vc (:url "https://git.sr.ht/~joram/graphql-ts-mode")
  :defer t)
(use-package hydra
  :vc (:url "https://github.com/abo-abo/hydra")
  :defer t)
(use-package jtsx
  :vc (:url "https://github.com/llemaitre19/jtsx")
  :defer t)
(use-package kdl-mode
  :vc (:url "https://github.com/taquangtrung/emacs-kdl-mode")
  :defer t)
(use-package kotlin-ts-mode
  :vc (:url "https://gitlab.com/bricka/emacs-kotlin-ts-mode")
  :defer t)
(use-package lua-mode
  :vc (:url "https://github.com/immerrr/lua-mode")
  :defer t)
(use-package nix-ts-mode
  :vc (:url "https://github.com/nix-community/nix-ts-mode")
  :defer t)
(use-package nsis-mode
  :vc (:url "https://github.com/mwolson/nsis-mode")
  :defer t)
(use-package prisma-ts-mode
  :vc (:url "https://github.com/nverno/prisma-ts-mode")
  :defer t)
(use-package rainbow-delimiters
  :vc (:url "https://github.com/Fanael/rainbow-delimiters")
  :defer t)
(use-package reformatter
  :vc (:url "https://github.com/purcell/emacs-reformatter")
  :defer t)
(use-package swift-ts-mode
  :vc (:url "https://github.com/rechsteiner/swift-ts-mode")
  :defer t)
(use-package hcl-mode
  :vc (:url "https://github.com/hcl-emacs/hcl-mode")
  :defer t)
(use-package terraform-mode
  :vc (:url "https://github.com/syohex/emacs-terraform-mode")
  :defer t)
(use-package tmux-mode
  :vc (:url "https://github.com/nverno/tmux-mode")
  :defer t)
(use-package vcl-mode :defer t)
(use-package zig-ts-mode
  :vc (:url "https://codeberg.org/meow_king/zig-ts-mode")
  :defer t)

;; All programming modes
(defun my-turn-on-display-line-numbers-mode ()
  (interactive)
  (unless (derived-mode-p 'special-mode)
    (display-line-numbers-mode 1)))

(defun my-turn-off-display-line-numbers-mode ()
  (interactive)
  (display-line-numbers-mode -1))

(add-hook 'conf-mode-hook #'my-turn-on-display-line-numbers-mode t)
(add-hook 'prog-mode-hook #'my-turn-on-display-line-numbers-mode t)
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode t)
(my-around-advice
 '(my-turn-on-display-line-numbers-mode rainbow-delimiters-mode)
 #'my-inhibit-in-indirect-md-buffers)

(add-hook 'lisp-interaction-mode-hook #'my-turn-off-display-line-numbers-mode t)

(jka-compr-update)

;; tmux support
(add-to-list 'auto-mode-alist '("\\.?tmux\\.conf\\(\\.[^.]+\\)?\\'" . tmux-mode))

;; Profiling
(use-package profiler
  :ensure nil
  :demand t)
(cl-defmacro my-with-cpu-profiling (&rest body)
  `(unwind-protect
       (progn
         (ignore-errors (profiler-cpu-log))
         (profiler-cpu-start profiler-sampling-interval)
         ,@body)
     (profiler-report)
     (profiler-stop)))

(defun my-profiler-stop-and-report ()
  (interactive)
  (profiler-report)
  (profiler-stop))

(keymap-global-set "<f8> 1" #'profiler-start)
(keymap-global-set "<f8> 2" #'my-profiler-stop-and-report)

;; Setup info for manually compiled packages
(add-to-list 'Info-default-directory-list (concat my-emacs-path "share/info"))

;; Magit settings
(use-package git-commit
  :ensure nil
  :defer t
  :custom
  (git-commit-major-mode 'org-mode)
  (git-commit-summary-max-length 120)
  :config
  (remove-hook 'git-commit-setup-hook #'git-commit-turn-on-auto-fill))

(defun my-magit-balance-windows-25/75 (top-window bottom-window)
  (let* ((total-height (+ (window-total-height top-window)
                          (window-total-height bottom-window)))
         (min-height window-min-height)
         (desired-top-height
          (max min-height
               (min (round (* total-height 0.25))
                    (- total-height min-height))))
         (delta (- desired-top-height
                   (window-total-height top-window))))
    (unless (zerop delta)
      (window-resize top-window delta nil 'safe))))

(defun my-magit-display-buffer-below-selected (buffer alist)
  (let ((top-window (selected-window)))
    (when-let* ((window (or (display-buffer-reuse-window buffer alist)
                            (display-buffer-below-selected buffer alist))))
      (when (and (not (eq top-window window))
                 (eq top-window (window-in-direction 'above window)))
        (my-magit-balance-windows-25/75 top-window window))
      window)))

(defun my-magit-display-buffer-top25-bottom75-v1 (buffer)
  (let ((detail-buffer-p
         (with-current-buffer buffer
           (derived-mode-p 'magit-diff-mode
                           'magit-process-mode
                           'magit-revision-mode
                           'magit-stash-mode)))
        (current-detail-p
         (derived-mode-p 'magit-diff-mode
                         'magit-process-mode
                         'magit-revision-mode
                         'magit-stash-mode)))
    (display-buffer
     buffer
     (cond ((eq (with-current-buffer buffer major-mode)
                'magit-status-mode)
            '(magit--display-buffer-fullcolumn))
           (detail-buffer-p
            (if current-detail-p
                '(display-buffer-same-window)
              '(my-magit-display-buffer-below-selected)))
           ('(display-buffer-same-window))))))

;; Don't overwrite M-w in magit mode, and clear mark when done
(defun my-magit-kill-ring-save ()
  (interactive)
  (call-interactively #'kill-ring-save)
  (deactivate-mark))

(use-package magit
  :defer t
  :custom
  (magit-define-global-key-bindings nil)
  (magit-diff-refine-hunk t)
  (magit-display-buffer-function 'my-magit-display-buffer-top25-bottom75-v1)
  (magit-log-section-commit-count 1)
  (magit-prefer-remote-upstream t)
  :config
  (keymap-set magit-mode-map "M-w" #'my-magit-kill-ring-save)
  (keymap-set magit-diff-section-map "RET" #'magit-diff-visit-worktree-file)
  (keymap-set magit-hunk-section-map "RET" #'magit-diff-visit-worktree-file))

(defun my-preload-magit ()
  (require 'magit)
  (require 'git-commit))

(my-defer-startup #'my-preload-magit)

(put 'checkdoc-allow-quoting-nil-and-t 'safe-local-variable 'booleanp)

;; Map some magit keys globally
(keymap-global-set "C-x V" nil)
(keymap-global-set "C-x V a" #'magit-blame)
(keymap-global-set "C-x V b" #'magit-show-refs-current)
(keymap-global-set "C-x V f" #'magit-file-dispatch)
(keymap-global-set "C-x V j" #'my-majutsu-log)
(keymap-global-set "C-x V l" #'magit-log-head)
(keymap-global-set "C-x V s" #'magit-status)
(keymap-global-set "C-x V v" #'magit-dispatch)

;; Don't display any minor modes on the mode-line
(use-package minions
  :vc (:url "https://github.com/tarsius/minions")
  :demand t
  :custom
  (minions-mode-line-delimiters '("" . ""))
  (minions-mode-line-lighter " ")
  :config
  (minions-mode 1))

(eval-when-compile
  (require 'org nil t))
;; Org Mode settings
(defun my-org-find-notes-file ()
  (interactive)
  (require 'org)
  (find-file org-default-notes-file))

(defun my-org-capture-note ()
  (interactive)
  (require 'org-capture)
  (org-capture nil "n"))

(use-package toc-org
  :vc (:url "https://github.com/snosov1/toc-org")
  :defer t)

(use-package org
  :ensure nil
  :defer t
  :custom
  (org-capture-templates
   '(("n" "Note" entry (file+headline "" "Notes") "* %?" :prepend t :empty-lines-after 1)))
  (org-default-notes-file "~/Documents/notes.org")
  (org-startup-folded nil)
  (org-startup-truncated nil)
  (org-yank-folded-subtrees nil)
  :config
  (require 'toc-org)
  (put 'toc-org-max-depth 'safe-local-variable 'integerp)
  (add-to-list 'safe-local-variable-values
               '(eval and (fboundp 'toc-org-mode) (toc-org-mode 1)))

  (keymap-set org-mode-map "M-<left>" #'left-word)
  (keymap-set org-mode-map "M-<right>" #'right-word))

;; Project settings
(defvar my-project-command-map
  (let ((map (make-sparse-keymap)))
    (keymap-set map "a a" #'project-remember-projects-under)
    (keymap-set map "c" #'project-compile)
    (keymap-set map "f" #'project-find-file)
    (keymap-set map "g g" #'my-gptel-start)
    (keymap-set map "g p" #'gptel-menu)
    (keymap-set map "g s" #'gptel-send)
    (keymap-set map "k" #'project-kill-buffers)
    (keymap-set map "n" #'my-org-find-notes-file)
    (keymap-set map "p" #'project-switch-project)
    (keymap-set map "t" #'project-switch-to-buffer)
    (keymap-set map "s r" #'rg-project)
    (keymap-set map "s s" #'my-consult-ripgrep)
    (keymap-set map "w p" #'my-copy-project-relative-path-of-current-buffer)
    (keymap-set map "w w" #'my-copy-path-of-current-buffer)
    (keymap-set map "!" #'project-async-shell-command)
    (keymap-set map "SPC" #'my-org-capture-note)
    map))

(keymap-global-set "C-c p" my-project-command-map)
(keymap-global-set "C-c C-p" my-project-command-map)

;; Other keybinds
(defun my-other-frame-or-window (&rest _args)
  (interactive)
  (let* ((old-window (selected-window))
         (window (next-window old-window)))
    (if (and (eq window old-window)
             window-system
             (string= (getenv "XDG_CURRENT_DESKTOP") "niri"))
        (call-interactively #'other-frame)
      (call-interactively #'other-window))))

(keymap-global-set "C-x g" #'mark-whole-buffer)
(keymap-global-set "C-x r r" #'rectangle-mark-mode)
(keymap-global-set "C-x o" #'my-other-frame-or-window)
(keymap-global-set "C-x p" #'my-other-frame-or-window)

(defun my-kill-emacs ()
  (interactive)
  (let* ((confirm-kill-emacs 'y-or-n-p)
         (cur-proc (frame-parameter nil 'client))
         (all-terminals (cl-remove-if-not
                         (lambda (term)
                           (terminal-live-p term))
                         (terminal-list)))
         (all-procs (cl-remove-if-not
                     (lambda (proc)
                       (terminal-live-p (process-get proc 'terminal)))
                     server-clients))
         (other-graphics (cl-remove-if
                          (lambda (proc)
                            (or (eq proc cur-proc)
                                (let ((frame (process-get proc 'frame)))
                                  (frame-parameter frame 'tty))))
                          all-procs))
         (kill-cmd (if (and (not window-system)
                            cur-proc
                            (processp cur-proc)
                            (or (and (> (length all-procs) 1)
                                     (>= (length other-graphics) 1))
                                (> (length all-terminals) 1)))
                       'save-buffers-kill-terminal
                     'save-buffers-kill-emacs)))
    (call-interactively kill-cmd t)))

(keymap-global-set "C-x C-c" #'my-kill-emacs)
(keymap-global-set "C-x C-M-s" #'my-kill-emacs)

;; Map some keys to find-function/find-variable
(defvar my-find-things-map
  (let ((map (make-sparse-keymap)))
    (keymap-set map "f" #'find-function)
    (keymap-set map "v" #'find-variable)
    (keymap-set map "l" #'find-library)
    (keymap-set map "a" #'find-face-definition)
    map)
  "My key customizations for find-function and related things.")

(keymap-global-set "C-x F" my-find-things-map)
(keymap-global-set "C-x f" my-find-things-map)

(use-package view
  :ensure nil
  :defer t
  :config
  (keymap-set view-mode-map "q" 'bury-buffer)
  (keymap-set view-mode-map "DEL" 'View-scroll-page-backward))

(use-package info
  :ensure nil
  :defer t
  :config
  (keymap-set Info-mode-map "DEL" 'Info-scroll-down))

;; diff-mode: Don't mess with M-q
(use-package diff-mode
  :ensure nil
  :defer t
  :config
  (keymap-set diff-mode-map "M-q" 'fill-paragraph))

;; Show keybind options while typing leading keys
(use-package which-key
  :ensure nil
  :defer t
  :init
  (my-defer-startup #'which-key-mode))

;; Typo prevention
(keymap-global-set "C-h C-n" #'describe-gnu-project)
(keymap-global-set "C-x 4" #'split-window-right)
(keymap-global-set "C-x C-b" #'consult-buffer)
(keymap-global-set "C-x C-d" #'find-file)
(keymap-global-set "C-x n" #'consult-buffer)

;; Disable some keybinds to avoid typos
(keymap-global-set "<insert>" (lambda () (interactive)))
(keymap-global-set "<insertchar>" (lambda () (interactive)))
(keymap-global-set "C-h g" (lambda () (interactive)))
(keymap-global-set "C-t" (lambda () (interactive)))
(keymap-global-set "C-z" (lambda () (interactive)))
(keymap-global-set "C-x C-z" (lambda () (interactive)))
(keymap-global-set "<mouse-2>" (lambda () (interactive)))
(keymap-global-set "<mouse-3>" (lambda () (interactive)))

;; Bind Apple-<key> to Alt-<key> for some Mac keys
(when (and my-remap-cmd-key-p (eq system-type 'darwin))
  (setopt ns-alternate-modifier 'meta)
  (setopt ns-command-modifier 'super))

(eval-when-compile
  (require 'git-rebase nil t))
(use-package git-rebase
  :ensure nil
  :defer t)
(defun my-set-super-bindings ()
  (interactive)
  (with-eval-after-load "cider-repl"
    (keymap-set cider-repl-mode-map "s-n" #'cider-repl-next-input)
    (keymap-set cider-repl-mode-map "s-p" #'cider-repl-previous-input))

  (with-eval-after-load "git-rebase"
    (keymap-set git-rebase-mode-map "s-n" #'git-rebase-move-line-down)
    (keymap-set git-rebase-mode-map "s-p" #'git-rebase-move-line-up))

  (with-eval-after-load "magit"
    (keymap-set magit-mode-map "s-1" #'magit-section-show-level-1-all)
    (keymap-set magit-mode-map "s-2" #'magit-section-show-level-2-all)
    (keymap-set magit-mode-map "s-3" #'magit-section-show-level-3-all)
    (keymap-set magit-mode-map "s-4" #'magit-section-show-level-4-all)
    (keymap-set magit-status-mode-map "s-c" #'my-magit-kill-ring-save)
    (keymap-set magit-status-mode-map "s-w" #'my-magit-kill-ring-save))

  (keymap-global-set "s-:" #'eval-expression)
  (keymap-global-set "s-;" #'eval-expression)
  (keymap-global-set "s-<" #'beginning-of-buffer)
  (keymap-global-set "s-," #'beginning-of-buffer)
  (keymap-global-set "s->" #'end-of-buffer)
  (keymap-global-set "s-." #'end-of-buffer)
  (keymap-global-set "s-<left>" #'left-word)
  (keymap-global-set "s-<right>" #'right-word)
  (keymap-global-set "s-1" #'shell-command)
  (keymap-global-set "s-!" #'shell-command)
  (keymap-global-set "s-$" #'ispell-word)
  (keymap-global-set "s-a" #'mark-whole-buffer)
  (keymap-global-set "s-c" #'kill-ring-save)
  (keymap-global-set "s-g" my-consult-M-g-map)
  (keymap-global-set "s-m" (lambda () (interactive)))
  (keymap-global-set "M-s-p" #'project-switch-project)
  (keymap-global-set "s-p" #'project-find-file)
  (keymap-global-set "s-q" #'my-kill-emacs)
  (keymap-global-set "s-w" #'kill-ring-save)
  (keymap-global-set "s-v" #'yank)
  (keymap-global-set "s-x" #'execute-extended-command)
  (keymap-global-set "s-y" #'consult-yank-pop)
  (keymap-global-set "C-s-<left>" #'backward-sexp)
  (keymap-global-set "C-s-<right>" #'forward-sexp)
  (keymap-global-set "C-s-n" #'forward-list)
  (keymap-global-set "C-s-p" #'backward-list)
  (keymap-global-set "C-s-x" #'eval-defun)
  (keymap-global-set "C-s-\\" #'indent-region))

(defun my-set-mac-bindings ()
  (interactive)
  (keymap-global-set "<home>" #'beginning-of-line)
  (keymap-global-set "<end>" #'end-of-line)
  (keymap-global-set "s-<up>" #'scroll-down-command)
  (keymap-global-set "s-<down>" #'scroll-up-command)
  (keymap-global-set "s-<left>" #'beginning-of-line)
  (keymap-global-set "s-<right>" #'end-of-line))

(defun my-init-client-keys ()
  (interactive)
  (when (and my-remap-cmd-key-p (memq window-system '(ns pgtk x)))
    (my-set-super-bindings))
  (when (and my-remap-cmd-key-p (eq window-system 'ns))
    (my-set-mac-bindings)))

(add-hook 'server-after-make-frame-hook #'my-init-client-keys t)
(my-init-client-keys)

;; Change to home dir
(defun my-change-to-default-dir ()
  (interactive)
  (setq-default default-directory (expand-file-name my-default-directory))
  (setq default-directory (expand-file-name my-default-directory)))

(add-hook 'after-init-hook #'my-change-to-default-dir t)

;; Start server
(when my-server-start-p (server-start))

;; Open a few frequently-used files
(mapc #'find-file-noselect my-recent-files)

;; Kill the startup screen that we displayed earlier
(defun my-kill-splash-screen ()
  (interactive)
  (let ((buf (get-buffer "*GNU Emacs*")))
    (when (and buf (buffer-live-p buf))
      (kill-buffer buf))))

(add-hook 'after-init-hook #'my-kill-splash-screen t)

(provide 'shared-init)
;;; shared-init.el ends here
