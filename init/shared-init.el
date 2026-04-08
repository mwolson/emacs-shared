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
  :vc (:url "https://github.com/purcell/inheritenv"
       :main-file "inheritenv.el")
  :defer t)

(use-package mise
  :vc (:url "https://github.com/eki3z/mise.el"
       :main-file "mise.el")
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
  :vc (:url "https://github.com/tarsius/cond-let"
       :main-file "cond-let.el")
  :defer t)
(use-package llama
  :vc (:url "https://github.com/tarsius/llama"
       :main-file "llama.el")
  :defer t)
(use-package macrostep
  :vc (:url "https://github.com/emacsorphanage/macrostep"
       :main-file "macrostep.el"
       :compile-files '("macrostep-c.el"))
  :defer t)
(use-package magit-section
  :vc (:url "https://github.com/magit/magit"
       :main-file "magit-section.el" :lisp-dir "lisp")
  :defer t)
(use-package parseclj
  :vc (:url "https://github.com/clojure-emacs/parseclj"
       :main-file "parseclj.el"
       :compile-files '("parseclj-alist.el"
                        "parseclj-ast.el"
                        "parseclj-lex.el"
                        "parseclj-parser.el"))
  :defer t)
(use-package parseedn
  :vc (:url "https://github.com/clojure-emacs/parseedn"
       :main-file "parseedn.el")
  :defer t)
(use-package sesman
  :vc (:url "https://github.com/vspinu/sesman"
       :main-file "sesman.el"
       :compile-files '("sesman-browser.el"))
  :defer t)
(use-package spinner
  :vc (:url "https://github.com/Malabarba/spinner.el"
       :main-file "spinner.el")
  :defer t)
(use-package transient
  :vc (:url "https://github.com/magit/transient"
       :main-file "transient.el")
  :defer t
  :config
  (transient-bind-q-to-quit))
(use-package websocket
  :vc (:url "https://github.com/ahyatt/emacs-websocket"
       :main-file "websocket.el")
  :defer t)
(use-package with-editor
  :vc (:url "https://github.com/magit/with-editor"
       :lisp-dir "lisp" :main-file "with-editor.el")
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

(defun my-inhibit-in-git-commit-buffers (orig-fun &rest args)
  "Don't run ORIG-FUN (with ARGS) in git commit buffers."
  (unless (derived-mode-p 'my-git-commit-gfm-mode)
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
  :vc (:url "https://github.com/radian-software/apheleia"
       :main-file "apheleia.el"
       :compile-files '("apheleia-*.el"))
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
  :vc (:url "https://github.com/alpha22jp/atomic-chrome"
       :main-file "atomic-chrome.el")
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
  (my-around-advice #'eglot-ensure #'my-inhibit-in-git-commit-buffers)
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
  :vc (:url "https://github.com/redguardtoo/js-comint"
       :main-file "js-comint.el")
  :commands (js-comint-send-string)
  :defer t)

(eval-when-compile
  (require 'treesit nil t))

(defun my-treesit-mark-defun (&optional steps)
  "Put mark at end of this function, point at beginning, for a treesit mode.

If STEPS is negative, mark `- arg - 1` extra functions backward.
The behavior for when STEPS is positive is not currently well-defined."
  (interactive)
  (treesit-end-of-defun)
  (let ((pt-max (point)))
    (treesit-beginning-of-defun 1)
    (setq steps (1- (- 0 (or steps 0))))
    (while (> steps 0)
      (treesit-beginning-of-defun 1)
      (cl-decf steps))
    (set-mark (point))
    (goto-char pt-max)))

(defun my-js-comint-send-defun (start end)
  "Send the function at point to the inferior Javascript process."
  (interactive "r")
  (unless (region-active-p)
    (save-mark-and-excursion
      (my-treesit-mark-defun)
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
  :vc (:url "https://github.com/dgutov/diff-hl"
       :main-file "diff-hl.el"
       :compile-files '("diff-hl-dired.el"
                        "diff-hl-margin.el"))
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
  :vc (:url "https://github.com/protesilaos/pulsar"
       :main-file "pulsar.el")
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
    (remove-hook 'consult-after-jump-hook #'recenter)
    (add-hook 'consult-after-jump-hook #'my-consult-recenter-for-preview t)
    (add-hook 'consult-after-jump-hook #'pulsar-reveal-entry t)))

(defvar my-icomplete-prospects-height 10
  "Number of completion candidates to show in icomplete-vertical-mode.
Also controls the page size for PgUp/PgDn in the minibuffer.")

(defun my-consult-recenter-for-preview ()
  "Recenter point for consult preview, preserving position when possible.
When consult-line previews the original position (empty minibuffer),
skip recentering entirely. Otherwise, only recenter if the cursor is
too close to the bottom of the window to leave room for the minibuffer
and icomplete candidates; if it has enough room, leave it in place."
  (if (and my-consult-line-start-pos
           (string-empty-p (minibuffer-contents-no-properties)))
      nil
    (let* ((ppos (posn-at-point))
           (cursor-row (and ppos (cdr (posn-actual-col-row ppos))))
           (win-height (window-body-height))
           (mb-height (window-body-height (minibuffer-window)))
           (mb-offset (/ mb-height 2)))
      (if (not cursor-row)
          (recenter (+ (/ win-height 2) mb-offset))
        (let* ((reserved (+ mb-height my-icomplete-prospects-height 4))
               (max-allowed-row (max 0 (- win-height reserved))))
          (when (> cursor-row max-allowed-row)
            (recenter (+ (/ win-height 2) mb-offset))))))
    (call-interactively #'pulsar-highlight-pulse)))

(my-defer-startup #'pulsar-global-mode)

;; SMerge mode, for editing files with inline diffs
(add-hook 'prog-mode-hook #'smerge-mode t)
(my-around-advice #'smerge-mode #'my-inhibit-in-indirect-md-buffers)

;; Tree-sitter
(use-package treesit
  :ensure nil
  :defer t
  :custom
  (treesit-font-lock-level 4))

;; Enable dumb-jump, which makes `C-c . .' jump to a function's definition
(use-package dumb-jump
  :vc (:url "https://github.com/jacktasia/dumb-jump"
       :main-file "dumb-jump.el")
  :demand t
  :custom
  (dumb-jump-selector 'completing-read)
  :config
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate))

(defvar my-xref-map
  (let ((map (make-sparse-keymap)))
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
(add-hook 'c-ts-base-mode-hook #'eglot-ensure t)
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

;; Elixir
(defun my-project-find-mix (dir)
  (when-let* ((root (locate-dominating-file dir "mix.exs")))
    (cons 'mix root)))

(add-to-list 'auto-mode-alist '("\\.exs?\\'" . elixir-ts-mode))
(add-to-list 'auto-mode-alist '("mix\\.lock\\'" . elixir-ts-mode))
(add-to-list 'my-md-code-aliases '(elixir . elixir-ts-mode))
(add-to-list 'eglot-server-programs
             '((elixir-ts-mode heex-ts-mode)
               . ("elixir-ls")))
(add-hook 'elixir-ts-mode-hook #'eglot-ensure t)
(add-hook 'heex-ts-mode-hook #'eglot-ensure t)

;; Go
(defun my-project-find-go-module (dir)
  (when-let* ((root (locate-dominating-file dir "go.mod")))
    (cons 'go-module root)))

(add-to-list 'auto-mode-alist '("\\.go\\'" . go-ts-mode))
(add-to-list 'auto-mode-alist '("/go\\.mod\\'" . go-mod-ts-mode))
(add-to-list 'my-md-code-aliases '(go . go-ts-mode))
(add-hook 'go-ts-mode-hook #'eglot-ensure t)
(add-hook 'go-mod-ts-mode-hook #'eglot-ensure t)

;; GraphQL
(add-to-list 'auto-mode-alist '("\\.\\(graphql\\|gql\\)\\'" . graphql-ts-mode))
(add-hook 'graphql-ts-mode-hook #'my-apheleia-set-js-formatter t)

;; HTML
(add-to-list 'major-mode-remap-alist '(html-mode . html-ts-mode))

(add-hook 'html-ts-mode-hook #'eglot-ensure t)
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
  (add-hook 'java-ts-mode-hook #'eglot-ensure t))

;; JSON
(add-to-list 'major-mode-remap-alist '(json-mode . json-ts-mode))
(add-to-list 'auto-mode-alist '("\\.jsonc?\\'" . json-ts-mode))
(add-hook 'json-ts-mode-hook #'add-node-modules-path t)
(add-hook 'json-ts-mode-hook #'eglot-ensure t)
(add-hook 'json-ts-mode-hook #'my-setup-web-ligatures t)
(add-hook 'json-ts-mode-hook #'my-apheleia-set-js-formatter t)

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
  :vc (:url "https://github.com/mwolson/eglot-typescript-preset"
       :main-file "eglot-typescript-preset.el")
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
    (add-hook hook #'my-apheleia-set-js-formatter t)))

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

(add-hook 'kdl-mode-hook #'my-setup-kdl-mode t)

;; Kotlin
(add-to-list 'auto-mode-alist '("\\.kts?\\'" . kotlin-ts-mode))
(add-to-list 'my-md-code-aliases '(kotlin . kotlin-ts-mode))

;; Lisp
(use-package slime
  :vc (:url "https://github.com/slime/slime"
       :main-file "slime.el"
       :compile-files '("contrib/*.el"
                        "lib/*.el"))
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
  :vc (:url "https://github.com/jrblevin/markdown-mode"
       :main-file "markdown-mode.el")
  :defer t
  :hook ((markdown-mode . add-node-modules-path)
         ;; disabled since file-completion in links doesn't work yet
         ;; (markdown-mode . eglot-ensure)
         (markdown-mode . my-setup-web-ligatures)
         (markdown-mode . my-turn-on-arrow-input)
         (markdown-mode . my-apheleia-set-markdown-formatter))
  :custom
  (markdown-command "npx marked")
  (markdown-enable-wiki-links t)
  (markdown-fontify-code-blocks-natively t)
  (markdown-spaces-after-code-fence 0)
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
 '((gfm-mode my-mdx-mode)
   . ("vscode-markdown-language-server" "--stdio"
      :initializationOptions
      (:markdownFileExtensions ["md" "mdx"]))))

;; MDX
(define-derived-mode my-mdx-mode gfm-mode "MDX"
  "Major mode for highlighting MDX files.")

(add-to-list 'auto-mode-alist '("\\.mdx\\'" . my-mdx-mode))
(add-to-list 'my-md-code-aliases '(mdx . my-mdx-mode))

;; Mermaid diagrams
(use-package mermaid-ts-mode
  :vc (:url "https://github.com/kiennq/mermaid-ts-mode"
       :main-file "mermaid-ts-mode.el")
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
  :vc (:url "https://github.com/mwolson/eglot-python-preset"
       :main-file "eglot-python-preset.el")
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
(add-hook 'rust-ts-mode-hook #'eglot-ensure t)

;; SCSS
(use-package flymake-stylelint
  :vc (:url "https://github.com/orzechowskid/flymake-stylelint"
       :main-file "flymake-stylelint.el")
  :defer t)

(add-hook 'scss-mode-hook #'add-node-modules-path t)
(add-hook 'scss-mode-hook #'flymake-stylelint-enable t)
(my-around-advice #'flymake-stylelint-enable
                  #'my-inhibit-in-indirect-md-buffers)

(add-to-list 'eglot-server-programs
             '((scss-mode)
               . ("vscode-css-language-server" "--stdio")))
(add-hook 'scss-mode-hook #'eglot-ensure t)

;; Svelte
(use-package svelte-ts-mode
  :vc (:url "https://github.com/leafOfTree/svelte-ts-mode"
       :main-file "svelte-ts-mode.el")
  :mode "\\.svelte\\'")

(add-hook 'svelte-ts-mode-hook #'add-node-modules-path t)
(add-hook 'svelte-ts-mode-hook #'eglot-ensure t)
(add-hook 'svelte-ts-mode-hook #'my-setup-web-ligatures t)
(add-hook 'svelte-ts-mode-hook #'my-apheleia-set-js-formatter t)

;; Swift
(add-to-list 'auto-mode-alist '("\\.swift\\(interface\\)?\\'" . swift-ts-mode))
(add-to-list 'my-md-code-aliases '(swift . swift-ts-mode))

;; TOML
(add-to-list 'auto-mode-alist '("\\.toml\\'" . toml-ts-mode))

;; Vue
(use-package vue-ts-mode
  :vc (:url "https://github.com/8uff3r/vue-ts-mode"
       :main-file "vue-ts-mode.el")
  :mode "\\.vue\\'")

(add-hook 'vue-ts-mode-hook #'add-node-modules-path t)
(add-hook 'vue-ts-mode-hook #'eglot-ensure t)
(add-hook 'vue-ts-mode-hook #'my-setup-web-ligatures t)
(add-hook 'vue-ts-mode-hook #'my-apheleia-set-js-formatter t)

(eval-when-compile
  (require 'css-mode nil t))
(defun my-vue-ts-set-fontify-css-colors ()
  (require 'css-mode)
  (setq-local font-lock-fontify-region-function #'css--fontify-region))

(with-eval-after-load "vue-ts-mode"
  (add-hook 'vue-ts-mode-hook #'my-vue-ts-set-fontify-css-colors t))

;; Web Mode
(use-package web-mode
  :vc (:url "https://github.com/fxbois/web-mode"
       :main-file "web-mode.el")
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
(add-hook 'yaml-ts-mode-hook #'my-apheleia-set-yaml-formatter t)
(add-to-list 'auto-mode-alist '("\\.ya?ml\\'" . yaml-ts-mode))

;; Zig
(add-to-list 'auto-mode-alist '("\\.zig\\'" . zig-ts-mode))
(add-to-list 'my-md-code-aliases '(zig . zig-ts-mode))
(add-to-list 'eglot-server-programs
             '((zig-ts-mode)
               . ("zls" :initializationOptions ())))
(add-hook 'zig-ts-mode-hook #'eglot-ensure t)

;; Consult, Embark, icomplete, completion-preview
(defvar my-minibuffer-from-consult-line nil)
(defvar my-consult-line-start-pos nil)

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
        (my-minibuffer-from-consult-line t)
        (my-consult-line-start-pos (point)))
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
  :vc (:url "https://github.com/minad/consult"
       :main-file "consult.el"
       :compile-files '("consult-*.el"))
  :defer t)

(use-package embark
  :vc (:url "https://github.com/oantolin/embark"
       :main-file "embark.el"
       :compile-files '("embark-org.el"))
  :defer t)

(use-package embark-consult
  :vc (:url "https://github.com/oantolin/embark"
       :main-file "embark-consult.el")
  :defer t
  :after (embark consult))

(use-package nerd-icons
  :vc (:url "https://github.com/rainstormstudio/nerd-icons.el"
       :main-file "nerd-icons.el"
       :compile-files '("data/nerd-icons-data-*.el"
                        "nerd-icons-data.el"
                        "nerd-icons-faces.el"))
  :defer t)

(use-package nerd-icons-completion
  :vc (:url "https://github.com/rainstormstudio/nerd-icons-completion"
       :main-file "nerd-icons-completion.el")
  :defer t)

(defun my-extended-command-predicate (symbol buffer)
  (and (command-completion-default-include-p symbol buffer)
       (transient-command-completion-not-suffix-only-p symbol buffer)))

(setq completion-category-defaults nil
      completion-category-overrides '((consult-location (styles substring basic partial-completion flex))
                                      (file (styles basic partial-completion)))
      completion-auto-help nil
      completion-ignore-case t
      completion-styles '(basic substring partial-completion flex)
      completions-detailed t
      consult-async-min-input 2
      consult-async-input-debounce 0.1
      consult-async-input-throttle 0.2
      consult-async-refresh-delay 0.15
      prefix-help-command #'embark-prefix-help-command
      read-buffer-completion-ignore-case t
      read-extended-command-predicate #'my-extended-command-predicate
      read-file-name-completion-ignore-case t
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
               (prospects-height my-icomplete-prospects-height)
               ;; 4 = 1 minibuffer input line + 3 rows of context
               (max-allowed-row (max 0 (- rows prospects-height 4)))
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
        (setq my-minibuffer-restore-pos nil)
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

(my-defer-startup #'my-load-icomplete)

(defun my-minibuffer-insert-last-history ()
  "Insert the most recent minibuffer history item, replacing current input."
  (interactive)
  (when-let* ((hist (symbol-value minibuffer-history-variable))
              (last (car hist)))
    (delete-minibuffer-contents)
    (insert last)))

(dolist (map (list minibuffer-local-map read-expression-map))
  (keymap-set map "C-c C-c" #'my-minibuffer-insert-last-history)
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

(keymap-global-set "C-c M-x" #'consult-mode-command)
(keymap-global-set "C-h b" #'embark-bindings)
(keymap-global-set "C-r" #'my-consult-line)
(keymap-global-set "C-s" #'my-consult-line)
(keymap-global-set "C-x b" #'consult-buffer)
(keymap-global-set "M-g" my-consult-M-g-map)
(keymap-global-set "M-y" #'consult-yank-pop)

;; icomplete-vertical-mode (replaces vertico), savehist (replaces prescient),
;; completion-preview-mode (replaces corfu), dabbrev (replaces cape)
(eval-when-compile
  (require 'dabbrev nil t)
  (require 'icomplete nil t))

(defun my-icomplete-tab ()
  "Complete to longest common prefix, advancing one fork point at a time.
When the input can be expanded to a longer common prefix among all
candidates, do that. When already at the longest common prefix,
take one more character from the first candidate and re-expand."
  (interactive)
  (let* ((beg (icomplete--field-beg))
         (end (icomplete--field-end))
         (string (buffer-substring-no-properties beg end))
         (md (completion-metadata
              string
              minibuffer-completion-table
              minibuffer-completion-predicate))
         (comp (completion-try-completion
                string
                minibuffer-completion-table
                minibuffer-completion-predicate
                (- (point) beg)
                md)))
    (cond
     ((null comp))
     ((eq t comp))
     (t
      (let ((completion (car comp))
            (comp-pos (cdr comp)))
        (if (not (string-equal completion string))
            (progn
              (completion--replace beg end completion)
              (goto-char (+ beg comp-pos)))
          ;; Already at longest common prefix: take one char from the first
          ;; sorted candidate to pass the fork, then re-expand.
          (let* ((all (completion-all-sorted-completions beg end))
                 (base-size (or (cdr (last all)) 0))
                 (firstnp (substring-no-properties (car all)))
                 ;; Skip past first completion if other items available and
                 ;; it's either a "./" or an exact match
                 (first (if (and (consp (cdr all))
                                 (or (string= firstnp
                                              (substring string base-size))
                                     (string= (directory-file-name firstnp)
                                              ".")))
                            (cadr all)
                          (car all)))
                 (suffix-len (- (length string) base-size)))
            (when (and (consp all) (< suffix-len (length first)))
              (let* ((new-suffix (substring first 0 (1+ suffix-len)))
                     (new-string (concat (substring string 0 base-size)
                                         new-suffix))
                     (new-comp (completion-try-completion
                                new-string
                                minibuffer-completion-table
                                minibuffer-completion-predicate
                                (length new-string)
                                md)))
                (cond
                 ((consp new-comp)
                  (completion--replace beg end (car new-comp))
                  (goto-char (+ beg (cdr new-comp))))
                 (t
                  (completion--replace beg end new-string)
                  (goto-char (+ beg (length new-string))))))))))))))

(defun my-icomplete-ret ()
  "In file prompts, exit with exact input; otherwise select the candidate."
  (interactive)
  (let* ((beg (icomplete--field-beg))
         (end (icomplete--field-end))
         (md (completion-metadata (buffer-substring-no-properties beg end)
                                  minibuffer-completion-table
                                  minibuffer-completion-predicate))
         (category (completion-metadata-get md 'category))
         (all (completion-all-sorted-completions beg end)))
    (if (and (eq category 'file)
             (null all))
        (exit-minibuffer)
      (icomplete-force-complete-and-exit))))

(eval-when-compile (require 'help-fns nil t))

(defun my-help-symbol-affixation (completions)
  "Also show variable docstrings and current values in help completions."
  (mapcar (lambda (c)
            (let* ((s (intern c))
                   (doc (or (condition-case nil (documentation s) (error nil))
                            (documentation-property s 'variable-documentation)))
                   (doc (and doc (substring doc 0 (string-search "\n" doc))))
                   (val (and (boundp s)
                             (not (fboundp s))
                             (let* ((print-level 1) (print-length 3)
                                    (raw (format "%s" (symbol-value s))))
                               (if (> (length raw) 30)
                                   (concat (substring raw 0 27) "...")
                                 raw)))))
              (list c (propertize
                       (format "%-4s" (help--symbol-class s))
                       'face 'completions-annotations)
                    (concat
                     (if val (propertize (format " = %s" val)
                                         'face 'font-lock-type-face)
                       "")
                     (if doc (propertize (format " -- %s" doc)
                                         'face 'completions-annotations)
                       "")))))
          completions))

(advice-add 'help--symbol-completion-table-affixation
            :override #'my-help-symbol-affixation)

(defun my-icomplete-page-down ()
  "Move forward one page in icomplete completions."
  (interactive)
  (dotimes (_ (ceiling (1- (/ my-icomplete-prospects-height 2))))
    (icomplete-forward-completions)))

(defun my-icomplete-page-up ()
  "Move backward one page in icomplete completions."
  (interactive)
  (dotimes (_ (ceiling (1- (/ my-icomplete-prospects-height 2))))
    (icomplete-backward-completions)))

(defun my-icomplete-backspace ()
  "Delete backward, continuing while file completions stay unchanged.
In file-category completion, delete one character backward then
keep deleting while the sorted completion list stays identical
and at least one input character remains. In other categories,
delete a single character.

This works because `completion-all-sorted-completions' returns an
improper list (comp1 comp2 ... compN . base-size) and its cache
is flushed by `after-change-functions' on each `delete-char', so
successive calls produce fresh cons cells. `equal' compares
string contents (ignoring text properties) and the trailing
base-size, giving us the right semantics."
  (interactive)
  (let* ((beg (icomplete--field-beg))
         (end (icomplete--field-end)))
    (if (or (<= end beg)
            (not (eq 'file (completion-metadata-get
                            (completion-metadata
                             (buffer-substring-no-properties beg end)
                             minibuffer-completion-table
                             minibuffer-completion-predicate)
                            'category))))
        (delete-char -1)
      (let ((prev (completion-all-sorted-completions beg end)))
        (delete-char -1)
        (setq end (icomplete--field-end))
        (while (and (> end beg)
                    (equal prev
                           (completion-all-sorted-completions beg end)))
          (delete-char -1)
          (setq end (icomplete--field-end)))))))

(defun my-load-icomplete ()
  (require 'icomplete)
  (setopt icomplete-compute-delay 0
          icomplete-delay-completions-threshold 0
          icomplete-hide-common-prefix nil
          icomplete-in-buffer t
          icomplete-max-delay-chars 0
          icomplete-prospects-height my-icomplete-prospects-height
          icomplete-scroll t
          icomplete-show-matches-on-no-input t
          icomplete-tidy-shadowed-file-names t
          icomplete-with-completion-tables t)
  (let ((icvmm-map icomplete-vertical-mode-minibuffer-map))
    (keymap-set icvmm-map "C-s" #'icomplete-forward-completions)
    (keymap-set icvmm-map "C-r" #'icomplete-backward-completions)
    (keymap-set icvmm-map "C-k" #'kill-line)
    (keymap-set icvmm-map "?" #'minibuffer-completion-help)
    (keymap-set icvmm-map "TAB" #'my-icomplete-tab)
    (keymap-set icvmm-map "C-c C-o" #'embark-export)
    (keymap-set icvmm-map "RET" #'my-icomplete-ret)
    (keymap-set icvmm-map "M-RET" #'exit-minibuffer)
    (keymap-set icvmm-map "<backspace>" #'my-icomplete-backspace)
    (keymap-set icvmm-map "<prior>" #'my-icomplete-page-up)
    (keymap-set icvmm-map "<next>" #'my-icomplete-page-down))

  (keymap-set occur-mode-map "r" #'occur-edit-mode)

  (icomplete-vertical-mode 1)
  (nerd-icons-completion-mode 1)

  (setopt savehist-additional-variables
          '(kill-ring search-ring regexp-search-ring))
  (setopt savehist-file (expand-file-name "var/savehist"
                                          user-emacs-directory))
  (savehist-mode 1)

  (setopt completion-preview-idle-delay 0.3
          completion-preview-minimum-symbol-length 2
          global-completion-preview-modes '((not org-mode) prog-mode shell-mode))
  (global-completion-preview-mode 1)

  (add-hook 'completion-at-point-functions #'dabbrev-capf)
  (setopt text-mode-ispell-word-completion nil))

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
  :vc (:url "https://github.com/0WD0/majutsu"
       :main-file "majutsu.el"
       :compile-files '("majutsu-*.el"))
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

  (cl-defmethod project-root ((project (head mix)))
    (cdr project))

  (add-hook 'project-find-functions #'my-project-find-go-module)
  (add-hook 'project-find-functions #'my-project-find-mix)
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
  :vc (:url "https://github.com/dajva/rg.el"
       :main-file "rg.el"
       :compile-files '("rg-*.el"
                        "wgrep-rg.el"))
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
  :vc (:url "https://github.com/browse-kill-ring/browse-kill-ring"
       :main-file "browse-kill-ring.el")
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
  :vc (:url "https://github.com/magnars/s.el"
       :main-file "s.el")
  :defer t)

(use-package add-node-modules-path
  :vc (:url "https://github.com/codesuki/add-node-modules-path"
       :main-file "add-node-modules-path.el")
  :defer t)
(use-package archive-rpm
  :vc (:url "https://github.com/nbarrientos/archive-rpm"
       :main-file "archive-rpm.el"
       :compile-files '("archive-cpio.el"))
  :defer t)
(use-package astro-ts-mode
  :vc (:url "https://github.com/Sorixelle/astro-ts-mode"
       :main-file "astro-ts-mode.el")
  :defer t)
(use-package basic-mode
  :vc (:url "https://github.com/dykstrom/basic-mode"
       :lisp-dir "src")
  :defer t)
(use-package clojure-ts-mode
  :vc (:url "https://github.com/clojure-emacs/clojure-ts-mode"
       :main-file "clojure-ts-mode.el")
  :defer t)
(use-package edit-indirect
  :vc (:url "https://github.com/Fanael/edit-indirect"
       :main-file "edit-indirect.el")
  :defer t)
(use-package fish-mode
  :vc (:url "https://github.com/wwwjfy/emacs-fish"
       :main-file "fish-mode.el")
  :defer t)
(use-package git-modes
  :vc (:url "https://github.com/magit/git-modes"
       :main-file "git-modes.el"
       :compile-files '("git*mode.el"))
  :defer t)
(use-package graphql-ts-mode
  :vc (:url "https://git.sr.ht/~joram/graphql-ts-mode"
       :main-file "graphql-ts-mode.el")
  :defer t)
(use-package jtsx
  :vc (:url "https://github.com/llemaitre19/jtsx"
       :main-file "jtsx.el")
  :defer t)
(use-package kdl-mode
  :vc (:url "https://github.com/taquangtrung/emacs-kdl-mode"
       :main-file "kdl-mode.el")
  :defer t)
(use-package kotlin-ts-mode
  :vc (:url "https://gitlab.com/bricka/emacs-kotlin-ts-mode"
       :main-file "kotlin-ts-mode.el")
  :defer t)
(use-package lua-mode
  :vc (:url "https://github.com/immerrr/lua-mode"
       :main-file "lua-mode.el")
  :defer t)
(use-package nix-ts-mode
  :vc (:url "https://github.com/nix-community/nix-ts-mode"
       :main-file "nix-ts-mode.el")
  :defer t)
(use-package nsis-mode
  :vc (:url "https://github.com/mattfidler/nsis-mode"
       :main-file "nsis-mode.el")
  :defer t)
(use-package prisma-ts-mode
  :vc (:url "https://github.com/nverno/prisma-ts-mode"
       :main-file "prisma-ts-mode.el")
  :defer t)
(use-package rainbow-delimiters
  :vc (:url "https://github.com/Fanael/rainbow-delimiters"
       :main-file "rainbow-delimiters.el")
  :defer t)
(use-package swift-ts-mode
  :vc (:url "https://github.com/rechsteiner/swift-ts-mode"
       :main-file "swift-ts-mode.el")
  :defer t)
(use-package hcl-mode
  :vc (:url "https://github.com/hcl-emacs/hcl-mode"
       :main-file "hcl-mode.el")
  :defer t)
(use-package terraform-mode
  :vc (:url "https://github.com/syohex/emacs-terraform-mode"
       :main-file "terraform-mode.el")
  :defer t)
(use-package tmux-mode
  :vc (:url "https://github.com/nverno/tmux-mode"
       :main-file "tmux-mode.el")
  :defer t)
(use-package vcl-mode :defer t)
(use-package zig-ts-mode
  :vc (:url "https://codeberg.org/meow_king/zig-ts-mode"
       :main-file "zig-ts-mode.el")
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
(eval-when-compile
  (require 'markdown-mode nil t))

(defun my-git-commit-gfm-fontify-headings (last)
  (if (save-excursion
        (beginning-of-line)
        (looking-at-p "#[ \t]"))
      (progn
        (forward-line 1)
        t)
    (markdown-fontify-headings last)))

(define-derived-mode my-git-commit-gfm-mode gfm-mode "Git Commit GFM"
  "Major mode for Git commit messages with GFM formatting."
  (setq-local apheleia-inhibit t)
  (setq-local comment-start "#")
  (setq-local comment-end "")
  (setq-local comment-start-skip "#+[ \t]*")
  (setq-local font-lock-defaults
              (list
               (mapcar
                (lambda (keyword)
                  (if (equal keyword '(markdown-fontify-headings))
                      '(my-git-commit-gfm-fontify-headings)
                    keyword))
                markdown-mode-font-lock-keywords)
               nil nil nil nil
               '(font-lock-multiline . t)
               '(font-lock-syntactic-face-function . markdown-syntactic-face)
               '(font-lock-extra-managed-props
                 . (composition display invisible rear-nonsticky
                                keymap help-echo mouse-face)))))

(use-package git-commit
  :ensure nil
  :defer t
  :custom
  (git-commit-major-mode 'my-git-commit-gfm-mode)
  (git-commit-summary-max-length 120))

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
  :vc (:url "https://github.com/magit/magit"
       :lisp-dir "lisp")
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
  :vc (:url "https://github.com/tarsius/minions"
       :main-file "minions.el")
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
  :vc (:url "https://github.com/snosov1/toc-org"
       :main-file "toc-org.el")
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
