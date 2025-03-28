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
  (load-file (concat (file-name-as-directory (expand-file-name my-emacs-path))
                     "init/early-shared-init.el")))

;; Add shared elisp directory (but prefer system libs)
(add-to-list 'load-path (concat my-emacs-path "elisp") t)

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
    (run-hooks 'my-deferred-startup-hook)))

(run-with-idle-timer 0.2 nil #'my-run-deferred-tasks)

;; Garbage collection settings
(defun my-restore-gc-settings ()
  (setq gc-cons-threshold  (* 67108864 2)) ; 128MB
  (setq gc-cons-percentage 0.1)
  (garbage-collect))

(my-defer-startup #'my-restore-gc-settings t)

;;; OS Setup

(setq source-directory "~/emacs-shared/extra/emacs")

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
                            (append extra-paths my-system-paths (list my-original-env-path))
                            (if (eq system-type 'windows-nt) ";" ":"))))

(my-update-system-paths)

;; Setup asdf, a version manager for node.js and other software
(add-to-list 'load-path (concat my-emacs-path "elisp/asdf-vm"))
(require 'asdf-vm)

(defun my-asdf-vm-init ()
  (interactive)
  (let ((paths (cons asdf-vm-shims-path (asdf-vm--tool-bin-path-listing (asdf-vm-tool-versions)))))
    (my-update-system-paths paths))
  (when (called-interactively-p 'interactive)
    (message "Updated Emacs system paths with asdf")))

(my-asdf-vm-init)

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
(when (file-exists-p custom-file)
  (load custom-file))

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

(defun my-replace-cdrs-in-alist (old-mode new-mode alist)
  "Replace cdr instances of OLD-MODE with NEW-MODE in ALIST."
  (mapc #'(lambda (el)
            (when (eq (cdr el) old-mode)
              (setcdr el new-mode)))
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

(with-eval-after-load "add-log"
  (setopt add-log-mailing-address my-changelog-address))

;; Load `dired' itself, with `tramp' extension
(require 'dired)
(require 'dired-x)
(require 'wdired)
(require 'ffap)

;; Load tramp
(require 'tramp)

;; List directories first in dired
(require 'ls-lisp)

;; Long lines support
(global-so-long-mode 1)
(add-to-list 'so-long-target-modes 'fundamental-mode)
(global-visual-wrap-prefix-mode)
(setopt visual-wrap-extra-indent 2)

;; Don't slow down ls and don't make dired output too wide on w32 systems
(setq w32-get-true-file-attributes nil)

;; Make shell commands run in unique buffer so we can have multiple at once, and run all shell
;; asynchronously.  Taken in part from EmacsWiki: ExecuteExternalCommand page.

(defadvice erase-buffer (around erase-buffer-noop disable)
  "Make erase-buffer do nothing; only used in conjunction with shell-command.")

(defadvice shell-command (around shell-command-unique-buffer activate)
  (if (or current-prefix-arg
          output-buffer)
      ;; if this is used programmatically, allow it to be synchronous
      ad-do-it

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

            ;; temporarily blow away erase-buffer while doing it, to avoid erasing the above
            (ad-activate-regexp "erase-buffer-noop")
            (unwind-protect
                (let ((process-environment (cons "PAGER=" process-environment)))
                  ad-do-it)
              (ad-deactivate-regexp "erase-buffer-noop"))))))))

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
            (incf changes)
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

;; Tree-sitter
(defun my-remap-major-mode (from-mode to-mode)
  "Remap one major mode to another, mostly for tree-sitter support."
  (add-to-list 'major-mode-remap-alist `(,from-mode . ,to-mode)))

;;; Programming Modes and Features

(defvar my-polymode-aliases '())

;; Apheleia for automatic running of prettier
(apheleia-global-mode 1)

;; Atomic Chrome: Edit Server support for launching Emacs from browsers
(when my-server-start-p
  (my-defer-startup #'atomic-chrome-start-server))

;; Compile buffers
(with-eval-after-load "compile"
  (keymap-set compilation-mode-map "M-g" #'recompile)
  (keymap-set compilation-shell-minor-mode-map "M-g" #'recompile))

(add-hook 'compilation-filter-hook #'ansi-color-compilation-filter t)

;; Editorconfig support
(with-eval-after-load "editorconfig"
  (put 'editorconfig-lisp-use-default-indent 'safe-local-variable #'always))

(editorconfig-mode 1)

;; Set up eglot for LSP features
(require 'eglot)
(setopt eglot-sync-connect nil)

(defun my-eglot-ensure ()
  "Ensure that eglot is running, except when in an active polymode."
  (unless (bound-and-true-p polymode-mode)
    (eglot-ensure)))

(defvar my-debug-jsonrpc nil
  "Whether to enable log messages for jsonrpc.")

(defun my-jsonrpc--log-event-real (&rest args)
  "Placeholder for `jsonrpc--log-event'."
  nil)

(defun my-jsonrpc--log-event (&rest args)
  "Control whether jsonrpc events are logged."
  (when my-debug-jsonrpc
    (apply #'my-jsonrpc--log-event-real args)))

(with-eval-after-load "eglot"
  (setq eglot-diagnostics-map
        (let ((map (make-sparse-keymap)))
          (keymap-set map "<mouse-3>" #'eglot-code-actions-at-mouse)
          map))

  (keymap-set eglot-mode-map "<f2>" #'eglot-rename)
  (fset #'my-jsonrpc--log-event-real (symbol-function 'jsonrpc--log-event))
  (fset #'jsonrpc--log-event #'my-jsonrpc--log-event)

  ;; to debug eglot:
  ;; (setq my-debug-jsonrpc t)
  (setopt eglot-extend-to-xref t
          eglot-send-changes-idle-time 0.2))

;; MariaDB/MySQL conf files
(add-to-list 'auto-mode-alist '("\\.cnf\\'" . conf-mode))

;; Mermaid diagrams
(add-to-list 'load-path (concat my-emacs-path "elisp/mermaid-ts-mode"))
(autoload #'mermaid-ts-mode "mermaid-ts-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.mmd\\'" . mermaid-ts-mode))

;; SystemD conf files
(add-to-list 'auto-mode-alist '("\\.service\\'" . conf-mode))

;; SSH conf files
(add-to-list 'auto-mode-alist '("_config\\'" . conf-mode))

;; Eslint
(defun eslint-fix-file ()
  (interactive)
  (message "Running eslint --fix")
  (redisplay t)
  (call-process "eslint" nil nil nil "--fix" (buffer-file-name))
  (message "Running eslint --fix...done"))

(defun eslint-fix-file-and-revert-maybe ()
  (interactive)
  (when (and my-eslint-fix-enabled-p (fboundp #'flymake-diagnostics))
    (eslint-fix-file)
    (revert-buffer t t)))

(defun my-eslint-disable-in-current-buffer ()
  (interactive)
  (flymake-mode nil)
  (set (make-local-variable 'my-eslint-fix-enabled-p) nil))

(defun my-eslint-setup ()
  (unless (bound-and-true-p polymode-mode)
    (node-repl-interaction-mode 1)
    (when (and (not (string-match-p "/node_modules/" default-directory))
               (executable-find "eslint"))
      (add-hook 'after-save-hook #'eslint-fix-file-and-revert-maybe t t))))

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

(with-eval-after-load "flymake"
  (keymap-set flymake-mode-map "C-c f" my-flymake-mode-map)
  (keymap-set flymake-mode-map "C-x f" my-flymake-mode-map))

;; NodeJS REPL
(defun my-js-comint-send-defun (start end)
  "Send the function at point to the inferior Javascript process."
  (interactive "r")
  (unless (region-active-p)
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
  (unless (bound-and-true-p polymode-mode)
    (node-repl-interaction-mode 1)))

(defun inferior-js-mode-hook-setup ()
  (add-hook 'comint-output-filter-functions #'js-comint-process-output))

(add-hook 'inferior-js-mode-hook #'inferior-js-mode-hook-setup t)

;; Highlight current line
(require 'hl-line-plus)
(hl-line-when-idle-interval 0.3)
(toggle-hl-line-when-idle 1)

;; Highlight changed lines
(with-eval-after-load "diff-hl"
  (diff-hl-margin-mode 1)

  (dolist (el diff-hl-margin-symbols-alist)
    (setcdr el " "))

  (setopt diff-hl-draw-borders nil
          diff-hl-margin-symbols-alist diff-hl-margin-symbols-alist
          diff-hl-update-async t))

(add-hook 'dired-mode-hook #'diff-hl-dired-mode)
(add-hook 'prog-mode-hook #'turn-on-diff-hl-mode)
(add-hook 'magit-post-refresh-hook #'diff-hl-magit-post-refresh)

;; Pulsar, for highlighting current line after a jump-style keybind
(with-eval-after-load "pulsar"
  (defface my-pulsar-face
    '((default :extend t)
      (t :inherit xref-match))
    "Face for pulsar."
    :group 'pulsar-faces)

  (add-to-list 'pulsar-pulse-functions #'diff-hl-next-hunk)
  (add-to-list 'pulsar-pulse-functions #'diff-hl-previous-hunk)
  (add-to-list 'pulsar-pulse-functions #'flymake-goto-next-error)
  (add-to-list 'pulsar-pulse-functions #'flymake-goto-prev-error)
  (cl-delete #'scroll-down-command pulsar-pulse-functions)
  (cl-delete #'scroll-up-command pulsar-pulse-functions)

  (with-eval-after-load "consult"
    (add-hook 'consult-after-jump-hook #'pulsar-recenter-top)
    (add-hook 'consult-after-jump-hook #'pulsar-reveal-entry))

  (setopt pulsar-delay 0.03
          pulsar-face 'my-pulsar-face))

(my-defer-startup #'pulsar-global-mode)

;; SMerge mode, for editing files with inline diffs
(add-hook 'prog-mode-hook #'smerge-mode t)

;; Transient
(with-eval-after-load "transient"
  (transient-bind-q-to-quit))

;; Tree-sitter
(with-eval-after-load "treesit"
  (setopt treesit-font-lock-level 4))

;; VTerm, a fast but sometimes unstable terminal emulator
(with-eval-after-load "vterm"
  (setq vterm-timer-delay 0.05))

;; Set up gptel
(defvar my-gptel--backends-defined nil)
(defvar my-gptel--claude nil)
(defvar my-gptel--codestral nil)
(defvar my-gptel--gemini nil)
(defvar my-gptel--groq nil)
(defvar my-gptel--local-ai nil)
(defvar my-gptel--mistral nil)
(defvar my-gptel-local-models
  '((Sky-T1-32B-Preview-Q4_K_S
     :description "Sky-T1-32B-Preview-Q4_K_S model"
     :capabilities (media tool json url)
     :context-window 256
     ;; temperature can go up to 0.2 for more creativity but higher chance of
     ;; syntax errors
     :request-params (:temperature 0.025 :top_k 20 :top_p 0.95))
    (Sky-T1-32B-Preview-IQ2_XXS
     :description "Sky-T1-32B-Preview-IQ2_XXS model"
     :capabilities (media tool json url)
     :context-window 256
     :request-params (:temperature 0.025 :top_k 25 :top_p 0.80))
    (FuseO1-DeepSeekR1-QwQ-SkyT1-32B-Preview-IQ4_XS
     :description "FuseO1-DeepSeekR1-QwQ-SkyT1-32B-Preview-IQ4_XS model"
     :capabilities (media reasoning json url)
     :context-window 256
     ;; temperature can go up to 0.5 for more creativity but higher chance of
     ;; syntax errors
     :request-params (:temperature 0.025 :top_k 25 :top_p 0.80))
    (DeepSeek-R1-Distill-Qwen-32B-Q2_K_L
     :description "DeepSeek-R1-Distill-Qwen-32B-Q2_K_L model"
     :capabilities (media reasoning json url)
     :context-window 256
     :request-params (:temperature 0.3 :top_k 25 :top_p 0.75))
    (phi-4-Q4_K_M
     :description "phi-4-Q4_K_M model"
     :capabilities (media json url)
     :context-window 256
     :request-params (:temperature 0.5 :top_k 20 :top_p 0.75))))

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
            :key #'gptel-api-key-from-auth-source
            :stream t
            :models '(claude-3-7-sonnet-20250219)
            :header (lambda () (when-let* ((key (gptel--get-api-key)))
                                 `(("x-api-key" . ,key)
                                   ("anthropic-version" . "2023-06-01")
                                   ("anthropic-beta" . "pdfs-2024-09-25")
                                   ("anthropic-beta" . "output-128k-2025-02-19")
                                   ("anthropic-beta" . "prompt-caching-2024-07-31"))))
            :request-params '(:thinking (:type "enabled" :budget_tokens 1024)
                              :temperature 1
                              :max_tokens 4096)))

    (setq my-gptel--codestral
          (gptel-make-openai "Codestral"
            :stream t
            :host "codestral.mistral.ai"
            :key #'gptel-api-key-from-auth-source
            :models '((codestral-latest
                       :description "Official codestral Mistral AI model"
                       :capabilities (tool json)
                       :context-window 256))))

    (require 'gptel-gemini)
    (setq my-gptel--gemini
          (gptel-make-gemini "Gemini"
            :stream t
            :key #'gptel-api-key-from-auth-source))

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
            :stream t
            :host "localhost:1337"
            :protocol "http"
            :models my-gptel-local-models))

    (setq my-gptel--mistral
          (gptel-make-openai "Mistral"
            :stream t
            :host "api.mistral.ai"
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

    (run-hooks 'my-gptel-ensure-backends-hook))

  (setq gptel-backend (symbol-value my-gptel-backend))
  (setopt gptel-model (or my-gptel-model (car (gptel-backend-models gptel-backend)))
          gptel-expert-commands t
          gptel-rewrite-default-action 'accept
          gptel-temperature my-gptel-temperature)

  ;; uncomment to debug gptel:
  ;; (setq gptel-log-level 'info)

  (when my-gptel-system-prompt
    (setq gptel--system-message my-gptel-system-prompt)))

(with-eval-after-load "gptel"
  (my-gptel-ensure-backends))

(with-eval-after-load "gptel-context"
  (let ((map gptel-context-buffer-mode-map))
    (keymap-set map "q" #'my-gptel-context-save-and-quit)))

(defun my-gptel-start ()
  "Start gptel with a default buffer name."
  (interactive)
  (require 'gptel)
  (let ((backend-name (format "*%s*" (gptel-backend-name gptel-backend))))
    (switch-to-buffer (gptel backend-name nil ""))))

(defun my-aidermacs-set-editor-model (model)
  (setopt aidermacs-default-model model
          aidermacs-editor-model model
          aidermacs-extra-args (list "--model" model)
          my-aidermacs-model model))

(defun my-gptel-toggle-local ()
  "Toggle between local AI and remote AI."
  (interactive)
  (require 'aidermacs)
  (require 'gptel)
  (require 'minuet)
  (let* ((use-local (cond ((eq gptel-backend (symbol-value my-gptel-backend-local))
                           nil)
                          ((eq gptel-backend (symbol-value my-gptel-backend-remote))
                           t)
                          (t t)))
         (backend-sym
          (if use-local my-gptel-backend-local my-gptel-backend-remote))
         (backend (symbol-value backend-sym))
         (backend-name
          (format "*%s*" (gptel-backend-name backend)))
         (model (or (if use-local my-gptel-model-local my-gptel-model-remote)
                    (car (gptel-backend-models backend)))))
    (setq gptel-backend backend
          my-gptel-backend backend-sym
          gptel-model model
          my-gptel-model model)
    (if use-local
        (progn
          (my-aidermacs-set-editor-model my-aidermacs-model-local)
          (setq minuet-provider 'openai-compatible
                my-minuet-provider 'openai-compatible)
          (setf (plist-get minuet-openai-compatible-options :optional)
                (copy-sequence (gptel--model-request-params gptel-model)))
          (minuet-set-optional-options minuet-openai-compatible-options
                                       :max_tokens 128))
      (my-aidermacs-set-editor-model my-aidermacs-model-remote)
      (setq minuet-provider my-minuet-provider-remote
            my-minuet-provider my-minuet-provider-remote))
    (message "gptel backend is now %s, aider %s, and minuet %s"
             backend-sym my-aidermacs-model minuet-provider)))

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
  (let ((buf (current-buffer)))
    (split-window-right)
    (other-window 1)
    (call-interactively #'my-gptel-start)))

(defun my-gptel-context-remove-all ()
  (interactive)
  (gptel-context-remove-all))

(autoload #'gptel-api-key-from-auth-source "gptel"
  "Lookup api key in the auth source." nil)
(autoload #'gptel-backend-name "gptel-openai"
  "Access slot \"name\" of ‘gptel-backend’ struct." nil)
(autoload #'gptel-context-add-file "gptel-context"
  "Add the file at PATH to the gptel context." t)
(autoload #'gptel-context--buffer-setup "gptel-context"
  "Set up the gptel context buffer." t)
(autoload #'gptel-context-confirm "gptel-context"
  "Confirm pending operations and return to gptel's menu." t)
(autoload #'gptel-context-remove-all "gptel-context"
  "Remove all gptel context." t)

(add-to-list 'load-path (concat my-emacs-path "elisp/gptel-fn-complete"))
(autoload #'gptel-fn-complete "gptel-fn-complete"
  "Complete function at point using an LLM." t)
(autoload #'gptel-fn-complete-mark-function "gptel-fn-complete"
  "Put mark at end of this function, point at beginning." t)

;; Aidermacs for aider AI integration
(with-eval-after-load "aidermacs-backends"
  ;; 'vterm is neat, but it crashes frequently on macOS
  (setopt aidermacs-backend 'vterm))

(my-aidermacs-set-editor-model my-aidermacs-model)
(my-defer-startup #'aidermacs-setup-minor-mode)

(defvar my-aidermacs-map
  (let ((map (make-sparse-keymap)))
    (keymap-set map "a" #'aidermacs-transient-menu)
    map))

(keymap-global-set "C-c a" my-aidermacs-map)

;; Minuet for AI completion
(defun my-minuet-maybe-turn-on-auto-suggest ()
  (when my-minuet-auto-suggest-p
    (minuet-auto-suggestion-mode 1)))

(defun my-minuet-get-longest (it1 it2)
  (if (< (length it1) (length it2)) it2 it1))

(defun my-minuet-complete-2 (items)
  (minuet--cleanup-suggestion t)
  (setq items (list (-reduce #'my-minuet-get-longest items)))
  ;; close current minibuffer session, if any
  (when (active-minibuffer-window)
    (abort-recursive-edit))
  (completion-in-region (point) (point) items))

(defun my-minuet-complete-1 ()
  "Complete code in region with LLM."
  (interactive)
  (let ((current-buffer (current-buffer))
        (available-p-fn (intern (format "minuet--%s-available-p" minuet-provider)))
        (complete-fn (intern (format "minuet--%s-complete" minuet-provider)))
        (context (minuet--get-context)))
    (unless (funcall available-p-fn)
      (minuet--log (format "Minuet provider %s is not available" minuet-provider))
      (error "Minuet provider %s is not available" minuet-provider))
    (funcall complete-fn
             context
             `(lambda (items)
                (with-current-buffer ,current-buffer
                  (when (and items (not (minuet--cursor-moved-p)))
                    (my-minuet-complete-2 items)))))))

(defun my-minuet-complete ()
  (interactive)
  (require 'minuet)
  (let ((minuet-n-completions 1)
        (minuet-add-single-line-entry nil))
    ;; (minuet-complete-with-minibuffer)
    (my-minuet-complete-1)))

(defun my-minuet-get-api-key (backend)
  (my-auth-source-get-api-key (gptel-backend-host backend)))

(defun my-minuet-llama-cpp-fim-qwen-prompt-function (ctx)
  (format "<|fim_prefix|>%s\n%s<|fim_suffix|>%s<|fim_middle|>"
          (plist-get ctx :language-and-tab)
          (plist-get ctx :before-cursor)
          (plist-get ctx :after-cursor)))

(defun my-minuet-sync-options-from-gptel (m-backend g-backend)
  (let* ((options-name (format "minuet-%s-options" (symbol-name m-backend)))
         (options (symbol-value (intern options-name))))
    (when (memq m-backend '(openai-compatible openai-fim-compatible))
      (plist-put options :api-key #'(lambda () "local-ai"))
      (plist-put options :name (gptel-backend-name g-backend))
      (setf (plist-get options :optional)
            (copy-sequence (gptel--model-request-params gptel-model)))
      (minuet-set-optional-options options :max_tokens 128))
    (cond ((eq m-backend 'openai-fim-compatible)
           (plist-put options :end-point
                      (format "%s://%s%s"
                              (gptel-backend-protocol g-backend)
                              (gptel-backend-host g-backend)
                              "/v1/completions"))
           (plist-put options :template
                      '(:prompt my-minuet-llama-cpp-fim-qwen-prompt-function
                        :suffix nil)))
          ((eq m-backend 'openai-compatible)
           (plist-put options :end-point
                      (format "%s://%s%s"
                              (gptel-backend-protocol g-backend)
                              (gptel-backend-host g-backend)
                              (gptel-backend-endpoint g-backend))))
          (t
           (plist-put options :api-key `(lambda () (my-minuet-get-api-key ,g-backend)))))
    (plist-put options :model (symbol-name (car (gptel-backend-models g-backend))))))

(defvar minuet-groq-options nil)

(defun minuet--groq-available-p ()
  "Check if Groq is available."
  (when-let* ((options minuet-groq-options)
              (env-var (plist-get options :api-key))
              (end-point (plist-get options :end-point))
              (model (plist-get options :model)))
    (minuet--get-api-key env-var)))

(defun minuet--groq-complete (context callback)
  "Complete code with Groq.
CONTEXT and CALLBACK will be passed to the base function."
  (minuet--openai-complete-base
   (copy-tree minuet-groq-options) context callback))

(with-eval-after-load "minuet"
  (my-gptel-ensure-backends)
  (setq minuet-groq-options
        `(:end-point "https://api.groq.com/openai/v1/chat/completions"
          :api-key "GROQ_API_KEY"
          :model "llama-3.3-70b-versatile"
          :system
          (:template minuet-default-system-template
           :prompt minuet-default-prompt
           :guidelines minuet-default-guidelines
           :n-completions-template minuet-default-n-completion-template)
          :fewshots minuet-default-fewshots
          :chat-input
          (:template minuet-default-chat-input-template
           :language-and-tab minuet--default-chat-input-language-and-tab-function
           :context-before-cursor minuet--default-chat-input-before-cursor-function
           :context-after-cursor minuet--default-chat-input-after-cursor-function)
          :optional nil))

  (my-minuet-sync-options-from-gptel 'claude my-gptel--claude)
  (my-minuet-sync-options-from-gptel 'codestral my-gptel--codestral)
  (my-minuet-sync-options-from-gptel 'groq my-gptel--groq)
  (my-minuet-sync-options-from-gptel 'openai gptel--openai)
  (my-minuet-sync-options-from-gptel 'openai-compatible my-gptel--local-ai)
  (my-minuet-sync-options-from-gptel 'openai-fim-compatible my-gptel--local-ai)

  ;; per minuet's README.md, prevent request timeout from too many tokens
  (minuet-set-optional-options minuet-codestral-options :stop ["\n\n"])
  (minuet-set-optional-options minuet-codestral-options :max_tokens 256)

  (setopt minuet-add-single-line-entry nil
          minuet-auto-suggestion-debounce-delay 0.5
          minuet-context-window 1384
          minuet-n-completions 1)
  (setq minuet-provider my-minuet-provider)

  (keymap-set minuet-active-mode-map "C-c C-a" #'minuet-accept-suggestion)
  (keymap-set minuet-active-mode-map "C-c C-k" #'minuet-dismiss-suggestion)
  (keymap-set minuet-active-mode-map "C-c C-n" #'minuet-next-suggestion)
  (keymap-set minuet-active-mode-map "C-c C-p" #'minuet-previous-suggestion)
  (keymap-set minuet-active-mode-map "<backtab>" #'minuet-previous-suggestion)
  (keymap-set minuet-active-mode-map "<tab>" #'minuet-next-suggestion)
  (keymap-set minuet-active-mode-map "<return>" #'minuet-accept-suggestion))

;; (add-hook 'prog-mode-hook #'minuet-auto-suggestion-mode)
(add-hook 'prog-mode-hook #'my-minuet-maybe-turn-on-auto-suggest t)

(defvar my-minuet-map
  (let ((map (make-sparse-keymap)))
    (keymap-set map "a" #'minuet-auto-suggestion-mode)
    (keymap-set map "c" #'my-minuet-complete)
    (keymap-set map "m" #'minuet-complete-with-minibuffer)
    map)
  "My key customizations for minuet.")

(keymap-global-set "C-c m" my-minuet-map)
(keymap-global-set "C-x m" my-minuet-map)

;; Enable dumb-jump, which makes `C-c . .' jump to a function's definition
(require 'dumb-jump)
(setopt dumb-jump-selector 'completing-read)
(add-hook 'xref-backend-functions #'dumb-jump-xref-activate)

(defvar my-xref-map
  (let ((map (make-sparse-keymap)))
    (keymap-set map "a a" #'my-gptel-add-function)
    (keymap-set map "a f" #'my-gptel-add-current-file)
    (keymap-set map "c" #'gptel-fn-complete)
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

;; Bash shell script and .env support
(my-remap-major-mode 'sh-mode 'bash-ts-mode)
(add-to-list 'auto-mode-alist '("\\.env\\(\\..*\\)?\\'" . bash-ts-mode))
(setopt sh-shell-file "/bin/bash")

;; C/C++
(my-remap-major-mode 'c-mode 'c-ts-mode)
(my-remap-major-mode 'c++-mode 'c++-ts-mode)
(my-remap-major-mode 'c-or-c++-mode 'c-or-c++-ts-mode)
(add-hook 'c-ts-base-mode-hook #'my-eglot-ensure)
(add-hook 'c-ts-base-mode-hook #'my-xref-minor-mode t)

;; C# - requires exactly v0.20.0 of its treesit grammar
;; `C-c . .` doesn't currently work, see this for ideas:
;; https://github.com/theschmocker/dotfiles/blob/33944638a5a59ddba01b64066daf50d46e5f0c3a/emacs/.doom.d/config.el#L807
(with-eval-after-load "csharp-ts-mode"
  (keymap-set csharp-mode-map "C-c ." nil))

(my-remap-major-mode 'csharp-mode 'csharp-ts-mode)
(add-hook 'csharp-ts-mode-hook #'my-xref-minor-mode t)
(when (executable-find "omnisharp")
  (add-hook 'csharp-ts-mode-hook #'my-eglot-ensure))

;; Clojure
(with-eval-after-load "cider-repl"
  (keymap-set cider-repl-mode-map "C-d" #'cider-quit))

(defvar my-clojure-modes
  '(clojure-ts-mode clojure-ts-clojurec-mode clojure-ts-clojurescript-mode))

(with-eval-after-load "apheleia-formatters"
  (setf (alist-get 'zprint apheleia-formatters)
        '("zprint" "{:style [:how-to-ns] :search-config? true}"))
  (dolist (mode my-clojure-modes)
    (setf (alist-get mode apheleia-mode-alist)
          'zprint)))

(my-remap-major-mode 'clojure-mode 'clojure-ts-mode)
(my-remap-major-mode 'clojurec-mode 'clojure-ts-clojurec-mode)
(my-remap-major-mode 'clojurescript-mode 'clojure-ts-clojurescript-mode)

(dolist (mode my-clojure-modes)
  (let ((hook (intern (concat (symbol-name mode) "-hook"))))
    (add-hook hook #'cider-mode t)
    (add-hook hook #'my-eglot-ensure t)))

(add-to-list 'eglot-server-programs
             `(,my-clojure-modes "clojure-lsp"))

;; CSS
(my-remap-major-mode 'css-mode 'css-ts-mode)
(add-hook 'css-ts-mode-hook #'my-eglot-ensure t)
(add-hook 'css-ts-mode-hook #'my-setup-web-ligatures t)

;; Emacs Lisp
(autoload #'plist-lisp-indent-install "plist-lisp-indent"
  "Use `plist-lisp-indent-function' to indent in the current Lisp buffer." nil)

(with-eval-after-load "elisp-mode"
  (add-hook 'emacs-lisp-mode-hook #'plist-lisp-indent-install t))

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

;; Erlang
(my-remap-major-mode 'erlang-mode 'erlang-ts-mode)

;; Go
(defun my-project-find-go-module (dir)
  (when-let ((root (locate-dominating-file dir "go.mod")))
    (cons 'go-module root)))

(with-eval-after-load "project"
  ;; from https://github.com/golang/tools/blob/master/gopls/doc/emacs.md#configuring-project-for-go-modules-in-emacs
  (cl-defmethod project-root ((project (head go-module)))
    (cdr project))

  (add-hook 'project-find-functions #'my-project-find-go-module))

(add-to-list 'auto-mode-alist '("\\.go\\'" . go-ts-mode))
(add-to-list 'auto-mode-alist '("/go\\.mod\\'" . go-mod-ts-mode))
(add-to-list 'my-polymode-aliases '(go . go-ts-mode))
(add-hook 'go-ts-mode-hook #'my-eglot-ensure)
(add-hook 'go-mod-ts-mode-hook #'my-eglot-ensure)

;; GraphQL
(add-to-list 'auto-mode-alist '("\\.\\(graphql\\|gql\\)\\'" . graphql-ts-mode))

;; HTML
(my-remap-major-mode 'html-mode 'html-ts-mode)
(add-hook 'html-ts-mode-hook #'my-eglot-ensure)
(add-hook 'html-ts-mode-hook #'my-setup-web-ligatures t)

;; Java
(my-remap-major-mode 'java-mode 'java-ts-mode)
(add-hook 'java-ts-mode-hook #'my-xref-minor-mode t)
(when (executable-find "jdtls")
  ;; see https://gist.github.com/rosholger/e519c04243ae7ccb5bbf7ebef3f1cec2
  ;; and the eglot-java package for more options / alternatives
  (add-to-list 'eglot-server-programs
               '((java-ts-mode java-mode) .
                 ("jdtls" :initializationOptions
                  (:extendedClientCapabilities
                   (:classFileContentsSupport t :skipProjectConfiguration t)))))
  (add-hook 'java-ts-mode-hook #'my-eglot-ensure))

;; JSON
(my-remap-major-mode 'json-mode 'json-ts-mode)
(add-to-list 'auto-mode-alist '("\\.jsonc?\\'" . json-ts-mode))
(add-hook 'json-ts-mode-hook #'add-node-modules-path t)
(add-hook 'json-ts-mode-hook #'my-eglot-ensure)
(add-hook 'json-ts-mode-hook #'my-setup-web-ligatures t)

;; JTSX (Astro, Javascript, Typescript, and JSX support)
(defvar my-jtsx-major-modes
  '(astro-ts-mode jtsx-jsx-mode jtsx-tsx-mode jtsx-typescript-mode))
(defvar my-jtsx-ts-major-modes '(jtsx-tsx-mode jtsx-typescript-mode))

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
(add-to-list 'auto-mode-alist '("/\\yarn.lock\\'" . conf-mode))
(add-to-list 'auto-mode-alist '("\\.yarnrc\\'" . yaml-ts-mode))
(add-to-list 'my-polymode-aliases '(javascript . jtsx-jsx-mode))
(add-to-list 'my-polymode-aliases '(typescript . jtsx-tsx-mode))
(my-remap-major-mode 'js-mode 'jtsx-jsx-mode)
(my-remap-major-mode 'ts-mode 'jtsx-tsx-mode)

(add-to-list 'eglot-server-programs
             `(astro-mode . ("astro-ls" "--stdio"
                             :initializationOptions
                             (:typescript (:tsdk ,(concat my-emacs-path "node_modules/typescript/lib"))))))

(dolist (mode my-jtsx-major-modes)
  (let ((hook (intern (concat (symbol-name mode) "-hook"))))
    (add-hook hook #'add-node-modules-path t)
    (add-hook hook #'my-eglot-ensure t)
    (add-hook hook #'my-node-repl-setup t)
    (add-hook hook #'my-eslint-setup t)
    (add-hook hook #'my-setup-web-ligatures t)))

(defclass eglot-deno (eglot-lsp-server) ()
  :documentation "A custom class for deno lsp.")

(cl-defmethod eglot-initialization-options ((server eglot-deno))
  "Passes through required deno initialization options"
  (list :enable t
        :lint t))

;; (if (executable-find "deno")
;;     (add-to-list 'eglot-server-programs
;;                  `(,my-jtsx-ts-major-modes
;;                    . (eglot-deno "deno" "lsp")))
(add-to-list 'eglot-server-programs
             `(,my-jtsx-ts-major-modes
               . ("typescript-language-server" "--stdio"
                  :initializationOptions
                  (:plugins [(:name "typescript-eslint-language-service"
                              :location ,my-emacs-path)]))))
;;)

;; Kotlin
(add-to-list 'auto-mode-alist '("\\.kts?\\'" . kotlin-ts-mode))
(add-to-list 'my-polymode-aliases '(kotlin . kotlin-ts-mode))

;; Lisp
(require 'slime)
(slime-setup '(slime-repl))
(setopt slime-auto-connect 'always)
(setopt slime-kill-without-query-p t)
(setopt slime-protocol-version 'ignore)

;; Don't warn me when opening some Common Lisp files
(put 'package 'safe-local-variable 'symbolp)
(put 'Package 'safe-local-variable 'symbolp)
(put 'syntax 'safe-local-variable 'symbolp)
(put 'Syntax 'safe-local-variable 'symbolp)
(put 'Base 'safe-local-variable 'integerp)
(put 'base 'safe-local-variable 'integerp)

;; Nix
(add-to-list 'auto-mode-alist '("\\.nix\\'" . nix-ts-mode))
(add-to-list 'my-polymode-aliases '(nix . nix-ts-mode))

;; Node.js
(defvar my-nodejs-compilation-regexp
  '("^[ \t]+at +\\(?:.+(\\)?\\([^()\n]+\\):\\([0-9]+\\):\\([0-9]+\\))?$" 1 2 3)
  "Highlight node.js stacktraces in *compile* buffers.")

(with-eval-after-load "compile"
  (add-to-list 'compilation-error-regexp-alist-alist
               (cons 'nodejs my-nodejs-compilation-regexp))
  (add-to-list 'compilation-error-regexp-alist 'nodejs))

;; Prisma support (a JS DB framework)
(add-to-list 'auto-mode-alist '("\\.prisma\\'" . prisma-ts-mode))

;; Python
(defun my-project-find-python-project (dir)
  (when-let ((root (locate-dominating-file dir "pyproject.toml")))
    (cons 'python-project root)))

(with-eval-after-load "project"
  (cl-defmethod project-root ((project (head python-project)))
    (cdr project))

  (add-hook 'project-find-functions #'my-project-find-python-project))

(add-to-list 'auto-mode-alist '("/uv\\.lock\\'" . conf-toml-mode))
(my-remap-major-mode 'python-mode 'python-ts-mode)
(add-to-list 'eglot-server-programs
             '((python-ts-mode python-mode)
               "basedpyright-langserver" "--stdio"))
(add-hook 'python-ts-mode-hook #'my-eglot-ensure) ; uses pyright

;; Rust
(add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-ts-mode))
(add-to-list 'my-polymode-aliases '(rust . rust-ts-mode))
(add-to-list 'eglot-server-programs
             '((rust-ts-mode rust-mode)
               . ("rust-analyzer" :initializationOptions (:check (:command "clippy")))))
(add-hook 'rust-ts-mode-hook #'my-eglot-ensure)

;; SCSS
(add-to-list 'load-path (concat my-emacs-path "elisp/flymake-stylelint"))
(autoload #'flymake-stylelint-enable "flymake-stylelint"
  "Enable flymake-stylelint." nil)

(add-hook 'scss-mode-hook #'add-node-modules-path t)
(add-hook 'scss-mode-hook #'flymake-stylelint-enable t)
(add-to-list 'eglot-server-programs
             '((scss-mode)
               . ("vscode-css-language-server" "--stdio")))
(add-hook 'scss-mode-hook #'my-eglot-ensure)

;; Swift
(add-to-list 'auto-mode-alist '("\\.swift\\(interface\\)?\\'" . swift-ts-mode))
(add-to-list 'my-polymode-aliases '(swift . swift-ts-mode))

;; Web Mode
(add-to-list 'auto-mode-alist '("\\.hbs\\'" . web-mode))
(add-hook 'web-mode-hook #'add-node-modules-path t)
(add-hook 'web-mode-hook #'my-setup-web-ligatures t)

(setopt web-mode-code-indent-offset 2
        web-mode-enable-auto-indentation nil
        web-mode-enable-auto-quoting nil
        web-mode-markup-indent-offset 2)

;; YAML
(defun my-run-prog-mode-hooks ()
  (run-hooks 'prog-mode-hook))

(add-hook 'yaml-ts-mode-hook #'my-run-prog-mode-hooks t)
(add-to-list 'auto-mode-alist '("\\.ya?ml\\'" . yaml-ts-mode))

;; Zig
(add-to-list 'auto-mode-alist '("\\.zig\\'" . zig-ts-mode))
(add-to-list 'my-polymode-aliases '(zig . zig-ts-mode))
(add-to-list 'eglot-server-programs
             '((zig-ts-mode) .
               ("zls" :initializationOptions ())))
(add-hook 'zig-ts-mode-hook #'my-eglot-ensure)

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

(defvar my-default-ripgrep-args "--hidden -i --no-ignore-vcs --ignore-file=.gitignore --glob=!.git/")

(defun my-consult-ripgrep (regexp rg-args &optional arg)
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

(with-eval-after-load "consult"
  (with-eval-after-load "minuet"
    (consult-customize minuet-complete-with-minibuffer)))

(with-eval-after-load "embark"
  (add-hook 'embark-collect-mode-hook #'consult-preview-at-point-mode))

(with-eval-after-load "marginalia"
  (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup t))

(defun my-vertico-insert-like-ivy ()
  (interactive)
  (let* ((mb (minibuffer-contents-no-properties))
         (lc (if (string= mb "") mb (substring mb -1))))
    (cond ((string-match-p "^[/~:]" lc) (self-insert-command 1 ?/))
          ((file-directory-p (vertico--candidate)) (vertico-insert))
          (t (self-insert-command 1 ?/)))))

(with-eval-after-load "vertico"
  (setopt vertico-count 10
          vertico-mouse-mode t
          vertico-resize nil)

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
  (add-hook 'rfn-eshadow-update-overlay-hook #'vertico-directory-tidy)

  (require 'vertico-prescient)
  (setopt prescient-sort-full-matches-first t)

  ;; disable prescient for consult-line since history doesn't make sense there
  (setopt vertico-prescient-completion-category-overrides
          (append `((consult-location (styles orderless basic)))
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

(add-hook 'completion-list-mode #'consult-preview-at-point-mode)
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
(defun my-setup-corfu-mode ()
  (setq-local completion-styles '(orderless-literal-only basic)
              completion-category-overrides nil
              completion-category-defaults nil))

(defun my-eglot-capf ()
  (setq-local completion-at-point-functions
              (list (cape-capf-super
                     #'eglot-completion-at-point
                     #'cape-file))))

(defun my-load-corfu ()
  (require 'orderless)
  (orderless-define-completion-style orderless-literal-only
    (orderless-style-dispatchers nil)
    (orderless-matching-styles '(orderless-literal)))

  (dolist (hook '(prog-mode-hook shell-mode-hook))
    (add-hook hook #'corfu-mode t))

  (add-hook 'corfu-mode-hook #'my-setup-corfu-mode)
  (add-hook 'eglot-managed-mode-hook #'my-eglot-capf)

  (require 'kind-icon)
  (plist-put kind-icon-default-style :height 0.35)
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter)

  (keymap-set corfu-map "<remap> <move-beginning-of-line>" nil)
  (keymap-set corfu-map "<remap> <move-end-of-line>" nil)
  (keymap-set corfu-map "<remap> <scroll-down-command>" nil)
  (keymap-set corfu-map "<remap> <scroll-up-command>" nil)

  (setopt corfu-auto t
          corfu-popupinfo-delay '(0.3 . 0.01)
          corfu-popupinfo-hide nil
          corfu-quit-no-match 'separator
          global-corfu-modes '((not vterm-mode) t)
          text-mode-ispell-word-completion nil)

  (dolist (el '("delete-backward-char\\'" "\\`backward-delete-char"))
    (setq corfu-auto-commands (delete el corfu-auto-commands)))

  (corfu-popupinfo-mode)
  (corfu-prescient-mode)
  (global-corfu-mode))

(my-defer-startup #'my-load-corfu)

(add-hook 'completion-at-point-functions #'cape-dabbrev)
(add-hook 'completion-at-point-functions #'cape-elisp-block)
(add-hook 'completion-at-point-functions #'cape-elisp-symbol)
(add-hook 'completion-at-point-functions #'cape-file)
(add-hook 'completion-at-point-functions #'cape-keyword)

(with-eval-after-load "dabbrev"
  (add-to-list 'dabbrev-ignored-buffer-regexps "\\` ")
  (add-to-list 'dabbrev-ignored-buffer-modes 'doc-view-mode)
  (add-to-list 'dabbrev-ignored-buffer-modes 'pdf-view-mode)
  (add-to-list 'dabbrev-ignored-buffer-modes 'tags-table-mode))

;; Set up project.el
(defun my-project-root ()
  "Return root directory of the current project."
  (project-root (project-current t)))

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
  (let ((filepath (file-relative-name (my-path-of-current-buffer) (my-project-root))))
    (kill-new filepath)
    (message "Copied '%s' to clipboard" filepath)))

(defun my-remove-project-switch-bindings (to-remove)
  (dolist (item (cdr project-prefix-map))
    (let* ((raw-key (car item))
           (cmd (cdr item)))
      (when (and (memq cmd to-remove) (integerp raw-key))
        (keymap-set project-prefix-map (string raw-key) nil))))
  (setq project-switch-commands
        (cl-remove-if #'(lambda (item) (memq (car item) to-remove))
                      project-switch-commands)))

(with-eval-after-load "project"
  (my-remove-project-switch-bindings '(project-eshell
                                       project-find-dir
                                       project-find-regexp
                                       project-query-replace-regexp
                                       project-vc-dir))
  (add-to-list 'project-switch-commands '(project-dired "Dired") t)
  (keymap-set project-prefix-map "b" #'consult-project-buffer)
  (keymap-set project-prefix-map "d" #'project-dired)
  (add-to-list 'project-switch-commands '(my-consult-ripgrep "Ripgrep") t)
  (keymap-set project-prefix-map "r" #'my-consult-ripgrep)
  (keymap-set project-prefix-map "s" #'my-consult-ripgrep)
  (add-to-list 'project-switch-commands '(magit-project-status "Magit") t)
  (keymap-set project-prefix-map "RET" #'magit-project-status)
  (keymap-set project-prefix-map "m" #'magit-project-status))

;; Insinuate with ripgrep
(defun my-rg-command-line-flags (&optional flags)
  (append flags (split-string-shell-command my-default-ripgrep-args)))

(setq rg-command-line-flags-function #'my-rg-command-line-flags)

(with-eval-after-load "rg"
  (keymap-set rg-mode-map "e" #'rg-rerun-change-regexp)
  (keymap-set rg-mode-map "r" #'wgrep-change-to-wgrep-mode))

;; Bind N and P in ediff so that I don't leave the control buffer
(defun my-ediff-next-difference (&rest args)
  (interactive)
  (save-selected-window
    (call-interactively 'ediff-next-difference)))

(defun my-ediff-previous-difference (&rest args)
  (interactive)
  (save-selected-window
    (call-interactively 'ediff-previous-difference)))

(defun my-ediff-extra-keys ()
  (keymap-set ediff-mode-map "N" #'my-ediff-next-difference)
  (keymap-set ediff-mode-map "P" #'my-ediff-previous-difference))
(add-hook 'ediff-keymap-setup-hook #'my-ediff-extra-keys t)

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

;; Enable wdired on "r"
(keymap-set dired-mode-map "r" 'wdired-change-to-wdired-mode)

;; Make tramp's backup directories the same as the normal ones
(setopt tramp-backup-directory-alist backup-directory-alist)

;; Navigate the kill ring when doing M-y
(browse-kill-ring-default-keybindings)

;; extension of mine to make list editing easy
(require 'edit-list)
(defalias 'my-edit-list 'edit-list)

;; All programming modes
(defun my-turn-on-display-line-numbers-mode ()
  (interactive)
  (display-line-numbers-mode 1))

(defun my-turn-off-display-line-numbers-mode ()
  (interactive)
  (display-line-numbers-mode -1))

(add-hook 'conf-mode-hook #'my-turn-on-display-line-numbers-mode t)

(add-hook 'prog-mode-hook #'my-turn-on-display-line-numbers-mode t)
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode t)

(add-hook 'lisp-interaction-mode-hook #'my-turn-off-display-line-numbers-mode t)

(kill-ring-deindent-mode 1)

;; Markdown support
(my-remap-major-mode 'gfm-mode 'poly-gfm-mode)
(my-remap-major-mode 'markdown-mode 'poly-gfm-mode)
(my-remap-major-mode 'poly-markdown-mode 'poly-gfm-mode)

(with-eval-after-load "gptel"
  (setopt gptel-default-mode #'poly-gfm-mode)
  (add-to-list 'gptel-prompt-prefix-alist '(poly-gfm-mode . "### ")))

(defun my-replace-mode-in-symbol (mode-sym)
  (intern
   (replace-regexp-in-string "-mode\\'" ""
                             (symbol-name mode-sym))))

(defun my-polymode-install-aliases ()
  (dolist (to-remap major-mode-remap-alist)
    (let ((from (my-replace-mode-in-symbol (car to-remap)))
          (to (cdr to-remap)))
      (unless (eq to 'poly-gfm-mode)
        (add-to-list 'polymode-mode-name-aliases (cons from to)))))
  (dolist (alias my-polymode-aliases)
    (add-to-list 'polymode-mode-name-aliases alias)))

(defun my-polymode-yank-chunk ()
  (interactive)
  (save-excursion
    (unless (buffer-narrowed-p)
      (polymode-toggle-chunk-narrowing))
    (set-mark (point-min))
    (goto-char (point-max))
    (call-interactively #'kill-ring-save)
    (widen)))

(with-eval-after-load "polymode"
  (keymap-set polymode-map "k" #'polymode-kill-chunk)
  (keymap-set polymode-map "w" #'my-polymode-yank-chunk)
  (keymap-set polymode-map "C-w" #'my-polymode-yank-chunk)
  (keymap-set polymode-minor-mode-map "C-c n" polymode-map)
  (easy-menu-add-item polymode-menu
                      nil
                      '["Yank chunk" my-polymode-yank-chunk]
                      "--"))

(with-eval-after-load "polymode-core"
  (my-polymode-install-aliases)
  (my-replace-cdrs-in-alist 'sh-mode 'bash-ts-mode 'polymode-mode-name-aliases)
  (my-replace-cdrs-in-alist 'shell-script-mode 'bash-ts-mode 'polymode-mode-name-aliases))

;; Don't mess with keys that I'm used to
(defun my-markdown-mode-keys ()
  (keymap-set markdown-mode-map "M-<left>" #'backward-word)
  (keymap-set markdown-mode-map "M-<right>" #'forward-word))

(add-hook 'markdown-mode-hook #'my-markdown-mode-keys t)

;; Support for .nsh files
(setq auto-mode-alist (append '(("\\.[Nn][Ss][HhIi]\\'" . nsis-mode)) auto-mode-alist))

;; Support for .plist files from https://www.emacswiki.org/emacs/MacOSXPlist
(add-to-list 'jka-compr-compression-info-list
             ["\\.plist$"
              "converting text XML to binary plist"
              "plutil"
              ("-convert" "binary1" "-o" "-" "-")
              "converting binary plist to text XML"
              "plutil"
              ("-convert" "xml1" "-o" "-" "-")
              nil nil "bplist"])

(jka-compr-update)

;; tmux support
(add-to-list 'auto-mode-alist '("\\.?tmux\\.conf\\(\\.[^.]+\\)?\\'" . tmux-mode))

;; Profiling
(require 'profiler)
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
(with-eval-after-load "git-commit"
  (setopt git-commit-major-mode 'org-mode)
  (remove-hook 'git-commit-setup-hook #'git-commit-turn-on-auto-fill))

;; Don't overwrite M-w in magit mode, and clear mark when done
(defun my-magit-kill-ring-save ()
  (interactive)
  (call-interactively #'kill-ring-save)
  (deactivate-mark))

(with-eval-after-load "magit"
  ;; magit-log currently has some kind of transient bug, so don't show transient menu
  (keymap-set magit-mode-map "l" #'magit-log-current)
  (keymap-set magit-mode-map "M-w" #'my-magit-kill-ring-save)
  (keymap-set magit-hunk-section-map "RET" #'magit-diff-visit-worktree-file))

(defun my-preload-magit ()
  (require 'magit)
  (require 'git-commit))

(my-defer-startup #'my-preload-magit)

;; Map some magit keys globally
(keymap-global-set "C-x V" nil)
(keymap-global-set "C-x V a" #'magit-blame)
(keymap-global-set "C-x V b" #'magit-show-refs-current)
(keymap-global-set "C-x V f" #'magit-file-dispatch)
(keymap-global-set "C-x V l" #'magit-log-head)
(keymap-global-set "C-x V s" #'magit-status)
(keymap-global-set "C-x V v" #'magit-dispatch)

;; Don't display any minor modes on the mode-line
(require 'minions)
(setopt minions-mode-line-delimiters '("" . ""))
(setopt minions-mode-line-lighter " ")
(minions-mode 1)

;; Org Mode settings
(defun my-org-find-notes-file ()
  (interactive)
  (require 'org)
  (find-file org-default-notes-file))

(defun my-org-capture-note ()
  (interactive)
  (require 'org-capture)
  (org-capture nil "n"))

(with-eval-after-load "org"
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
    (keymap-set map "a s" #'my-asdf-vm-init)
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
(keymap-global-set "C-x g" #'mark-whole-buffer)
(keymap-global-set "C-x r r" #'rectangle-mark-mode)
(keymap-global-set "C-x p" #'other-window)

(defun my-kill-emacs ()
  (interactive)
  (let* ((confirm-kill-emacs 'y-or-n-p)
         (cur-proc (frame-parameter nil 'client))
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
         (kill-cmd (if (and cur-proc
                            (processp cur-proc)
                            (> (length all-procs) 1)
                            (>= (length other-graphics) 1))
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

(with-eval-after-load "view"
  ;; Make the `q' key bury the current buffer when viewing help
  (keymap-set view-mode-map "q" 'bury-buffer)
  ;; Make the <DEL> key scroll backwards in View mode
  (keymap-set view-mode-map "DEL" 'View-scroll-page-backward))

(with-eval-after-load "info"
  ;; Make the <DEL> key scroll backwards in Info mode
  (keymap-set Info-mode-map "DEL" 'Info-scroll-down))

;; diff-mode: Don't mess with M-q
(with-eval-after-load "diff-mode"
  (keymap-set diff-mode-map "M-q" 'fill-paragraph))

;; Show keybind options while typing leading keys
(my-defer-startup #'which-key-mode)

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

(defun my-set-super-bindings ()
  (interactive)
  (with-eval-after-load "cider-repl"
    (keymap-set cider-repl-mode-map "s-n" #'cider-repl-next-input)
    (keymap-set cider-repl-mode-map "s-p" #'cider-repl-previous-input))

  (with-eval-after-load "magit"
    (keymap-set magit-mode-map "s-2" #'magit-section-show-level-2-all)
    (keymap-set magit-mode-map "s-4" #'magit-section-show-level-4-all)
    (keymap-set magit-status-mode-map "s-c" #'my-magit-kill-ring-save)
    (keymap-set magit-status-mode-map "s-w" #'my-magit-kill-ring-save))

  (with-eval-after-load "polymode"
    (keymap-set polymode-minor-mode-map "s-n" 'polymode-map))

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
  (keymap-global-set "s-p" #'project-find-file)
  (keymap-global-set "s-q" #'fill-paragraph)
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
