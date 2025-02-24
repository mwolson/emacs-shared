;;; Emacs initialization settings common to multiple computers
;;
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
(defun my-defer-startup (func)
  "Defer running a task until sometime after Emacs has started."
  (add-hook 'my-deferred-startup-hook func))
(defun my-run-deferred-tasks ()
  (unless (eq system-type 'windows-nt)
    (run-hooks 'my-deferred-startup-hook)))

(run-with-idle-timer 0.2 nil #'my-run-deferred-tasks)

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
       (defalias 'man 'woman)))

;;; Customizations

;; Load customizations
(setq custom-file (if my-settings-shared-p
                      (concat my-emacs-path "init/settings.el")
                    (locate-user-emacs-file "settings.el")))
(when (file-exists-p custom-file)
  (load custom-file))

;;; Functions

(defmacro match-data-changed (&rest body)
  "Determine whether the match data has been modified by BODY."
  (let ((mdata (make-symbol "temp-buffer")))
    `(let ((,mdata (match-data)))
       (prog1 ,@body
         (if (equal ,mdata (match-data))
             (message "Match data has not been changed")
           (message "Match data has been changed!"))))))

(put 'match-data-changed 'lisp-indent-function 0)
(put 'match-data-changed 'edebug-form-spec '(body))

(defun byte-compile-this-file-temporarily ()
  (interactive)
  (let ((file buffer-file-name))
    (byte-compile-file file)
    (save-match-data
      (when (string-match "\\.el\\'" file)
        (delete-file (concat file "c"))))))

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

;;; Things that can't be changed easily using `customize'

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

;;; Base Programs and Features

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
  (define-key compilation-mode-map (kbd "M-g") #'recompile)
  (define-key compilation-shell-minor-mode-map (kbd "M-g") #'recompile))

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
          (define-key map (kbd "<mouse-3>") #'eglot-code-actions-at-mouse)
          map))
  (define-key eglot-mode-map (kbd "<f2>") #'eglot-rename)
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

;; npmrc conf files
(add-to-list 'auto-mode-alist '("\\.npmrc\\'" . conf-mode))

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
    (define-key map (kbd "c") #'flymake-start)
    (define-key map (kbd "l") #'flymake-show-buffer-diagnostics)
    (define-key map (kbd "L") #'flymake-switch-to-log-buffer)
    (define-key map (kbd "n") #'flymake-goto-next-error)
    (define-key map (kbd "p") #'flymake-goto-prev-error)
    map)
  "My key customizations for flymake.")

(with-eval-after-load "flymake"
  (define-key flymake-mode-map (kbd "C-c f") my-flymake-mode-map)
  (define-key flymake-mode-map (kbd "C-x f") my-flymake-mode-map))

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
  (let ((text (buffer-substring-no-properties (point-at-bol) (point-at-eol))))
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
    (define-key map (kbd "C-x C-e") 'my-js-comint-send-last-sexp)
    (define-key map (kbd "C-c C-l") 'my-js-comint-send-line)
    (define-key map (kbd "C-c C-r") 'my-js-comint-send-region)
    (define-key map (kbd "C-c C-z") 'js-comint-start-or-switch-to-repl)
    (define-key map (kbd "C-M-x") 'my-js-comint-send-defun)
    (define-key map (kbd "C-s-x") 'my-js-comint-send-defun)
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
  (add-hook 'comint-output-filter-functions 'js-comint-process-output))

(add-hook 'inferior-js-mode-hook 'inferior-js-mode-hook-setup t)

;; Highlight current line
(require 'hl-line-plus)
(hl-line-when-idle-interval 0.3)
(toggle-hl-line-when-idle 1)

;; SMerge mode, for editing files with inline diffs
(add-hook 'prog-mode-hook 'smerge-mode t)

;; Transient
(with-eval-after-load "transient"
  (transient-bind-q-to-quit))

;; Tree-sitter
(with-eval-after-load "treesit"
  (setopt treesit-font-lock-level 4))

;; vterm
(add-to-list 'load-path (concat my-emacs-path "elisp/vterm"))
(require 'vterm)

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
     ;; temperature can go down to 0 for more predictability but less creativity
     :request-params (:temperature 0.2 :top_p 0.80 :top_k 20))
    (Sky-T1-32B-Preview-IQ2_XXS
     :description "Sky-T1-32B-Preview-IQ2_XXS model"
     :capabilities (media tool json url)
     :context-window 256
     :request-params (:temperature 0.025 :top_p 0.80 :top_k 25))
    (FuseO1-DeepSeekR1-QwQ-SkyT1-32B-Preview-IQ4_XS
     :description "FuseO1-DeepSeekR1-QwQ-SkyT1-32B-Preview-IQ4_XS model"
     :capabilities (media reasoning json url)
     :context-window 256
     ;; temperature can go up to 0.5 for more creativity but higher chance of
     ;; syntax errors
     :request-params (:temperature 0.025 :top_p 0.80 :top_k 25))
    (DeepSeek-R1-Distill-Qwen-32B-Q2_K_L
     :description "DeepSeek-R1-Distill-Qwen-32B-Q2_K_L model"
     :capabilities (media reasoning json url)
     :context-window 256
     :request-params (:temperature 0.3 :top_p 0.75 :top_k 25))
    (phi-4-Q4_K_M
     :description "phi-4-Q4_K_M model"
     :capabilities (media json url)
     :context-window 256
     :request-params (:temperature 0.5 :top_p 0.75 :top_k 20))))

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
    (unless (alist-get 'claude-3-7-sonnet-20250219 gptel--anthropic-models)
      (add-to-list 'gptel--anthropic-models
                   '(claude-3-7-sonnet-20250219
                     :description "Highest level of intelligence and capability" :capabilities
                     (media tool-use cache)
                     :mime-types
                     ("image/jpeg" "image/png" "image/gif" "image/webp" "application/pdf")
                     :context-window 200 :input-cost 3 :output-cost 15 :cutoff-date "2024-11")))

    (setq my-gptel--claude
          (gptel-make-anthropic "Claude"
            :stream t
            :key #'gptel-api-key-from-auth-source))

    (setq my-gptel--codestral
          (gptel-make-openai "Codestral"
            :stream t
            :host "codestral.mistral.ai"
            :key #'gptel-api-key-from-auth-source
            :models '((codestral-latest
                       :description "Official codestral Mistral AI model"
                       :capabilities (tool json)
                       :context-window 256))))

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

  (setopt gptel-backend (symbol-value my-gptel-backend)
          gptel-model (or my-gptel-model (car (gptel-backend-models gptel-backend)))
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
    (define-key map (kbd "q") #'my-gptel-context-save-and-quit)))

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
  (call-interactively #'gptel-rewrite))

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
  (setopt aidermacs-backend 'comint))

(add-to-list 'load-path (concat my-emacs-path "elisp/aidermacs"))
(my-aidermacs-set-editor-model my-aidermacs-model)

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

  (define-key minuet-active-mode-map (kbd "C-c C-a") #'minuet-accept-suggestion)
  (define-key minuet-active-mode-map (kbd "C-c C-k") #'minuet-dismiss-suggestion)
  (define-key minuet-active-mode-map (kbd "C-c C-n") #'minuet-next-suggestion)
  (define-key minuet-active-mode-map (kbd "C-c C-p") #'minuet-previous-suggestion)
  (define-key minuet-active-mode-map (kbd "<backtab>") #'minuet-previous-suggestion)
  (define-key minuet-active-mode-map (kbd "<tab>") #'minuet-next-suggestion)
  (define-key minuet-active-mode-map (kbd "<return>") #'minuet-accept-suggestion))

;; (with-eval-after-load "company"
;;   (autoload #'company-minuet-setup "company-minuet" "`company-mode' completion for minuet." t)
;;   (add-hook 'prog-mode-hook #'company-minuet-setup))

;; (autoload #'minuet-capf-setup "minuet-capf" "Setup completion-at-point for minuet." t)

;; (add-hook 'prog-mode-hook #'minuet-auto-suggestion-mode)

(add-to-list 'load-path (concat my-emacs-path "elisp/minuet"))
(autoload #'minuet-auto-suggestion-mode "minuet" "Toggle automatic code suggestions." t)
(autoload #'minuet-complete-with-minibuffer "minuet" "Complete using minibuffer interface." t)
(autoload #'minuet-show-suggestion "minuet" "Show code suggestion using overlay at point." t)
(add-hook 'prog-mode-hook #'my-minuet-maybe-turn-on-auto-suggest t)

(defvar my-minuet-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "a") #'minuet-auto-suggestion-mode)
    (define-key map (kbd "c") #'my-minuet-complete)
    (define-key map (kbd "m") #'minuet-complete-with-minibuffer)
    map)
  "My key customizations for minuet.")

(global-set-key (kbd "C-c m") my-minuet-map)
(global-set-key (kbd "C-x m") my-minuet-map)

;; Enable dumb-jump, which makes `C-c . .' jump to a function's definition
(require 'dumb-jump)
(setopt dumb-jump-selector 'ivy)
(add-hook 'xref-backend-functions #'dumb-jump-xref-activate)

(defvar my-xref-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "a a") #'my-gptel-add-function)
    (define-key map (kbd "a f") #'my-gptel-add-current-file)
    (define-key map (kbd "c") #'gptel-fn-complete)
    (define-key map (kbd "k") #'my-gptel-context-remove-all)
    (define-key map (kbd "l") #'my-gptel-toggle-local)
    (define-key map (kbd "q") #'my-gptel-query-function)
    (define-key map (kbd "r") #'my-gptel-rewrite-function)
    (define-key map (kbd "v") #'my-gptel-view-context)
    (define-key map (kbd ".") #'xref-find-definitions)
    (define-key map (kbd ",") #'xref-go-back)
    (define-key map (kbd "/") #'xref-find-references)
    map)
  "My key customizations for AI and xref.")

(global-set-key (kbd "C-c .") my-xref-map)
(global-set-key (kbd "C-c C-.") my-xref-map)
(global-set-key (kbd "C-x .") my-xref-map)
(global-set-key (kbd "C-x C-.") my-xref-map)

(defvar my-xref-minor-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c .") my-xref-map)
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
  (define-key csharp-mode-map (kbd "C-c .") nil))

(my-remap-major-mode 'csharp-mode 'csharp-ts-mode)
(add-hook 'csharp-ts-mode-hook #'my-xref-minor-mode t)
(when (executable-find "omnisharp")
  (add-hook 'csharp-ts-mode-hook #'my-eglot-ensure))

;; Clojure
(with-eval-after-load "cider-repl"
  (define-key cider-repl-mode-map (kbd "C-d") #'cider-quit))

(defvar my-clojure-modes
  '(clojure-ts-mode clojure-ts-clojurec-mode clojure-ts-clojurescript-mode))

(with-eval-after-load "apheleia-formatters"
  (setf (alist-get 'zprint apheleia-formatters)
        '("zprint" "{:style [:how-to-ns] :search-config? true}"))
  (dolist (mode my-clojure-modes)
    (setf (alist-get mode apheleia-mode-alist)
          'zprint)))

(add-to-list 'load-path (concat my-emacs-path "elisp/clojure-ts-mode"))
(autoload #'clojure-ts-mode "clojure-ts-mode" nil t)
(my-remap-major-mode 'clojure-mode 'clojure-ts-mode)
(my-remap-major-mode 'clojurec-mode 'clojure-ts-clojurec-mode)
(my-remap-major-mode 'clojurescript-mode 'clojure-ts-clojurescript-mode)

(dolist (mode my-clojure-modes)
  (let ((hook (intern (concat (symbol-name mode) "-hook"))))
    (add-hook hook #'cider-mode t)
    (add-hook hook #'my-eglot-ensure t)))

(add-to-list 'eglot-server-programs
             `(,my-clojure-modes "clojure-lsp"))

;; Emacs Lisp
(autoload #'plist-lisp-indent-install "plist-lisp-indent"
  "Use `plist-lisp-indent-function' to indent in the current Lisp buffer." nil)

(with-eval-after-load "elisp-mode"
  (add-hook 'emacs-lisp-mode-hook #'plist-lisp-indent-install t))

;; Erlang
(add-to-list 'load-path (concat my-emacs-path "elisp/erlang-ts"))
(my-remap-major-mode 'erlang-mode 'erlang-ts-mode)
(autoload #'erlang-ts-mode "erlang-ts"
  "Major mode for editing erlang with tree-sitter." t)

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
(add-hook 'json-ts-mode-hook #'my-eglot-ensure t)
(add-hook 'json-ts-mode-hook #'my-setup-web-ligatures t)

;; JTSX (Javascript, Typescript, and JSX support)
(defvar my-jtsx-major-modes '(astro-mode jtsx-jsx-mode jtsx-tsx-mode jtsx-typescript-mode))
(defvar my-jtsx-ts-major-modes '(jtsx-tsx-mode jtsx-typescript-mode))

(add-to-list 'load-path (concat my-emacs-path "elisp/jtsx"))
(add-to-list 'auto-mode-alist '("\\.[cm]js\\'" . jtsx-jsx-mode))
(add-to-list 'auto-mode-alist '("\\.jsx?\\'" . jtsx-jsx-mode))
(add-to-list 'auto-mode-alist '("\\.ts\\'" . jtsx-typescript-mode))
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . jtsx-tsx-mode))
(autoload #'jtsx-jsx-mode "jtsx" "Major mode extending `js-ts-mode'." t)
(autoload #'jtsx-tsx-mode "jtsx" "Major mode extending `tsx-ts-mode'." t)
(autoload #'jtsx-typescript-mode "jtsx" "Major mode extending `typescript-ts-mode'." t)
(add-to-list 'my-polymode-aliases '(javascript . jtsx-jsx-mode))
(add-to-list 'my-polymode-aliases '(typescript . jtsx-tsx-mode))
(my-remap-major-mode 'js-mode 'jtsx-jsx-mode)
(my-remap-major-mode 'ts-mode 'jtsx-tsx-mode)

(define-derived-mode astro-mode web-mode "astro")
(add-to-list 'auto-mode-alist '(".*\\.astro\\'" . astro-mode))
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
(add-to-list 'load-path (concat my-emacs-path "elisp/kotlin-ts-mode"))
(add-to-list 'auto-mode-alist '("\\.kts?\\'" . kotlin-ts-mode))
(autoload #'kotlin-ts-mode "kotlin-ts-mode" "Major mode for editing Kotlin." t)
(add-to-list 'my-polymode-aliases '(kotlin . kotlin-ts-mode))

;; Lisp REPL using SLIME
(require 'slime)
(slime-setup '(slime-repl))
(setopt slime-auto-connect 'always)
(setopt slime-kill-without-query-p t)
(setopt slime-protocol-version 'ignore)

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
(add-to-list 'load-path (concat my-emacs-path "elisp/prisma-ts-mode"))
(autoload #'prisma-ts-mode "prisma-ts-mode" "Major mode for editing prisma source code." t)
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
(require 'flymake-stylelint)
(add-hook 'scss-mode-hook 'add-node-modules-path t)
(add-hook 'scss-mode-hook 'flymake-stylelint-enable t)

;; Swift
(add-to-list 'load-path (concat my-emacs-path "elisp/swift-ts-mode"))
(autoload #'swift-ts-mode "swift-ts-mode" "Major mode for editing Swift." t)
(add-to-list 'auto-mode-alist '("\\.swift\\(interface\\)?\\'" . swift-ts-mode))
(add-to-list 'my-polymode-aliases '(swift . swift-ts-mode))

;; Web Mode
(add-to-list 'auto-mode-alist '("\\.hbs\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
(add-hook 'web-mode-hook #'add-node-modules-path t)
(add-hook 'web-mode-hook #'my-setup-web-ligatures t)

;; Zig
(add-to-list 'load-path (concat my-emacs-path "elisp/zig-ts-mode"))
(autoload #'zig-ts-mode "zig-ts-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.zig\\'" . zig-ts-mode))
(add-to-list 'my-polymode-aliases '(zig . zig-ts-mode))
(add-to-list 'eglot-server-programs
             '((zig-ts-mode) .
               ("zls" :initializationOptions ())))
(add-hook 'zig-ts-mode-hook #'my-eglot-ensure)

;; Load amx, which makes M-x work better on Ivy
(add-hook 'after-init-hook 'amx-mode t)

;; Ivy, Counsel, and Swiper
(require 'counsel)
(ivy-mode 1)
(setopt ivy-use-virtual-buffers t)
(setopt ivy-count-format "(%d/%d) ")
(setq ivy-re-builders-alist
      '((t . ivy--regex-plus)))
(setopt counsel-find-file-at-point t)
(setopt counsel-mode-override-describe-bindings t)
(counsel-mode 1)

(define-key ivy-minibuffer-map (kbd "C-r") 'ivy-previous-line-or-history)
(define-key ivy-occur-grep-mode-map "r" 'ivy-wgrep-change-to-wgrep-mode)

(global-set-key (kbd "C-s") 'swiper-isearch)
(global-set-key (kbd "C-r") 'swiper-isearch)
(global-set-key (kbd "C-c C-r") 'ivy-resume)

;; Tell fd to ignore any other .gitignore files that it finds in subdirectories,
;; mostly for submodule purposes, and only use the one in the top-level git repo
;;
;; (setq sample-git-fd-args
;;       (concat sample-git-fd-args " --no-ignore-vcs --ignore-file .gitignore"))

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
  (define-key project-prefix-map "d" #'project-dired)
  (add-to-list 'project-switch-commands '(my-counsel-ripgrep "Ripgrep") t)
  (define-key project-prefix-map "r" #'my-counsel-ripgrep)
  (define-key project-prefix-map "s" #'my-counsel-ripgrep)
  (add-to-list 'project-switch-commands '(magit-project-status "Magit") t)
  (define-key project-prefix-map (kbd "RET") #'magit-project-status)
  (define-key project-prefix-map "m" #'magit-project-status))

;; Insinuate with ripgrep
(defvar my-default-ripgrep-args "--hidden -i --no-ignore-vcs --ignore-file=.gitignore --glob=!.git/")

(defun my-rg-command-line-flags (&optional flags)
  (append flags (split-string-shell-command my-default-ripgrep-args)))

(setq rg-command-line-flags-function #'my-rg-command-line-flags)

(with-eval-after-load "rg"
  (define-key rg-mode-map (kbd "e") #'rg-rerun-change-regexp)
  (define-key rg-mode-map (kbd "r") #'wgrep-change-to-wgrep-mode))

(defun my-counsel-ripgrep (regexp rg-args &optional arg)
  "Run a Counsel Ripgrep search with `REGEXP' rooted at the current project root.

With \\[universal-argument], also prompt for extra rg arguments and set into RG-ARGS."
  (interactive
   (list (and (use-region-p) (buffer-substring-no-properties (region-beginning) (region-end)))
         (if current-prefix-arg
             (read-from-minibuffer "Additional rg args: " my-default-ripgrep-args nil nil nil my-default-ripgrep-args)
           my-default-ripgrep-args)))
  (let ((counsel-rg-base-command "rg --no-heading --line-number %s ."))
    (counsel-rg regexp (my-project-root) rg-args)))

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
  (define-key ediff-mode-map (kbd "N") #'my-ediff-next-difference)
  (define-key ediff-mode-map (kbd "P") #'my-ediff-previous-difference))
(add-hook 'ediff-keymap-setup-hook 'my-ediff-extra-keys t)

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
  (define-key texinfo-mode-map (kbd "C-c C-p") #'makeinfo-buffer)
  (define-key texinfo-mode-map (kbd "C-c C-v") #'my-texinfo-view-file))
(add-hook 'texinfo-mode-hook 'my-texinfo-extra-keys t)

;; Don't warn me when opening some Common Lisp files
(put 'package 'safe-local-variable 'symbolp)
(put 'Package 'safe-local-variable 'symbolp)
(put 'syntax 'safe-local-variable 'symbolp)
(put 'Syntax 'safe-local-variable 'symbolp)
(put 'Base 'safe-local-variable 'integerp)
(put 'base 'safe-local-variable 'integerp)

;; Enable wdired on "r"
(define-key dired-mode-map "r" 'wdired-change-to-wdired-mode)

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

(add-hook 'conf-mode-hook 'my-turn-on-display-line-numbers-mode t)
(add-hook 'prog-mode-hook 'my-turn-on-display-line-numbers-mode t)
(add-hook 'lisp-interaction-mode-hook 'my-turn-off-display-line-numbers-mode t)

;; Markdown support
(add-to-list 'load-path (concat my-emacs-path "elisp/poly-markdown"))
(autoload #'poly-markdown-mode "poly-markdown" t)
(autoload #'poly-gfm-mode "poly-markdown" t)
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
  (define-key polymode-map (kbd "k") #'polymode-kill-chunk)
  (define-key polymode-map (kbd "w") #'my-polymode-yank-chunk)
  (define-key polymode-map (kbd "C-w") #'my-polymode-yank-chunk)
  (define-key polymode-minor-mode-map (kbd "C-c n") polymode-map)
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
  (define-key markdown-mode-map (kbd "<M-right>") #'forward-word)
  (define-key markdown-mode-map (kbd "<M-left>") #'backward-word))
(add-hook 'markdown-mode-hook #'my-markdown-mode-keys t)

;; Support for .nsh files
(autoload #'nsis-mode "nsis-mode" "NSIS mode" t)
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
(add-to-list 'load-path (concat my-emacs-path "elisp/tmux-mode"))
(autoload #'tmux-mode "tmux-mode" "tmux mode" t)
(add-to-list 'auto-mode-alist '("\\.?tmux\\.conf\\(\\.[^.]+\\)?\\'" . tmux-mode))

;; YAML changes
(defun my-run-prog-mode-hooks ()
  (run-hooks 'prog-mode-hook))

;; YAML mode should be treated like a programming mode, such as showing
;; line numbers and applying editorconfig standards
(add-hook 'yaml-ts-mode-hook #'my-run-prog-mode-hooks t)
(add-to-list 'auto-mode-alist '("\\.ya?ml\\'" . yaml-ts-mode))

;; Profiling
(require 'profiler)
(cl-defmacro with-cpu-profiling (&rest body)
  `(unwind-protect
       (progn
         (ignore-errors (profiler-cpu-log))
         (profiler-cpu-start profiler-sampling-interval)
         ,@body)
     (profiler-report-cpu)
     (profiler-cpu-stop)))

;; Company: auto-completion for various modes
(setopt company-idle-delay 0.2)
(setopt company-tooltip-align-annotations t)
(setopt company-tooltip-limit 20)
(add-hook 'after-init-hook 'global-company-mode t)
(add-hook 'after-init-hook 'company-statistics-mode t)

(with-eval-after-load "company"
  (define-key company-active-map (kbd "<tab>") 'company-complete-selection))

;; Consult: completion in minbuffers for minuet and other modes
(with-eval-after-load "consult"
  (with-eval-after-load "minuet"
    (consult-customize minuet-complete-with-minibuffer)))

;; Setup info for manually compiled packages
(add-to-list 'Info-default-directory-list (concat my-emacs-path "share/info"))

;; Magit settings
(with-eval-after-load "git-commit"
  ;; Kill auto-fill in git-commit mode
  (remove-hook 'git-commit-setup-hook #'git-commit-turn-on-auto-fill))

;; Don't overwrite M-w in magit mode, and clear mark when done
(defun my-magit-kill-ring-save ()
  (interactive)
  (call-interactively #'kill-ring-save)
  (deactivate-mark))

(with-eval-after-load "magit"
  (setopt magit-completing-read-function 'ivy-completing-read)
  ;; magit-log currently has some kind of transient bug, so don't show transient menu
  (define-key magit-mode-map (kbd "l") #'magit-log-current)
  (define-key magit-mode-map (kbd "M-w") #'my-magit-kill-ring-save)
  (define-key magit-hunk-section-map (kbd "RET") #'magit-diff-visit-worktree-file))

(defun my-preload-magit ()
  (require 'magit)
  (require 'git-commit))

(my-defer-startup #'my-preload-magit)

;; Map some magit keys globally
(global-set-key (kbd "C-x V") nil)
(global-set-key (kbd "C-x V a") 'magit-blame)
(global-set-key (kbd "C-x V b") 'magit-show-refs-current)
(global-set-key (kbd "C-x V l") 'magit-log-head)
(global-set-key (kbd "C-x V s") 'magit-status)

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
  (define-key org-mode-map (kbd "<M-left>") #'left-word)
  (define-key org-mode-map (kbd "<M-right>") #'right-word))

;; Project settings
(defvar my-project-command-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "a a") #'project-remember-projects-under)
    (define-key map (kbd "a s") #'my-asdf-vm-init)
    (define-key map (kbd "c") #'project-compile)
    (define-key map (kbd "f") #'project-find-file)
    (define-key map (kbd "g a") #'aidermacs-transient-menu)
    (define-key map (kbd "g g") #'my-gptel-start)
    (define-key map (kbd "g p") #'gptel-menu)
    (define-key map (kbd "g s") #'gptel-send)
    (define-key map (kbd "k") #'project-kill-buffers)
    (define-key map (kbd "n") #'my-org-find-notes-file)
    (define-key map (kbd "p") #'project-switch-project)
    (define-key map (kbd "t") #'project-switch-to-buffer)
    (define-key map (kbd "s r") #'rg-project)
    (define-key map (kbd "s s") #'my-counsel-ripgrep)
    (define-key map (kbd "w p") #'my-copy-project-relative-path-of-current-buffer)
    (define-key map (kbd "w w") #'my-copy-path-of-current-buffer)
    (define-key map (kbd "!") #'project-async-shell-command)
    (define-key map (kbd " ") #'my-org-capture-note)
    map))

(global-set-key (kbd "C-c p") my-project-command-map)
(global-set-key (kbd "C-c C-p") my-project-command-map)
(global-set-key (kbd "C-x g") #'mark-whole-buffer)
(global-set-key (kbd "C-x r r") #'rectangle-mark-mode)
(global-set-key (kbd "C-x p") #'other-window)

(defun my-kill-emacs ()
  (interactive)
  (let ((confirm-kill-emacs 'y-or-n-p))
    (call-interactively 'save-buffers-kill-emacs t)))
(global-set-key (kbd "C-x C-c") #'my-kill-emacs)
(global-set-key (kbd "C-x C-M-s") #'my-kill-emacs)

;; Map some keys to find-function/find-variable
(defvar my-find-things-map
  (let ((map (make-sparse-keymap)))
    (define-key map "f" #'find-function)
    (define-key map "v" #'find-variable)
    (define-key map "l" #'find-library)
    (define-key map "a" #'find-face-definition)
    map)
  "My key customizations for find-function and related things.")

(global-set-key (kbd "C-x F") my-find-things-map)
(global-set-key (kbd "C-x f") my-find-things-map)

(with-eval-after-load "view"
  ;; Make the `q' key bury the current buffer when viewing help
  (define-key view-mode-map "q" 'bury-buffer)
  ;; Make the <DEL> key scroll backwards in View mode
  (define-key view-mode-map [delete] 'View-scroll-page-backward))

(with-eval-after-load "info"
  ;; Make the <DEL> key scroll backwards in Info mode
  (define-key Info-mode-map [delete] 'Info-scroll-down))

;; diff-mode: Don't mess with M-q
(with-eval-after-load "diff-mode"
  (define-key diff-mode-map (kbd "M-q") 'fill-paragraph))

;; Typo prevention
(global-set-key (kbd "C-x 4") #'split-window-right)
(global-set-key (kbd "C-x C-b") #'ivy-switch-buffer)
(global-set-key (kbd "C-x C-d") #'counsel-find-file)
(global-set-key (kbd "C-x n") #'ivy-switch-buffer)

;; Disable some keybinds to avoid typos
(global-set-key [insert] (lambda () (interactive)))
(global-set-key [insertchar] (lambda () (interactive)))
(global-set-key (kbd "C-t") (lambda () (interactive)))
(global-set-key (kbd "C-z") (lambda () (interactive)))
(global-set-key (kbd "C-x C-z") (lambda () (interactive)))
(global-set-key (kbd "<mouse-2>") (lambda () (interactive)))
(global-set-key (kbd "<mouse-3>") (lambda () (interactive)))

;; Bind Apple-<key> to Alt-<key> for some Mac keys
(when (and my-remap-cmd-key-p (eq system-type 'darwin))
  (setopt ns-alternate-modifier 'meta)
  (setopt ns-command-modifier 'super))

(defun my-set-super-bindings ()
  (interactive)
  (with-eval-after-load "cider-repl"
    (define-key cider-repl-mode-map (kbd "s-n") #'cider-repl-next-input)
    (define-key cider-repl-mode-map (kbd "s-p") #'cider-repl-previous-input))

  (with-eval-after-load "magit"
    (define-key magit-mode-map (kbd "s-2") #'magit-section-show-level-2-all)
    (define-key magit-mode-map (kbd "s-4") #'magit-section-show-level-4-all)
    (define-key magit-status-mode-map (kbd "s-c") #'my-magit-kill-ring-save)
    (define-key magit-status-mode-map (kbd "s-w") #'my-magit-kill-ring-save))

  (with-eval-after-load "polymode"
    (define-key polymode-minor-mode-map (kbd "s-n") 'polymode-map))

  (global-set-key (kbd "s-:") #'eval-expression)
  (global-set-key (kbd "s-;") #'eval-expression)
  (global-set-key (kbd "s-<") #'beginning-of-buffer)
  (global-set-key (kbd "s-,") #'beginning-of-buffer)
  (global-set-key (kbd "s->") #'end-of-buffer)
  (global-set-key (kbd "s-.") #'end-of-buffer)
  (global-set-key (kbd "<s-left>") #'left-word)
  (global-set-key (kbd "<s-right>") #'right-word)
  (global-set-key (kbd "s-1") #'shell-command)
  (global-set-key (kbd "s-!") #'shell-command)
  (global-set-key (kbd "s-$") #'ispell-word)
  (global-set-key (kbd "s-a") #'mark-whole-buffer)
  (global-set-key (kbd "s-c") #'kill-ring-save)
  (global-set-key (kbd "s-m") (lambda () (interactive)))
  (global-set-key (kbd "s-p") #'project-find-file)
  (global-set-key (kbd "s-q") #'fill-paragraph)
  (global-set-key (kbd "s-w") #'kill-ring-save)
  (global-set-key (kbd "s-v") #'yank)
  (global-set-key (kbd "s-x") #'counsel-M-x)
  (global-set-key (kbd "s-y") #'counsel-yank-pop)
  (global-set-key (kbd "<C-s-left>") #'backward-sexp)
  (global-set-key (kbd "<C-s-right>") #'forward-sexp)
  (global-set-key (kbd "C-s-n") #'forward-list)
  (global-set-key (kbd "C-s-p") #'backward-list)
  (global-set-key (kbd "C-s-x") #'eval-defun)
  (global-set-key (kbd "C-s-\\") #'indent-region))

(defun my-set-mac-bindings ()
  (interactive)
  (global-set-key (kbd "<home>") #'beginning-of-line)
  (global-set-key (kbd "<end>") #'end-of-line)
  (global-set-key (kbd "s-<up>") #'scroll-down-command)
  (global-set-key (kbd "s-<down>") #'scroll-up-command)
  (global-set-key (kbd "s-<left>") #'beginning-of-line)
  (global-set-key (kbd "s-<right>") #'end-of-line))

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
