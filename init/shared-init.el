;;; Emacs initialization settings common to multiple computers
;;
;; Author: Michael Olson
;;
;; The contents of this file may be used, distributed, and modified
;; without restriction.

(require 'cl-seq)

;; Uncomment this to debug warnings
;;
;; (require 'warnings)
;; (defun display-warning (type message)
;;   (setq debug-on-error t)
;;   (error message))

;;; Options that change behavior of this file

(defvar my-default-font      nil)
(defvar my-default-emoji-font nil)
(defvar my-default-emoji-size nil)
(defvar my-theme             nil)
(defvar my-modus-theme       'modus-vivendi-deuteranopia)
(defvar my-modus-theme-overrides
  '((bg-main             "#1C1C1C")
    (bg-mode-line-active "#484d67")
    (comment             fg-dim)))
(defvar my-use-themes-p      (boundp 'custom-theme-load-path))
(defvar my-eslint-fix-enabled-p nil)
(defvar my-frame-height      50)
(defvar my-frame-width       120)
(defvar my-frame-maximize-if-pixel-width-lte 1440)
(defvar my-frame-maximize-p  t)
(defvar my-frame-pad-width   (if (eq system-type 'darwin) 65 nil))
(defvar my-frame-pad-height  (if (eq system-type 'darwin) 15 nil))
(defvar my-gptel-backend     'my-gptel--claude)
(defvar my-gptel-model       nil)
(defvar my-remap-cmd-key-p   t)
(defvar my-default-directory "~/")
(defvar my-changelog-address "user@example.com")
(defvar my-email-address     "user@example.com")
(defvar my-full-name         "Jane Doe")
(defvar my-emacs-path)
(setq my-emacs-path          (file-name-as-directory (expand-file-name my-emacs-path)))

(defvar my-server-start-p    t)
(defvar my-recent-files      nil)
(defvar my-settings-shared-p (not (file-exists-p (locate-user-emacs-file "settings.el"))))
(defvar my-system-paths
  (cond ((eq system-type 'darwin)
         ;; see also https://taonaw.com/2024/10/06/emacsplus-path-in.html
         `(,(concat my-emacs-path "bin")
           ,(concat my-emacs-path "node_modules/.bin")
           "~/bin"
           "/opt/homebrew/bin"
           "/opt/homebrew/sbin"
           "/usr/local/bin"
           "/System/Cryptexes/App/usr/bin"
           "/var/run/com.apple.security.cryptexd/codex.system/bootstrap/usr/local/bin"
           "/var/run/com.apple.security.cryptexd/codex.system/bootstrap/usr/bin"
           "/var/run/com.apple.security.cryptexd/codex.system/bootstrap/usr/appleinternal/bin"))
        ((eq system-type 'windows-nt)
         `(,(concat "C:/Program Files/Emacs/emacs-" emacs-version "/bin")
           "C:/Program Files/Git/bin"
           "C:/msys64/usr/bin"
           "c:/msys64/ucrt64/bin"
           ,(concat my-emacs-path "node_modules/.bin")
           "C:/Program Files/maven/bin"))
        (t `(,(concat my-emacs-path "node_modules/.bin")
             "/opt/maven/bin"))))
(setq my-system-paths (cl-remove-if-not #'file-exists-p my-system-paths))

(let* ((after- "<~|")
       (after-> (concat after- ">"))
       (after--> (concat after-> "-"))
       (-2+ "\\(-\\{2,\\}\\)")
       (-3+ "\\(-\\{3,\\}\\)")
       (after< "<>~|=")
       (after<- (concat after< "-"))
       (after>- "<>~|=-")
       (after= "<>~|=!"))

  (defvar my-prog-mode-ligatures
    ;; see https://github.com/mickeynp/ligature.el/wiki#cascadia--fira-code
    ;; some of the more obnoxious ones are removed
    `(("-" ,(format "[%s]+" after-->))
      ("<" ,(format "[%s]+" after<-))
      (">" ,(format "[%s]+" after>-))
      ("=" ,(format "[%s]+" after=))
      "www" "**" "***" "**/" "*/" "\\\\" "\\\\\\" "::"
      ":::" ":=" "!!" "!=" "!=="
      "##" "###" "####"
      ".-" ".=" ".." "..<" "..." "??" ";;" ";;;" "/*" "/**"
      "//" "///" "&&" "||" "||=" "|=" "|>"
      "++" "+++" "+>"
      "~>" "~~" "~~>" "%%"))

  (defvar my-web-mode-ligatures
    (mapcar (lambda (el)
              (cond ((stringp el) el)
                    ((string= (car el) "-")
                     `("-" ,(format "\\(-*[%s][%s]*\\|%s[%s]+\\|%s\\)"
                                    after- after-->
                                    -2+ after-->
                                    -2+)))
                    ((string= (car el) "<")
                     `("<" ,(format "\\(-*[%s][%s]*\\|%s\\)"
                                    after< after<- -3+)))
                    (t el)))
            my-prog-mode-ligatures)))

;; Add shared elisp directory (but prefer system libs)
(add-to-list 'load-path (concat my-emacs-path "elisp") t)

;;; Display

;; Allow maximizing frame
(require 'maxframe)
(when my-frame-pad-width
  (setq mf-max-width (- (display-pixel-width) my-frame-pad-width)))
(when my-frame-pad-height
  (setq mf-max-height (- (display-pixel-height) my-frame-pad-height)))

(defun my-default-font ()
  (or my-default-font
      (cond
       ((eq system-type 'darwin) "Fira Code Retina-18")
       ((eq system-type 'windows-nt) "Fira Code-11")
       ((memq window-system '(pgtk x)) "Fira Code-13")
       (t "Fira Code-17"))))

(defun my-default-emoji-font ()
  (or my-default-emoji-font
      (cond
       ((memq window-system '(pgtk x)) "Noto Color Emoji")
       (t "Noto Color Emoji"))))

(defun my-default-emoji-size ()
  (or my-default-emoji-size
      (cond
       ((eq system-type 'darwin) 18)
       ((eq system-type 'windows-nt) 11)
       ((memq window-system '(pgtk x)) 17)
       (t 21))))

(defun my-reset-font ()
  (interactive)
  (let ((default-font (my-default-font))
        (emoji-font (my-default-emoji-font))
        (emoji-size (my-default-emoji-size)))
    (set-frame-font default-font nil t)
    (set-face-attribute 'fixed-pitch nil :font default-font)
    (set-fontset-font t nil (font-spec :size emoji-size :name emoji-font))))

(defun my-reset-frame-size ()
  "Reset the size of the current frame according to `default-frame-alist'."
  (interactive)
  (let ((maximize-p my-frame-maximize-p))
    (when (and maximize-p my-frame-maximize-if-pixel-width-lte)
      (setq maximize-p (<= (display-pixel-width) my-frame-maximize-if-pixel-width-lte)))
    (cond ((and maximize-p (memq window-system '(pgtk x w32)))
           (set-frame-parameter nil 'fullscreen 'maximized))
          (maximize-p
           (maximize-frame))
          (t
           (dolist (param '(width height))
             (set-frame-parameter nil param (cdr (assoc param default-frame-alist))))))))

(defun my-reset-theme ()
  (interactive)
  (when my-use-themes-p
    (if my-modus-theme
        (progn
          (setopt modus-themes-common-palette-overrides my-modus-theme-overrides)
          (modus-themes-select my-modus-theme))
      (load-theme my-theme t))))

;; Support for font ligatures
(defun my-enable-ligatures ()
  ;; Enable ligatures in programming modes
  (interactive)
  (require 'ligature)
  (ligature-set-ligatures 'prog-mode my-prog-mode-ligatures)
  (global-ligature-mode t))

;; This function should be called on the emacsclient commandline in cases where no file is being passed on commandline.
(defun my-init-client-display ()
  (interactive)
  (if window-system
      (progn
        (my-reset-font)
        (add-to-list 'default-frame-alist
                     (cons 'font (cdr (assq 'font (frame-parameters)))))
        (when (or (not my-frame-maximize-p) my-frame-maximize-if-pixel-width-lte)
          (add-to-list 'default-frame-alist (cons 'height my-frame-height))
          (add-to-list 'default-frame-alist (cons 'width my-frame-width)))
        (when (eq window-system 'mac)
          ;; redisplay slowness https://github.com/hlissner/doom-emacs/issues/2217
          (add-to-list 'default-frame-alist '(inhibit-double-buffering . t))
          (ignore-errors
            (mac-auto-operator-composition-mode)))
        (when (memq window-system '(pgtk w32 x))
          (my-enable-ligatures))
        ;; Make sure DEL key does what I want
        (normal-erase-is-backspace-mode 1)
        ;; Show the menu if we are using X
        (set-frame-parameter nil 'menu-bar-lines 1))
    ;; Don't show the menu unless we are using X
    (set-frame-parameter nil 'menu-bar-lines 0))
  ;; Don't show scroll bars
  (ignore-errors (scroll-bar-mode -1))
  ;; Don't show the tool bar
  (ignore-errors (tool-bar-mode -1))
  ;; Initialize color theme
  (my-reset-theme)
  ;; Maximize frame or re-apply frame settings
  (when window-system
    (global-tab-line-mode 1)
    (my-reset-frame-size)))

;; Initialize display settings on startup
(my-init-client-display)

;; Give people something to look at while we load
(display-startup-screen)
(redisplay t)
(add-hook 'server-after-make-frame-hook 'my-init-client-display t)

;; Modeline theme
;; currently too large
;;
;; (require 'spaceline-config)
;; (spaceline-emacs-theme)

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

;;; Programs and features

;; Load `dired' itself, with `tramp' extension
(require 'dired)
(require 'dired-x)
(require 'wdired)
(require 'ffap)

;; Load tramp
(require 'tramp)

;; List directories first in dired
(require 'ls-lisp)

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

;; Support for s6-overlay containers: https://github.com/just-containers/s6-overlay
(setq auto-mode-interpreter-regexp
      (replace-regexp-in-string "/bin/env" "/\\(?:usr/\\)?bin/\\(?:with-cont\\)?env"
                                auto-mode-interpreter-regexp t t))

;; .env file support
(add-to-list 'auto-mode-alist '("\\.env\\..*\\'" . dotenv-mode))

;; Editorconfig support
(editorconfig-mode 1)

;; Edit Server support through Atomic Chrome / GhostText
(defun my-start-atomic-chrome ()
  (require 'atomic-chrome)
  (atomic-chrome-start-server))

(when my-server-start-p
  (my-defer-startup #'my-start-atomic-chrome))

;; Long lines support
(global-so-long-mode 1)
(add-to-list 'so-long-target-modes 'fundamental-mode)
(add-to-list 'so-long-target-modes 'web-mode)

;; Lisp REPL using SLIME
(require 'slime)
(slime-setup '(slime-repl))
(setopt slime-auto-connect 'always)
(setopt slime-kill-without-query-p t)
(setopt slime-protocol-version 'ignore)

;; Improved JSX support (disabled)
;;
;; (my-replace-cdrs-in-alist 'js-mode 'rjsx-mode 'interpreter-mode-alist)
;; (add-to-list 'auto-mode-alist '("\\.jsx?\\'" . rjsx-mode))
;;
;; Use plain old js-mode since it doesn't freeze when loading ES7 code with decorators
;; (add-to-list 'auto-mode-alist '("\\.jsx?\\'" . js-mode))

;; MariaDB/MySQL conf files
(add-to-list 'auto-mode-alist '("\\.cnf\\'" . conf-mode))

;; SystemD conf files
(add-to-list 'auto-mode-alist '("\\.service\\'" . conf-mode))

;; SSH conf files
(add-to-list 'auto-mode-alist '("_config\\'" . conf-mode))

;; Python uv lock files
(add-to-list 'auto-mode-alist '("/uv\\.lock\\'" . conf-toml-mode))

;; Flymake setup

(require 'flymake-stylelint)
(add-hook 'scss-mode-hook 'add-node-modules-path t)
(add-hook 'scss-mode-hook 'flymake-stylelint-enable t)

;; NodeJS REPL setup

(require 'js-comint)

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
   (node-repl-interaction-mode nil)
   (t nil)))

;; (when (eq system-type 'windows-nt)
;;   (setopt js-comint-program-command "C:/Program Files/nodejs/node.exe"))

(defun inferior-js-mode-hook-setup ()
  (add-hook 'comint-output-filter-functions 'js-comint-process-output))

(add-hook 'inferior-js-mode-hook 'inferior-js-mode-hook-setup t)

;; Web Mode setup

(defvar my--js-files-regex "\\.\\([jt]sx?\\|mjs\\)\\'")

(add-to-list 'auto-mode-alist '("\\.hbs\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.json\\'" . web-mode))

(define-derived-mode my-ts-web-mode web-mode "Web"
  "Variant of Web Mode that allows JS/TS-specific hooks to be executed.")

(add-to-list 'auto-mode-alist `(,my--js-files-regex . my-ts-web-mode))

(setq web-mode-content-types-alist `(("jsx" . ,my--js-files-regex)))

(defun my-define-web-mode (file-ext)
  (let* ((sym-name (symbol-name file-ext))
         (filename (concat "." sym-name))
         (mode-sym (intern (concat "my-" sym-name "-mode")))
         (mode-name-alias (intern (concat "my-" sym-name))))
    (eval `(defun ,mode-sym (&rest mode-args)
             (cl-letf (((symbol-function 'buffer-file-name)
                        (lambda () ,filename)))
               (apply #'my-ts-web-mode mode-args))))))

(my-define-web-mode 'js)
(my-replace-cdrs-in-alist 'js-mode 'my-js-mode 'interpreter-mode-alist)

;; (with-eval-after-load "web-mode"
;;   (define-key web-mode-map (kbd "C-c C-j") nil))

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

(defun my-ts-init ()
  "Hooks for Web mode JS/TS files."
  (node-repl-interaction-mode 1)
  (when (and (not (string-match-p "/node_modules/" default-directory))
             (executable-find "eslint"))
    (add-hook 'after-save-hook #'eslint-fix-file-and-revert-maybe t t)))

(defun my-eslint-disable-in-current-buffer ()
  (interactive)
  (flymake-mode nil)
  (set (make-local-variable 'my-eslint-fix-enabled-p) nil))

(defun my-setup-web-ligatures ()
  (interactive)
  (setq-local ligature-composition-table nil)
  (ligature-set-ligatures 'web-mode my-web-mode-ligatures))

(add-hook 'web-mode-hook #'add-node-modules-path t)
(add-hook 'web-mode-hook #'my-setup-web-ligatures t)
(add-hook 'my-ts-web-mode-hook #'my-ts-init t)

;; JS2 Mode setup (disabled)

(defun my-set-js2-mocha-externs ()
  (setq js2-additional-externs
        (mapcar 'symbol-name '(after afterEach before beforeEach describe expect it))))

(with-eval-after-load "js2-mode"
  ;; BUG: self is not a browser extern, just a convention that needs checking
  (setq js2-browser-externs (delete "self" js2-browser-externs))

  ;; Consider the chai 'expect()' statement to have side-effects, so we don't warn about it
  (defun js2-add-strict-warning (msg-id &optional msg-arg beg end)
    (if (and js2-compiler-strict-mode
             (not (and (string= msg-id "msg.no.side.effects")
                       (string= (buffer-substring-no-properties beg (+ beg 7)) "expect("))))
        (js2-report-warning msg-id msg-arg beg
                            (and beg end (- end beg)))))

  ;; Add support for some mocha testing externs
  (add-hook 'js2-init-hook #'my-set-js2-mocha-externs t))

;; Highlight node.js stacktraces in *compile* buffers
(defvar my-nodejs-compilation-regexp
  '("^[ \t]+at +\\(?:.+(\\)?\\([^()\n]+\\):\\([0-9]+\\):\\([0-9]+\\))?$" 1 2 3))

(with-eval-after-load "compile"
  (add-to-list 'compilation-error-regexp-alist-alist
               (cons 'nodejs my-nodejs-compilation-regexp))
  (add-to-list 'compilation-error-regexp-alist 'nodejs))

;; Highlight current line
(require 'hl-line-plus)
(hl-line-when-idle-interval 0.3)
(toggle-hl-line-when-idle 1)

;; Enable dumb-jump, which makes `C-c . .' jump to a function's definition
(require 'dumb-jump)
(setopt dumb-jump-selector 'ivy)
(add-hook 'xref-backend-functions #'dumb-jump-xref-activate)

(defvar my-xref-map
  (let ((map (make-sparse-keymap)))
    (define-key map "." #'xref-find-definitions)
    (define-key map "," #'xref-go-back)
    map)
  "My key customizations for dumb-jump.")

(global-set-key (kbd "C-c .") my-xref-map)

(defvar my-xref-minor-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c .") my-xref-map)
    map))

(define-minor-mode my-xref-minor-mode
  "Minor mode for jumping to variable and function definitions"
  :keymap my-xref-minor-mode-map)

;; Java
(require 'java-mode-indent-annotations)
(require 'google-c-style)
(add-hook 'c-mode-common-hook 'google-set-c-style t)
(add-hook 'c-mode-common-hook 'my-xref-minor-mode t)

;; Kotlin
(add-to-list 'auto-mode-alist '("\\.kts?\\'" . kotlin-mode) t)
(autoload #'kotlin-mode "kotlin-mode" "Major mode for editing Kotlin." t nil)

;; C#
(with-eval-after-load "csharp-mode"
  (define-key csharp-mode-map (kbd "C-c .") nil))

;; ANSI colors in compile buffer
(require 'ansi-color)
(defun colorize-compilation-buffer ()
  (read-only-mode 0)
  (ansi-color-apply-on-region (point-min) (point-max))
  (read-only-mode 1))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer t)

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

;; Set up gptel
(defvar my-gptel--claude
  (gptel-make-anthropic "Claude"
    :stream t
    :key #'gptel-api-key-from-auth-source))

(defun my-gptel-start ()
  "Start gptel with a default buffer name."
  (interactive)
  (require 'gptel)
  (let* ((backend (symbol-value my-gptel-backend))
         (backend-name
          (format "*%s*" (gptel-backend-name backend)))
         (model (or my-gptel-model (car (gptel-backend-models backend)))))
    (setq gptel-backend backend
          gptel-model model)
    (with-suppressed-warnings ((obsolete warning-level-aliases))
      (switch-to-buffer (gptel backend-name)))))

;; Elysium for AI queries in code
(add-to-list 'load-path (concat my-emacs-path "elisp/elysium"))
(autoload #'elysium-query "elysium" "send query to elysium" t)
(add-hook 'prog-mode-hook 'smerge-mode t)

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

(defun my-project-find-go-module (dir)
  (when-let ((root (locate-dominating-file dir "go.mod")))
    (cons 'go-module root)))

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
  (define-key project-prefix-map "m" #'magit-project-status)

  ;; from https://github.com/golang/tools/blob/master/gopls/doc/emacs.md#configuring-project-for-go-modules-in-emacs
  (cl-defmethod project-root ((project (head go-module)))
    (cdr project))

  (add-hook 'project-find-functions #'my-project-find-go-module))

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

;; Set up apheleia for automatic running of prettier
(apheleia-global-mode 1)

;; Set up eglot for LSP features
(defvar my-eglot-diagnostics-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<mouse-3>") 'eglot-code-actions-at-mouse)
    map)
  "Keymap active in Eglot-backed Flymake diagnostic overlays.")
(setq eglot-diagnostics-map my-eglot-diagnostics-map)

(require 'eglot)
(setopt eglot-send-changes-idle-time 0.2)
(add-hook 'c-mode-common-hook 'eglot-ensure)
(add-hook 'csharp-mode-hook 'eglot-ensure)
(add-hook 'my-ts-web-mode-hook 'eglot-ensure)
(add-hook 'rust-mode-hook 'eglot-ensure)
(add-hook 'go-ts-mode-hook 'eglot-ensure)
(add-hook 'go-mod-ts-mode-hook 'eglot-ensure)

(defclass eglot-deno (eglot-lsp-server) ()
  :documentation "A custom class for deno lsp.")

(cl-defmethod eglot-initialization-options ((server eglot-deno))
  "Passes through required deno initialization options"
  (list :enable t
        :lint t))

(if (executable-find "deno")
    (add-to-list 'eglot-server-programs
                 '((my-ts-web-mode) .
                   (eglot-deno "deno" "lsp")))
  (add-to-list 'eglot-server-programs
               `((my-ts-web-mode) .
                 ("typescript-language-server" "--stdio"
                  :initializationOptions
                  (:plugins [(:name "typescript-eslint-language-service"
                                    :location ,my-emacs-path)])))))

(add-to-list 'eglot-server-programs
             '((rust-ts-mode rust-mode) .
               ("rust-analyzer" :initializationOptions (:check (:command "clippy")))))

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

;; Tree-sitter
(add-to-list 'treesit-extra-load-path (concat my-emacs-path "extra/tree-sitter-module/dist"))
(require 'go-ts-mode)

;; All programming modes
(defun my-turn-on-display-line-numbers-mode ()
  (interactive)
  (display-line-numbers-mode 1))

(defun my-turn-off-display-line-numbers-mode ()
  (interactive)
  (display-line-numbers-mode -1))

(add-hook 'prog-mode-hook 'my-turn-on-display-line-numbers-mode t)
(add-hook 'lisp-interaction-mode-hook 'my-turn-off-display-line-numbers-mode t)

;; Markdown support

(add-to-list 'load-path (concat my-emacs-path "elisp/poly-markdown"))
(autoload #'poly-gfm-mode "poly-markdown" t)

(add-to-list 'auto-mode-alist '("\\.md\\'" . poly-gfm-mode))
(add-to-list 'auto-mode-alist '("\\.mdx\\'" . poly-gfm-mode))

(defun my-define-web-polymode (file-ext)
  (let* ((sym-name (symbol-name file-ext))
         (filename (concat "." sym-name))
         (mode-sym (intern (concat "my-" sym-name "-mode")))
         (mode-name-alias (intern (concat "my-" sym-name))))
    (eval `(defun ,mode-sym (&rest mode-args)
             (cl-letf (((symbol-function 'buffer-file-name)
                        (lambda () ,filename)))
               (apply #'web-mode mode-args))))
    (add-to-list 'polymode-mode-name-aliases (cons file-ext mode-name-alias))))

(defun my-define-ts-web-polymode (file-ext)
  (let* ((sym-name (symbol-name file-ext))
         (filename (concat "." sym-name))
         (mode-sym (intern (concat "my-" sym-name "-mode")))
         (mode-name-alias (intern (concat "my-" sym-name))))
    (eval `(defun ,mode-sym (&rest mode-args)
             (cl-letf (((symbol-function 'buffer-file-name)
                        (lambda () ,filename)))
               (apply #'my-ts-web-mode mode-args))))
    (add-to-list 'polymode-mode-name-aliases (cons file-ext mode-name-alias))))

(with-eval-after-load "polymode-core"
  ;; Commented out since the font-locking for Web mode tends to bleed into other areas
  ;; of the file.
  ;; (dolist (file-ext '(hbs html json))
  ;;   (my-define-web-polymode file-ext))
  ;; (dolist (file-ext '(js jsx))
  ;;   (my-define-ts-web-polymode file-ext))
  ;; (add-to-list 'polymode-mode-name-aliases '(javascript . my-js))
  (add-to-list 'polymode-mode-name-aliases '(javascript . js)))

(with-eval-after-load "poly-markdown"
  (my-replace-cdrs-in-alist 'poly-markdown-mode 'poly-gfm-mode 'auto-mode-alist))

;; Prefer Github-flavored Markdown
(my-replace-cdrs-in-alist 'markdown-mode 'poly-gfm-mode 'auto-mode-alist)

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
(add-hook 'yaml-mode-hook #'my-run-prog-mode-hooks t)

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
  (define-key magit-mode-map (kbd "M-w") #'my-magit-kill-ring-save))

(defun my-preload-magit ()
  (require 'magit)
  (require 'git-commit))

(my-defer-startup #'my-preload-magit)

;; Map some magit keys globally
(global-set-key "\C-xV" nil)
(global-set-key "\C-xVa" 'magit-blame)
(global-set-key "\C-xVb" 'magit-show-refs-current)
(global-set-key "\C-xVl" 'magit-log-head)
(global-set-key "\C-xVs" 'magit-status)

;; Don't display any minor modes on the mode-line
(require 'minions)
(setopt minions-mode-line-delimiters '("" . ""))
(setopt minions-mode-line-lighter " ")
(minions-mode 1)

;; Clojure mode settings
(with-eval-after-load "clojure-mode"
  (require 'cider))

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
    (define-key map (kbd "g g") #'my-gptel-start)
    (define-key map (kbd "g p") #'gptel-menu)
    (define-key map (kbd "g q") #'elysium-query)
    (define-key map (kbd "g r") #'gptel-rewrite)
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
(global-set-key (kbd "C-x g") 'goto-line)
(global-set-key (kbd "C-x r r") 'rectangle-mark-mode)

(defun my-kill-emacs ()
  (interactive)
  (let ((confirm-kill-emacs 'y-or-n-p))
    (call-interactively 'save-buffers-kill-emacs t)))
(global-set-key (kbd "C-x C-c") #'my-kill-emacs)
(global-set-key (kbd "C-x C-M-s") #'my-kill-emacs)

;; Make adding entries to debian/changelog easy
(global-set-key "\C-xD" nil)
(global-set-key "\C-xDa" 'debian-changelog-add-entry)

;; Map some keys to find-function/find-variable
(defvar my-find-things-map
  (let ((map (make-sparse-keymap)))
    (define-key map "f" #'find-function)
    (define-key map "v" #'find-variable)
    (define-key map "l" #'find-library)
    (define-key map "a" #'find-face-definition)
    map)
  "My key customizations for find-function and related things.")

(global-set-key "\C-xF" my-find-things-map)
(global-set-key "\C-xf" my-find-things-map)

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

(with-eval-after-load "cider-repl"
  (define-key cider-repl-mode-map (kbd "C-d") #'cider-quit))

;; Use Ivy instead of the buffer list when I typo it
(global-set-key "\C-x\C-b" 'ivy-switch-buffer)

;; Disable some keybinds to avoid typos
(global-set-key [insert] (lambda () (interactive)))
(global-set-key [insertchar] (lambda () (interactive)))
(global-set-key "\C-t" (lambda () (interactive)))
(global-set-key "\C-z" (lambda () (interactive)))
(global-set-key "\C-x\C-z" (lambda () (interactive)))
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
  (global-set-key (kbd "s-q") #'my-kill-emacs)
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

(add-hook 'server-after-make-frame-hook 'my-init-client-keys t)
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
