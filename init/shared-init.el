;;; Emacs initialization settings common to multiple computers
;;
;; Author: Michael Olson
;;
;; The contents of this file may be used, distributed, and modified
;; without restriction.

(require 'cl)

;;; Options that change behavior of this file

(defvar my-default-font      (cond
                              ((eq window-system 'w32) "Inconsolata-15")
                              (t "Inconsolata-18")))
(defvar my-theme             'sanityinc-tomorrow-eighties)
(defvar my-frame-height      50)
(defvar my-frame-width       120)
(defvar my-frame-maximize-p  t)
(defvar my-frame-pad-width   (if (eq system-type 'darwin) 65 nil))
(defvar my-frame-pad-height  (if (eq system-type 'darwin) 15 nil))
(defvar my-default-directory "~/")
(defvar my-changelog-address "user@example.com")
(defvar my-email-address     "user@example.com")
(defvar my-full-name         "Jane Doe")
(defvar my-emacs-path)
(setq my-emacs-path          (file-name-as-directory (expand-file-name my-emacs-path)))

(defvar my-server-start-p    t)
(defvar my-use-themes        (boundp 'custom-theme-load-path))
(defvar my-emacs-features    '(org))
(defvar my-recent-files      nil)
(defvar my-settings-shared-p (not (file-exists-p (locate-user-emacs-file "settings.el"))))
(defvar my-system-paths
  (cond ((eq system-type 'darwin)
         '("~/emacs-shared/bin"
           "~/bin"
           "/Applications/Xcode.app/Contents/Developer/usr/bin"
           "/usr/local/bin"))
        ((eq system-type 'windows-nt)
         '("C:/Program Files (x86)/Emacs/bin"
	   "C:/MinGW/bin"
	   "C:/MinGW/msys/1.0/bin"
           "C:/Program Files/maven/bin"
	   "C:/Program Files (x86)/Aspell/bin"
	   "C:/Program Files (x86)/Git/bin"
	   "C:/Program Files (x86)/PuTTY"))
        (t '("/opt/maven/bin"))))
(setq my-system-paths (remove-if-not #'file-exists-p my-system-paths))

;; SLIME settings
(defvar my-slime-function (and (executable-find "node") #'my-slime-connect-nodejs))

;;; Display

;; Add shared elisp directory (but prefer system libs)
(add-to-list 'load-path (concat my-emacs-path "elisp") t)

;; Activate packages
(package-initialize)

;; Allow maximizing frame
(require 'maxframe)
(when my-frame-pad-width
  (setq mf-max-width (- (display-pixel-width) my-frame-pad-width)))
(when my-frame-pad-height
  (setq mf-max-height (- (display-pixel-height) my-frame-pad-height)))

(defun my-reset-font ()
  (interactive)
  (when my-default-font
    (set-default-font my-default-font)))

(defun my-reset-frame-size ()
  "Reset the size of the current frame according to `default-frame-alist'."
  (interactive)
  (cond ((and my-frame-maximize-p (memq window-system '(w32 x)))
         (set-frame-parameter nil 'fullscreen 'maximized))
        (my-frame-maximize-p
         (maximize-frame))
        (t
         (dolist (param '(width height))
           (set-frame-parameter nil param (cdr (assoc param default-frame-alist)))))))

(defun my-reset-theme ()
  (interactive)
  (when my-use-themes
    (load-theme my-theme t)))

;; This function should be called on the emacsclient commandline in cases where no file is being passed on commandline.
(defun my-init-client ()
  (interactive)
  (if window-system
      (progn
        (my-reset-font)
        (when my-default-font
          (add-to-list 'default-frame-alist
                       (cons 'font (cdr (assq 'font (frame-parameters))))))
        (unless my-frame-maximize-p
          (add-to-list 'default-frame-alist (cons 'height my-frame-height))
          (add-to-list 'default-frame-alist (cons 'width my-frame-width)))
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
    (my-reset-frame-size)))

;; Initialize display settings on startup
(my-init-client)

;; Give people something to look at while we load
(display-startup-screen)
(redisplay t)
(add-hook 'server-visit-hook 'my-init-client)

;; Modeline theme
(require 'spaceline-config)
(spaceline-emacs-theme)

;; Tasks that are run after initial startup for appearance of speed
(defvar my-deferred-startup-hook '(display-startup-echo-area-message))
(defun my-defer-startup (func)
  "Defer running a task until sometime after Emacs has started."
  (add-hook 'my-deferred-startup-hook func))
(defun my-run-deferred-tasks ()
  (run-hooks 'my-deferred-startup-hook))

(run-with-idle-timer 0.2 nil #'my-run-deferred-tasks)

;;; OS Setup

;; Make it easier to use find-library to get to this file
(add-to-list 'load-path (concat my-emacs-path "init"))

(when my-system-paths
  (setq exec-path (append my-system-paths exec-path))
  (setenv "PATH" (mapconcat (lambda (path)
                              (if (eq system-type 'windows-nt)
                                  (replace-regexp-in-string "/" "\\\\" path)
                                path))
                            (append my-system-paths (list (getenv "PATH")))
                            (if (eq system-type 'windows-nt) ";" ":"))))

;; Setup manpage browsing
(if (eq system-type 'windows-nt)
    (progn
      (setenv "MANPATH" (concat "C:\\MinGW\\share\\man;"
                                "C:\\Program Files (x86)\\Git\\share\\man"))
      (require 'woman)
      (defalias 'man 'woman))
  (require 'man))

;;; Customizations

;; Default values for some customization options
(setq directory-free-space-args "-Pkl")

(when (eq system-type 'windows-nt)
  (setq directory-free-space-args nil))

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
      (when (string-match "\\.el$" file)
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

(defun increment-number-at-point ()
  (interactive)
  (skip-chars-backward "0123456789")
  (or (looking-at "[0123456789]+")
      (error "No number at point"))
  (replace-match (number-to-string (1+ (string-to-number (match-string 0))))))

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
  (setq user-full-name my-full-name)
  ;; changelog email addresses
  (setq add-log-mailing-address my-changelog-address)
  (setq debian-changelog-mailing-address my-changelog-address)
  ;; email addresses
  (setq post-email-address my-email-address)
  (setq user-mail-address my-email-address))
(my-update-personal-info)

;;; Programs and features

(defun my-emacs-feature-enabled (feature)
  (and (boundp 'my-emacs-features)
       (memq feature my-emacs-features)))

;; Load `dired' itself, with `tramp' extension
(require 'dired)
(require 'dired-x)
(require 'wdired)

;; Load tramp
(add-to-list 'load-path (concat my-emacs-path "elisp/tramp/lisp"))
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
          (bufferp output-buffer)
          (stringp output-buffer))
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
(add-to-list 'load-path (concat my-emacs-path "elisp/docker-tramp"))
(require 'docker-tramp)

(defun my-docker-machine-env ()
  (interactive)
  (let* ((machine "default")
         (out (shell-command-to-string (concat "docker-machine env " machine)))
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

;; Improved Javascript support
(add-to-list 'auto-mode-alist '("\\.jsx?\\'" . js2-jsx-mode))
(add-to-list 'interpreter-mode-alist '("node" . js2-jsx-mode))

(eval-after-load "js2-mode"
  '(progn
     ;; BUG: self is not a browser extern, just a convention that needs checking
     (setq js2-browser-externs (delete "self" js2-browser-externs))

     ;; Consider the chai 'expect()' statement to have side-effects, so we don't warn about it
     (defun js2-add-strict-warning (msg-id &optional msg-arg beg end)
       (if (and js2-compiler-strict-mode
                (not (and (string= msg-id "msg.no.side.effects")
                          (string= (buffer-substring-no-properties beg (+ beg 7)) "expect("))))
           (js2-report-warning msg-id msg-arg beg
                               (and beg end (- end beg)))))

     (eval-after-load "auto-complete"
       '(progn
          (add-to-list 'ac-modes 'js2-jsx-mode)))

     ;; Add support for some mocha testing externs
     (setq-default js2-additional-externs
                   (mapcar 'symbol-name
                           '(after afterEach before beforeEach describe it)))))

;; Highlight node.js stacktraces in *compile* buffers
(defvar my-nodejs-compilation-regexp
  '("^[ \t]+at +\\(?:.+(\\)?\\([^()\n]+\\):\\([0-9]+\\):\\([0-9]+\\))?$" 1 2 3))

(eval-after-load "compile"
  '(progn
     (add-to-list 'compilation-error-regexp-alist-alist
                  (cons 'nodejs my-nodejs-compilation-regexp))
     (add-to-list 'compilation-error-regexp-alist 'nodejs)))

;; Highlight current line
(require 'hl-line)
(global-hl-line-mode 1)

;; Node REPL using SLIME
(require 'slime)
(autoload 'slime-js-minor-mode "slime-js" nil t)
(defun my-turn-on-slime-js ()
  (interactive)
  (slime-js-minor-mode 1))
(add-hook 'js-mode-hook #'my-turn-on-slime-js)
(slime-setup '(slime-repl slime-js))
(setq slime-auto-connect 'always)
(setq slime-kill-without-query-p t)
(setq slime-protocol-version 'ignore)

(defun my-slime-connect-nodejs ()
  (interactive)
  (let ((process (slime-connect "localhost" 4005)))
    (setf (slime-connection-name process) "NODE")))

;; Make this configurable, since we may have SLIME users who don't want node.js connection made
;; unconditionally
(when (and my-slime-function (fboundp my-slime-function))
  (defalias 'slime my-slime-function))

;; CIDER setup
(require 'ac-cider)
(add-hook 'cider-mode-hook 'ac-flyspell-workaround)
(add-hook 'cider-mode-hook 'ac-cider-setup)
(add-hook 'cider-repl-mode-hook 'ac-cider-setup)
(eval-after-load "auto-complete"
  '(progn
     (add-to-list 'ac-modes 'cider-mode)
     (add-to-list 'ac-modes 'cider-repl-mode)))

;; Java
(require 'java-mode-indent-annotations)
(require 'google-c-style)
(add-hook 'c-mode-common-hook 'google-set-c-style)

;; ANSI colors in compile buffer
(require 'ansi-color)
(defun colorize-compilation-buffer ()
  (toggle-read-only)
  (ansi-color-apply-on-region (point-min) (point-max))
  (toggle-read-only))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)

;; Load smex, which makes M-x work better on Ivy
(add-hook 'after-init-hook 'smex-initialize)

;; Ivy, Counsel, and Swiper
(add-to-list 'load-path (concat my-emacs-path "elisp/swiper"))
(require 'counsel)
(ivy-mode 1)
(setq ivy-use-virtual-buffers t)
(setq ivy-count-format "(%d/%d) ")
(setq ivy-re-builders-alist
      '((ivy-switch-buffer . ivy--regex-plus)
        (swiper . ivy--regex-plus)
        (t . ivy--regex-fuzzy)))
(setq counsel-mode-override-describe-bindings t)
(counsel-mode 1)

(define-key ivy-minibuffer-map (kbd "C-r") 'ivy-previous-line-or-history)
(define-key ivy-occur-grep-mode-map "r" 'ivy-wgrep-change-to-wgrep-mode)

(global-set-key (kbd "C-s") 'counsel-grep-or-swiper)
(global-set-key (kbd "C-c C-r") 'ivy-resume)

;; Enable projectile, a way to quickly find files in projects
(require 'projectile)
(projectile-global-mode 1)
(global-set-key "\C-cp" projectile-mode-map)
(setq projectile-completion-system 'ivy)
(setq projectile-indexing-method 'alien)

;; Insinuate with ripgrep
(defun my-projectile-ripgrep (regexp &optional arg)
  "Run a Ripgrep search with `REGEXP' rooted at the current projectile project root.

With an optional prefix argument ARG, find a symbol at point for the initial value of REGEXP."
  (interactive (list (and current-prefix-arg (projectile-symbol-or-selection-at-point))))
  (counsel-rg regexp (projectile-project-root)))

(eval-after-load "ripgrep"
  '(progn
     (define-key ripgrep-search-mode-map (kbd "TAB") #'compilation-next-error)
     (define-key ripgrep-search-mode-map (kbd "<backtab>") #'compilation-previous-error)))

(define-key projectile-command-map "s" #'my-projectile-ripgrep)

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
(add-hook 'ediff-keymap-setup-hook 'my-ediff-extra-keys)

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
(add-hook 'texinfo-mode-hook 'my-texinfo-extra-keys)

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
(setq tramp-backup-directory-alist backup-directory-alist)

;; Navigate the kill ring when doing M-y
(browse-kill-ring-default-keybindings)

;; extension of mine to make list editing easy
(require 'edit-list)

;; Markdown support, preferring Github-flavored Markdown
(mapc #'(lambda (el)
          (when (eq (cdr el) 'markdown-mode)
            (setcdr el 'gfm-mode)))
      auto-mode-alist)

;; Don't mess with keys that I'm used to
(defun my-markdown-mode-keys ()
  (define-key markdown-mode-map (kbd "<M-right>") #'forward-word)
  (define-key markdown-mode-map (kbd "<M-left>") #'backward-word))
(add-hook 'markdown-mode-hook #'my-markdown-mode-keys)

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

;; auto-completion for various modes
(require 'auto-complete-config)
(ac-config-default)

;; Setup info for manually compiled packages
(add-to-list 'Info-default-directory-list (concat my-emacs-path "share/info"))

;; Magit settings
(eval-after-load "git-commit"
  '(progn
     ;; Kill auto-fill in git-commit mode
     (remove-hook 'git-commit-setup-hook #'git-commit-turn-on-auto-fill)))

;; Don't overwrite M-w in magit mode, and clear mark when done
(defun my-magit-kill-ring-save ()
  (interactive)
  (call-interactively #'kill-ring-save)
  (deactivate-mark))

;; Monkey-patch this to prefer having `P u` go to an upstream with same name as the current branch
(defun magit-remote-branch-at-point ()
  (magit-section-case
    (branch (concat (car (save-match-data (magit-list-remotes)))
                    "/" (magit-section-value it)))
    (commit (magit-name-remote-branch (magit-section-value it)))
    (status (concat (car (save-match-data (magit-list-remotes)))
                    "/" (magit-get-current-branch)))))

(defadvice magit-read-starting-point (around magit-read-starting-point-remote-off activate)
  (let ((magit-prefer-remote-upstream nil))
    ad-do-it))

(eval-after-load "magit"
  '(progn
     (setq magit-completing-read-function 'ivy-completing-read)
     (define-key magit-mode-map (kbd "M-w") #'my-magit-kill-ring-save)))

(defun my-preload-magit ()
  (require 'magit)
  (require 'git-commit))

(my-defer-startup #'my-preload-magit)

;; Map some magit keys globally
(global-set-key "\C-xV" nil)
(global-set-key "\C-xVa" 'magit-blame-popup)
(global-set-key "\C-xVb" 'magit-show-refs-current)
(global-set-key "\C-xVl" 'magit-log-head)
(global-set-key "\C-xVs" 'magit-status)

;; Don't display some minor modes on the mode-line
(eval-after-load "auto-complete" '(diminish 'auto-complete-mode))
(eval-after-load "autorevert" '(diminish 'auto-revert-mode))
(eval-after-load "counsel" '(diminish 'counsel-mode))
(eval-after-load "ivy" '(diminish 'ivy-mode))
(eval-after-load "slime-js" '(diminish 'slime-js-minor-mode))

;;; BEGIN Org ;;;
(when (my-emacs-feature-enabled 'org)

(defun my-org-find-notes-file ()
  (interactive)
  (require 'org)
  (find-file org-default-notes-file))

(defun my-org-capture-note ()
  (interactive)
  (require 'org-capture)
  (org-capture nil "n"))

(define-key projectile-command-map "n" #'my-org-find-notes-file)
(define-key projectile-command-map " " #'my-org-capture-note)

(eval-after-load "org"
  '(progn
     (define-key org-mode-map (kbd "<M-left>") #'left-word)
     (define-key org-mode-map (kbd "<M-right>") #'right-word)))
);;; END org ;;;

;;; Key customizations

(global-set-key "\C-xg" 'goto-line)

(defun my-kill-emacs ()
  (interactive)
  (call-interactively 'save-buffers-kill-emacs t))
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

(eval-after-load "view"
  '(progn
     ;; Make the `q' key bury the current buffer when viewing help
     (define-key view-mode-map "q" 'bury-buffer)
     ;; Make the <DEL> key scroll backwards in View mode
     (define-key view-mode-map [delete] 'View-scroll-page-backward)))

(eval-after-load "info"
  '(progn
     ;; Make the <DEL> key scroll backwards in Info mode
     (define-key Info-mode-map [delete] 'Info-scroll-down)))

;; diff-mode: Don't mess with M-q
(eval-after-load "diff-mode"
  '(progn
     (define-key diff-mode-map (kbd "M-q") 'fill-paragraph)))

;; Use Ivy instead of the buffer list when I typo it
(global-set-key "\C-x\C-b" 'ivy-switch-buffer)

;; Disable some keybinds to avoid typos
(global-set-key [insert] (lambda () (interactive)))
(global-set-key [insertchar] (lambda () (interactive)))
(global-set-key "\C-t" (lambda () (interactive)))
(global-set-key "\C-z" (lambda () (interactive)))
(global-set-key "\C-x\C-z" (lambda () (interactive)))

;; Change to home dir
(defun my-change-to-default-dir ()
  (interactive)
  (setq-default default-directory (expand-file-name my-default-directory))
  (setq default-directory (expand-file-name my-default-directory)))
(add-hook 'after-init-hook #'my-change-to-default-dir)

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
(add-hook 'after-init-hook #'my-kill-splash-screen)

(provide 'shared-init)
;;; shared-init.el ends here
