;;; Emacs initialization settings common to multiple computers
;;
;; Author: Michael Olson
;;
;; The contents of this file may be used, distributed, and modified
;; without restriction.

(require 'cl)

;;; Options that change behavior of tbis file

(defvar my-default-font      "Inconsolata-14")
(defvar my-frame-height      50)
(defvar my-frame-width       120)
(defvar my-frame-maximize-p  (if (memq window-system '(ns w32)) t nil))
(defvar my-frame-pad-width   (if (eq window-system 'ns) 65 nil))
(defvar my-frame-pad-height  (if (eq window-system 'ns) 15 nil))
(defvar my-default-directory "~/")
(defvar my-changelog-address "user@example.com")
(defvar my-email-address     "user@example.com")
(defvar my-full-name         "Jane Doe")
(defvar my-irc-handle        "anonymous")
(defvar my-emacs-path)
(setq my-emacs-path          (file-name-as-directory (expand-file-name my-emacs-path)))

(defvar my-server-start-p    t)
(defvar my-use-themes        (boundp 'custom-theme-load-path))
(defvar my-emacs-features    (if (string-equal "root" (getenv "USER"))
                                 nil
                               '(erc magit)))
(defvar my-recent-files      nil)
(defvar my-settings-shared-p (not (file-exists-p (locate-user-emacs-file "settings.el"))))
(defvar my-system-paths
  (cond ((eq system-type 'darwin)
         '("~/bin"
           "/Applications/Xcode.app/Contents/Developer/usr/bin"
           "/usr/local/bin"))
        ((eq system-type 'windows-nt)
         '("C:/Program Files (x86)/Emacs/bin"
	   "C:/MinGW/bin"
	   "C:/MinGW/msys/1.0/bin"
	   "C:/Program Files (x86)/Git/bin"
	   "C:/Program Files (x86)/Aspell/bin"
	   "C:/Program Files (x86)/PuTTY"
           "C:/Program Files (x86)/sbt/bin"))
        (t nil)))
(setq my-system-paths (remove-if-not #'file-exists-p my-system-paths))

;; EMMS settings
(defvar my-emms-info-annoying-titles
  '("[[:blank:]]*([^(]*\\(Album\\|LP\\|Remastered\\)\\( Version\\)?)\\([[:blank:]]\\|$\\)"
    "[[:blank:]]*\\[Explicit\\]$")
  "List of annoying things in titles to strip out.")
(defvar my-emms-np-interval 5
  "How often (in seconds) to check whether the current song has changed.")
(defvar my-music-local-path  "~/Music")
(defvar my-music-remote-path "~/Music")
(defvar my-emms-playlist-dir (concat my-music-local-path "/playlists"))

;; Muse settings
(defvar my-muse-path         (concat my-emacs-path "tpl/muse/"))

;; grep-find settings
(defvar my-ignored-dir-regex  "\\(CVS\\|\\.svn\\|\\.bzr\\|\\.hg\\|\\.git\\)")
(defvar my-ignored-file-regex "\\(.*\\.min\\..*\\)")

;; SLIME settings
(defvar my-slime-function    (and (executable-find "node") #'my-slime-connect-nodejs))

;;; Display

;; Add shared elisp directory (but prefer system libs)
(add-to-list 'load-path (concat my-emacs-path "elisp") t)

;; Add Solarized themes
(when my-use-themes
  (add-to-list 'custom-theme-load-path (concat my-emacs-path "elisp/emacs-color-theme-solarized")))

;; Allow maximizing frame
(add-to-list 'load-path (concat my-emacs-path "elisp/maxframe-el"))
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
  (cond (my-frame-maximize-p
         (maximize-frame))
        (t
         (dolist (param '(width height))
           (set-frame-parameter nil param (cdr (assoc param default-frame-alist)))))))

(defun my-reset-theme ()
  (interactive)
  (when my-use-themes
    (load-theme 'solarized-dark t)))

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

;; Give the user something to look at while we load
(display-startup-screen)
(redisplay t)

(add-hook 'server-visit-hook 'my-init-client)
(global-set-key (kbd "C-x W") 'my-reset-frame-size)

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
(setq directory-free-space-args "-Pkl"
      grep-find-command (concat "find . -type f ! -regex \".*/" my-ignored-dir-regex "/.*\""
                                " ! -regex \".*/" my-ignored-file-regex "\""
                                " -print0 | xargs -0" (if (eq system-type 'darwin) "" " -e")
                                " egrep -nH -e "))

(when (eq system-type 'windows-nt)
  (setq directory-free-space-args nil))

;; Load customizations
(setq custom-file (if my-settings-shared-p
                      (concat my-emacs-path "init/settings.el")
                    (locate-user-emacs-file "settings.el")))
(when (file-exists-p custom-file)
  (load custom-file))

;;; Process startup files

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
  ;; IRC handle
  (setq erc-email-userid my-irc-handle)
  (setq erc-nick my-irc-handle)
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
(require 'tramp)

;; List directories first in dired
(require 'ls-lisp)

;; Don't slow down ls and don't make dired output too wide on w32 systems
(setq w32-get-true-file-attributes nil)

;; Make shell commands run in unique buffer so we can have multiple at once, and run all shell
;; asynchronously.  Taken in part from EmacsWiki: ExecuteExternalCommand page.

(defadvice erase-buffer (around erase-buffer-noop)
  "Make erase-buffer do nothing; only used in conjunction with shell-command.")

(defadvice shell-command (around shell-command-unique-buffer)
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
(ad-activate 'shell-command)

;; Load php-mode
(add-to-list 'load-path (concat my-emacs-path "elisp/php-mode"))
(require 'php-mode)

;; Load Sepia for editing Perl files
(add-to-list 'load-path (concat my-emacs-path "elisp/sepia"))
(setq sepia-perl5lib (list (expand-file-name (concat my-emacs-path "elisp/sepia/lib"))))

;; Don't make sepia the default Perl mode; its q() font-lock handling is terrible
;(defalias 'perl-mode 'sepia-mode)
(require 'perl-mode)
(require 'sepia)
;(put 'cperl-array-face 'face-alias 'font-lock-variable-name-face)
;(put 'cperl-hash-face 'face-alias 'font-lock-variable-name-face)
;(put 'cperl-nonoverridable-face 'face-alias 'font-lock-type-face)

;; courtesy of Jon Philpott
(defun my-sepia-buffer-name ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward "^package \\(.+?\\);" nil t)
      (let* ((pkg (match-string 1))
             (buf (get-buffer pkg)))
        (if (and buf (not (eq buf (current-buffer))))
            (rename-buffer pkg t)
          (rename-buffer pkg))))))
;(add-hook 'sepia-mode-hook #'my-sepia-buffer-name)
(add-hook 'perl-mode-hook #'my-sepia-buffer-name)

;; Restore C-c p binding
(defun my-sepia-restore-keys ()
  (when (my-emacs-feature-enabled 'muse)
    (local-set-key "\C-cp" my-muse-prefix-map)))
(add-hook 'sepia-mode-hook #'my-sepia-restore-keys)

;; sepia is not allowed to turn on eldoc
(defun sepia-install-eldoc ()
  nil)

;; Template toolkit support
(add-to-list 'load-path (concat my-emacs-path "elisp/tt-mode"))
(autoload 'tt-mode "tt-mode")
(setq auto-mode-alist
      (append '(("\\.\\(tt\\|tpl\\)$" . tt-mode)) auto-mode-alist))

;; Improved Javascript support
(add-to-list 'load-path (concat my-emacs-path "elisp/js2-mode"))
(autoload 'js2-mode "js2-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

(eval-after-load 'js2-mode
  `(progn
     ;; BUG: self is not a browser extern, just a convention that needs checking
     (setq js2-browser-externs (delete "self" js2-browser-externs))

     ;; Consider the chai 'expect()' statement to have side-effects, so we don't warn about it
     (defun js2-add-strict-warning (msg-id &optional msg-arg beg end)
       (if (and js2-compiler-strict-mode
                (not (and (string= msg-id "msg.no.side.effects")
                          (string= (buffer-substring-no-properties beg (+ beg 7)) "expect("))))
           (js2-report-warning msg-id msg-arg beg
                               (and beg end (- end beg)))))))

;; Add support for some mocha testing externs
(setq-default js2-additional-externs
              (mapcar 'symbol-name
                      '(after afterEach before beforeEach describe it)))

;; Highlight node.js stacktraces in *compile* buffers
(defvar my-nodejs-compilation-regexp
  '("^[ \t]+at +\\(?:.+(\\)?\\([^()\n]+\\):\\([0-9]+\\):\\([0-9]+\\))?$" 1 2 3))

(add-to-list 'compilation-error-regexp-alist-alist
             (cons 'nodejs my-nodejs-compilation-regexp))
(add-to-list 'compilation-error-regexp-alist 'nodejs)

;; Open files that start with "#!/usr/bin/env node" in js2-mode
(add-to-list 'interpreter-mode-alist '("node" . js2-mode))

;; Add support for Jade templates
(add-to-list 'load-path (concat my-emacs-path "elisp/jade-mode"))
(require 'sws-mode)
(require 'jade-mode)
(add-to-list 'auto-mode-alist '("\\.styl$" . sws-mode))
(add-to-list 'auto-mode-alist '("\\.jade$" . jade-mode))

;; Add support for .feature files used with Cucumber
(add-to-list 'load-path (concat my-emacs-path "elisp/feature-mode"))
(require 'feature-mode)
(add-to-list 'auto-mode-alist '("\.feature$" . feature-mode))

;; Node REPL using SLIME
(add-to-list 'load-path (concat my-emacs-path "elisp/slime"))
(add-to-list 'load-path (concat my-emacs-path "elisp/slime/contrib"))
(require 'slime)
(autoload 'slime-js-minor-mode "slime-js" nil t)
(defun my-turn-on-slime-js ()
  (interactive)
  (slime-js-minor-mode 1))
(add-hook 'js2-mode-hook #'my-turn-on-slime-js)
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

;; Clojure-mode and nrepl setup
(add-to-list 'load-path (concat my-emacs-path "elisp/clojure-mode"))
(require 'clojure-mode)
(add-to-list 'load-path (concat my-emacs-path "elisp/dash"))
(add-to-list 'load-path (concat my-emacs-path "elisp/pkg-info"))
(add-to-list 'load-path (concat my-emacs-path "elisp/s"))
(add-to-list 'load-path (concat my-emacs-path "elisp/nrepl"))
(require 'nrepl)
(require 'clojure-test-mode)

;; Enable LUA mode
(add-to-list 'load-path (concat my-emacs-path "elisp/lua-mode"))
(autoload 'lua-mode "lua-mode" "Lua editing mode." t)
(add-to-list 'auto-mode-alist '("\\.lua$" . lua-mode))
(add-to-list 'interpreter-mode-alist '("lua" . lua-mode))

;; Protobuf
(require 'protobuf-mode)
(add-to-list 'auto-mode-alist '("\\.proto\\'" . protobuf-mode))

;; Scala
(add-to-list 'load-path (concat my-emacs-path "elisp/scala-mode2"))
(require 'scala-mode2)
(add-to-list 'load-path (concat my-emacs-path "elisp/sbt-mode"))
(require 'sbt-mode)

;; ANSI colors in compile buffer
(require 'ansi-color)
(defun colorize-compilation-buffer ()
  (toggle-read-only)
  (ansi-color-apply-on-region (point-min) (point-max))
  (toggle-read-only))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)

;; Load Emacs Chrome edit server
(add-to-list 'load-path (concat my-emacs-path "elisp/emacs_chrome/servers"))
(require 'edit-server)
(edit-server-start)

;; Enable edit-server to deal with HTML-ified text entry boxes
(add-to-list 'load-path (concat my-emacs-path "elisp/edit-server-htmlize"))
(require 'edit-server-htmlize)
(add-hook 'edit-server-start-hook 'edit-server-maybe-dehtmlize-buffer)
(add-hook 'edit-server-done-hook  'edit-server-maybe-htmlize-buffer)

;; Load smex, which makes M-x work like ido-mode
(add-to-list 'load-path (concat my-emacs-path "elisp/smex"))
(require 'smex)
(add-hook 'after-init-hook 'smex-initialize)

(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
(global-set-key (kbd "C-c M-x") 'smex-update-and-run)
;; This is your old M-x.
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

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

;; Find funtions and files at point
(require 'ffap)
(ffap-bindings)

;; Make completing-read use ido-mode, especially for find-function-at-point
(add-to-list 'load-path (concat my-emacs-path "elisp/ido-ubiquitous"))
(require 'ido-ubiquitous)

;; Grand Unified Debugger
(require 'gud)

;; Navigate the kill ring when doing M-y
(add-to-list 'load-path (concat my-emacs-path "elisp/browse-kill-ring"))
(require 'browse-kill-ring)
(browse-kill-ring-default-keybindings)

;; extension of mine to make list editing easy
(require 'edit-list)

;; Load flyspell mode
(require 'flyspell)

;; Don't ever hide code when people use allout
(add-hook 'allout-mode-hook 'allout-show-all)

;; Markdown support
(autoload 'markdown-mode "markdown-mode"
   "Major mode for editing Markdown files" t)
(setq auto-mode-alist
   (cons '("\\.\\(markdown\\|md\\)" . markdown-mode) auto-mode-alist))

;; Don't mess with keys that I'm used to
(defun my-markdown-mode-keys ()
  (define-key markdown-mode-map (kbd "<M-right>") #'forward-word)
  (define-key markdown-mode-map (kbd "<M-left>") #'backward-word))
(add-hook 'markdown-mode-hook #'my-markdown-mode-keys)

;;; BEGIN confluence ;;;
(when (my-emacs-feature-enabled 'confluence)

;;; Setup
(add-to-list 'load-path (concat my-emacs-path "elisp/confluence"))
(require 'confluence)

(let ((settings (locate-user-emacs-file "confluence-auth.el")))
  (when (file-exists-p settings)
    (load settings)))

(add-to-list 'edit-server-url-major-mode-alist
             '("^[^/]+/confluence/" . confluence-edit-mode))

(add-hook 'confluence-edit-mode-hook 'visual-line-mode)

(defun my-confluence-set-keys ()
  (local-set-key "\C-c" confluence-prefix-map)
  ;; Restore C-c p binding
  (local-set-key "\C-cp" my-muse-prefix-map))
(add-hook 'confluence-edit-mode-hook #'my-confluence-set-keys)

(defun my-text-mode-confluence-keys ()
  (local-set-key (kbd "C-c .") #'confluence-get-page-at-point))
(add-hook 'text-mode-hook #'my-text-mode-confluence-keys)

;;; Keybinds
(global-set-key "\C-cwf" 'confluence-get-page)

);;; END confluence

;;; BEGIN eclim ;;;

(when (my-emacs-feature-enabled 'eclim)

;; Setup
(add-to-list 'load-path (concat my-emacs-path "elisp/emacs-eclim"))
(require 'eclim)
(require 'eclimd)
(global-eclim-mode)

);;; END eclim

;;; BEGIN emms ;;;

(when (my-emacs-feature-enabled 'emms)

;;; Setup

(add-to-list 'load-path (concat my-emacs-path "elisp/emms/lisp"))

;; Initialize
(require 'emms)
(require 'emms-browser)
(require 'emms-cache)
(require 'emms-info)
(require 'emms-lastfm)
(require 'emms-player-mpd)
(require 'emms-playing-time)
(require 'emms-playlist-mode)
(require 'emms-streams)
(require 'emms-tag-editor)
(require 'emms-volume)

(setq emms-player-mpd-music-directory my-music-remote-path)

;; Load authentication info
(let ((settings (locate-user-emacs-file "emms-auth.el")))
  (when (file-exists-p settings)
    (load settings)))

(emms-cache 1)
(emms-playing-time 1)
;(emms-lastfm 1)                    ; must come after emms-playing-time

;;; Tag editor customizations

;; Use eyeD3 to edit mp3 tags, since it can handle ID3v2
(setcdr (assoc "mp3" emms-tag-editor-tagfile-functions)
        'my-emms-tag-editor-tag-mp3)

(defun my-emms-tag-editor-tag-mp3 (track)
  "Commit changes to an MP3 file according to TRACK."
  (let ((args nil)
        (alist '((info-artist      . "a")
                 (info-title       . "t")
                 (info-album       . "A")
                 (info-tracknumber . "n")
                 (info-year        . "Y")
                 (info-genre       . "G"))))
    (mapc
     (lambda (tag)
       (let* ((info-tag (intern (concat "info-" tag)))
              (val (emms-track-get track info-tag)))
         (when (and (stringp val) (not (string= val "")))
           ;; Sanitize track number if in format "NN/MM", otherwise eyeD3 will choke on it.
           (when (and (eq info-tag 'info-tracknumber)
                      (string-match "\\`\\([0-9]+\\)/" val))
             (setq val (match-string 1 val)))
           (setq args (nconc (list (concat "-" (cdr (assoc info-tag alist)))
                                   val)
                             args)))))
     '("artist" "title" "album" "tracknumber" "year" "genre"))
    (when args
      (apply #'call-process "eyeD3" nil
             (get-buffer-create emms-tag-editor-log-buffer)
             nil
             "--no-color"
             (append args (list (emms-track-name track)))))))

; The name of emms-playlist-mode is *way* too long
(defun my-emms-sanify-mode-name ()
  (setq mode-name "EMMS"))
(add-hook 'emms-playlist-mode-hook #'my-emms-sanify-mode-name)

;; Only show files in emms-browser, not playlists
(emms-browser-make-filter
 "all-files" (emms-browser-filter-only-type 'file))
(emms-browser-set-filter (assoc "all-files" emms-browser-filters))

;;; Track descriptions

(defun my-upcase-initials (string)
  "Do `upcase-initials' on STRING, but do not uppercase letters
that come after quote characters."
  (with-temp-buffer
    (insert (upcase-initials string))
    (goto-char (point-min))
    (while (re-search-forward "['`]\\([[:upper:]]\\)" nil t)
      (downcase-region (match-beginning 1) (match-end 1)))
    (buffer-string)))

(defun my-emms-info-track-description (track)
  "Return a description of the current track."
  (let ((artist (emms-track-get track 'info-artist))
        (title (emms-track-get track 'info-title))
        (name (emms-track-get track 'name))
        (type (emms-track-type track)))
    (cond ((and (stringp artist) (not (string= artist ""))
                (stringp title) (not (string= title "")))
           (let ((desc (format "%s - %s" artist title)))
             (if (memq type '(file url))
                 desc
               (concat (symbol-name type) ": " desc))))
          ((and (stringp title) (not (string= title "")))
           (my-upcase-initials title))
          ((null name)
           "Invalid track!")
          ((memq type '(url streamlist))
           "")
          (t
           (concat (symbol-name type) ": " name)))))

;;; Track info

(defun my-emms-info-filter-titles (track)
  "Remove annoying things from titles."
  (let ((title (emms-track-get track 'info-title)))
    (when title
      (save-match-data
        (dolist (regexp my-emms-info-annoying-titles)
          (setq title (replace-regexp-in-string regexp "" title))))
      (emms-track-set track 'info-title title))))

(defun my-emms-info-fromname (track)
  "Add track information to TRACK.
This is a useful element for `emms-info-functions'."
  (when (and (memq (emms-track-type track) '(file playlist))
             (not (or (emms-track-get track 'info-artist)
                      (emms-track-get track 'info-title))))
    (let ((name (emms-track-get track 'name))
          artist title)
      (unless (or (null name) (string-match "\\`http://" name))
        (setq name (file-name-sans-extension (file-name-nondirectory name)))
        (save-match-data
          (when (string-match "\\`\\([^-]+?\\) *- *\\(.*\\)\\'" name)
            (setq artist (match-string 1 name)
                  title (match-string 2 name))
            (setq artist (my-upcase-initials
                          (emms-replace-regexp-in-string "_" " " artist)))
            (setq title (my-upcase-initials
                         (emms-replace-regexp-in-string "_" " " title)))
            (emms-track-set track 'info-artist artist)
            (emms-track-set track 'info-title title)))))))

;;; Utilities

;; Show the currently-playing song
(defun np (&optional arg)
  (interactive "P")
  (emms-player-mpd-show arg))

;; Reset the cached info for all songs in the current buffer
(defun my-emms-reset-cache ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (emms-walk-tracks
      (let ((track (emms-playlist-track-at (point))))
        (emms-cache-set (emms-track-get track 'type)
                        (emms-track-get track 'name)
                        nil)))))

;; Open file at point in playlist
(defun my-emms-playlist-open-file ()
  (interactive)
  (unless (emms-playlist-track-at)
    (emms-playlist-next))
  (let ((name (emms-track-get (emms-playlist-track-at) 'name)))
    (when name
      (find-file name))))

;; Switch to the radio buffer
(defun my-emms-streams ()
  (interactive)
  (let ((buf (get-buffer emms-stream-buffer-name)))
    (if buf
        (switch-to-buffer buf)
      (emms-streams))))

;; Switch to either the radio buffer or the current EMMS playlist
(defun my-emms-switch-to-current-playlist ()
  (interactive)
  (if (and (boundp 'emms-stream-playlist-buffer)
           (eq emms-stream-playlist-buffer emms-playlist-buffer))
      (switch-to-buffer emms-stream-buffer-name)
    (if (or (null emms-playlist-buffer)
            (not (buffer-live-p emms-playlist-buffer)))
        (error "No current Emms buffer")
      (switch-to-buffer emms-playlist-buffer))))

;; Bring up the browser interface with my preferences
(defvar my-emms-browser-buffer nil)

(defun my-emms-switch-to-browser ()
  (interactive)
  (if (and my-emms-browser-buffer
           (buffer-live-p my-emms-browser-buffer))
      (switch-to-buffer my-emms-browser-buffer)
    (emms-browse-by-artist)
    (emms-browser-expand-to-level-2)
    (setq my-emms-browser-buffer (current-buffer)))
  (isearch-forward))

;;; Every time the song changes, show me its description

;; Internal
(defvar emms-np-last nil
  "Description for most recently-played track.")
(defvar emms-np-timer nil
  "Timer used by `emms-np-maybe'.")

(defun emms-np-maybe ()
  (condition-case nil
      (emms-player-mpd-show
       nil
       (lambda (buffer desc)
         (when (and (stringp desc)
                    (not (string= desc "")))
           (unless (and (stringp emms-np-last)
                        (string= emms-np-last desc))
             (setq emms-np-last desc)
             (message "%s" desc)))))
    (error (emms-np-remove))))

(defun emms-np-insinuate ()
  (unless emms-np-timer
    (setq emms-np-timer (run-with-idle-timer my-emms-np-interval t #'emms-np-maybe))))

(defun emms-np-remove ()
  (interactive)
  (when emms-np-timer
    (emms-cancel-timer emms-np-timer)
    (setq emms-np-timer nil)))

(add-hook 'emms-player-started-hook #'emms-np-insinuate)
(add-hook 'emms-player-stopped-hook #'emms-np-remove)
(add-hook 'emms-player-finished-hook #'emms-np-remove)

;;; Fancy playlist tree display

(defun my-emms-iterate-directory (dir regex file-fn dir-fn closure)
  "Call FILE-FN for each file under DIR that matches REGEX, and call
DIR-FN for each directory under DIR that matches REGEX.

The FILE-FN and DIR-FN functions should both accept three
arguments: the name of the file, its depth level in the tree, and
the CLOSURE argument."
  (let ((dirs (list (cons dir 0))))
    (while dirs
      (cond
       ((file-directory-p (caar dirs))
        (if (string-match "/\\.\\.?$" (caar dirs))
            (setq dirs (cdr dirs))
          (let ((depth (cdar dirs)))
            (unless (equal depth 0)
              (funcall dir-fn (caar dirs) (cdar dirs) closure))
            (setq depth (1+ depth))
            (setq dirs
                  (condition-case nil
                      (nconc (mapcar `(lambda (entry)
                                        (cons entry ,depth))
                                     (directory-files (caar dirs) t))
                             (cdr dirs))
                    (error (cdr dirs)))))))
       ((string-match regex (caar dirs))
        (funcall file-fn (caar dirs) (cdar dirs) closure)
        (setq dirs (cdr dirs)))
       (t
        (setq dirs (cdr dirs)))))))

(defun my-fancy-directory-tree-insert-file (file depth type)
  (unless (let ((case-fold-search nil))
            (string-match emms-source-file-exclude-regexp file))
    (when (>= depth 1)
      (with-current-emms-playlist
        (let ((inhibit-read-only t))
          (insert (make-string (* (1- depth) 2) ?\ )))))
    (emms-playlist-insert-track
     (emms-track type (expand-file-name file)))))

(defun my-fancy-directory-tree-insert-dir (dir depth ignored)
  (emms-playlist-ensure-playlist-buffer)
  (let ((inhibit-read-only t))
    (unless (bobp)
      (insert "\n"))
    (when (>= depth 1)
      (insert (make-string (* (1- depth) 2) ?\ )
              "[ " (my-upcase-initials (file-name-nondirectory dir))
              " ]\n"))))

;;;###autoload (autoload 'emms-play-fancy-playlist-directory-tree
;;;###autoload           "emms-init" nil t)
;;;###autoload (autoload 'emms-add-fancy-playlist-directory-tree
;;;###autoload           "emms-init" nil t)
(define-emms-source fancy-playlist-directory-tree (dir)
  "An EMMS source for multiple directory trees of playlist files.
If DIR is not specified, it is queried from the user."
  (interactive (list
                (emms-read-directory-name "Play directory tree: "
                                          emms-source-file-default-directory
                                          emms-source-file-default-directory
                                          t)))
  (my-emms-iterate-directory (expand-file-name dir) "^[^.]"
                             #'my-fancy-directory-tree-insert-file
                             #'my-fancy-directory-tree-insert-dir
                             'playlist))

;;;###autoload (autoload 'emms-play-fancy-directory-tree
;;;###autoload           "emms-init" nil t)
;;;###autoload (autoload 'emms-add-fancy-directory-tree
;;;###autoload           "emms-init" nil t)
(define-emms-source fancy-directory-tree (dir)
  "An EMMS source for multiple directory trees of files.
If DIR is not specified, it is queried from the user."
  (interactive (list
                (emms-read-directory-name "Play directory tree: "
                                          emms-source-file-default-directory
                                          emms-source-file-default-directory
                                          t)))
  (my-emms-iterate-directory (expand-file-name dir) "^[^.]"
                             #'my-fancy-directory-tree-insert-file
                             #'my-fancy-directory-tree-insert-dir
                             'file))

(defun my-emms-rebuild-main ()
  (interactive)
  (let ((buffer (get-buffer "*EMMS Playlist*")))
    (unless buffer
      (setq buffer (setq emms-playlist-buffer (emms-playlist-new))))
    (let ((emms-playlist-buffer buffer))
      (with-current-emms-playlist
        (emms-playlist-mode-clear)
        (emms-add-fancy-playlist-directory-tree my-emms-playlist-dir)))))

;;; Initialization

(define-emms-source misc ()
  (emms-add-fancy-playlist-directory-tree my-emms-playlist-dir))

(ignore-errors
  (my-emms-rebuild-main))

;;; Key customizations

(define-key emms-playlist-mode-map "o" 'my-emms-playlist-open-file)
(define-key emms-playlist-mode-map "q" 'emms-playlist-mode-current-kill)
(define-key emms-playlist-mode-map (kbd "SPC") 'emms-pause)

(defvar my-emms-prefix-map
  (let ((map (make-sparse-keymap)))
    (define-key map " " 'emms-pause)
    (define-key map "0" 'emms-playlist-mode-clear)
    (define-key map "b" 'my-emms-switch-to-browser)
    (define-key map "c" 'emms-player-mpd-connect)
    (define-key map "l"
      (let ((label-map (make-sparse-keymap)))
        (define-key label-map "b" 'emms-lastfm-radio-ban)
        (define-key label-map "l" 'emms-lastfm-radio-love)
        (define-key label-map "s" 'emms-lastfm-radio-skip)
        label-map))
    (define-key map "n" 'emms-player-mpd-next)
    (define-key map "o" 'my-emms-switch-to-current-playlist)
    (define-key map "p" 'emms-player-mpd-previous)
    (define-key map "q" 'emms-stop)
    (define-key map "Q" 'emms-player-mpd-clear)
    (define-key map "r" 'my-emms-streams)
    (define-key map "R" 'my-emms-rebuild-main)
    (define-key map "s" 'emms-seek)
    (define-key map "t" 'my-emms-toggle-remote)
    (define-key map "-" 'emms-volume-mode-minus)
    (define-key map "+" 'emms-volume-mode-plus)
    (define-key map "\C-c" 'my-emms-reset-cache)
    map)
  "My key customizations for EMMS.")

(global-set-key "\C-cm" my-emms-prefix-map)

);;; END emms ;;;

;;; BEGIN erc ;;;

(when (my-emacs-feature-enabled 'erc)

;;; Setup

;; NOTE: Due to Emacs bug #14121, we have to load erc in a separate top-level block from the
;; use of erc-response.contents below, which is why the my-emacs-feature-enabled part is repeated.

;(add-to-list 'load-path (concat my-emacs-path "elisp/erc"))
(require 'erc))

(when (my-emacs-feature-enabled 'erc)

;; Load authentication info
(let ((settings (locate-user-emacs-file "erc-auth.el")))
  (when (file-exists-p settings)
    (load settings)))

;;; Hack ERC to make buttonized nicks not do anything
(require 'erc-button)
(defun erc-nick-popup (nick) nil)
(defun finger (user host) nil)

;;; New ERC commands

(defun erc-cmd-EMMS (&rest ignore)
  "Display the current emms track to the current ERC buffer."
  (emms-player-mpd-show nil (lambda (buffer desc)
                              (with-current-buffer buffer
                                (erc-send-message desc)))))

(defalias 'erc-cmd-NP 'erc-cmd-EMMS)

;;; Misc. hacks

(setq erc-fill-column fill-column)

;; Function to make ERC more themeable -- don't need this atm
(defun mwolson/make-erc-themeable ()
  (interactive)
  (require 'rcirc)  ; for face definitions
  (put 'erc-input-face        'face-alias 'rcirc-timestamp)
  (put 'erc-my-nick-face      'face-alias 'rcirc-my-nick)
  (put 'erc-keyword-face      'face-alias 'show-paren-match)
  (put 'erc-nick-default-face 'face-alias 'font-lock-string-face)
  (put 'erc-notice-face       'face-alias 'font-lock-function-name-face)
  (put 'erc-timestamp-face    'face-alias 'rcirc-timestamp))

;; Insert 2 different left timestamps: one for day change, one for time

(require 'erc-stamp)
(defun erc-insert-timestamp-left (string)
  "Insert timestamps at the beginning of the line."
  (goto-char (point-min))
  (let* ((ct (current-time))
         (ts-left (erc-format-timestamp ct erc-timestamp-format-left)))
    ;; insert left timestamp
    (unless (string-equal ts-left erc-timestamp-last-inserted-left)
      (erc-put-text-property 0 (length ts-left) 'field 'erc-timestamp ts-left)
      (insert ts-left "\n")
      (setq erc-timestamp-last-inserted-left ts-left)))
  (let* ((ignore-p (and erc-timestamp-only-if-changed-flag
                        (string-equal string erc-timestamp-last-inserted)))
         (len (length string))
         (s (if ignore-p (make-string len ? ) string)))
    (unless ignore-p (setq erc-timestamp-last-inserted string))
    (erc-put-text-property 0 len 'field 'erc-timestamp s)
    (erc-put-text-property 0 len 'invisible 'timestamp s)
    (insert s)))

;;; Remove trailing whitespace in messages

(defun my-erc-remove-trailing-whitespace (proc parsed)
  "Remove trailing whitespace from the current message.
Some IM clients use an OTR plug-in that sends some annoying
trailing space to the screen, so we want to clean that up."
  (let ((msg (erc-response.contents parsed)))
    (when (stringp msg)
      (setf (erc-response.contents parsed)
            (erc-replace-regexp-in-string "[[:space:]]+\\'" "" msg))
      nil)))
(add-hook 'erc-server-PRIVMSG-functions 'my-erc-remove-trailing-whitespace)

;;; Key customizations

;; Make C-c RET (or C-c C-RET) send messages instead of RET
(define-key erc-mode-map (kbd "RET") nil)
(define-key erc-mode-map (kbd "C-c RET") 'erc-send-current-line)
(define-key erc-mode-map (kbd "C-c C-RET") 'erc-send-current-line)

;; Disable some commands that I never want to execute
(define-key erc-mode-map "\C-c\C-c" nil)
(define-key erc-mode-map "\C-c\C-e" nil)
(define-key erc-mode-map "\C-c\C-f" nil)
(define-key erc-mode-map "\C-c\C-p" nil)
(define-key erc-mode-map "\C-c\C-q" nil)
(define-key erc-mode-map "\C-c\C-r" nil)

);;; END erc ;;;

;;; BEGIN magit ;;;

(when (my-emacs-feature-enabled 'magit)

;; Load magit
(add-to-list 'load-path (concat my-emacs-path "elisp/magit"))
(require 'magit)
(require 'magit-blame)
(require 'magit-svn)

;; Add svn support to magit
(defun my-enable-magit-svn ()
  (unless (string= "" (shell-command-to-string "git config svn-remote.svn.url"))
    (magit-svn-mode 1)))
(add-hook 'magit-mode-hook 'my-enable-magit-svn)

;; Setup info
(add-to-list 'Info-default-directory-list (concat my-emacs-path "share/info"))

;; Hacks

(defun my-magit-diff-head (dir)
  (interactive (list (or (and (not current-prefix-arg)
                              (magit-get-top-dir default-directory))
                         (magit-read-top-dir))))
  (let ((default-directory (or (magit-get-top-dir dir) default-directory)))
    (magit-diff "HEAD"))
  (other-window 1))

(defun my-magit-log-edit ()
  (interactive)
  (magit-log-edit)
  (turn-on-muse-list-edit-minor-mode)
  (set (make-variable-buffer-local 'fill-paragraph-function) nil))

;; Map some magit keys
(global-set-key "\C-xV" nil)
(global-set-key "\C-xVb" 'magit-branch-manager)
(global-set-key "\C-xVc" 'my-magit-log-edit)
(global-set-key "\C-xVl" 'magit-log)
(global-set-key "\C-xVs" 'magit-status)
(global-set-key "\C-xV=" 'my-magit-diff-head)

;; Don't overwrite Alt-left movement keys in magit modes
(define-key magit-commit-mode-map (kbd "<M-left>") 'backward-word)
(define-key magit-status-mode-map (kbd "<M-left>") 'backward-word)

);;; END magit ;;;

;;; BEGIN muse ;;;

(when (my-emacs-feature-enabled 'muse)

;;; Setup

;; Add to load path
(add-to-list 'load-path (concat my-emacs-path "elisp/muse/lisp"))
(add-to-list 'load-path (concat my-emacs-path "elisp/muse/contrib") t)
(add-to-list 'load-path (concat my-emacs-path "elisp/muse/experimental"))

;; Initialize
(require 'outline)       ; I like outline-style faces
(require 'muse)          ; load generic module
(require 'muse-colors)   ; load coloring/font-lock module
(require 'muse-mode)     ; load authoring mode
(require 'muse-blosxom)  ; load blosxom module
(require 'muse-docbook)  ; load DocBook publishing style
(require 'muse-html)     ; load (X)HTML publishing style
(require 'muse-ikiwiki)  ; load Ikiwiki support
(require 'htmlize-hack)  ; work around htmlize bug with Emacs 23
(require 'muse-latex)    ; load LaTeX/PDF publishing styles
(require 'muse-latex2png) ; publish <latex> tags
(require 'muse-project)  ; load support for projects
(require 'muse-texinfo)  ; load Info publishing style
(require 'muse-wiki)     ; load Wiki support
(require 'muse-xml)      ; load XML support
;;(require 'muse-message)  ; load message support (experimental)
(require 'remember)

;; Templates: header/footer
(setq muse-html-footer (concat my-muse-path "generic-footer.html"))
(setq muse-html-header (concat my-muse-path "generic-header.html"))
(setq muse-latex-header (concat my-muse-path "header.tex"))
(setq muse-latex-slides-header (concat my-muse-path "slides-header.tex"))
(setq muse-xhtml-footer (concat my-muse-path "generic-footer.html"))
(setq muse-xhtml-header (concat my-muse-path "generic-header.html"))

(setq muse-html-style-sheet
      "<link rel=\"stylesheet\" type=\"text/css\" charset=\"utf-8\" media=\"all\" href=\"/common.css\" />
<link rel=\"stylesheet\" type=\"text/css\" charset=\"utf-8\" media=\"screen\" href=\"/screen.css\" />
<link rel=\"stylesheet\" type=\"text/css\" charset=\"utf-8\" media=\"print\" href=\"/print.css\" />")

(setq muse-xhtml-style-sheet
      "<link rel=\"stylesheet\" type=\"text/css\" charset=\"utf-8\" media=\"all\" href=\"/common.css\" />
<link rel=\"stylesheet\" type=\"text/css\" charset=\"utf-8\" media=\"screen\" href=\"/screen.css\" />
<link rel=\"stylesheet\" type=\"text/css\" charset=\"utf-8\" media=\"print\" href=\"/print.css\" />")

;; Setup projects

;; Here is an example of making a customized version of your favorite publisher.  All this does is run
;; `my-muse-blosoxm-finalize' on the published file immediately after saving it.
(muse-derive-style "my-blosxom" "blosxom-xhtml"
                   :final 'my-muse-blosxom-finalize)

;; This uses a different header and footer than normal
(muse-derive-style "my-xhtml" "xhtml"
                   :header (concat my-muse-path "header.html")
                   :footer (concat my-muse-path "footer.html"))

;; Define a draft style which provides extra space between sections

(defvar muse-latex-draft-markup-strings
  '((chapter      . "\\bigskip\n\\bigskip\n\\chapter{")
    (section      . "\\bigskip\n\\bigskip\n\\section{")
    (subsection   . "\\bigskip\n\\bigskip\n\\subsection{")
    (subsubsection . "\\bigskip\n\\bigskip\n\\subsubsection{"))
  "Strings used for marking up Latex draft text.")

(muse-derive-style "latex-draft" "latex"
                   :strings 'muse-latex-draft-markup-strings)
(muse-derive-style "pdf-draft" "latex-draft"
                   :final   'muse-latex-pdf-generate
                   :browser 'muse-latex-pdf-browse-file
                   :link-suffix 'muse-latex-pdf-extension
                   :osuffix 'muse-latex-pdf-extension)


;;; Functions

;; Switch to the given project and prompt for a file
(defun my-muse-project-find-file (project)
  (interactive)
  (let ((muse-current-project (muse-project project)))
    (call-interactively 'muse-project-find-file)))

(defun my-muse-blosxom-finalize (file output-path target)
;;  (my-muse-prepare-entry-for-xanga output-path)
;; For now, do nothing.
  )

;; Turn a word or phrase into a clickable Wikipedia link
(defun my-muse-dictize (beg end)
  (interactive "r")
  (let* ((text (buffer-substring-no-properties beg end))
         (link (concat "dict:" (replace-regexp-in-string " " "_" text t t))))
    (delete-region beg end)
    (insert "[[" link "][" text "]]")))

(defun my-muse-surround-math (&optional beg end)
  "If a region is higlighted, surround it with <math>...</math>.
If no region is highlighted, insert <math></math> and leave the point
between the two tags."
  (interactive (list (ignore-errors (mark)) (point)))
  (if (and beg end)
      (save-restriction
        (narrow-to-region beg end)
        (goto-char (point-min))
        (insert "<math>")
        (goto-char (point-max))
        (insert "</math>"))
    (insert "<math>")
    (save-excursion (insert "</math>"))))

(defun my-muse-cdotize-region (beg end)
  (interactive "r")
  (save-restriction
    (narrow-to-region beg end)
    (goto-char (point-min))
    (while (re-search-forward " *\\* *" nil t)
      (replace-match " \\\\cdot "))))

;;; Key customizations

;; Local
(add-hook 'muse-mode-hook #'footnote-mode)

;; Global
(defun my-find-remember-file ()
  (interactive)
  (find-file remember-data-file))

(defvar my-muse-prefix-map
  (let ((map (make-sparse-keymap)))
    (define-key map "C" #'my-muse-cdotize-region)
    (define-key map "l" 'muse-blosxom-new-entry)
    (define-key map "M" #'my-muse-surround-math)
    (define-key map "n" #'my-find-remember-file)
    (define-key map "W" #'my-muse-dictize)
    map)
  "My key customizations for Muse.")

(global-set-key "\C-cp" my-muse-prefix-map)

);;; END muse ;;;


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

;; Make the `q' key bury the current buffer when viewing help
(require 'view)
(define-key view-mode-map "q" 'bury-buffer)

;; Make the <DEL> key scroll backwards in View mode
(define-key view-mode-map [delete] 'View-scroll-page-backward)

;; Make the <DEL> key scroll backwards in Info mode
(require 'info)
(define-key Info-mode-map [delete] 'Info-scroll-down)

;; diff-mode: Don't mess with M-q
(require 'diff-mode)
(define-key diff-mode-map (kbd "M-q") 'fill-paragraph)

;; SML mode: Don't mess with M-SPC
;(define-key sml-mode-map (kbd "M-SPC") 'just-one-space)

;; Use IDO instead of the buffer list when I typo it
(global-set-key "\C-x\C-b" 'ido-switch-buffer)

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
