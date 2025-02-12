;;; Emacs initialization settings common to multiple computers
;;
;; Author: Michael Olson
;;
;; The contents of this file may be used, distributed, and modified
;; without restriction.
;; Uncomment this to debug warnings
;;
;; (require 'warnings)
;; (defun display-warning (type message)
;;   (setq debug-on-error t)
;;   (error message))

(require 'cl-seq)
(require 'package)
(package-initialize)

;;; Options that change behavior of this repo

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
(defvar my-gptel-backend-local 'my-gptel--local-ai)
(defvar my-gptel-backend-remote 'my-gptel--claude)
(defvar my-gptel-model       nil)
(defvar my-gptel-model-local 'Sky-T1-32B-Flash-Q4_K_S)
(defvar my-gptel-model-remote nil)
(defvar my-gptel-system-prompt nil)
(defvar my-gptel-temperature 0.0)
(defvar my-minuet-auto-suggest-p nil)
(defvar my-minuet-provider   'codestral)
(defvar my-minuet-provider-remote 'codestral)
(defvar my-remap-cmd-key-p   t)
(defvar my-default-directory "~/")
(defvar my-changelog-address "user@example.com")
(defvar my-email-address     "user@example.com")
(defvar my-full-name         "Jane Doe")
(defvar my-emacs-path)
(setq my-emacs-path          (file-name-as-directory (expand-file-name my-emacs-path)))

(defvar my-server-start-p    (cond ((eq system-type 'darwin) t)
                                   ((memq window-system '(pgtk x w32)) t)
                                   (t nil)))
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

;;; Display

;; Ligatures
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

(with-eval-after-load "maxframe"
  (when my-frame-pad-width
    (setopt mf-max-width (- (display-pixel-width) my-frame-pad-width)))
  (when my-frame-pad-height
    (setopt mf-max-height (- (display-pixel-height) my-frame-pad-height))))

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
       ((eq system-type 'darwin) "Apple Color Emoji")
       ((eq system-type 'windows-nt) "Segoe UI Emoji")
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
    (cond ((and maximize-p (memq window-system '(pgtk ns x w32)))
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

;; Initialize display settings
(my-init-client-display)
(add-hook 'server-after-make-frame-hook #'my-init-client-display t)

;; Display startup screen
(setq native-comp-async-report-warnings-errors 'silent)
(display-startup-screen)

(provide 'early-shared-init)
;;; early-shared-init.el ends here
