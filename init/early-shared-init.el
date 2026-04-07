;;; early-shared-init.el --- -*- lexical-binding: t -*-
;;
;; Description: Emacs initialization settings common to multiple computers
;; Author: Michael Olson
;;
;; The contents of this file may be used, distributed, and modified
;; without restriction.
;;
;; Uncomment this to debug warnings:
;;
;; (require 'warnings)
;; (defun display-warning (type message)
;;   (setq debug-on-error t)
;;   (error message))

(setq gc-cons-percentage 0.6)
(setq gc-cons-threshold most-positive-fixnum)

;; Show a bell icon instead of beeping
(defvar my-bell-icon-display ""
  "Variable to hold the bell icon display state.")

(defvar my-bell-icon-flashing nil)

(defun my-remove-bell-icon ()
  (when my-bell-icon-flashing
    (setq mode-line-format (remove 'my-bell-icon-display mode-line-format)
          my-bell-icon-display ""
          my-bell-icon-flashing nil)
    (force-mode-line-update)))

(defun my-flash-bell-icon ()
  "Briefly display a bell icon in the mode line."
  (unless my-bell-icon-flashing
    (run-with-timer 0.3 nil #'my-remove-bell-icon)
    (setq my-bell-icon-flashing t
          my-bell-icon-display (if (eq window-system 'ns) " * " " 🔔 ")
          mode-line-format (append mode-line-format '(my-bell-icon-display)))
    (force-mode-line-update)))

(setq ring-bell-function #'my-flash-bell-icon)

;;; Options that change behavior of this repo
(require 'cl-seq)

(defvar my-default-font      nil)
(defvar my-default-emoji-font nil)
(defvar my-default-emoji-size nil)
(defvar my-theme             nil)
(defvar my-modus-theme       'modus-vivendi-deuteranopia)
(defvar my-modus-theme-overrides
  '((bg-added-fringe     "#335533")
    (bg-changed-fringe   "#7a6100")
    (bg-removed-fringe   "#553333")
    (bg-main             "#1c1c1c")
    (bg-mode-line-active "#484d67")
    (comment             fg-dim)))
(defvar my-use-themes-p      (boundp 'custom-theme-load-path))
(defvar my-frame-height      50)
(defvar my-frame-width       120)
(defvar my-frame-maximize-if-pixel-width-lte 1440)
(defvar my-frame-maximize-p  t)
(defvar my-frame-pad-width   (if (eq system-type 'darwin) 65 nil))
(defvar my-frame-pad-height  (if (eq system-type 'darwin) 15 nil))
(defvar my-mise-exclude-file-regexps '(".*"))
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
           "~/.local/bin"
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
           "~/.local/bin"
           "C:/Program Files/maven/bin"))
        (t `(,(concat my-emacs-path "node_modules/.bin")
             "~/.local/bin"
             "/opt/maven/bin"))))
(setq my-system-paths (cl-remove-if-not #'file-exists-p my-system-paths))

;; Initialize early packages
(require 'use-package)

(setopt use-package-vc-prefer-newest t)

(use-package vcupp
  :vc (:url "https://github.com/mwolson/vcupp")
  :init
  ;; Bootstrap: add vcupp to load-path directly since
  ;; vcupp-preload-package is not yet available.  This only depends on
  ;; the directory existing (guaranteed after git pull), unlike loading
  ;; autoloads which can be missing if package-vc--unpack-1 failed
  ;; silently during an upgrade.
  (when-let* ((dir (expand-file-name "vcupp" package-user-dir))
              ((file-directory-p dir)))
    (add-to-list 'load-path dir))
  :demand t)

(eval-and-compile
  (vcupp-suppress-native-comp-jit))

(vcupp-ensure-packages-on-install)

(use-package compile-angel
  :vc (:url "https://github.com/jamescherti/compile-angel.el"
       :main-file "compile-angel.el")
  :init (vcupp-preload-package 'compile-angel)
  :defer t)

(use-package ligature
  :vc (:url "https://github.com/mickeynp/ligature.el"
       :main-file "ligature.el")
  :init (vcupp-preload-package 'ligature)
  :defer t)

(use-package maxframe
  :vc (:url "https://github.com/rmm5t/maxframe.el"
       :main-file "maxframe.el")
  :init (vcupp-preload-package 'maxframe)
  :commands (maximize-frame)
  :defer t
  :config
  (when my-frame-pad-width
    (setopt mf-max-width (- (display-pixel-width) my-frame-pad-width)))
  (when my-frame-pad-height
    (setopt mf-max-height (- (display-pixel-height) my-frame-pad-height))))

(use-package modus-themes
  :vc (:url "https://github.com/protesilaos/modus-themes")
  :init (vcupp-preload-package 'modus-themes)
  :defer t)

;;; Display

;; Ligatures
(let* ((after- "<|")
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

(defun my-setup-web-ligatures ()
  (interactive)
  (setq-local ligature-composition-table nil)
  (ligature-set-ligatures major-mode my-web-mode-ligatures))

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
  (let ((maximize-p (and my-frame-maximize-p
                         (not (string= (or (getenv "XDG_CURRENT_DESKTOP") "")
                                       "niri")))))
    (when (and maximize-p my-frame-maximize-if-pixel-width-lte)
      (setq maximize-p (<= (display-pixel-width)
                           my-frame-maximize-if-pixel-width-lte)))
    (cond ((and maximize-p (memq window-system '(pgtk ns x w32)))
           (set-frame-parameter nil 'fullscreen 'maximized))
          (maximize-p
           (require 'maxframe)
           (maximize-frame))
          (t
           (dolist (param '(width height))
             (set-frame-parameter
              nil param (cdr (assoc param default-frame-alist))))))))

(defvar my-tab-active-background "#8b008b"
  "Background color of the active tab in tab-line.
Default is `magenta4'.")

(defvar my-tab-box-gap 6
  "Pixels of tab-line background to show above and below each tab.")

(defvar my-tab-line-height 1.2
  "Height to apply to the tab-line base face.")

(defvar my-tab-height 0.7
  "Height to apply to tab faces on the tab-line.")

(defun my-reset-tab-line-faces ()
  "Shrink tab-line tab faces and remove bolding, leaving the menu button alone.
Uses `custom-set-faces' so the specs survive face recalculations.
A `:box' border colored to match the tab-line background creates a
visible gap around each tab."
  (interactive)
  (modus-themes-with-colors
    (let ((box (list :line-width (cons 1 my-tab-box-gap) :color bg-tab-bar)))
      (custom-set-faces
       `(tab-line ((t (:height ,my-tab-line-height))))
       `(tab-line-tab ((t (:height ,my-tab-height :box ,box))))
       `(tab-line-tab-current
         ((t (:weight normal :height ,my-tab-height
              :background ,my-tab-active-background :box ,box))))
       `(tab-line-tab-inactive
         ((t (:weight normal :height ,my-tab-height :box ,box))))
       `(tab-line-tab-inactive-alternate
         ((t (:weight normal :height ,my-tab-height :box ,box))))))))

(defun my-reset-theme ()
  (interactive)
  (when my-use-themes-p
    (if my-modus-theme
        (progn
          (require 'modus-themes)
          (setopt modus-themes-common-palette-overrides
                  my-modus-theme-overrides)
          (modus-themes-select my-modus-theme)
          (my-reset-tab-line-faces))
      (load-theme my-theme t))))

;; Support for font ligatures
(defun my-enable-ligatures ()
  ;; Enable ligatures in programming modes
  (interactive)
  (require 'ligature)
  (ligature-set-ligatures 'markdown-mode my-prog-mode-ligatures)
  (ligature-set-ligatures 'prog-mode my-prog-mode-ligatures)
  (global-ligature-mode t))

;; Tab-line menu button
(defvar my-tab-line-menu-map
  (let ((map (make-sparse-keymap)))
    (define-key map [tab-line mouse-1] #'tab-bar-menu-bar)
    map)
  "Keymap for the tab-line menu button.")

(defvar my-tab-line-left-margin
  (propertize " " 'face '(:inherit tab-line
                          :box (:line-width (1 . 0)
                                :style flat-button)))
  "Invisible left margin spacer for the tab line.")

(defvar my-tab-line-menu-button
  (propertize " ☰ "
              'face '(:foreground "#ffffff")
              'mouse-face 'tab-line-highlight
              'local-map my-tab-line-menu-map
              'help-echo "Menu (F10)")
  "Menu button.")

;; Initialize client displays
(defvar my-init-client-display-hook '()
  "Functions called after Emacs is started or a server frame is displayed.")

(defun my-init-client-display ()
  (interactive)
  (if window-system
      (progn
        (my-reset-font)
        (add-to-list 'default-frame-alist
                     `(font . ,(cdr (assq 'font (frame-parameters)))))
        (when (or (not my-frame-maximize-p)
                  my-frame-maximize-if-pixel-width-lte)
          (add-to-list 'default-frame-alist `(height . ,my-frame-height))
          (add-to-list 'default-frame-alist `(width . ,my-frame-width)))
        (when (memq window-system '(ns pgtk w32 x))
          (my-enable-ligatures))
        (normal-erase-is-backspace-mode 1)
        (set-frame-parameter nil 'menu-bar-lines 1))
    ;; terminal settings
    (add-to-list 'default-frame-alist '(menu-bar-lines . 0))
    (set-frame-parameter nil 'menu-bar-lines 0)
    (setq menu-bar-mode nil)
    (require 'mwheel))
  (run-hooks 'my-init-client-display-hook)
  (add-to-list 'default-frame-alist '(tool-bar-lines . 0))
  (add-to-list 'default-frame-alist '(horizontal-scroll-bars))
  (add-to-list 'default-frame-alist '(vertical-scroll-bars))
  (setq scroll-bar-mode nil
        tool-bar-mode nil)
  (my-reset-theme)
  (when window-system
    (global-tab-line-mode 1)
    ;; it could do this without advice, but would have to set the value on
    ;; existing early buffers like *Messages*
    (advice-add 'tab-line-format :filter-return
                (lambda (result)
                  (append  (list my-tab-line-left-margin
                                 my-tab-line-menu-button)
                           result)))
    (advice-add 'tab-line-tab-name-format-default :filter-return
                (lambda (result)
                  (let ((face (get-text-property 0 'face result)))
                    (concat (propertize " " 'display '(space :width (6))
                                        'face face)
                            result))))
    (my-reset-frame-size)))

;; Initialize client frames
(defun my-init-client ())

;; Initialize display settings
(defun my-populate-scratch-buffer ()
  (interactive)
  (get-scratch-buffer-create)
  (with-current-buffer (get-buffer "*scratch*")
    (when (zerop (buffer-size))
      (insert (substitute-command-keys initial-scratch-message))
      (set-buffer-modified-p nil))))

(unless noninteractive
  ;;(my-reset-theme)
  (pop-to-buffer-same-window (messages-buffer))
  (my-populate-scratch-buffer)
  (my-init-client-display)
  (add-hook 'server-after-make-frame-hook #'my-init-client-display t))

(provide 'early-shared-init)
;;; early-shared-init.el ends here
