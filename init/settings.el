;;; Customizations

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(Man-notify-method (quote bully))
 '(add-log-keep-changes-together t)
 '(apropos-do-all t)
 '(auto-compression-mode t nil (jka-compr))
 '(auto-save-file-name-transforms (quote ((".*" "~/.emacs.d/backup/" t))))
 '(backup-directory-alist (quote ((".*" . "~/.emacs.d/backup"))))
 '(blink-cursor-mode nil nil (frame))
 '(blink-matching-delay 10)
 '(blink-matching-paren-on-screen t)
 '(browse-url-new-window-flag t)
 '(calendar-mark-diary-entries-flag t)
 '(case-replace nil)
 '(column-number-mode t)
 '(confirm-kill-emacs (quote y-or-n-p))
 '(confluence-save-page-comments nil)
 '(confluence-save-page-minor-edits nil)
 '(cperl-close-paren-offset -4)
 '(cperl-indent-level 4)
 '(cperl-indent-parens-as-block t)
 '(cperl-merge-trailing-else nil)
 '(css-indent-offset 2)
 '(current-language-environment "UTF-8")
 '(custom-safe-themes t)
 '(default-input-method "TeX")
 '(delete-selection-mode t)
 '(diff-switches "-u")
 '(dired-dwim-target t)
 '(dired-recursive-copies (quote always))
 '(dired-recursive-deletes (quote always))
 '(doc-view-resolution 132)
 '(ediff-window-setup-function (quote ediff-setup-windows-plain))
 '(eldoc-minor-mode-string " E")
 '(electric-indent-mode nil)
 '(enable-recursive-minibuffers t)
 '(ffap-machine-p-known (quote reject))
 '(fill-column 119)
 '(flyspell-issue-welcome-flag nil)
 '(footnote-body-tag-spacing 1)
 '(gc-cons-threshold 100000000)
 '(git-commit-summary-max-length 120)
 '(icomplete-mode t)
 '(ido-default-buffer-method (quote selected-window))
 '(ido-enable-flex-matching t)
 '(ido-mode (quote buffer) nil (ido))
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(ispell-local-dictionary "american")
 '(ispell-personal-dictionary nil)
 '(ispell-silently-savep t)
 '(js-indent-level 2)
 '(js2-allow-keywords-as-property-names nil)
 '(js2-auto-indent-flag nil)
 '(js2-basic-offset 2)
 '(js2-bounce-indent-flag nil)
 '(js2-cleanup-whitespace nil)
 '(js2-concat-multiline-strings nil)
 '(js2-highlight-level 1)
 '(js2-include-node-externs t)
 '(js2-mirror-mode nil)
 '(js2-mode-escape-quotes nil)
 '(js2-rebind-eol-bol-keys nil)
 '(js2-skip-preprocessor-directives t)
 '(ls-lisp-dirs-first t)
 '(ls-lisp-ignore-case t)
 '(ls-lisp-support-shell-wildcards nil)
 '(ls-lisp-use-insert-directory-program nil)
 '(ls-lisp-verbosity (quote (uid gid)))
 '(magit-display-buffer-function (quote magit-display-buffer-fullframe-status-v1))
 '(magit-prefer-remote-upstream t)
 '(mail-interactive t)
 '(mail-user-agent (quote gnus-user-agent))
 '(max-lisp-eval-depth 1000)
 '(max-specpdl-size 1000)
 '(message-log-max 500)
 '(mouse-wheel-mode t)
 '(org-capture-templates
   (quote
    (("n" "Note" entry
      (file+headline "" "Notes")
      "* %?" :prepend t :empty-lines-after 1))))
 '(org-default-notes-file "~/Documents/notes.org")
 '(org-startup-folded nil)
 '(package-archives
   (quote
    (("melpa-stable" . "http://stable.melpa.org/packages/")
     ("gnu" . "http://elpa.gnu.org/packages/"))))
 '(package-selected-packages
   (quote
    (maxframe markdown-mode lua-mode smex dockerfile-mode browse-kill-ring atom-one-dark-theme ripgrep flx-ido projectile yaml-mode js2-mode ac-cider slime cider auto-complete magit)))
 '(post-jump-header nil)
 '(projectile-mode-line
   (quote
    (:eval
     (if
         (file-remote-p default-directory)
         " Proj"
       (format " Proj[%s]"
               (projectile-project-name))))))
 '(projectile-switch-project-action (quote magit-status))
 '(recentf-mode nil)
 '(require-final-newline nil)
 '(save-place t nil (saveplace))
 '(save-place-limit 20)
 '(select-enable-clipboard t)
 '(show-paren-mode t)
 '(show-paren-style (quote parenthesis))
 '(smex-save-file "~/.emacs.d/.smex-items")
 '(sql-product (quote postgres))
 '(tab-stop-list
   (quote
    (4 8 12 16 20 24 28 32 36 40 44 48 52 56 60 64 68 72 76 80 84 88 92 96 100 104 108 112 116 120)))
 '(tab-width 8)
 '(tramp-auto-save-directory "~/.emacs.d/.autosave.d")
 '(tramp-backup-directory-alist (quote (("." . "~/.emacs.d/backup"))))
 '(uniquify-buffer-name-style (quote forward) nil (uniquify))
 '(vc-handled-backends nil)
 '(visible-bell t)
 '(woman-fill-column 95)
 '(woman-fontify t)
 '(woman-use-own-frame nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(woman-italic ((t (:underline t :slant italic))) t))
