;;; Customizations

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(Man-notify-method 'bully)
 '(add-log-keep-changes-together t)
 '(apropos-do-all t)
 '(auto-compression-mode t nil (jka-compr))
 '(auto-save-file-name-transforms '((".*" "~/.emacs.d/backup/" t)))
 '(backup-directory-alist '((".*" . "~/.emacs.d/backup")))
 '(blink-cursor-mode nil nil (frame))
 '(blink-matching-delay 10)
 '(blink-matching-paren-on-screen t)
 '(browse-url-new-window-flag t)
 '(calendar-mark-diary-entries-flag t)
 '(case-replace nil)
 '(column-number-mode t)
 '(confirm-kill-emacs 'y-or-n-p)
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
 '(dired-recursive-copies 'always)
 '(dired-recursive-deletes 'always)
 '(doc-view-resolution 132)
 '(ediff-window-setup-function 'ediff-setup-windows-plain)
 '(eldoc-minor-mode-string " E")
 '(electric-indent-mode nil)
 '(enable-recursive-minibuffers t)
 '(fill-column 119)
 '(flyspell-issue-welcome-flag nil)
 '(footnote-body-tag-spacing 1)
 '(gc-cons-threshold 100000000)
 '(git-commit-summary-max-length 120)
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
 '(js2-strict-missing-semi-warning nil)
 '(js2-strict-trailing-comma-warning nil)
 '(ls-lisp-dirs-first t)
 '(ls-lisp-ignore-case t)
 '(ls-lisp-support-shell-wildcards nil)
 '(ls-lisp-use-insert-directory-program nil)
 '(ls-lisp-verbosity '(uid gid))
 '(magit-display-buffer-function 'magit-display-buffer-fullframe-status-v1)
 '(magit-log-section-commit-count 1)
 '(magit-prefer-remote-upstream t)
 '(mail-interactive t)
 '(mail-user-agent 'gnus-user-agent)
 '(markdown-command "npx marked")
 '(max-lisp-eval-depth 1000)
 '(max-specpdl-size 1000 t)
 '(message-log-max 500)
 '(mouse-wheel-mode t)
 '(org-capture-templates
   '(("n" "Note" entry
      (file+headline "" "Notes")
      "* %?" :prepend t :empty-lines-after 1)))
 '(org-default-notes-file "~/Documents/notes.org")
 '(org-startup-folded nil)
 '(package-archives
   '(("melpa-stable" . "http://stable.melpa.org/packages/")
     ("gnu" . "http://elpa.gnu.org/packages/")))
 '(package-check-signature nil)
 '(package-selected-packages
   '(archive-rpm amx terraform-mode atomic-chrome swift-mode csharp-mode dotenv-mode poly-markdown polymode js-comint flymake-eslint add-node-modules-path erlang editorconfig el-mock dumb-jump company-statistics company package-utils wgrep swiper diminish counsel ivy color-theme-sanityinc-tomorrow maxframe markdown-mode lua-mode dockerfile-mode browse-kill-ring ripgrep flx projectile yaml-mode slime cider magit hydra web-mode))
 '(post-jump-header nil)
 '(projectile-mode-line
   '(:eval
     (if
         (file-remote-p default-directory)
         "[-]"
       (format "[%s]"
               (projectile-project-name)))))
 '(recentf-mode nil)
 '(require-final-newline nil)
 '(save-place t nil (saveplace))
 '(save-place-limit 20)
 '(select-enable-clipboard t)
 '(show-paren-mode t)
 '(show-paren-style 'parenthesis)
 '(sql-product 'postgres)
 '(tab-stop-list
   '(4 8 12 16 20 24 28 32 36 40 44 48 52 56 60 64 68 72 76 80 84 88 92 96 100 104 108 112 116 120))
 '(tab-width 8)
 '(tramp-auto-save-directory "~/.emacs.d/.autosave.d")
 '(tramp-backup-directory-alist '(("." . "~/.emacs.d/backup")))
 '(uniquify-buffer-name-style 'forward nil (uniquify))
 '(vc-handled-backends nil)
 '(visible-bell t)
 '(web-mode-code-indent-offset 2)
 '(web-mode-enable-auto-indentation nil)
 '(web-mode-enable-auto-quoting nil)
 '(web-mode-markup-indent-offset 2)
 '(woman-fill-column 95)
 '(woman-fontify t)
 '(woman-use-own-frame nil)
 '(xref-search-program 'ripgrep)
 '(xref-show-definitions-function 'xref-show-definitions-completing-read))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(line-number-current-line ((t (:inherit highlight))))
 '(tab-line ((t (:inherit variable-pitch :background "dim gray" :foreground "black" :height 0.9))))
 '(web-mode-html-tag-face ((t (:inherit font-lock-constant-face))))
 '(woman-italic ((t (:underline t :slant italic)))))
