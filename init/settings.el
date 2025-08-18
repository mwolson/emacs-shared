;; settings.el --- -*- lexical-binding: t -*-
;;
;; Customizations

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
 '(compilation-scroll-output 'first-error)
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
 '(eldoc-documentation-strategy 'eldoc-documentation-compose)
 '(eldoc-echo-area-use-multiline-p nil)
 '(electric-indent-mode nil)
 '(enable-recursive-minibuffers t)
 '(fill-column 100)
 '(flyspell-issue-welcome-flag nil)
 '(footnote-body-tag-spacing 1)
 '(git-commit-summary-max-length 120)
 '(image-use-external-converter t)
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(ispell-local-dictionary "american")
 '(ispell-personal-dictionary nil)
 '(ispell-silently-savep t)
 '(js-indent-level 2)
 '(ls-lisp-dirs-first t)
 '(ls-lisp-ignore-case t)
 '(ls-lisp-support-shell-wildcards nil)
 '(ls-lisp-use-insert-directory-program nil)
 '(ls-lisp-verbosity '(uid gid))
 '(magit-define-global-key-bindings nil)
 '(magit-diff-refine-hunk t)
 '(magit-display-buffer-function 'magit-display-buffer-same-window-except-diff-v1)
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
   '(("n" "Note" entry (file+headline "" "Notes") "* %?" :prepend t :empty-lines-after 1)))
 '(org-default-notes-file "~/Documents/notes.org")
 '(org-startup-folded nil)
 '(org-startup-truncated nil)
 '(org-yank-folded-subtrees nil)
 '(package-archives
   '(("melpa" . "http://melpa.org/packages/") ("gnu" . "http://elpa.gnu.org/packages/")
     ("nongnu" . "https://elpa.nongnu.org/nongnu/")))
 '(package-check-signature nil)
 '(package-selected-packages
   '(add-node-modules-path aidermacs apheleia archive-rpm astro-ts-mode atomic-chrome basic-mode
                           browse-kill-ring cape cider clojure-ts-mode
                           color-theme-sanityinc-tomorrow consult corfu corfu-prescient
                           corfu-terminal diff-hl diminish dumb-jump editorconfig el-mock embark
                           embark-consult erlang erlang-ts fish-mode flx git-modes gptel
                           graphql-ts-mode hydra js-comint jtsx kind-icon kotlin-ts-mode ligature
                           lua-mode magit marginalia markdown-mode maxframe minions minuet mise
                           modus-themes nerd-icons-completion nix-ts-mode nsis-mode orderless
                           package-build package-utils plz poly-markdown polymode prisma-ts-mode
                           pulsar rainbow-delimiters reformatter rg slime swift-ts-mode
                           terraform-mode tmux-mode toc-org transient vcl-mode vertico
                           vertico-prescient vterm web-mode wgrep zig-ts-mode))
 '(post-jump-header nil)
 '(project-vc-merge-submodules nil)
 '(recentf-mode nil)
 '(require-final-newline nil)
 '(save-place t nil (saveplace))
 '(save-place-limit 20)
 '(select-enable-clipboard t)
 '(select-enable-primary t)
 '(show-paren-mode t)
 '(show-paren-style 'parenthesis)
 '(sql-product 'postgres)
 '(tab-stop-list
   '(4 8 12 16 20 24 28 32 36 40 44 48 52 56 60 64 68 72 76 80 84 88 92 96 100 104 108 112 116 120))
 '(tab-width 8)
 '(tramp-auto-save-directory "~/.emacs.d/.autosave.d")
 '(tramp-backup-directory-alist '(("." . "~/.emacs.d/backup")))
 '(uniquify-buffer-name-style 'forward nil (uniquify))
 '(woman-fill-column 95)
 '(woman-fontify t)
 '(woman-use-own-frame nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(line-number-current-line ((t (:inherit highlight))))
 '(web-mode-html-tag-face ((t (:inherit font-lock-constant-face))))
 '(web-mode-json-key-face ((t (:foreground unspecified :inherit font-lock-variable-name-face))))
 '(woman-italic ((t (:underline t :slant italic)))))

(provide 'settings)
;;; settings.el ends here
