;; settings.el --- -*- lexical-binding: t -*-
;;
;; Customizations

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(Man-notify-method 'pushy)
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
 '(current-language-environment "UTF-8")
 '(custom-safe-themes t)
 '(default-input-method "TeX")
 '(delete-selection-mode t)
 '(diff-switches "-u")
 '(doc-view-resolution 132)
 '(electric-indent-mode nil)
 '(enable-recursive-minibuffers t)
 '(fill-column 100)
 '(flyspell-issue-welcome-flag nil)
 '(footnote-body-tag-spacing 1)
 '(image-use-external-converter t)
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(ispell-local-dictionary "american")
 '(ispell-personal-dictionary nil)
 '(ispell-silently-savep t)
 '(mail-interactive t)
 '(mail-user-agent 'gnus-user-agent)
 '(max-lisp-eval-depth 1000)
 '(max-specpdl-size 1000 t)
 '(message-log-max 500)
 '(mouse-wheel-mode t)
 '(package-archives
   '(("gnu" . "https://elpa.gnu.org/packages/")))
 '(package-check-signature nil)
 '(post-jump-header nil)
 '(project-vc-merge-submodules nil)
 '(recentf-mode nil)
 '(require-final-newline nil)
 '(save-place t nil (saveplace))
 '(save-place-limit 20)
 '(save-place-mode t nil (saveplace))
 '(select-enable-clipboard t)
 '(show-paren-mode t)
 '(show-paren-style 'parenthesis)
 '(tab-stop-list
   '(4 8 12 16 20 24 28 32 36 40 44 48 52 56 60 64 68 72 76 80 84 88 92 96 100 104 108 112 116 120))
 '(tab-width 8)
 '(uniquify-buffer-name-style 'forward nil (uniquify)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(line-number-current-line ((t (:inherit highlight)))))

(provide 'settings)
;;; settings.el ends here
