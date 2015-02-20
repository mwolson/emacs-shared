;;; google-c-style.el --- Google's C/C++ style for c-mode

;; Keywords: c, tools

;; google-c-style.el is Copyright (C) 2008 Google Inc. All Rights Reserved.
;;
;; It is free software; you can redistribute it and/or modify it under the
;; terms of either:
;;
;; a) the GNU General Public License as published by the Free Software
;; Foundation; either version 1, or (at your option) any later version, or
;;
;; b) the "Artistic License".

;;; Commentary:

;; Provides the google C/C++ coding style. You may wish to add
;; `google-set-c-style' to your `c-mode-common-hook' after requiring this
;; file. For example:
;;
;;    (add-hook 'c-mode-common-hook 'google-set-c-style)
;;
;; If you want the RETURN key to go to the next line and space over
;; to the right place, add this to your .emacs right after the load-file:
;;
;;    (add-hook 'c-mode-common-hook 'google-make-newline-indent)

;;; Code:

;; For some reason 1) c-backward-syntactic-ws is a macro and 2)  under Emacs 22
;; bytecode cannot call (unexpanded) macros at run time:
(eval-when-compile (require 'cc-defs))

;; Wrapper function needed for Emacs 21 and XEmacs (Emacs 22 offers the more
;; elegant solution of composing a list of lineup functions or quantities with
;; operators such as "add")
(defun google-c-lineup-expression-plus-4 (langelem)
  "Indents to the beginning of the current C expression plus 4 spaces.

This implements title \"Function Declarations and Definitions\"
of the Google C++ Style Guide for the case where the previous
line ends with an open parenthese.

\"Current C expression\", as per the Google Style Guide and as
clarified by subsequent discussions, means the whole expression
regardless of the number of nested parentheses, but excluding
non-expression material such as \"if(\" and \"for(\" control
structures.

Suitable for inclusion in `c-offsets-alist'."
  (save-excursion
    (back-to-indentation)
    ;; Go to beginning of *previous* line:
    (c-backward-syntactic-ws)
    (back-to-indentation)
    (cond
     ;; We are making a reasonable assumption that if there is a control
     ;; structure to indent past, it has to be at the beginning of the line.
     ((looking-at "\\(\\(if\\|for\\|while\\)\\s *(\\)")
      (goto-char (match-end 1)))
     ;; For constructor initializer lists, the reference point for line-up is
     ;; the token after the initial colon.
     ((looking-at ":\\s *")
      (goto-char (match-end 0))))
    (vector (+ 4 (current-column)))))

(defun google-c-backwards-cascaded-call (stmt-start operator)
  "Try to move backwards to the previous instance of `operator'.
Return non-nil if we are looking at one."
  (and (zerop (c-backward-token-2 1 t stmt-start))
       (eq (char-after) ?\()
       (zerop (c-backward-token-2 2 t stmt-start))
       (cond ((looking-at operator) t)
             ((eq (char-after) ?\>)
              (forward-char)
              (and (c-backward-<>-arglist nil stmt-start)
                   (eq (char-after) ?\<)
                   (c-backward-token-2 1 t stmt-start)
                   (looking-at operator))))))

(defun google-c-skip-comments (limit)
  "If the point is at the beginning of a comment, skip past it.
Return non-nil if there was one, nil otherwise."
  (when (and (looking-at c-current-comment-prefix)
             (< 0 (- (match-end 0) (point))))
    (c-skip-comments-and-strings limit)))

(defun google-c-lineup-cascaded-calls (langelem)
  "Line up \"cascaded calls\" under each other.
If the line begins with \"->\" or \".\" and the preceding line ends
with one or more function calls preceded by the same token, then the
arrow is lined up with the first of those tokens.  E.g.:

result = proc->add(17)->add(18)
             ->add(19) +           <- google-c-lineup-cascaded-calls
  offset;                          <- google-c-lineup-cascaded-calls (inactive)

Further, if that rule doesn't apply, lines beginning with \"->\" or
\".\" will always be lined up according to c-basic-offset.  E.g.:

result = proc
    ->add(17)
    ->add(18)
    ->add(19)                      <- google-c-lineup-cascaded-calls

In any other situation nil is returned to allow use in list
expressions.

Works with: topmost-intro-cont, statement-cont, arglist-cont,
arglist-cont-nonempty, func-decl-cont, comment-intro."
  (if (and (eq (c-langelem-sym langelem) 'arglist-cont-nonempty)
	   (not (eq (c-langelem-2nd-pos c-syntactic-element)
		    (c-most-enclosing-brace (c-parse-state)))))
      ;; The innermost open paren is not ours, so don't do
      ;; anything.  This can occur for arglist-cont-nonempty with
      ;; nested arglist starts on the same line.
      nil
    (save-excursion
      (back-to-indentation)
      (while (google-c-skip-comments (point-max))
        (c-forward-syntactic-ws))
      (let ((operator (and (looking-at "->\\|\\.")
			   (regexp-quote (match-string 0))))
	    (stmt-start (c-langelem-pos langelem)) col)
	(when (and operator
		   (looking-at operator))
          (if (not (google-c-backwards-cascaded-call stmt-start operator))
              (progn
                (back-to-indentation)
                (vector (+ 4 (current-column))))
            (setq col (current-column))
            (while (google-c-backwards-cascaded-call stmt-start operator)
              (setq col (current-column)))
            (vector col)))))))

(defun google-c-lineup-new-class-instance (langelem)
  "If we are after a new class instance, use 2 spaces, otherwise fall through.

Suitable for arglist-cont-nonempty and inexpr-class."
  (save-excursion
    (if (eq (c-langelem-sym langelem) 'inexpr-class)
        0
      (goto-char (c-langelem-pos langelem))
      (cond
       ((looking-at ".*?new[^\n(]+()\\s *{\\s *$")
        (vector (current-column)))
       (t nil)))))

(defun google-c-lineup-blocks (langelem)
  "Treatment for blocks.

If we are on first line of block contents, use 2 additional spaces (total 4).
If we are on 2nd or more line of block contents, align to previous item.
If we are on the final ')}' part, then close the block.
If our innermost block-like thing is a paren continuation, indent exactly 4 spaces.
If we are on a return statement, align after 'return' and any whitespace.

Suitable for arglist-cont-nonempty, statement-cont, brace-list-intro, brace-list-close."
  (save-excursion
    (back-to-indentation)
    (let ((block-complete (looking-at "}"))
          (before-equals (save-excursion
                           (c-backward-syntactic-ws)
                           (backward-char)
                           (looking-at "="))))
      (beginning-of-line)
      (backward-up-list 1)
      (cond
       (block-complete
        (back-to-indentation)
        (vector (current-column)))
       (before-equals nil)
       ((looking-at "{")
        (back-to-indentation)
        (cond ((eq (c-langelem-sym langelem) 'statement-cont)
               (goto-char (c-langelem-pos langelem))
               (if (looking-at "return")
                   ;; this a return statement, which we align differently
                   (progn
                     (goto-char (match-end 0))
                     (c-forward-syntactic-ws)
                     (vector (current-column)))
                 ;; if the line before our containing line ends with "{", then align to containing line
                 (let ((col (current-column)))
                   (c-backward-syntactic-ws)
                   (backward-char)
                   (when (looking-at "{")
                     (vector col)))))
              ((memq (c-langelem-sym langelem) '(arglist-cont-nonempty brace-list-intro))
               (vector (+ 2 (current-column))))))
       ((looking-at "(")
        (when (eq (c-langelem-sym langelem) 'arglist-cont-nonempty)
          (back-to-indentation)
          (vector (+ 4 (current-column)))))))))

(defun google-c-lineup-parens (langelem)
  "If we see an expression while surrounded by parens, line it up to 1+ paren level.
If this is a function call, line up to the 2+ base indent level instead.

Suitable for `arglist-cont-nonempty'"
  (save-excursion
    (back-to-indentation)
    (while (google-c-skip-comments (point-max))
      (c-forward-syntactic-ws))
    (let ((saved-point (point))
          (stmt-start (c-langelem-pos langelem)))
      (backward-up-list 1)
      (cond ((not (eq (char-after) ?\())
             ;; we are in a block or some other construct, don't modify it
             nil)
            ((and (c-backward-token-2 1 t stmt-start)
                  (looking-at "[[:alpha:]]"))
             ;; we are at a function call, so indent 4
             (progn (back-to-indentation)
                    (vector (+ 4 (current-column)))))
            (t
             ;; we are at an expression, so indent to 1+ paren
             (goto-char saved-point)
             (c-lineup-arglist-intro-after-paren langelem))))))

;;;###autoload
(defconst google-c-style
  `((c-recognize-knr-p . nil)
    (c-enable-xemacs-performance-kludge-p . t) ; speed up indentation in XEmacs
    (c-recognize-<>-arglists . t)
    (c-basic-offset . 2)
    (indent-tabs-mode . nil)
    (c-comment-only-line-offset . 0)
    (c-hanging-braces-alist . ((defun-open after)
                               (defun-close before after)
                               (class-open after)
                               (class-close before after)
                               (inexpr-class-open after)
                               (inexpr-class-close before)
                               (namespace-open after)
                               (inline-open after)
                               (inline-close before after)
                               (block-open after)
                               (block-close . c-snug-do-while)
                               (extern-lang-open after)
                               (extern-lang-close after)
                               (statement-case-open after)
                               (substatement-open after)))
    (c-hanging-colons-alist . ((case-label)
                               (label after)
                               (access-label after)
                               (member-init-intro before)
                               (inher-intro)))
    (c-hanging-semi&comma-criteria
     . (c-semi&comma-no-newlines-for-oneline-inliners
        c-semi&comma-inside-parenlist
        c-semi&comma-no-newlines-before-nonblanks))
    (c-indent-comments-syntactically-p . t)
    (comment-column . 40)
    (c-indent-comment-alist . ((other . (space . 2))))
    (c-cleanup-list . (brace-else-brace
                       brace-elseif-brace
                       brace-catch-brace
                       empty-defun-braces
                       defun-close-semi
                       list-close-comma
                       scope-operator))
    (c-offsets-alist . ((arglist-intro google-c-lineup-expression-plus-4)
                        (arglist-cont-nonempty
                         . (google-c-lineup-new-class-instance
                            google-c-lineup-parens
                            google-c-lineup-blocks
                            ++))
                        (arglist-cont
                         . (google-c-lineup-cascaded-calls
                            0))
                        (inexpr-class
                         . (google-c-lineup-new-class-instance
                            +))
                        (func-decl-cont
                         . (google-c-lineup-cascaded-calls
                            ++))
                        (member-init-intro . ++)
                        (brace-list-intro
                         . (google-c-lineup-blocks
                            +))
                        (brace-list-close
                         . (google-c-lineup-blocks
                            0))
                        (inher-intro . ++)
                        (comment-intro
                         . (google-c-lineup-cascaded-calls
                            0))
                        (arglist-close . c-lineup-arglist)
                        (topmost-intro . 0)
                        (topmost-intro-cont . ++)
                        (block-open . 0)
                        (inline-open . 0)
                        (substatement-open . 0)
                        (annotation-var-cont . 0)
                        (statement-cont
                         .
                         (,(when (fboundp 'c-no-indent-after-java-annotations)
                             'c-no-indent-after-java-annotations)
                          google-c-lineup-cascaded-calls
                          google-c-lineup-blocks
                          ,(when (fboundp 'c-lineup-assignments)
                             'c-lineup-assignments)
                          ++))
                        (label . /)
                        (case-label . +)
                        (statement-case-open . +)
                        (statement-case-intro . +) ; case w/o {
                        (access-label . /)
                        (innamespace . 0))))
  "Google C/C++ Programming Style.")

;;;###autoload
(defun google-set-c-style ()
  "Set the current buffer's c-style to Google C/C++ Programming
  Style. Meant to be added to `c-mode-common-hook'."
  (interactive)
  (make-local-variable 'c-tab-always-indent)
  (setq c-tab-always-indent t)
  (c-add-style "Google" google-c-style t))

;;;###autoload
(defun google-make-newline-indent ()
  "Sets up preferred newline behavior. Not set by default. Meant
  to be added to `c-mode-common-hook'."
  (interactive)
  (define-key c-mode-base-map "\C-m" 'newline-and-indent)
  (define-key c-mode-base-map [ret] 'newline-and-indent))

(provide 'google-c-style)
;;; google-c-style.el ends here
