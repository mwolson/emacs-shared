;;; treesit-python-ts-fix.el --- -*- lexical-binding: t -*-
;;
;; Description: Workaround for Emacs bug#79687 in python-ts-mode
;; Author: Michael Olson
;;
;; The contents of this file may be used, distributed, and modified
;; without restriction.

;;; Commentary:

;; Emacs 30.2 serialises `:match' / `:equal' / `:pred' to `#match' /
;; `#equal' / `#pred' (no trailing `?'), which libtree-sitter >=0.26
;; rejects at query parse time. The upstream fix (commit b0143530 on
;; master, Emacs 31.x) changes both the serialiser and the C
;; dispatcher; it has not been backported to emacs-30 as of 30.2. On
;; Arch we ship libtree-sitter.so.0.26, so every `:match'-using query
;; in `python--treesit-settings' fails with `treesit-query-error
;; "Syntax error at"' the first time it hits tree-sitter -- including
;; during markdown native fontification of ```python``` blocks.
;;
;; Rebuild `python--treesit-settings' replacing the `:match' clauses
;; with capture-name-is-a-function fontifiers (the same mechanism used
;; by `@python--treesit-fontify-string' upstream). Captures named
;; after a function are left as plain captures by
;; `treesit-font-lock-rules', so no `#match' predicate is emitted.
;;
;; Removal plan: delete this whole file and its `require' in
;; `shared-init.el' once we upgrade to an Emacs that carries the
;; bug#79687 fix (Emacs 31+, or a backport landing in a future 30.x
;; point release).

;;; Code:

(eval-when-compile
  (require 'python nil t))

(with-eval-after-load 'python
  (defvar my-python--treesit-builtins-regex
    (rx-to-string `(seq bol (or ,@python--treesit-builtins) eol)))
  (defvar my-python--treesit-special-attributes-regex
    (rx-to-string `(seq bol (or ,@python--treesit-special-attributes) eol)))
  (defvar my-python--treesit-exceptions-regex
    (rx-to-string `(seq bol (or ,@python--treesit-exceptions) eol)))

  (defun my-python--treesit-fontify-self (node override start end &rest _)
    (when (equal (treesit-node-text node t) "self")
      (treesit-fontify-with-override
       (treesit-node-start node) (treesit-node-end node)
       'font-lock-keyword-face override start end)))

  (defun my-python--treesit-fontify-builtin (node override start end &rest _)
    (when (string-match-p my-python--treesit-builtins-regex
                          (treesit-node-text node t))
      (treesit-fontify-with-override
       (treesit-node-start node) (treesit-node-end node)
       'font-lock-builtin-face override start end)))

  (defun my-python--treesit-fontify-special-attribute (node override start end &rest _)
    (when (string-match-p my-python--treesit-special-attributes-regex
                          (treesit-node-text node t))
      (treesit-fontify-with-override
       (treesit-node-start node) (treesit-node-end node)
       'font-lock-builtin-face override start end)))

  (defun my-python--treesit-fontify-exception (node override start end &rest _)
    (when (string-match-p my-python--treesit-exceptions-regex
                          (treesit-node-text node t))
      (treesit-fontify-with-override
       (treesit-node-start node) (treesit-node-end node)
       'font-lock-type-face override start end)))

  (setq python--treesit-settings
        (treesit-font-lock-rules
         :feature 'comment
         :language 'python
         '((comment) @font-lock-comment-face)

         :feature 'string
         :language 'python
         '((string) @python--treesit-fontify-string
           (interpolation ["{" "}"] @font-lock-misc-punctuation-face))

         :feature 'keyword
         :language 'python
         `([,@python--treesit-keywords] @font-lock-keyword-face
           ((identifier) @my-python--treesit-fontify-self))

         :feature 'definition
         :language 'python
         '((function_definition
            name: (identifier) @font-lock-function-name-face)
           (class_definition
            name: (identifier) @font-lock-type-face)
           (parameters (identifier) @font-lock-variable-name-face)
           (parameters (typed_parameter (identifier) @font-lock-variable-name-face))
           (parameters (default_parameter name: (identifier) @font-lock-variable-name-face)))

         :feature 'builtin
         :language 'python
         '((call function: (identifier) @my-python--treesit-fontify-builtin)
           (attribute attribute: (identifier) @my-python--treesit-fontify-special-attribute))

         :feature 'decorator
         :language 'python
         '((decorator "@" @font-lock-type-face)
           (decorator (call function: (identifier) @font-lock-type-face))
           (decorator (identifier) @font-lock-type-face)
           (decorator [(attribute) (call (attribute))] @python--treesit-fontify-dotted-decorator))

         :feature 'function
         :language 'python
         '((call function: (identifier) @font-lock-function-call-face)
           (call function: (attribute
                            attribute: (identifier) @font-lock-function-call-face)))

         :feature 'constant
         :language 'python
         '([(true) (false) (none)] @font-lock-constant-face)

         :feature 'assignment
         :language 'python
         '((assignment left: (identifier)
                       @font-lock-variable-name-face)
           (assignment left: (attribute
                              attribute: (identifier)
                              @font-lock-variable-name-face))
           (augmented_assignment left: (identifier)
                                 @font-lock-variable-name-face)
           (named_expression name: (identifier)
                             @font-lock-variable-name-face)
           (for_statement left: (identifier) @font-lock-variable-name-face)
           (pattern_list [(identifier)
                          (list_splat_pattern (identifier))]
                         @font-lock-variable-name-face)
           (tuple_pattern [(identifier)
                           (list_splat_pattern (identifier))]
                          @font-lock-variable-name-face)
           (list_pattern [(identifier)
                          (list_splat_pattern (identifier))]
                         @font-lock-variable-name-face))

         :feature 'type
         :language 'python
         :override t
         ;; Dropped the isinstance/issubclass second-arg type face for
         ;; builtin types (str/int/dict/...): those sub-patterns match
         ;; on the outer call's function name via `:match', which we
         ;; cannot express without a predicate. Exception types still
         ;; fontify via `my-python--treesit-fontify-exception'.
         '(((identifier) @my-python--treesit-fontify-exception)
           (type [(identifier) (none)] @font-lock-type-face)
           (type (attribute attribute: (identifier) @font-lock-type-face))
           (type (_ !attribute [[(identifier) (none)] @font-lock-type-face
                                (attribute attribute: (identifier) @font-lock-type-face)]))
           (type (subscript (attribute attribute: (identifier) @font-lock-type-face)))
           (type (binary_operator) @python--treesit-fontify-union-types)
           (class_definition
            superclasses:
            (argument_list [(identifier) @font-lock-type-face
                            (attribute attribute: (identifier) @font-lock-type-face)
                            (subscript (identifier) @font-lock-type-face)
                            (subscript (attribute attribute: (identifier) @font-lock-type-face))]))
           (class_pattern (dotted_name (identifier) @font-lock-type-face :anchor)))

         :feature 'escape-sequence
         :language 'python
         :override t
         '((escape_sequence) @font-lock-escape-face)

         :feature 'number
         :language 'python
         '([(integer) (float)] @font-lock-number-face)

         :feature 'property
         :language 'python
         '((attribute
            attribute: (identifier) @font-lock-property-use-face)
           (class_definition
            body: (block
                   (expression_statement
                    (assignment left:
                                (identifier) @font-lock-property-use-face)))))

         :feature 'operator
         :language 'python
         `([,@python--treesit-operators] @font-lock-operator-face)

         :feature 'bracket
         :language 'python
         '(["(" ")" "[" "]" "{" "}"] @font-lock-bracket-face)

         :feature 'delimiter
         :language 'python
         '(["," "." ":" ";" (ellipsis)] @font-lock-delimiter-face)

         :feature 'variable
         :language 'python
         '((identifier) @python--treesit-fontify-variable))))

(provide 'treesit-python-ts-fix)
;;; treesit-python-ts-fix.el ends here
