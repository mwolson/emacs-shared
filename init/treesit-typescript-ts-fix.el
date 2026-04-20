;;; treesit-typescript-ts-fix.el --- -*- lexical-binding: t -*-
;;
;; Description: Workaround for Emacs bug#79687 in js-ts-mode and typescript-ts-mode
;; Author: Michael Olson
;;
;; The contents of this file may be used, distributed, and modified
;; without restriction.

;;; Commentary:

;; See `treesit-python-ts-fix.el' for background on bug#79687.
;;
;; `js--treesit-font-lock-settings' and
;; `typescript-ts-mode--font-lock-settings' both embed `:match'
;; predicates for UPPERCASE constants and NaN/Infinity identifiers,
;; which Emacs 30.2 serialises to `#match' (no trailing `?').
;; libtree-sitter >=0.26 rejects that at query parse time with
;; `treesit-query-error "Syntax error at"', breaking jtsx-jsx-mode /
;; jtsx-tsx-mode / jtsx-typescript-mode (all three inherit font-lock
;; from these parents on Emacs >=29.3).
;;
;; Rebuild the affected settings using capture-name-is-a-function
;; fontifiers so no `#match' predicate is emitted.
;;
;; Removal plan: delete this whole file and its `require' in
;; `shared-init.el' once we upgrade to an Emacs that carries the
;; bug#79687 fix (Emacs 31+, or a backport landing in a future 30.x
;; point release).

;;; Code:

(eval-when-compile
  (require 'js nil t)
  (require 'typescript-ts-mode nil t))

(defvar my-js--treesit-constant-regex "\\`[A-Z_][0-9A-Z_]*\\'")
(defvar my-js--treesit-number-identifier-regex "\\`\\(?:NaN\\|Infinity\\)\\'")

(defun my-js--treesit-fontify-constant (node override start end &rest _)
  (when (string-match-p my-js--treesit-constant-regex
                        (treesit-node-text node t))
    (treesit-fontify-with-override
     (treesit-node-start node) (treesit-node-end node)
     'font-lock-constant-face override start end)))

(defun my-js--treesit-fontify-number-identifier (node override start end &rest _)
  (when (string-match-p my-js--treesit-number-identifier-regex
                        (treesit-node-text node t))
    (treesit-fontify-with-override
     (treesit-node-start node) (treesit-node-end node)
     'font-lock-number-face override start end)))

(with-eval-after-load 'js
  (setq js--treesit-font-lock-settings
        (treesit-font-lock-rules
         :language 'javascript
         :feature 'comment
         '([(comment) (hash_bang_line)] @font-lock-comment-face)

         :language 'javascript
         :feature 'constant
         '(((identifier) @my-js--treesit-fontify-constant)
           [(true) (false) (null)] @font-lock-constant-face)

         :language 'javascript
         :feature 'keyword
         `([,@js--treesit-keywords] @font-lock-keyword-face
           [(this) (super)] @font-lock-keyword-face)

         :language 'javascript
         :feature 'string
         '((regex pattern: (regex_pattern)) @font-lock-regexp-face
           (string) @font-lock-string-face)

         :language 'javascript
         :feature 'string-interpolation
         :override t
         '((template_string) @js--fontify-template-string
           (template_substitution ["${" "}"] @font-lock-misc-punctuation-face))

         :language 'javascript
         :feature 'definition
         `(,@(js--treesit-font-lock-compatibility-definition-feature)

           (class_declaration
            name: (identifier) @font-lock-type-face)

           (function_declaration
            name: (identifier) @font-lock-function-name-face)

           (method_definition
            name: (property_identifier) @font-lock-function-name-face)

           (formal_parameters
            [(identifier) @font-lock-variable-name-face
             (array_pattern (identifier) @font-lock-variable-name-face)
             (object_pattern (shorthand_property_identifier_pattern) @font-lock-variable-name-face)])

           (variable_declarator
            name: (identifier) @font-lock-variable-name-face)

           (variable_declarator
            name: [(array_pattern (identifier) @font-lock-variable-name-face)
                   (object_pattern
                    (shorthand_property_identifier_pattern) @font-lock-variable-name-face)])

           ;; full module imports
           (import_clause (identifier) @font-lock-variable-name-face)
           ;; named imports with aliasing
           (import_clause (named_imports (import_specifier
                                          alias: (identifier) @font-lock-variable-name-face)))
           ;; named imports without aliasing
           (import_clause (named_imports (import_specifier
                                          !alias
                                          name: (identifier) @font-lock-variable-name-face)))

           ;; full namespace import (* as alias)
           (import_clause (namespace_import (identifier) @font-lock-variable-name-face)))

         :language 'javascript
         :feature 'assignment
         '((assignment_expression
            left: (_) @js--treesit-fontify-assignment-lhs))

         :language 'javascript
         :feature 'function
         '((call_expression
            function: [(identifier) @font-lock-function-call-face
                       (member_expression
                        property:
                        (property_identifier) @font-lock-function-call-face)]))

         :language 'javascript
         :feature 'jsx
         '((jsx_opening_element name: (_) @font-lock-function-call-face)
           (jsx_closing_element name: (_) @font-lock-function-call-face)
           (jsx_self_closing_element name: (_) @font-lock-function-call-face)
           (jsx_attribute (property_identifier) @font-lock-constant-face))

         :language 'javascript
         :feature 'property
         '(((property_identifier) @font-lock-property-use-face)
           (pair value: (identifier) @font-lock-variable-use-face)
           ((shorthand_property_identifier) @font-lock-property-use-face))

         :language 'javascript
         :feature 'number
         '((number) @font-lock-number-face
           ((identifier) @my-js--treesit-fontify-number-identifier))

         :language 'javascript
         :feature 'operator
         `([,@js--treesit-operators] @font-lock-operator-face
           (ternary_expression ["?" ":"] @font-lock-operator-face))

         :language 'javascript
         :feature 'bracket
         '((["(" ")" "[" "]" "{" "}"]) @font-lock-bracket-face)

         :language 'javascript
         :feature 'delimiter
         '((["," "." ";" ":"]) @font-lock-delimiter-face)

         :language 'javascript
         :feature 'escape-sequence
         :override t
         '((escape_sequence) @font-lock-escape-face)

         ;; "document" should be first, to avoid overlap.
         :language 'jsdoc
         :override t
         :feature 'document
         '((document) @font-lock-doc-face)

         :language 'jsdoc
         :override t
         :feature 'keyword
         '((tag_name) @font-lock-constant-face)

         :language 'jsdoc
         :override t
         :feature 'bracket
         '((["{" "}"]) @font-lock-bracket-face)

         :language 'jsdoc
         :override t
         :feature 'property
         '((type) @font-lock-type-face)

         :language 'jsdoc
         :override t
         :feature 'definition
         '((identifier) @font-lock-variable-name-face))))

(with-eval-after-load 'typescript-ts-mode
  (defun typescript-ts-mode--font-lock-settings (language)
    "Tree-sitter font-lock settings.
Argument LANGUAGE is either `typescript' or `tsx'."
    (let ((func-exp (tsx-ts-mode--font-lock-compatibility-function-expression language)))
      (treesit-font-lock-rules
       :language language
       :feature 'comment
       `([(comment) (hash_bang_line)] @font-lock-comment-face)

       :language language
       :feature 'constant
       `(((identifier) @my-js--treesit-fontify-constant)
         [(true) (false) (null)] @font-lock-constant-face)

       :language language
       :feature 'keyword
       `([,@typescript-ts-mode--keywords] @font-lock-keyword-face
         [(this) (super)] @font-lock-keyword-face)

       :language language
       :feature 'string
       `((regex pattern: (regex_pattern)) @font-lock-regexp-face
         (string) @font-lock-string-face
         (template_string) @js--fontify-template-string
         (template_substitution ["${" "}"] @font-lock-misc-punctuation-face))

       :language language
       :override t ;; for functions assigned to variables
       :feature 'declaration
       `((,func-exp
          name: (identifier) @font-lock-function-name-face)
         (function_declaration
          name: (identifier) @font-lock-function-name-face)
         (function_signature
          name: (identifier) @font-lock-function-name-face)

         (method_definition
          name: (property_identifier) @font-lock-function-name-face)
         (method_signature
          name: (property_identifier) @font-lock-function-name-face)
         (required_parameter (identifier) @font-lock-variable-name-face)
         (optional_parameter (identifier) @font-lock-variable-name-face)

         (variable_declarator
          name: (identifier) @font-lock-function-name-face
          value: [(,func-exp) (arrow_function)])

         (variable_declarator
          name: (identifier) @font-lock-variable-name-face)

         (enum_declaration (identifier) @font-lock-type-face)

         (extends_clause value: (identifier) @font-lock-type-face)
         ;; extends React.Component<T>
         (extends_clause value: (member_expression
                                 object: (identifier) @font-lock-type-face
                                 property: (property_identifier) @font-lock-type-face))

         (arrow_function
          parameter: (identifier) @font-lock-variable-name-face)

         (variable_declarator
          name: (array_pattern
                 (identifier)
                 (identifier) @font-lock-function-name-face)
          value: (array (number) (,func-exp)))

         (catch_clause
          parameter: (identifier) @font-lock-variable-name-face)

         ;; full module imports
         (import_clause (identifier) @font-lock-variable-name-face)
         ;; named imports with aliasing
         (import_clause (named_imports (import_specifier
                                        alias: (identifier) @font-lock-variable-name-face)))
         ;; named imports without aliasing
         (import_clause (named_imports (import_specifier
                                        !alias
                                        name: (identifier) @font-lock-variable-name-face)))

         ;; full namespace import (* as alias)
         (import_clause (namespace_import (identifier) @font-lock-variable-name-face)))

       :language language
       :feature 'identifier
       `((nested_type_identifier
          module: (identifier) @font-lock-type-face)

         (type_identifier) @font-lock-type-face

         (predefined_type) @font-lock-type-face

         (new_expression
          constructor: (identifier) @font-lock-type-face)

         (enum_body (property_identifier) @font-lock-type-face)

         (enum_assignment name: (property_identifier) @font-lock-type-face)

         (variable_declarator
          name: (identifier) @font-lock-variable-name-face)

         (for_in_statement
          left: (identifier) @font-lock-variable-name-face)

         (arrow_function
          parameters:
          [(_ (identifier) @font-lock-variable-name-face)
           (_ (_ (identifier) @font-lock-variable-name-face))
           (_ (_ (_ (identifier) @font-lock-variable-name-face)))]))

       :language language
       :feature 'property
       `((property_signature
          name: (property_identifier) @font-lock-property-name-face)
         (public_field_definition
          name: (property_identifier) @font-lock-property-name-face)

         (pair key: (property_identifier) @font-lock-property-use-face)

         ((shorthand_property_identifier) @font-lock-property-use-face))

       :language language
       :feature 'expression
       `((assignment_expression
          left: [(identifier) @font-lock-function-name-face
                 (member_expression
                  property: (property_identifier) @font-lock-function-name-face)]
          right: [(,func-exp) (arrow_function)]))

       :language language
       :feature 'function
       '((call_expression
          function:
          [(identifier) @font-lock-function-call-face
           (member_expression
            property: (property_identifier) @font-lock-function-call-face)]))

       :language language
       :feature 'pattern
       `((pair_pattern
          key: (property_identifier) @font-lock-property-use-face
          value: [(identifier) @font-lock-variable-name-face
                  (assignment_pattern left: (identifier) @font-lock-variable-name-face)])

         (array_pattern (identifier) @font-lock-variable-name-face)

         ((shorthand_property_identifier_pattern) @font-lock-variable-name-face))

       :language language
       :feature 'jsx
       (append (tsx-ts-mode--font-lock-compatibility-bb1f97b language)
               `((jsx_attribute (property_identifier) @typescript-ts-jsx-attribute-face)))

       :language language
       :feature 'number
       `((number) @font-lock-number-face
         ((identifier) @my-js--treesit-fontify-number-identifier))

       :language language
       :feature 'operator
       `([,@typescript-ts-mode--operators] @font-lock-operator-face
         (ternary_expression ["?" ":"] @font-lock-operator-face))

       :language language
       :feature 'bracket
       '((["(" ")" "[" "]" "{" "}"]) @font-lock-bracket-face)

       :language language
       :feature 'delimiter
       '((["," "." ";" ":"]) @font-lock-delimiter-face)

       :language language
       :feature 'escape-sequence
       :override t
       '((escape_sequence) @font-lock-escape-face)))))

(provide 'treesit-typescript-ts-fix)
;;; treesit-typescript-ts-fix.el ends here
