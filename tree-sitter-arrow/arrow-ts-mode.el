;;; arrow-ts-mode.el --- Tree-sitter-based syntax highlighting for Arrow  -*- lexical-binding: t; -*-

;; Author: generated
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))
;; Keywords: languages, tree-sitter
;; URL: https://github.com/user/arrow-ts-mode

;;; Commentary:
;;
;; A tree-sitter-based major mode for the Arrow flowchart / pipeline
;; language.  Provides syntax highlighting via tree-sitter font-lock
;; rules, indentation, and navigation support.
;;
;; Requirements:
;;   - Emacs 29.1+ (built-in tree-sitter support)
;;   - The tree-sitter-arrow grammar shared library installed
;;
;; Installation:
;;
;;   1. Build the grammar:
;;
;;        cd tree-sitter-arrow
;;        npm install
;;        npx tree-sitter generate
;;        cc -shared -o arrow.so -fPIC -I src src/parser.c
;;
;;   2. Place the compiled `arrow.so` (or `arrow.dylib` on macOS) in
;;      your tree-sitter grammar directory.  By default Emacs looks in
;;      `~/.emacs.d/tree-sitter/` — rename it `libtree-sitter-arrow.so`
;;      (or `.dylib`):
;;
;;        mkdir -p ~/.emacs.d/tree-sitter
;;        cp arrow.so ~/.emacs.d/tree-sitter/libtree-sitter-arrow.so
;;
;;   3. Load this file:
;;
;;        (require 'arrow-ts-mode)
;;
;;      Or place it on your `load-path` and `(use-package arrow-ts-mode)`.
;;
;;   4. Open any `.arrow` file — or in org-mode, use the `arrow`
;;      source block language.  `arrow-ts-mode` activates automatically.
;;
;; Highlight categories:
;;
;;   face                      Arrow syntax
;;   ────────────────────────  ──────────────────────────────
;;   font-lock-comment-face    -- comments
;;   font-lock-variable-name   node names  (D, E, End, …)
;;   font-lock-type-face       module name in definitions
;;   font-lock-function-name   facilitator names (/Fac)
;;   font-lock-string-face     label text (:label)
;;   font-lock-operator-face   > and >? arrows
;;   font-lock-keyword-face    := operator, * (map)
;;   font-lock-warning-face    ! (force-rerun)
;;   font-lock-preprocessor    # (cache)
;;   font-lock-bracket-face    ( ) , ;

;;; Code:

(require 'treesit)

(defgroup arrow-ts nil
  "Tree-sitter-based Arrow language support."
  :group 'languages
  :prefix "arrow-ts-")

;; ── Font-lock rules ──────────────────────────────────────────────────────────

(defvar arrow-ts-mode--font-lock-rules
  '(
    ;; Level 1 — comments (always active)
    :language arrow
    :feature comment
    ((comment) @font-lock-comment-face)

    ;; Level 2 — keywords & operators
    :language arrow
    :feature keyword
    ([(define_op) (star)] @font-lock-keyword-face)

    :language arrow
    :feature operator
    ([(arrow_op) (arrow_query_op)] @font-lock-operator-face)

    ;; Level 3 — core identifiers
    :language arrow
    :feature definition
    ((definition
      name: (module_name (identifier) @font-lock-type-face)))

    :language arrow
    :feature variable
    ((annotated_node
      name: (node_name (identifier) @font-lock-variable-name-face)))

    :language arrow
    :feature function
    ((facilitator
      name: (identifier) @font-lock-function-name-face))

    :language arrow
    :feature string
    ((label
      text: (identifier) @font-lock-string-face))

    ;; Level 4 — delimiters, flags & punctuation
    :language arrow
    :feature delimiter
    ([(semicolon) "," ] @font-lock-delimiter-face)

    :language arrow
    :feature bracket
    (["(" ")"] @font-lock-bracket-face)

    :language arrow
    :feature flag
    ([
      (cache_flag) @font-lock-preprocessor-face
      (force_flag) @font-lock-warning-face
     ])

    :language arrow
    :feature punctuation
    ([(slash) (colon)] @font-lock-punctuation-face))
  "Tree-sitter font-lock rules for `arrow-ts-mode'.")

;; ── Indentation ──────────────────────────────────────────────────────────────

(defvar arrow-ts-mode--indent-rules
  '((arrow
     ((parent-is "source_file") column-0 0)
     ((node-is ")") parent-bol 0)
     ((parent-is "group") parent-bol 2)
     (no-node parent-bol 0)))
  "Tree-sitter indentation rules for `arrow-ts-mode'.")

;; ── Navigation (Imenu) ──────────────────────────────────────────────────────

(defvar arrow-ts-mode--imenu-settings
  '(("Module" "\\`definition\\'" nil
     (lambda (node)
       (treesit-node-text
        (treesit-node-child-by-field-name node "name") t))))
  "Imenu categories for `arrow-ts-mode'.
Each module definition (`name := …`) gets an Imenu entry.")

;; ── Major mode definition ────────────────────────────────────────────────────

(defvar arrow-ts-mode-syntax-table
  (let ((table (make-syntax-table)))
    ;; Treat -- as comment start (two dashes)
    (modify-syntax-entry ?- ". 12" table)
    (modify-syntax-entry ?\n ">" table)
    ;; Parens
    (modify-syntax-entry ?\( "()" table)
    (modify-syntax-entry ?\) ")(" table)
    ;; Punctuation
    (modify-syntax-entry ?> "." table)
    (modify-syntax-entry ?: "." table)
    (modify-syntax-entry ?/ "." table)
    (modify-syntax-entry ?\; "." table)
    (modify-syntax-entry ?* "." table)
    (modify-syntax-entry ?# "." table)
    (modify-syntax-entry ?! "." table)
    ;; Underscore and dot are word constituents (identifiers)
    (modify-syntax-entry ?_ "w" table)
    (modify-syntax-entry ?. "w" table)
    table)
  "Syntax table for `arrow-ts-mode'.")

;;;###autoload
(define-derived-mode arrow-ts-mode prog-mode "Arrow"
  "Major mode for the Arrow flowchart language, powered by tree-sitter.

Key bindings:
\\{arrow-ts-mode-map}"
  :group 'arrow-ts
  :syntax-table arrow-ts-mode-syntax-table

  (unless (treesit-ready-p 'arrow)
    (error "Tree-sitter grammar for Arrow is not available"))

  ;; Font-lock
  (setq-local treesit-font-lock-settings
              (apply #'treesit-font-lock-rules
                     arrow-ts-mode--font-lock-rules))
  (setq-local treesit-font-lock-feature-list
              '((comment)
                (keyword operator)
                (definition variable function string)
                (delimiter bracket flag punctuation)))

  ;; Indentation
  (setq-local treesit-simple-indent-rules arrow-ts-mode--indent-rules)

  ;; Imenu
  (setq-local treesit-simple-imenu-settings arrow-ts-mode--imenu-settings)

  ;; Comment support
  (setq-local comment-start "-- ")
  (setq-local comment-end "")

  ;; Navigation — defun = definition or track_line
  (setq-local treesit-defun-type-regexp
              (rx (or "definition" "track_line")))

  (treesit-major-mode-setup))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.arrow\\'" . arrow-ts-mode))

;; ── Org-babel integration ────────────────────────────────────────────────────
;; When editing #+begin_src arrow blocks in org, use arrow-ts-mode
;; for native syntax highlighting (requires org 9.6+).

(with-eval-after-load 'org-src
  (add-to-list 'org-src-lang-modes '("arrow" . arrow-ts)))

(provide 'arrow-ts-mode)
;;; arrow-ts-mode.el ends here
