# tree-sitter-arrow

Tree-sitter grammar and Emacs major mode for the **Arrow** flowchart / pipeline language.

## What gets highlighted

| Syntax             | Example               | Face                          |
|--------------------|-----------------------|-------------------------------|
| Comments           | `-- this is ignored`  | `font-lock-comment-face`      |
| Node names         | `D`, `End`, `Node1`   | `font-lock-variable-name-face`|
| Module definitions | `prep := A > B`       | `font-lock-type-face`         |
| Facilitators       | `/Interaction1`       | `font-lock-function-name-face`|
| Labels             | `: my label`          | `font-lock-string-face`       |
| Arrows             | `>`, `>?`             | `font-lock-operator-face`     |
| Define / map       | `:=`, `*`             | `font-lock-keyword-face`      |
| Cache flag         | `#`                   | `font-lock-preprocessor-face` |
| Force flag         | `!`                   | `font-lock-warning-face`      |
| Brackets           | `(`, `)`, `,`, `;`    | `font-lock-bracket-face`      |

## Quick start

```bash
# 1. Install tree-sitter CLI (if you don't have it)
npm install -g tree-sitter-cli

# 2. Build and install the grammar for Emacs
cd tree-sitter-arrow
npm install
./build.sh        # compiles + copies to ~/.emacs.d/tree-sitter/

# 3. Load the Emacs mode
```

In your Emacs config:

```elisp
(add-to-list 'load-path "/path/to/tree-sitter-arrow")
(require 'arrow-ts-mode)
```

Or with `use-package`:

```elisp
(use-package arrow-ts-mode
  :load-path "/path/to/tree-sitter-arrow")
```

## What you get

- **Syntax highlighting** — all Arrow constructs get distinct faces
- **Indentation** — fork groups `(…)` indent by 2
- **Imenu** — `M-x imenu` lists module definitions (`name := …`)
- **Comment support** — `M-;` inserts `-- ` comments
- **Defun navigation** — `C-M-a` / `C-M-e` move between top-level flows
- **Org-babel** — `#+begin_src arrow` blocks get tree-sitter highlighting (org 9.6+)

## Files

```
tree-sitter-arrow/
├── grammar.js              # Tree-sitter grammar definition
├── queries/
│   └── highlights.scm      # Highlight queries (used by Emacs + CLI)
├── arrow-ts-mode.el        # Emacs 29+ major mode
├── build.sh                # Build & install script
├── src/                    # Generated parser (after `tree-sitter generate`)
│   └── parser.c
└── test.arrow              # Sample file for testing
```

## Requirements

- **Emacs 29.1+** (built-in tree-sitter support via `treesit`)
- **Node.js** (for `tree-sitter generate`)
- **C compiler** (for building the shared library)

## Grammar overview

The grammar parses these Arrow constructs:

```
A > B                   sequence
(A, B)                  parallel fork
A > B : label           labeled transition
A > B / Facilitator     facilitator annotation
flow ; flow             independent tracks
name := flow            module definition
A > B >? C              or-join fork
A > B* > C              parallel map
A#                      cache flag
A!                      force-rerun flag
-- comment              line comment
```
