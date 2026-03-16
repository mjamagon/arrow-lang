#!/usr/bin/env bash
set -euo pipefail

# Build the tree-sitter-arrow grammar for Emacs
#
# Usage:
#   ./build.sh          — build + install to ~/.emacs.d/tree-sitter/
#   ./build.sh --no-install  — build only (produces libtree-sitter-arrow.{so,dylib})

INSTALL=1
[[ "${1:-}" == "--no-install" ]] && INSTALL=0

echo "==> Generating parser from grammar.js …"
npx tree-sitter generate

echo "==> Compiling shared library …"
OS="$(uname -s)"
case "$OS" in
  Darwin)
    EXT="dylib"
    # Apple clang uses -dynamiclib, not -shared
    ARCH="$(uname -m)"
    if [[ "$ARCH" == "arm64" ]]; then
      ARCHFLAGS="-arch arm64"
    elif [[ "$ARCH" == "x86_64" ]]; then
      ARCHFLAGS="-arch x86_64"
    else
      ARCHFLAGS=""
    fi
    cc -dynamiclib -o "libtree-sitter-arrow.$EXT" -fPIC \
       $ARCHFLAGS \
       -I src src/parser.c \
       -install_name "@rpath/libtree-sitter-arrow.$EXT"
    ;;
  *)
    EXT="so"
    cc -shared -o "libtree-sitter-arrow.$EXT" -fPIC \
       -I src src/parser.c
    ;;
esac

echo "    ✓ libtree-sitter-arrow.$EXT"

if [[ "$INSTALL" -eq 1 ]]; then
  # Emacs 29+ looks in ~/.emacs.d/tree-sitter/ by default.
  # It also checks treesit-extra-load-path if you set it.
  DEST="${HOME}/.emacs.d/tree-sitter"
  mkdir -p "$DEST"
  cp "libtree-sitter-arrow.$EXT" "$DEST/"
  echo "==> Installed to $DEST/libtree-sitter-arrow.$EXT"

  # On macOS, some Emacs builds (e.g. emacs-plus, railwaycat) look
  # for .dylib while others expect .so.  Install both names to be safe.
  if [[ "$OS" == "Darwin" ]]; then
    SO_DEST="$DEST/libtree-sitter-arrow.so"
    ln -sf "libtree-sitter-arrow.$EXT" "$SO_DEST" 2>/dev/null || \
      cp "libtree-sitter-arrow.$EXT" "$SO_DEST"
    echo "    + symlinked .so → .dylib for compatibility"
  fi
fi

echo "==> Done."
