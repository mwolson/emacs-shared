#!/bin/bash

NOTICES=()
TOPDIR=$PWD

function qpopd() {
    popd > /dev/null
}

function qpushd() {
    pushd "$1" > /dev/null
}

function qwhich() {
    which "$1" > /dev/null 2>&1
}

function uname_grep() {
    uname | grep "$1" > /dev/null 2>&1
}

function notify() {
    local text="$1"

    NOTICES+=("$1")
}

function compile_check() {
    if ! qwhich "$1"; then
        notify "Warning: Could not find \"$1\" in your path, skipped compilation"
        BUILD=
    fi
}

function update_submodule () {
    local subdir="$1"

    qpushd "$subdir"
    git submodule init
    git submodule update --depth 1
    qpopd
}

function byte_compile() {
    emacs --script "$TOPDIR"/byte-compile-local.el "$@" 2>&1 | \
        grep -v '^Loading '
}

function get_treesit_dir() {
    emacs --script "$TOPDIR"/get-treesit-dir.el "$@" 2>&1 | \
        grep -v '^Loading '
}

function install_treesit_grammar() {
    emacs --script "$TOPDIR"/install-treesit-grammar.el "$@" 2>&1 | \
        grep -v '^Loading '
}

if uname_grep 'MINGW' || uname_grep 'MSYS_NT'; then
    OS=Windows
    BIN_TPL=tpl/bin/windows
elif uname_grep 'Darwin'; then
    OS=macOS
    BIN_TPL=tpl/bin/mac
else
    OS=Linux
    if grep "^Ubuntu" < /etc/issue > /dev/null 2>&1; then
        OS_VARIANT=Ubuntu
        BIN_TPL=tpl/bin/ubuntu
    else
        BIN_TPL=tpl/bin/linux
    fi
fi

BUILD=y
compile_check which
compile_check make
compile_check node
compile_check npm
if [[ $OS == Windows ]]; then
    compile_check ninja

    if ! uname_grep 'MINGW64_NT'; then
        # Symptom of building in Git Bash is that Emacs will crash when trying to use magit
        notify "Warning: Cannot compile binaries unless you are in an MSYS2 MinGW 64-bit terminal, skipped compilation"
        BUILD=
    fi
fi

REQUIRED_EMACS_VERSION=29.4

# Set this environment variable to rebuild git-for-windows manpages; otherwise use pre-built ones
: ${BUILD_GIT_MANPAGES:=}

if [[ $OS == Windows ]]; then
    if ! qwhich emacs-${REQUIRED_EMACS_VERSION}; then
        PATH="/c/Program Files/Emacs/emacs-${REQUIRED_EMACS_VERSION}/bin":"$PATH"

        if ! qwhich emacs-${REQUIRED_EMACS_VERSION}; then
            echo >&2 "Error: Could not find \"emacs-${REQUIRED_EMACS_VERSION}\" in your path"
            exit 1
        fi
    fi
elif [[ $OS == macOS ]]; then
    if ! test -e /Applications/Emacs.app; then
        echo >&2 "Error: Emacs does not seem to be installed in Applications"
        exit 1
    fi
    if ! test -e /opt/homebrew/opt/emacs-plus@29/Emacs.app/Contents/MacOS/Emacs; then
        echo >&2 "Error: Could not find Emacs Homebrew installation"
        exit 1
    fi
elif [[ $OS_VARIANT == Ubuntu ]]; then
    if ! test -x /usr/local/bin/emacs; then
        echo >&2 "Error: /usr/local/bin/emacs does not exist"
        exit 1
    fi
elif ! qwhich emacs; then
    echo >&2 "Error: Could not find \"emacs\" in your path"
    exit 1
fi

# Create bin directory
rm -fr bin
mkdir bin
cp tpl/bin/shared/* bin
cp "$BIN_TPL"/* bin

DESTDIR=$PWD
PATH="$PWD"/bin:"$PATH"

# Make sure we're using the recommended version of Emacs
EMACS_VERSION=$(emacs --batch -q --no-site-file --eval "(message \"%s\" emacs-version)" 2>&1)

if [[ z$EMACS_VERSION != z${REQUIRED_EMACS_VERSION}* ]]; then
    echo >&2 "Error: Your version of Emacs is \"$EMACS_VERSION\", but should be \"${REQUIRED_EMACS_VERSION}.x\""
    exit 1
fi

if ! qwhich make; then
    notify "Warning: Could not find \"make\" in your path, skipped compilation"
fi

if [[ $OS == Windows ]]; then
    if ! qwhich ninja; then
        notify "Warning: Could not find \"ninja\" in your path, skipped compilation"
    fi
fi

set -e

echo "OS      : $OS"
echo "BUILD   : $BUILD"
echo "BUILD_GIT_MANPAGES : $BUILD_GIT_MANPAGES"
echo "DESTDIR : $DESTDIR"
echo

git submodule init
git submodule sync
git submodule update --depth 1
echo

pnpm i

emacs --script install-packages.el 2>&1 | grep -v '^Loading '

qpushd share/man
git submodule init
git submodule update --depth 1
qpopd

if test -n "$BUILD_GIT_MANPAGES"; then
    qpushd extra/git
    git submodule init
    git submodule update --depth 1

    # check this for most up-to-date path:
    # https://packages.msys2.org/package/docbook-xsl?repo=msys&variant=x86_64
    xmlcatalog --noout \
      --add rewriteURI \
      http://docbook.sourceforge.net/release/xsl/current \
      /ucrt64/share/xml/docbook/xsl-stylesheets-1.79.2 \
      /etc/xml/catalog

    # check this for most up-to-date path:
    # https://packages.msys2.org/package/docbook-xml?repo=msys&variant=x86_64
    xmlcatalog --noout \
      --add rewriteURI \
      http://www.oasis-open.org/docbook/xml/4.5/xsl/current \
      /ucrt64/share/xml/docbook/xml-dtd-4.5 \
      /etc/xml/catalog
    xmlcatalog --noout \
      --add rewriteURI \
      http://www.oasis-open.org/docbook/xml/4.5 \
      /ucrt64/share/xml/docbook/xml-dtd-4.5 \
      /etc/xml/catalog

    # see also end of INSTALL doc at https://github.com/git-for-windows/git/blob/main/INSTALL#L229
    make prefix=$DESTDIR install-man

    notify "Built manpages, make sure to check them in"
    qpopd
fi

dir_elisp_submodules="archive-rpm gptel ligature polymode"
for mod in $dir_elisp_submodules; do
    update_submodule elisp/"$mod"
    byte_compile "$mod" elisp/"$mod"
done

file_elisp_submodules="asdf-vm erlang-ts poly-markdown prisma-ts-mode tmux-mode zig-ts-mode"
for mod in $file_elisp_submodules; do
    update_submodule elisp/"$mod"
    byte_compile "$mod" elisp/"$mod"/"$mod".el
done

update_submodule extra/emacs

install_treesit_grammar \
    markdown_inline \
    "https://github.com/tree-sitter-grammars/tree-sitter-markdown" \
    split_parser "tree-sitter-markdown-inline/src"

install_treesit_grammar \
    prisma \
    "https://github.com/victorhqc/tree-sitter-prisma"

update_submodule extra/tree-sitter-module
qpushd extra/tree-sitter-module
tree_sitter_modules="bash clojure dockerfile erlang go gomod nix python yaml zig"
rm -fr dist
<<< $tree_sitter_modules xargs rm -fr
<<< $tree_sitter_modules xargs -P4 -n1 ./build.sh
cp -f dist/* "$(get_treesit_dir)/"
qpopd

for idx in ${!NOTICES[@]}; do
    echo -e >&2 "\n${NOTICES[$idx]}"
done

echo -e >&2 "\nBootstrap complete! Your Emacs is ready for use."
