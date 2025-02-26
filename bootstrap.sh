#!/bin/bash

. "$(dirname $0)/include.sh"

NOTICES=()
OS="$(get_os)"

function compile_check() {
    if ! qwhich "$1"; then
        notify "Warning: Could not find \"$1\" in your path, skipped compilation"
        BUILD=
    fi
}

function notify() {
    local text="$1"

    NOTICES+=("$1")
}

if [[ "$OS" == Windows ]]; then
    BIN_TPL=tpl/bin/windows
elif [[ "$OS" == macOS ]]; then
    BIN_TPL=tpl/bin/mac
else
    if grep "^Ubuntu" < /etc/issue > /dev/null 2>&1; then
        OS_VARIANT=Ubuntu
        BIN_TPL=tpl/bin/ubuntu
    else
        BIN_TPL=tpl/bin/linux
    fi
fi

BUILD=y
compile_check which
compile_check cmake
compile_check make
compile_check node
compile_check pnpm
if [[ $OS == Windows ]]; then
    compile_check ninja

    if ! uname_grep 'MINGW64_NT'; then
        # Symptom of building in Git Bash is that Emacs will crash when trying to use magit
        notify "Warning: Cannot compile binaries unless you are in an MSYS2 MinGW 64-bit terminal, skipped compilation"
        BUILD=
    fi
fi

REQUIRED_EMACS_VERSION=30.1

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
    if [[ !  -e /Applications/Emacs.app ]]; then
        echo >&2 "Error: Emacs does not seem to be installed in Applications"
        exit 1
    fi
    if [[ ! -e /opt/homebrew/opt/emacs-plus@30/Emacs.app/Contents/MacOS/Emacs ]]; then
        echo >&2 "Error: Could not find Emacs Homebrew installation"
        exit 1
    fi
elif [[ $OS_VARIANT == Ubuntu ]]; then
    if [[ ! -x /usr/local/bin/emacs ]]; then
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

if [[ -n "$BUILD" ]]; then
    pnpm install --quiet
    pnpm run compile:json-server
fi

emacs_script "$(get_topdir)"/install-packages.el

if [[ -n "$BUILD" ]]; then
    echo "Compiling vterm..."
    emacs_script "$(get_topdir)"/install-vterm.el
fi

set_treesit_dir

qpushd share/man
git submodule init
git submodule update --depth 1
qpopd

if [[ -n "$BUILD_GIT_MANPAGES" ]]; then
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

for fullmod in recipes/*; do
    mod=${fullmod##*/}
    update_submodule elisp/"$mod"
    byte_compile "$mod" elisp/"$mod"
done

file_elisp_submodules="
    asdf-vm flymake-stylelint gptel-fn-complete mermaid-ts-mode
"
for mod in $file_elisp_submodules; do
    update_submodule elisp/"$mod"
    byte_compile "$mod" elisp/"$mod"/"$mod".el
done

update_submodule extra/emacs

if [[ -n "$BUILD" ]]; then
    "$(get_topdir)"/install-treesit-grammar.sh \
        markdown_inline markdown "tree-sitter-markdown-inline/src"

    "$(get_topdir)"/install-treesit-grammar.sh \
        tsx typescript "tsx/src"

    "$(get_topdir)"/install-treesit-grammar.sh \
        typescript "" "typescript/src"

    tree_sitter_modules="
        bash c c-sharp clojure cpp dockerfile erlang go gomod java javascript
        jsdoc json kotlin mermaid nix prisma python rust yaml zig
    "
    <<< $tree_sitter_modules xargs -P4 -n1 "$(get_topdir)"/install-treesit-grammar.sh

    "$(get_topdir)"/install-treesit-grammar.sh swift "" "" pnpm
else
    notify "Warning: tree-sitter modules will not be built, some major modes will not work"
fi

for idx in ${!NOTICES[@]}; do
    echo -e >&2 "\n${NOTICES[$idx]}"
done

echo -e >&2 "\nBootstrap complete! Your Emacs is ready for use."
