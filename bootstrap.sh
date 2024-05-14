#!/bin/bash

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

function compile_check() {
    if ! qwhich "$1"; then
        echo >&2 "Warning: Could not find \"$1\" in your path, skipping compilation"
        BUILD=
    fi
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
compile_check cmake
compile_check node
compile_check npm
if [[ $OS == Windows ]]; then
    compile_check ninja

    if ! uname_grep 'MINGW64_NT'; then
        # Symptom of building in Git Bash is that Emacs will crash when trying to use magit
        echo >&2 "Warning: Cannot build libgit unless you are in an MSYS2 MinGW 64-bit terminal, skipping compilation"
        BUILD=
    fi
fi

if [[ $OS == macOS ]]; then
    REQUIRED_EMACS_VERSION=29.1
else
    REQUIRED_EMACS_VERSION=29.3
fi

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
    if ! test -e /Applications/Emacs.app/Contents/MacOS/Emacs; then
        echo >&2 "Error: Emacs does not seem to be installed in Applications"
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

if ! qwhich cmake; then
    echo >&2 "Warning: Could not find \"cmake\" in your path, skipping compilation"
fi

if ! qwhich make; then
    echo >&2 "Warning: Could not find \"make\" in your path, skipping compilation"
fi

if [[ $OS == Windows ]]; then
    if ! qwhich ninja; then
        echo >&2 "Warning: Could not find \"ninja\" in your path, skipping compilation"
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

qpushd elisp/archive-rpm
git submodule init
git submodule update --depth 1
emacs --batch -q --eval='(package-install-file default-directory)' 2>&1 | grep -v '^Loading '
qpopd

qpushd elisp/ligature
git submodule init
git submodule update --depth 1
qpopd

qpushd extra/emacs
git submodule init
git submodule update --depth 1
qpopd

qpushd extra/tree-sitter-module
git submodule init
git submodule update --depth 1
# disabling until wider adoption is reached
# JOBS=4 ./batch.sh
qpopd

if test -n "$BUILD"; then
    npm ci

    qpushd elisp/libegit2
    git submodule init
    git submodule update --depth 1
    rm -fr build
    mkdir -p build
    cd build

    if [[ $OS == Windows ]]; then
        cmake -G Ninja ..
        ninja
        echo
    else
        cmake ..
        make
    fi

    qpopd

    # install manually, since they reference the wrong "libgit" package
    cp elisp/magit/lisp/magit-libgit.el elisp/
else
    rm -fr elisp/libegit2/build
    rm -f elisp/magit-libgit.el*
fi

emacs --batch -q -l install-packages.el 2>&1 | grep -v '^Loading '

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

    echo >&2 "Built manpages, make sure to check them in"
    qpopd
fi

echo >&2
echo >&2 "Bootstrap complete! Your Emacs is ready for use."
