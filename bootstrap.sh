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
    OS=OSX
    BIN_TPL=tpl/bin/mac
else
    OS=Linux
    BIN_TPL=tpl/bin/linux
fi

BUILD=y
compile_check which
compile_check make
compile_check cmake
if [[ $OS == Windows ]]; then
    compile_check ninja

    if ! uname_grep 'MINGW64_NT'; then
        echo >&2 "Warning: Cannot build libgit unless you are in an MSYS2 MinGW 64-bit terminal, skipping compilation"
        BUILD=
    fi
fi

REQUIRED_EMACS_VERSION=27.2

# Set this environment variable to rebuild docs; otherwise use pre-built ones
: ${BUILD_DOCS:=}

if [[ $OS == Windows ]]; then
    if ! qwhich emacs-${REQUIRED_EMACS_VERSION}; then
        PATH="/c/Program Files/Emacs/x86_64/bin":"$PATH"

        if ! qwhich emacs-${REQUIRED_EMACS_VERSION}; then
            echo >&2 "Error: Could not find \"emacs-${REQUIRED_EMACS_VERSION}\" in your path"
            exit 1
        fi
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

echo "OS         : $OS"
echo "BUILD      : $BUILD"
echo "BUILD_DOCS : $BUILD_DOCS"
echo "DESTDIR    : $DESTDIR"
echo

git submodule init
git submodule sync
git submodule update
echo

function install_info() {
    name=$1
    install -m 644 ${name} "$DESTDIR"/share/info
    install-info --info-dir="$DESTDIR"/share/info "$DESTDIR"/share/info/${name}
}

if test -n "$BUILD"; then
    qpushd elisp/libegit2
    git submodule init
    git submodule update
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

if test -n "$BUILD"; then
    if test -n "$BUILD_DOCS"; then
        rm -fr share/info
        mkdir -p share/info
    fi
fi

echo >&2
echo >&2 "Bootstrap complete!  Your Emacs is ready for use."
