#!/bin/bash

if uname | grep 'MINGW' || uname | grep 'MSYS_NT' > /dev/null; then
    OS=Windows
    BIN_TPL=tpl/bin/windows
elif uname | grep 'Darwin' > /dev/null; then
    OS=OSX
    BIN_TPL=tpl/bin/mac
else
    OS=Linux
    BIN_TPL=tpl/bin/linux
fi

if which make > /dev/null; then
    BUILD=y
else
    BUILD=
fi

REQUIRED_EMACS_VERSION=27.1

# Create bin directory
rm -fr bin
mkdir bin
cp tpl/bin/shared/* bin
cp "$BIN_TPL"/* bin
PATH="$PWD"/bin:"$PATH"

# Set this environment variable to rebuild docs; otherwise use pre-built ones
: ${BUILD_DOCS:=}

DESTDIR=$PWD

# Make sure we're using the recommended version of Emacs
EMACS_VERSION=$(emacs --batch -q --no-site-file --eval "(message \"%s\" emacs-version)" 2>&1)

if ! which emacs > /dev/null; then
    echo >&2 "Error: Could not find \"emacs\" in your path"
    exit 1
fi

if [[ z$EMACS_VERSION != z${REQUIRED_EMACS_VERSION}* ]]; then
    echo >&2 "Error: Your version of Emacs is \"$EMACS_VERSION\", but should be \"${REQUIRED_EMACS_VERSION}.x\""
    exit 1
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

emacs --batch -q -l install-packages.el 2>&1 | grep -v '^Loading '

if test -n "$BUILD"; then
    if test -n "$BUILD_DOCS"; then
        rm -fr share/info
        mkdir -p share/info
    fi
fi

echo >&2
echo >&2 "Bootstrap complete!  Your Emacs is ready for use."
