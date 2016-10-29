#!/bin/bash

if uname | grep 'MINGW32' > /dev/null; then
    OS=Windows
    ETC=etc/windows
elif uname | grep 'Darwin' > /dev/null; then
    OS=OSX
    ETC=etc/mac
else
    OS=Linux
    ETC=etc/linux
fi

if which make > /dev/null; then
    BUILD=y
else
    BUILD=
fi

REQUIRED_EMACS_VERSION=25.1

# Create bin directory
rm -fr bin
mkdir bin
cp etc/shared/* bin
cp "$ETC"/* bin
PATH="$(pwd)"/bin:"$PATH"

# Set this environment variable to rebuild docs; otherwise use pre-built ones
: ${BUILD_DOCS:=}

DESTDIR=$(pwd)

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

    # EMMS
    (
        cd elisp/emms
        make clean
        # Don't compile emms-setup.elc, since it causes problems if no dir named ~/.emacs.d/emms exists
        touch lisp/emms-setup.elc
        make lisp

        if test -n "$BUILD_DOCS"; then
            make docs
            cd doc
            install_info emms.info
        fi
        rm -f lisp/emms-setup.elc
    )

    # js2-mode
    (
        cd elisp/js2-mode
        make clean all
    )

    # Muse
    (
        cd elisp/muse
        make clean lisp

        if test -n "$BUILD_DOCS"; then
            cd texi
            make muse.info
            install_info muse.info
        fi
    )

    # sbt-mode
    (
        cd elisp/sbt-mode
        make clean all
    )

    # scala-mode2
    (
        cd elisp/scala-mode2
        make clean all
    )

    # Tramp
    (
        cd elisp/tramp
        # HATE ./configure, not running that on Windows
        cp ../../extra/tramp/Makefile .
        cp ../../extra/tramp/lisp/* lisp/
        cp ../../extra/tramp/texi/* texi/
        make lisp

        if test -n "$BUILD_DOCS"; then
            (
                cd texi
                make tramp
                cd ../info
                install_info tramp
            )
        fi
    )
fi

echo >&2
echo >&2 "Bootstrap complete!  Your Emacs is ready for use."
