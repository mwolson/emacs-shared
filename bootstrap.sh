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

# Create bin directory
rm -fr bin
mkdir bin
cp etc/shared/* bin
cp "$ETC"/* bin
PATH="$(pwd)"/bin:"$PATH"

# Set this environment variable to rebuild docs; otherwise use pre-built ones
: ${BUILD_DOCS:=}

DESTDIR=$(pwd)

# Make sure we're using a version of Emacs that can at least load epa
EPA_WORKS=$(emacs --batch -q --no-site-file --eval "(message (locate-library \"epa\"))" 2>&1)

if test -z "$EPA_WORKS"; then
    echo >&2 "Error: Your version of Emacs is too old, should be at least version 23.1"
    exit 1
fi

if test -z "ELISP_DIR"; then
    echo >&2 "Error: Could not find \"emacs\" in your path"
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

if test -n "$BUILD"; then
    if test -n "$BUILD_DOCS"; then
        rm -fr share/info
        mkdir -p share/info
    fi

    # company-mode
    # (
    #     cd elisp/company-mode
    #     make clean compile
    # )

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
        # Skip compilation on OS X since we get an error when using compiled version there
        if test "$OS" != "OSX"; then
            cd elisp/js2-mode
            make clean all
        fi
    )

    # magit
    (
        cd elisp/magit
        # configure magit to find dependencies
	echo <<EOF > config.mk
LOAD_PATH  = -L $DESTDIR/elisp/magit/lisp
LOAD_PATH += -L $DESTDIR/elisp
EOF
        make clean lisp

        if test -n "$BUILD_DOCS"; then
            make docs install-docs DESTDIR="$DESTDIR" PREFIX=
        fi
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

    # Sepia
    (
        cd elisp/sepia

        if test -n "$BUILD_DOCS"; then
            # Makefile.PL?  No thanks.
            makeinfo --no-split -o sepia.info sepia.texi
            install_info sepia.info
            rm -f sepia.info
        fi
    )

    # SLIME
    (
        cd elisp/slime/doc

        if test -n "$BUILD_DOCS"; then
            make clean slime.info
            install_info slime.info
            make clean
        fi
    )
fi

echo >&2
echo >&2 "Bootstrap complete!  Your Emacs is ready for use."
