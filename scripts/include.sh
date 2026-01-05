#!/bin/bash

function qpopd() {
    popd > /dev/null
}

function qpushd() {
    pushd "$1" > /dev/null
}

function qwhich() {
    command -v "$1" >/dev/null 2>&1
}

function uname_grep() {
    uname | grep "$1" > /dev/null 2>&1
}

function get_os() {
    if uname_grep 'MINGW' || uname_grep 'MSYS_NT'; then
        echo Windows
    elif uname_grep 'Darwin'; then
        echo macOS
    else
        echo Linux
    fi
}

export _DOTFILES_TOPDIR=

function get_topdir() {
    if [[ -n "${_DOTFILES_TOPDIR}" ]]; then
        echo "${_DOTFILES_TOPDIR}"
    else
        qpushd "$(dirname "${BASH_SOURCE[0]}")/.."
        echo "$PWD"
        qpopd
    fi
}

export _DOTFILES_TOPDIR="$(get_topdir)"

function update_submodule () {
    local subdir="$1"

    qpushd "$(get_topdir)/${subdir}"
    git submodule init
    git submodule update --depth 1
    qpopd
}

function emacs_script() {
    emacs --script "$@" 2>&1 | grep -v '^Loading '
}

function byte_compile() {
    if [[ -d "$2" ]]; then
        rm -f "$2"/*.elc
    elif [[ -f "$2" ]]; then
        rm -f "${2}c"
    else
        echo "Warning: unknown type for $2"
    fi

    emacs_script "$(get_topdir)"/scripts/byte-compile-local.el "$@"
}

function native_comp_all() {
    emacs_script "$(get_topdir)"/scripts/native-comp-all.el
}

function set_treesit_dir() {
    emacs_script "$(get_topdir)"/scripts/get-treesit-dir.el "$@" > \
        "$(get_topdir)"/build/.treesit-dir
}

function get_treesit_dir() {
    cat "$(get_topdir)"/build/.treesit-dir
}

