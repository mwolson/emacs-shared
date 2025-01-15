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
        qpushd "$(dirname $0)"
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

function byte_compile() {
    emacs --script "$(get_topdir)"/byte-compile-local.el "$@" 2>&1 | \
        grep -v '^Loading '
}

function set_treesit_dir() {
    emacs --script "$(get_topdir)"/get-treesit-dir.el "$@" 2>&1 | \
        grep -v '^Loading ' > "$(get_topdir)"/build/.treesit-dir
}

function get_treesit_dir() {
    cat "$(get_topdir)"/build/.treesit-dir
}

function install_treesit_grammar() {
    emacs --script "$(get_topdir)"/install-treesit-grammar.el "$@" 2>&1 | \
        grep -v '^Loading '
}
