#!/bin/bash

. "$(dirname $0)/include.sh"

function get_so_ext() {
    local os="$(get_os)"

    if [[ "$os" == Windows ]]; then
        echo dll
    elif [[ "$os" == macOS ]]; then
        echo dylib
    else
        echo so
    fi
}

HAS_ANY_CPP=

function compile_file() {
    local component=$1

    if [[ -f "${component}.c" ]]; then
        cc -fPIC -c -I. "${component}.c"
    elif [[ -f "${component}.cc" ]]; then
        c++ -fPIC -c -I. "${component}.cc"
        HAS_ANY_CPP=y
    fi
}

function link_grammar() {
    local lib=$1

    if [[ -z "$HAS_ANY_CPP" ]]; then
        cc -fPIC -shared *.o -o "${lib}"
    else
        c++ -fPIC -shared *.o -o "${lib}"
    fi
}

function install_grammar() {
    local lang=$1
    local repo=${2:-"$1"}
    local subdir=${3:-src}
    local needs_pnpm=$4
    local soext="$(get_so_ext)"

    update_submodule extra/"tree-sitter-${repo}"
    qpushd "$(get_topdir)"/extra/tree-sitter-"${repo}"

    if [[ -n "${needs_pnpm}" ]]; then
        echo "Installing ${lang} tree-sitter dependencies..."
        pnpm import --quiet
        pnpm install --prod --quiet
    fi

    qpushd "$subdir"
    compile_file parser
    compile_file scanner
    link_grammar "libtree-sitter-${lang}.${soext}"
    cp "libtree-sitter-${lang}.${soext}" "$(get_treesit_dir)/"
    echo "Copying libtree-sitter-${lang}.${soext} to $(get_treesit_dir)"
    qpopd

    qpopd
}

install_grammar "$@"
