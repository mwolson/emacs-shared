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

function get_treesit_abi() {
    emacs_script "$(get_topdir)"/scripts/get-treesit-abi.el
}

function get_tree_sitter_cmd() {
    local local_cmd

    local_cmd="$(get_topdir)/node_modules/.bin/tree-sitter"
    if [[ -x "${local_cmd}" ]]; then
        echo "${local_cmd}"
        return 0
    fi

    if qwhich tree-sitter; then
        echo tree-sitter
        return 0
    fi

    return 1
}

function semver_gt() {
    local lhs="$1"
    local rhs="$2"
    local lhs_a lhs_b lhs_c rhs_a rhs_b rhs_c

    IFS=. read -r lhs_a lhs_b lhs_c <<< "${lhs}"
    IFS=. read -r rhs_a rhs_b rhs_c <<< "${rhs}"

    lhs_a=${lhs_a:-0}
    lhs_b=${lhs_b:-0}
    lhs_c=${lhs_c:-0}
    rhs_a=${rhs_a:-0}
    rhs_b=${rhs_b:-0}
    rhs_c=${rhs_c:-0}

    if (( lhs_a != rhs_a )); then
        (( lhs_a > rhs_a ))
        return
    fi

    if (( lhs_b != rhs_b )); then
        (( lhs_b > rhs_b ))
        return
    fi

    (( lhs_c > rhs_c ))
}

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

function generate_grammar() {
    local lang=$1
    local tree_sitter_cmd
    local ts_cli_version
    local ts_generate_args=(generate --no-bindings)

    if ! tree_sitter_cmd="$(get_tree_sitter_cmd)"; then
        echo >&2 "Error: Could not find \"tree-sitter\" in your path"
        return 1
    fi

    if ! qwhich node; then
        echo >&2 "Error: Could not find \"node\" in your path"
        return 1
    fi

    ts_cli_version="$("${tree_sitter_cmd}" --version | awk '{print $2}' | sed 's/[^0-9.].*$//')"
    if [[ -n "${ts_cli_version}" ]] && semver_gt "${ts_cli_version}" "0.20.2"; then
        ts_generate_args+=(--abi "$(get_treesit_abi)")
    fi

    "${tree_sitter_cmd}" "${ts_generate_args[@]}"
}

function install_grammar() {
    local lang=$1
    local repo=${2:-"$1"}
    local subdir=${3:-src}
    local build_mode=$4
    local soext="$(get_so_ext)"

    update_submodule extra/"tree-sitter-${repo}"
    qpushd "$(get_topdir)"/extra/tree-sitter-"${repo}"

    if [[ "${build_mode}" == generate ]]; then
        echo "Generating ${lang} tree-sitter sources..."
        if ! generate_grammar "${lang}"; then
            qpopd
            return 1
        fi
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
