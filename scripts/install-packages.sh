#!/bin/bash

. "$(cd "$(dirname "${BASH_SOURCE[0]}")" &>/dev/null && pwd)"/include.sh

emacs_script "$(get_topdir)"/scripts/install-packages.el
