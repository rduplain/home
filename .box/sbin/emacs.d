#!/usr/bin/env bash
# Sync ~/.emacs.d files from upstream.

check() {
    return 1
}

install() {
    reqd_echo 'use default upstream with `emacs-sync` ...'
    "$HOME"/bin/emacs-sync get
}

reqd_main "$@"
