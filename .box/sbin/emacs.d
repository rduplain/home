#!/usr/bin/env bash
# Sync ~/.emacs.d files with upstream.

check() {
    return 1
}

install() {
    # Assume host 'espresso' is primary development machine.
    if [ "$(uname -n)" = "espresso" ] && [ -d "$HOME/.emacs.d/straight" ]; then
        reqd_echo 'put latest to default upstream with `emacs-sync` ...'
        "$HOME"/bin/emacs-sync put
    else
        reqd_echo 'get latest from default upstream with `emacs-sync` ...'
        "$HOME"/bin/emacs-sync get
    fi
}

reqd_main "$@"
