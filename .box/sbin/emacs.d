#!/usr/bin/env bash
# Sync ~/.emacs.d files from upstream.

source "$REQD_LIB"/ssh.bash

check() {
    return 1
}

install() {
    if ! ssh_agent_has_keys; then
        reqd_error "no keys available via ssh-agent. skipping ..."
        return
    fi

    reqd_echo 'use default upstream with `emacs-sync` ...'
    "$HOME"/bin/emacs-sync get
}

reqd_main "$@"
