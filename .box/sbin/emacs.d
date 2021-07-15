#!/usr/bin/env bash
# Sync ~/.emacs.d files with upstream.

check() {
    return 1
}

install() {
    # Assume host 'redeye' is primary development machine.
    if [ "$(< /etc/hostname)" = "redeye" ] && [ -d "$HOME/.emacs.d/straight" ]; then
        reqd_echo 'put latest to default upstream with `emacs-sync` ...'
        "$HOME"/bin/emacs-sync put
    else
        reqd_echo 'get latest from default upstream with `emacs-sync` ...'
        "$HOME"/bin/emacs-sync get
    fi
}

reqd_main "$@"
