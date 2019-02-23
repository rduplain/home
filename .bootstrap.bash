#!/bin/bash
# Bootstrap ~/.homegit git repository of $HOME files.

PROG=.bootstrap.bash

HOME_URL=${HOME_URL:-git@github.com:rduplain/home.git}
HOME_REV=${HOME_REV:-master}

HOMEGIT_DIR=${HOMEGIT_DIR:-"$HOME"/.homegit}

exec_shell() {
    # Print a message to stdout and `exec bash`, replacing current process.

    echo "Executing configured shell."
    exec bash
}

set_host_config() {
    # Set host configuration files.

    host_dir=.config/host/${HOSTNAME:-default}

    # Use a subshell to isolate PWD change.
    (
        cd "$HOME"
        for config in "$host_dir"/{Xresources,xsession,fluxbox}; do
            if [ -e "$config" ]; then
                filename="$(basename "$config")"
                ln -s "$config" ".${filename}"
            fi
        done
    )
}

main() {
    # Bootstrap $HOME git.

    if [ -e "$HOMEGIT_DIR" ]; then
        echo "$PROG: $HOMEGIT_DIR already exists." >&2
        exec_shell
    fi

    cd "$HOME"
    git clone $HOME_URL homegit-bootstrap
    mv homegit-bootstrap/.git "$HOMEGIT_DIR"
    rm -fr homegit-bootstrap
    GIT_DIR="$HOMEGIT_DIR" git checkout $HOME_REV >/dev/null
    GIT_DIR="$HOMEGIT_DIR" git checkout .
    GIT_DIR="$HOMEGIT_DIR" git config --add status.showUntrackedFiles no

    set_host_config

    exec_shell
}

main "$@"
