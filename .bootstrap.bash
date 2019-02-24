#!/bin/bash
# Bootstrap ~/.homegit git repository of $HOME files.
#
# This program replaces itself with `exec bash` to drop into a configured
# shell. Regardless of $SHELL, bash is the configured shell in ~/.homegit.

PROG=.bootstrap.bash

HOME_URL=${HOME_URL:-git@github.com:rduplain/home.git}
HOME_REV=${HOME_REV:-master}

HOMEGIT_DIR=${HOMEGIT_DIR:-"$HOME"/.homegit}

clear_trap() {
    # Clear shell trap.

    trap - EXIT
}

exec_shell() {
    # Print a message to stdout and `exec bash`, replacing current process.

    echo "Executing configured shell."
    clear_trap
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

set_trap() {
    # Set shell trap in order to do the right thing on program exit.

    trap 'trap_ensure_shell' EXIT
}

trap_ensure_shell() {
    # Trap to ensure a configured shell in case of error.
    #
    # Support replacing this bootstrap program with a shell, falling back to an
    # unconfigured shell on error.

    if [ $? -ne 0 ]; then
        echo "Bootstrap hit an error; executing unconfigured shell."
        clear_trap
        exec bash
    fi
}

main() {
    # Bootstrap $HOME git.

    set -e # Exit immediately on command error.

    if [ -e "$HOMEGIT_DIR" ]; then
        echo "$PROG: $HOMEGIT_DIR already exists." >&2
        exec_shell
    fi

    set_trap

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
