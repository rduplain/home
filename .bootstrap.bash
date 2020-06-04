#!/usr/bin/env bash
# Bootstrap ~/.homegit git repository of $HOME files.
#
# This program replaces itself with `exec bash` to drop into a configured
# shell. Regardless of $SHELL, bash is the configured shell via ~/.homegit.

# Ensure execution continues if variables are unset.
set +u

PROG=.bootstrap.bash

# Configure $HOME repository.
HOME_URL=${HOME_URL:-git@github.com:rduplain/home.git}
HOME_REV=${HOME_REV:-master}

HOMEGIT_DIR=${HOMEGIT_DIR:-"$HOME"/.homegit}

# Configure initial .box/bin/reqd & .box/bin/qwerty.sh to bootstrap `box`.
# These need not be latest, but "recent enough" for `box` to initialize.
: ${REQD_GIT:=https://github.com/rduplain/reqd.git}
: ${REQD_REV:=v2.2}

: ${QWERTY_SH_GIT:=https://github.com/rduplain/qwerty.sh.git}
: ${QWERTY_SH_REV:=v0.7}

# Configure qwerty.sh for initial download of .box/bin/ files.
: ${RAW_URL:=https://raw.githubusercontent.com}
: ${QWERTY_SH_URL:=$RAW_URL/rduplain/qwerty.sh/$QWERTY_SH_REV/qwerty.sh}
: ${QWERTY_SH:="curl --proto '=https' --tlsv1.2 -sSf $QWERTY_SH_URL | sh -s -"}

bootstrap_box_bin() {
    # Download executables to .box/bin/.

    cd "$HOME"

    eval "$QWERTY_SH" -b "$QWERTY_SH_REV" --chmod=a+x \
         "$QWERTY_SH_GIT" qwerty.sh:.box/bin/qwerty.sh

    eval "$QWERTY_SH" -b "$REQD_REV" --chmod=a+x \
         "$REQD_GIT" bin/reqd:.box/bin/reqd
}

clear_trap() {
    # Clear shell trap.

    trap - EXIT
}

exec_shell() {
    # Print a message to stdout and `exec bash`, replacing current process.

    echo "Executing configured shell."
    clear_trap
    exec bash -l
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

    set_trap

    if [ -e "$HOMEGIT_DIR" ]; then
        echo "$PROG: $HOMEGIT_DIR already exists." >&2
        exec_shell
    fi

    clone_dir="homegit-bootstrap-$(date +%s)"

    cd "$HOME"
    git clone $HOME_URL "$clone_dir"
    mv "$clone_dir"/.git "$HOMEGIT_DIR"
    rm -fr "$clone_dir"
    GIT_DIR="$HOMEGIT_DIR" git checkout $HOME_REV >/dev/null
    GIT_DIR="$HOMEGIT_DIR" git checkout .
    GIT_DIR="$HOMEGIT_DIR" git config --add status.showUntrackedFiles no

    bootstrap_box_bin

    bash -l -c "$HOME/.box/bin/box"

    exec_shell
}

main "$@"
