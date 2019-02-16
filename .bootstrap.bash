#!/bin/bash
# Bootstrap ~/.homegit git repository of $HOME files.

PROG=.bootstrap.bash

HOME_URL=${HOME_URL:-git@github.com:rduplain/home.git}
HOME_REV=${HOME_REV:-master}

HOMEGIT_DIR=${HOMEGIT_DIR:-"$HOME"/.homegit}

main() {
    # Bootstrap $HOME git.

    if [ -e "$HOMEGIT_DIR" ]; then
        echo "$PROG: $HOMEGIT_DIR already exists."
        return 3
    fi

    cd "$HOME"
    git clone $HOME_URL homegit-bootstrap
    mv homegit-bootstrap/.git .homegit
    rm -fr homegit-bootstrap
    GIT_DIR="$HOME"/.homegit git checkout $HOME_REV >/dev/null
    GIT_DIR="$HOME"/.homegit git checkout .
    GIT_DIR="$HOME"/.homegit git config --add status.showUntrackedFiles no
}

main "$@"
