# functions.bash - GNU bash source script for shell utility functions
# Copyright 2007-2009 Ron DuPlain <ron.duplain@espresso-labs.com>
#
# A note on style: quotes are required throughout, to allow values with spaces.

# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

function ship () {
    # Run `export` with same syntax, but only if target value is existing file.
    # usage: ship VARIABLE=value
    #
    # similar to: export VARIABLE=value
    #
    # Note: this does not affect VARIABLE if target value does not exist.
    # Note: export options -fn -p are not supported.
    # Returns exit status:
    #     2 if no arguments are given
    #     1 if a given target does not exist
    #     0 otherwise

    # Return now if there are no arguments.
    [[ $# -eq 0 ]] && return 2

    local result=0
    local statement
    for statement in "$@"; do
        # Get the target of the statement (right-hand of equals sign).
        # i.e. find the target of the variable in: variable=target
        # Strip off STATEMENT from left to first equal sign.
        local target=${statement#*=}

        # Determine if the target exists, and export if so.
        if [ -e "$target" ]; then
            export "$statement"
        else
            result=1
        fi
    done
    return $result
}

function receive () {
    # Run `source` with same syntax, but only if the target file exists.
    # usage: receive path/to/file
    #
    # similar to: source path/to/file
    #
    # Note: this does nothing if target file does not exist.
    # Overload: sendfile includes a receive command; this isn't it.
    # Returns exit status:
    #     2 if no arguments are given
    #     1 if target file does not exist
    #     exit status of `source` otherwise

    # Return now if there are no arguments.
    [[ $# -eq 0 ]] && return 2

    local target="$1"
    shift

    [[ -f "$target" ]] && source "$target" "$@"
}

function forhost () {
    # Run a given command if the current hostname matches the first argument.
    # usage: forhost host command args
    #
    # similar to: command args (if current hostname matches forhost host arg)
    #
    # Note: this does nothing if current hostname does not match host arg.
    # Returns exit status:
    #     2 if no arguments are given
    #     1 if host does not match
    #     exit status of command otherwise

    # Return now if there are no arguments.
    [[ $# -eq 0 ]] && return 2

    local result=0
    local target_host=$1
    shift;
    if [ `uname -n` = "$target_host" ]; then
        "$@"
        result=$?
    else
        result=1
    fi
    return $result
}

function prune_colons () {
    # Clean colons in a colon-separated PATH-like environment variable.
    # usage: prune_colons ENV
    #
    # Note that ENV should be passed by name. For example:
    #     prune_colons PATH
    #     prune_colons LD_LIBRARY_PATH
    #
    # Returns exit status:
    #     2 if no arguments are given
    #     0 otherwise

    # Return now if there are no arguments.
    [[ $# -eq 0 ]] && return 2

    # Get the environment variable name.
    local envname=$1
    local env=${!envname}

    # Repeatedly remove double colons until no double colons exist.
    local envswap=${env//::/:}
    until [ "$env" = "$envswap" ]; do
        env=$envswap
        envswap=${env//::/:}
    done

    env=${env#:} # remove leading colon
    env=${env%:} # remove trailing colon

    # Restore the environment variable.
    export $envname="${env}"
    return 0
}

function _pend () {
    # Provide core of prepend and append functions.
    # usage: _pend pre ARGS
    # usage: _pend post ARGS
    #
    # Returns exit status:
    #     2 if no arguments are given
    #     1 if any given argument is not a directory
    #     0 otherwise

    local mode=$1
    shift;

    # Return now if there are no arguments.
    [[ $# -eq 0 ]] && return 2

    local envname=$1
    local env=${!envname}
    shift;

    # Get the arguments in targets, in the right order.
    local -a targets # Use array to allow values with spaces.
    case $mode in
        "pre")
            # Reorder list, so first is prepended last, LIFO.
            local -i x=1
            local -i count=$#
            for value in "$@"; do
                let index=$count-$x
                targets[$index]=$value
                let x++
            done
            ;;
        *)
            # Maintain same order in list, so first is appended first, FIFO.
            local -i x=0
            for value in "$@"; do
                targets[$x]=$value
                let x++
            done
            ;;
    esac

    # Assume current working directory if no target is given.
    if [ -z "${targets[*]}" ]; then
        targets[0]=`pwd`
    fi

    # Iterate through all targets and prepend/append according to mode.
    local result=0
    local target
    local -i k
    for (( k=0; $k < ${#targets[*]}; k++ )); do
        target=${targets[$k]}
        # Verify new target exists before prepending it.
        if [ -d "$target" ]; then
            # Add target while removing previous entries matching target.
            # Tack on colons, for pattern matching in parameter expansion.
            env=:${env}: # to be sure :$target: matches.
            [[ $mode == "pre" ]] && env=$target:${env//:$target:/:}
            [[ $mode == "post" ]] && env=${env//:$target:/:}:$target
            # Don't worry about too many colons; prune_colons later.
        else
            result=1
        fi
    done

    export $envname="${env}"
    prune_colons $envname
    return $result
}

function prepend () {
    # Prepend each argument to a :-delimited environment variable, LIFO.
    # usage: prepend env NEWVALUE1 [NEWVALUE2, NEWVALUE3, ...]
    #
    # Note that env should be passed by name. For example:
    #     prepend PATH /usr/bin /bin
    #     prepend LD_LIBRARY_PATH /usr/lib /lib
    #
    # Last in, first out (LIFO) suggests NEWVALUE1 prepends after NEWVALUE2.
    # Note: arguments are prepended if and only if the path exists.
    # Note: argument is removed from variable before prepending.
    #
    # Returns exit status:
    #     2 if no arguments are given
    #     1 if any given argument is not a directory
    #     0 otherwise

    _pend pre "$@"
}

function append () {
    # Append each argument to a :-delimited environment variable, FIFO.
    # usage: append env NEWVALUE1 [NEWVALUE2, NEWVALUE3, ...]
    #
    # Note that env should be passed by name. For example:
    #     append PATH /usr/bin /bin
    #     append LD_LIBRARY_PATH /usr/lib /lib
    #
    # First in, first out (FIFO) suggests NEWVALUE1 appends before NEWVALUE2.
    # Note: arguments are appended if and only if the path exists.
    # Note: argument is removed from variable before appending.
    #
    # Returns exit status:
    #     2 if no arguments are given
    #     1 if any given argument is not a directory
    #     0 otherwise

    _pend post "$@"
}
