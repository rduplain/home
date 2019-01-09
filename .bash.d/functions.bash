# functions.bash - utilities for a common bashrc across hosts
#
# Copyright (c) 2007-2019, Ron DuPlain <ron.duplain@gmail.com>
#
# Released under the BSD License.
#
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are met:
#
#     * Redistributions of source code must retain the above copyright notice,
#       this list of conditions and the following disclaimer.
#     * Redistributions in binary form must reproduce the above copyright
#       notice, this list of conditions and the following disclaimer in the
#       documentation and/or other materials provided with the distribution.
#
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
# AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
# IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
# ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
# LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
# CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
# SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
# INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
# CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
# ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
# POSSIBILITY OF SUCH DAMAGE.

function silently() {
    # Run given command silencing stdout & stderr.
    #
    # usage: silently command [ARG...]
    #
    # Exit status:
    #     exit status of command

    "$@" >/dev/null 2>&1
}

function ship() {
    # Run `export` with same syntax, but only if target value is existing file.
    #
    # usage: ship VARIABLE=value
    #
    # similar to: export VARIABLE=value
    #
    # Note: this does not affect VARIABLE if target value does not exist.
    # Note: export options -fn -p are not supported.
    #
    # Exit status:
    #     202 if no arguments are given
    #     200 if a given target does not exist
    #     0 otherwise

    # Return now if there are no arguments.
    [[ $# -eq 0 ]] && return 202

    local result=0
    local variable

    for variable in "$@"; do
        # Get the target of the variable (right-hand of equals sign).
        # i.e. find the target of the variable in: variable=target
        #
        # Parse VARIABLE from left to first equal sign.
        local target="${variable#*=}"

        # Determine if the target exists, and export if so.
        if [ -e "$target" ]; then
            export "$variable"
        else
            result=200
        fi
    done

    return $result
}

function receive() {
    # Run `source` with same syntax, but only if the target file exists.
    #
    # usage: receive path/to/file
    #
    # similar to: source path/to/file
    #
    # Note: This does nothing if target file does not exist.
    # Overload: sendfile includes a receive command; this isn't it.
    #
    # Exit status:
    #     202 if no arguments are given
    #     200 if target file does not exist
    #     exit status of `source` otherwise

    # Return now if there are no arguments.
    [[ $# -eq 0 ]] && return 202

    local target="$1"
    shift

    if [ -f "$target" ] || [ -L "$target" ]; then
        . "$target" "$@"
    else
        return 200
    fi
}

function when_command() {
    # Run a given command line when another given command is installed.
    #
    # usage: when_command command1 command2 args
    #
    # similar to: command2 args # if command1 is found.
    #
    # Note: This does nothing if command1 is not found.
    #
    # The first command argument is not assumed to be the desired command, as
    # to allow invocation to programs which have command as a dependency.
    # See `when_installed`.
    #
    # Exit status:
    #     202 if no arguments are given
    #     200 if command1 is not found
    #     exit status of command otherwise

    # Return now if there are no arguments.
    [[ $# -eq 0 ]] && return 202

    local result=0
    local target_command="$1"
    shift

    if command_exists "$target_command"; then
        "$@"
        result=$?
    else
        result=200
    fi

    return $result
}

function when_installed() {
    # Run a given command line when first argument is a command which is found.
    #
    # usage: when_command command args
    #
    # similar to: command args # if command is found.
    #
    # Note: This does nothing if command is not found.
    #
    # See `when_command`.
    #
    # Exit status:
    #     202 if no arguments are given
    #     200 if command is not found
    #     exit status of command otherwise

    # Return now if there are no arguments.
    [[ $# -eq 0 ]] && return 202

    local result=0

    if command_exists "$1"; then
        "$@"
        result=$?
    else
        result=200
    fi

    return $result
}

function when_file() {
    # Run a given command when given file exists.
    #
    # usage: when_file file command args
    #
    # similar to: command args # if file exists.
    #
    # Note: This does nothing if file does not exist.
    #
    # Exit status:
    #     202 if no arguments are given
    #     200 if file does not exist
    #     exit status of command otherwise

    # Return now if there are no arguments.
    [[ $# -eq 0 ]] && return 202

    local result=0
    local target_file="$1"
    shift

    if [ -e "$target_file" ]; then
        "$@"
        result=$?
    else
        result=200
    fi

    return $result
}

function when_host() {
    # Run a given command when current hostname matches first argument.
    #
    # usage: when_host host command args
    #
    # similar to: command args # if current hostname matches host arg.
    #
    # Note: This does nothing if current hostname does not match host arg.
    #
    # Exit status:
    #     202 if no arguments are given
    #     200 if host does not match
    #     exit status of command otherwise

    # Return now if there are no arguments.
    [[ $# -eq 0 ]] && return 202

    local result=0
    local target_host="$1"
    shift

    if [ "$(uname -n)" = "$target_host" ]; then
        "$@"
        result=$?
    else
        result=200
    fi

    return $result
}

function prune_colons() {
    # Clean colons in colon-separated PATH-like environment variable, in place.
    #
    # usage: prune_colons ENV
    #
    # Note that ENV should be passed by name. For example:
    #
    #     prune_colons PATH
    #     prune_colons LD_LIBRARY_PATH
    #
    # Exit status:
    #     202 if no arguments are given
    #     0 otherwise

    # Return now if there are no arguments.
    [[ $# -eq 0 ]] && return 202

    # Get the environment variable name.
    local envname="$1"
    local env="${!envname}"

    # Repeatedly remove double colons until no double colons exist.
    local envswap="${env//::/:}"
    until [ "$env" = "$envswap" ]; do
        env="$envswap"
        envswap="${env//::/:}"
    done

    env="${env#:}" # Remove leading colon.
    env="${env%:}" # Remove trailing colon.

    # Restore the environment variable.
    export $envname="${env}"
    return 0
}

function _pend() {
    # Provide core of prepend and append functions.
    #
    # usage: _pend pre ARGS
    # usage: _pend ap ARGS
    #
    # Exit status:
    #     202 if no arguments are given
    #     1 if any given argument is not a directory
    #     0 otherwise

    local mode="$1"
    shift

    # Return now if there are no arguments.
    [[ $# -eq 0 ]] && return 202

    local envname="$1"
    local env="${!envname}"
    shift

    # Determine the right order of dirs, LIFO or FIFO.
    local -a dirs
    case $mode in
        "pre")
            # Reorder list, so first is prepended last, LIFO.
            local -i x=1
            local -i count=$#
            for dir in "$@"; do
                let index=$count-$x
                dirs[$index]="$dir"
                let x++
            done
            ;;
        *)
            # Maintain same order in list, so first is appended first, FIFO.
            for dir in "$@"; do
                dirs[${#dirs[@]}]="$dir"
            done
            ;;
    esac

    # Assume current working directory if no dir is given.
    if [ ${#dirs[@]} -eq 0 ]; then
        dirs[0]="$PWD"
    fi

    # Iterate through all dirs and prepend/append according to mode.
    local result=0
    local dir
    local -i k

    for dir in "${dirs[@]}"; do
        # Verify new dir exists before prepending it.
        if [ -e "$dir" ]; then
            # Add dir while removing previous entries matching dir.
            # Tack on colons, for pattern matching in parameter expansion.
            env=":${env}:" # to be sure :$dir: matches.
            [[ $mode == "pre" ]] && env="$dir:${env//:$dir:/:}"
            [[ $mode == "ap" ]] && env="${env//:$dir:/:}:$dir"
            # Don't worry about too many colons; prune_colons later.
        else
            result=1
        fi
    done

    export $envname="${env}"
    prune_colons $envname
    return $result
}

function prepend() {
    # Prepend each argument to :-delimited variable, in place, LIFO.
    #
    # usage: prepend ENV NEWVALUE1 [NEWVALUE2, NEWVALUE3, ...]
    #
    # Note that env should be passed by name. For example:
    #     prepend PATH /usr/bin /bin
    #     prepend LD_LIBRARY_PATH /usr/lib /lib
    #
    # With last in, first out (LIFO), NEWVALUE1 prepends after NEWVALUE2.
    #
    # Note: Arguments are prepended if and only if the path exists, and
    #       argument is removed from variable before prepending.
    #
    # Exit status:
    #     202 if no arguments are given
    #     1 if any given argument is not a directory
    #     0 otherwise

    _pend pre "$@"
}

function append() {
    # Append each argument to :-delimited variable, in place, FIFO.
    #
    # usage: append ENV NEWVALUE1 [NEWVALUE2, NEWVALUE3, ...]
    #
    # Note that env should be passed by name. For example:
    #     append PATH /usr/bin /bin
    #     append LD_LIBRARY_PATH /usr/lib /lib
    #
    # With first in, first out (FIFO), NEWVALUE1 appends before NEWVALUE2.
    #
    # Note: Arguments are appended if and only if the path exists, and
    #       argument is removed from variable before appending.
    #
    # Exit status:
    #     202 if no arguments are given
    #     1 if any given argument is not a directory
    #     0 otherwise

    _pend ap "$@"
}

function dedupe_path() {
    # Remove duplicates from :-delimited variable.
    #
    # usage: dedupe_path ENV
    #
    # Note that env should be passed by name. For example:
    #     dedupe_path PATH
    #     dedupe_path LD_LIBRARY_PATH
    #
    # Supports one or more variables to dedupe, for example:
    #     dedupe_path PATH LD_LIBRARY_PATH
    #
    # Exit status:
    #     202 if no arguments are given
    #     0 otherwise

    # Return now if there are no arguments.
    [[ $# -eq 0 ]] && return 202

    for envname in "$@"; do
        local env="${!envname}"
        # Support paths with spaces.
        #
        # First, escape spaces.
        # Then, use the shell to parse arguments, replacing all ':' with ' '.
        env="${env// /\\ }"
        eval set -- "${env//:/ }"
        prepend $envname "$@"
    done
}

function command_exists() {
    # Check if command exists, looking for programs and bash functions/aliases.
    #
    # usage: command_exists COMMAND
    #
    # Exit status:
    #     202 if no arguments are given
    #     1 if given command is not found
    #     0 otherwise

    # Return now if there are no arguments.
    [[ $# -eq 0 ]] && return 202

    local command="$1"
    shift

    type -t "$command" >/dev/null 2>&1
}
