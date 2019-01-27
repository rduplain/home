# Bash library functions for creating a wrapper executable.

command_exists() {
    # Check if command exists, looking for programs and bash functions/aliases.

    # Return now if there are no arguments.
    [ $# -eq 0 ] && return 2

    local command="$1"
    shift

    type -t "$command" >/dev/null 2>&1
}

requested() {
    # Check if first argument is found in remaining arguments.

    that="$1"
    shift

    for arg in "$@"; do
        if [ "$arg" = "$that" ]; then
            return 0
        fi
    done

    return 1
}

requested_only() {
    # Like `requested` but when there's only one argument.

    that="$1"
    shift

    [ $# -eq 1 ] && [ "$1" = "$that" ]
}

set_aside_this_and_find_that() {
    # Set aside directory $1 and search PATH for command $2.

    if [ $# -ne 2 ]; then
        echo "usage: set_aside_this_and_find_that DIRECTORY COMMAND" >&2
        return 2
    fi

    local directory_aside="$1"
    local command="$2"
    shift 2

    # Loop in a subshell to isolate `IFS` and `set` changes.
    (
        IFS=:
        set -o noglob

        found=''

        for path in $PATH; do
            if [ "$path" = "$directory_aside" ]; then
                continue
            fi
            if [ -x "$path/$command" ]; then
                found="$path/$command"
                printf %s "$found"
                break
            fi
        done

        if [ -z "$found" ]; then
            echo "$command: command not found" >&2
        fi
    )
}
