# Build a wrapper executable.

command_exists() {
    # Check if command exists, looking for programs and bash functions/aliases.
    #
    # usage: command_exists COMMAND
    #
    # Exit status:
    #     2 if no arguments are given
    #     1 if given command is not found
    #     0 otherwise

    # Return now if there are no arguments.
    [[ $# -eq 0 ]] && return 2

    local command="$1"
    shift

    type -t "$command" >/dev/null 2>&1
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

    # Loop in a subshell to contain IFS and set changes.
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