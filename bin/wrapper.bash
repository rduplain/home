# Bash library functions for creating a wrapper executable.

command_exists() {
    # Check if command exists, looking for programs and bash functions/aliases.

    # Return now if there are no arguments.
    [ $# -eq 0 ] && return 2

    local command="$1"
    shift

    type -t "$command" >/dev/null 2>&1
}

prompt_yn() {
    # Prompt given message with [Y/n], returning success/zero on "yes."

    if [ $# -ne 1 ]; then
        echo "usage: prompt_yn <message>" >&2
        return 2
    fi

    local message reply
    message="$1"
    shift

    read -e -p "${message} [Y/n] " reply

    case "$reply" in
        Y | y | "" )
            return
            ;;
        N | n)
            return 1
            ;;
        *)
            prompt_yn "$message"
            ;;
    esac
}

requested() {
    # Check if first argument is found in remaining arguments.

    local arg that

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

    local that

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

    local command directory_aside

    directory_aside="$1"
    command="$2"
    shift 2

    # Loop in a subshell to isolate `IFS` and `set` changes.
    (
        IFS=:
        set -o noglob

        found=

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

walkback() {
    # Walk the directory tree upward until the given function returns zero.
    #
    # Pass stdio through. The given function can write a value to stdout.

    # Return now if incorrect number of arguments.
    [ $# -ne 1 ] && return 2

    local arrived dir fn oldpwd

    fn="$1"
    shift

    oldpwd="$PWD"
    dir="$PWD"

    while [ -n "$dir" ]; do
        cd "$dir"

        if "$fn"; then
            cd "$oldpwd"
            return
        fi

        dir="${dir%/*}"

        if [ -z "$dir" ] && [ -z "$arrived" ]; then
            arrived=true
            dir=/
        fi
    done

    cd "$oldpwd"
    return 1
}
