# core.bash - Core shell API and language improvements.

contains() {
    # Check whether first argument exists in remaining arguments.

    local match
    match="$1"
    shift

    case "$*" in *"$match"*) return 0;; esac; return 1
}

startswith() {
    # Check whether first argument exists at the start of remaining arguments.

    local substr
    substr="$1"
    shift

    case "$*" in "$substr"*) return 0;; esac; return 1
}

with_cd() {
    # Run command within a directory change.

    local dir
    dir="$1"
    shift

    pushd "$dir" > /dev/null
    "$@"
    popd > /dev/null
}
