# core.bash - Core shell API and language improvements.

with_cd() {
    # Run command within a directory change.

    local dir="$1"
    shift

    pushd "$dir" > /dev/null
    "$@"
    popd > /dev/null
}
