# box.bash - Utilities to share between `box` & `reqd`.
#
# Assume core.bash is sourced.

home-ref() {
    # Report git revision for given ref.

    with_cd "$HOME" "$HOME"/bin/homegit rev-parse --short "$1"
}

home-head() {
    # Report $HOME git HEAD revision.

    with_cd "$HOME" "$HOME"/bin/homegit rev-parse --short HEAD
}

home-upstream() {
    # Report upstream ref, e.g. 'origin/master', to stdout (without quotes).

    with_cd "$HOME" "$HOME"/bin/homegit rev-parse \
            --abbrev-ref --symbolic-full-name '@{upstream}' 2>/dev/null
}
