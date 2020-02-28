# box.bash - Utilities to share between `box` & `reqd`.
#
# Assume core.bash is sourced.

home-rev() {
    # Report $HOME git HEAD revision.

    with_cd "$HOME" "$HOME"/bin/homegit rev-parse --short HEAD
}
