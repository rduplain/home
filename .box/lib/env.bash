# env.bash - Manage environment variables.

sourceable() {
    # Prefix each line of env argument with `export` as needed; print output.
    #
    # This does not support multi-line statements.

    local env_file line_before_hash
    env_file="$1"
    shift

    while read -r line; do
        line_before_hash=${line%%\#*}
        if [[ "$line_before_hash" == *=* ]]; then
            # Line has '=' before '#'. Send it along.
            if [[ "$line_before_hash" == "export *" ]]; then
                echo "$line"
            else
                echo "export $line"
            fi
        fi
    done < "$env_file"
}
