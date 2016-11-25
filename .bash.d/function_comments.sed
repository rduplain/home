#!/bin/sed -nf
# Generate a markdown README from comments in functions.bash.
#
# Usage:
#     ./function_comments.sed functions.bash > README.md

# a: Append an empty line.
# p: Print result of the sed block to stdout.

1 {                  # With line 1 from file:
    s/# /## /g       # Replace comment with markdown header level 2.
    a
    p
}

3,5 {                # With lines 3 and 5 from file:
    s/# *//g         # Uncomment the line; remove '#' and all spaces after it.
    p
}

5 a

/^function/,/^$/ {   # Match line starting with 'function ...' until empty line.
    s/^function /**`/# Replace 'function ' with '**`'.
    s/() {$/`**\n/   # Replace ' () {' with '`**' and an extra line feed.
    s/    #/    /g   # Replace '    #' with '    '
    s/^ *$//g        # Trim lines with just whitespace to empty lines.
    p
}
