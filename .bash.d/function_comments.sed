#!/bin/sed -nf
# Generate some markdown from the comments in the functions.bash file.
# Usage:
#     ./function_comments.sed functions.bash > README.md

1,1 { # Insert a header, and insert spaces before and after first line.
    i Utility functions for GNU Bash to simplify rc scripts & env manipulation
    i ========================================================================
    # Above: insert header. Below: insert empty lines before, after first line.
    i
    a
}

1,/^$/ {             # match from the first line until the first empty line
    s/# *//g         # remove the comment symbol and spaces following it
}

/^function/,/^$/ {   # match line starting with 'function ...' until empty line
    s/^function /**`/# change 'function ' to '**`'
    s/ () {$/`**\n/  # change ' () {' to '`**' with an extra line feed
    s/    #/    /g   # change '    #' to '    '
    s/^ *$//g        # clear out (but don't remove) lines with just spaces
    p                # print result to stdout
}
