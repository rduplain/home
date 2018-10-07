# Interactive Ruby - rc for the interactive Ruby interpreter.

# Use tab completion.
require 'irb/completion'

# Save history across sessions.
require 'irb/ext/save-history'
IRB.conf[:SAVE_HISTORY] = 1000000

# Set prompt to simple '>>'.
IRB.conf[:PROMPT_MODE] = :SIMPLE

# Print a blank line at exit, to give shell prompt its own line.
at_exit {puts}

require 'rubygems'
