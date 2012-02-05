# Interactive Ruby - rc for the interactive Ruby interpreter.

## Basics

# Use tab completion.
require 'irb/completion'

# Save history across sessions.
require 'irb/ext/save-history'
IRB.conf[:SAVE_HISTORY] = 1000000

# Set prompt to simple '>>'.
IRB.conf[:PROMPT_MODE] = :SIMPLE

# Print a blank line at exit, to give shell prompt its own line.
at_exit {puts}

## Convenient Functions

# Function to toggle irb's verbosity
# http://tagaholic.me/2009/05/29/exploring-how-to-configure-irb.html
def irb_verbosity_toggle
  irb_context.echo ? irb_context.echo = false : irb_context.echo = true
end


require 'rubygems'
