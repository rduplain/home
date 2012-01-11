"""Run custom commands when starting a Python interactive session."""

import os
import sys

def _print_debug(message):
    """Print debug information to stderr.

    Keyword arguments:
    message -- message to print.
    """
    print >> sys.stderr, message

# Enable tab completion.
try:
    import readline
except ImportError:
    _print_debug("Module readline not available.")
else:
    import rlcompleter
    readline.parse_and_bind("tab: complete")
    del rlcompleter
    del readline

# Enable cross-session history.
try:
    import readline
except ImportError:
    _print_debug("Module readline not available.")
else:
    history_file = os.path.join(os.environ["HOME"], ".python_history")
    try:
        readline.read_history_file(history_file)
    except IOError:
        _print_debug('History file not available.')

    import atexit
    # Set number of lines to save. Negative values mean infinite.
    readline.set_history_length(100000)
    atexit.register(readline.write_history_file, history_file)

    del history_file
    del atexit
    del readline

# Clean up the evidence.
del _print_debug
del sys
del os

# Reset __vars__ to default, for transparency of rc file.
try:
    del __file__
    __doc__ = None
except NameError:
    pass
