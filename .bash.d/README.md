Utility functions for GNU Bash to simplify rc scripts & env manipulation
========================================================================

functions.bash - GNU bash source script for shell utility functions

Copyright 2007-2009 Ron DuPlain <ron.duplain@espresso-labs.com>

**`ship`**

     Run `export` with same syntax, but only if target value is existing file.
     usage: ship VARIABLE=value

     similar to: export VARIABLE=value

     Note: this does not affect VARIABLE if target value does not exist.
     Note: export options -fn -p are not supported.
     Returns exit status:
         2 if no arguments are given
         1 if a given target does not exist
         0 otherwise

**`receive`**

     Run `source` with same syntax, but only if the target file exists.
     usage: receive path/to/file

     similar to: source path/to/file

     Note: this does nothing if target file does not exist.
     Overload: sendfile includes a receive command; this isn't it.
     Returns exit status:
         2 if no arguments are given
         1 if target file does not exist
         exit status of `source` otherwise

**`forhost`**

     Run a given command if the current hostname matches the first argument.
     usage: forhost host command args

     similar to: command args (if current hostname matches forhost host arg)

     Note: this does nothing if current hostname does not match host arg.
     Returns exit status:
         2 if no arguments are given
         1 if host does not match
         exit status of command otherwise

**`prune_colons`**

     Clean colons in a colon-separated PATH-like environment variable.
     usage: prune_colons ENV

     Note that ENV should be passed by name. For example:
         prune_colons PATH
         prune_colons LD_LIBRARY_PATH

     Returns exit status:
         2 if no arguments are given
         0 otherwise

**`_pend`**

     Provide core of prepend and append functions.
     usage: _pend pre ARGS
     usage: _pend post ARGS

     Returns exit status:
         2 if no arguments are given
         1 if any given argument is not a directory
         0 otherwise

**`prepend`**

     Prepend each argument to a :-delimited environment variable, LIFO.
     usage: prepend env NEWVALUE1 [NEWVALUE2, NEWVALUE3, ...]

     Note that env should be passed by name. For example:
         prepend PATH /usr/bin /bin
         prepend LD_LIBRARY_PATH /usr/lib /lib

     Last in, first out (LIFO) suggests NEWVALUE1 prepends after NEWVALUE2.
     Note: arguments are prepended if and only if the path exists.
     Note: argument is removed from variable before prepending.

     Returns exit status:
         2 if no arguments are given
         1 if any given argument is not a directory
         0 otherwise

**`append`**

     Append each argument to a :-delimited environment variable, FIFO.
     usage: append env NEWVALUE1 [NEWVALUE2, NEWVALUE3, ...]

     Note that env should be passed by name. For example:
         append PATH /usr/bin /bin
         append LD_LIBRARY_PATH /usr/lib /lib

     First in, first out (FIFO) suggests NEWVALUE1 appends before NEWVALUE2.
     Note: arguments are appended if and only if the path exists.
     Note: argument is removed from variable before appending.

     Returns exit status:
         2 if no arguments are given
         1 if any given argument is not a directory
         0 otherwise

