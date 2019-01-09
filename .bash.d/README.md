## functions.bash - utilities for a common bashrc across hosts

Copyright (c) 2007-2019, Ron DuPlain <ron.duplain@gmail.com>

Released under the BSD License.

**`ship`**

     Run `export` with same syntax, but only if target value is existing file.

     usage: ship VARIABLE=value

     similar to: export VARIABLE=value

     Note: this does not affect VARIABLE if target value does not exist.
     Note: export options -fn -p are not supported.

     Exit status:
         202 if no arguments are given
         200 if a given target does not exist
         0 otherwise

**`receive`**

     Run `source` with same syntax, but only if the target file exists.

     usage: receive path/to/file

     similar to: source path/to/file

     Note: This does nothing if target file does not exist.
     Overload: sendfile includes a receive command; this isn't it.

     Exit status:
         202 if no arguments are given
         200 if target file does not exist
         exit status of `source` otherwise

**`when_command`**

     Run a given command line when another given command is installed.

     usage: when_command command1 command2 args

     similar to: command2 args # if command1 is found.

     Note: This does nothing if command1 is not found.

     The first command argument is not assumed to be the desired command, as
     to allow invocation to programs which have command as a dependency.
     See `when_installed`.

     Exit status:
         202 if no arguments are given
         200 if command1 is not found
         exit status of command otherwise

**`when_installed`**

     Run a given command line when first argument is a command which is found.

     usage: when_command command args

     similar to: command args # if command is found.

     Note: This does nothing if command is not found.

     See `when_command`.

     Exit status:
         202 if no arguments are given
         200 if command is not found
         exit status of command otherwise

**`when_file`**

     Run a given command when given file exists.

     usage: when_file file command args

     similar to: command args # if file exists.

     Note: This does nothing if file does not exist.

     Exit status:
         202 if no arguments are given
         200 if file does not exist
         exit status of command otherwise

**`when_host`**

     Run a given command when current hostname matches first argument.

     usage: when_host host command args

     similar to: command args # if current hostname matches host arg.

     Note: This does nothing if current hostname does not match host arg.

     Exit status:
         202 if no arguments are given
         200 if host does not match
         exit status of command otherwise

**`prune_colons`**

     Clean colons in colon-separated PATH-like environment variable, in place.

     usage: prune_colons ENV

     Note that ENV should be passed by name. For example:

         prune_colons PATH
         prune_colons LD_LIBRARY_PATH

     Exit status:
         202 if no arguments are given
         0 otherwise

**`_pend`**

     Provide core of prepend and append functions.

     usage: _pend pre ARGS
     usage: _pend ap ARGS

     Exit status:
         202 if no arguments are given
         1 if any given argument is not a directory
         0 otherwise

**`prepend`**

     Prepend each argument to :-delimited variable, in place, LIFO.

     usage: prepend ENV NEWVALUE1 [NEWVALUE2, NEWVALUE3, ...]

     Note that env should be passed by name. For example:
         prepend PATH /usr/bin /bin
         prepend LD_LIBRARY_PATH /usr/lib /lib

     With last in, first out (LIFO), NEWVALUE1 prepends after NEWVALUE2.

     Note: Arguments are prepended if and only if the path exists, and
           argument is removed from variable before prepending.

     Exit status:
         202 if no arguments are given
         1 if any given argument is not a directory
         0 otherwise

**`append`**

     Append each argument to :-delimited variable, in place, FIFO.

     usage: append ENV NEWVALUE1 [NEWVALUE2, NEWVALUE3, ...]

     Note that env should be passed by name. For example:
         append PATH /usr/bin /bin
         append LD_LIBRARY_PATH /usr/lib /lib

     With first in, first out (FIFO), NEWVALUE1 appends before NEWVALUE2.

     Note: Arguments are appended if and only if the path exists, and
           argument is removed from variable before appending.

     Exit status:
         202 if no arguments are given
         1 if any given argument is not a directory
         0 otherwise

**`dedupe_path`**

     Remove duplicates from :-delimited variable.

     usage: dedupe_path ENV

     Note that env should be passed by name. For example:
         dedupe_path PATH
         dedupe_path LD_LIBRARY_PATH

     Supports one or more variables to dedupe, for example:
         dedupe_path PATH LD_LIBRARY_PATH

     Exit status:
         202 if no arguments are given
         0 otherwise

**`command_exists`**

     Check if command exists, looking for programs and bash functions/aliases.

     usage: command_exists COMMAND

     Exit status:
         202 if no arguments are given
         1 if given command is not found
         0 otherwise

