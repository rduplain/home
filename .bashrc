. "$HOME/.bash.d/functions.bash"

if [ -z "$PS1" ] && [ -n "$GDMSESSION" ]; then
    # Loaded via X desktop manager. Treat as interactive session.
    export PS1="init> "
fi

function rehash() {
    # Re-read bashrc and perform relevant rehash routines.

    unset BASHRC_INITIALIZED
    . "$HOME/.bashrc"
    for envtool in pyenv rbenv; do
        command_exists $envtool && $envtool rehash
    done
}

function set_title() {
    # Set window title of xterm-like terminal emulators.

    echo -ne "\033]0;$@\007"
}

export -f rehash set_title

function workon_walk() {
    # Walk the directory tree upward until:
    #  * a virtualenv is found which matches the name of a directory
    #  * the root directory is reached (no virtualenv found)
    #
    # Activate the identified virtualenv with virtualenvwrapper's workon.
    dir=${PWD}
    while [ -n "$dir" ]; do
        workon "${dir##*/}" >/dev/null 2>&1
        if [ $? -eq 0 ]; then
            break
        fi
        dir=${dir%/*}
    done
}

function source_these() {
    # Source all files found by name, in directory provided as last argument.
    #
    # Example:
    #
    #     source_these .bashrc .env path/to/project

    local dir last length path

    if [ $# -eq 0 ]; then
        exit 2
    elif [ $# -eq 1 ]; then
        dir="$PWD"
    else
        for last in "$@"; do true; done
        dir="$last"
    fi

    let length=$#-1
    for filename in "${@:1:$length}"; do
        path="$dir/$filename"
        if [ -e "$path" ]; then
            . "$path"
        fi
    done
}

function walk_root_to_curdir() {
    # Walk directory ancestry of current directory, calling given function.
    #
    # Call in order for $dir from / to current directory: command $dir

    local line_command="$@"
    local curdir="$PWD"
    local dir_count=$(grep -o '/' <<< "$curdir" | wc -l)

    local paths

    local index=$dir_count
    local dir="$curdir"
    local next
    while [ -n "$dir" ]; do
        paths[$index]="$dir"
        let index=index-1
        next="${dir%/*}"

        # Include '/'.
        if [ -z "$next" ] && [ "$dir" != "/" ]; then
            dir=/
        else
            dir="$next"
        fi
    done

    for path in "${paths[@]}"; do
        $line_command "$path"
    done
}

function omit_home() {
    # Call command unless last argument is $HOME.
    #
    # Useful with walk_root_to_curdir.

    for last in "$@"; do true; done
    if [ "$last" != "$HOME" ]; then
        "$@"
    fi
}

export -f workon_walk source_these walk_root_to_curdir omit_home

function commands() {
    history | cut -c8- | sort | uniq -c | sort -rn | head
}

function docker-cleanup() {
    if ! command_exists docker; then
        echo 'docker: command not found' >&2
        return 2
    fi

    exited=$(docker ps -a -q -f status=exited)
    if [ -n "$exited" ]; then
        docker rm -v $exited
    fi

    dangling=$(docker images -f dangling=true -q)
    if [ -n "$dangling" ]; then
        docker rmi $dangling
    fi
}

export -f commands docker-cleanup

prepend PATH /bin /sbin
prepend LD_LIBRARY_PATH /lib /lib32 /lib/i386-linux-gnu /lib64 /lib/x86_64-linux-gnu

prepend PATH /usr/bin /usr/sbin
prepend LD_LIBRARY_PATH /usr/lib /usr/lib32 /usr/lib/i386-linux-gnu /usr/lib64 /usr/lib/x86_64-linux-gnu
prepend MANPATH /usr/man /usr/share/man

prepend PATH /usr/local/bin /usr/local/sbin
prepend LD_LIBRARY_PATH /usr/local/lib /usr/local/lib32 /usr/local/lib/i386-linux-gnu /usr/local/lib64 /usr/local/lib/x86_64-linux-gnu
prepend MANPATH /usr/local/man /usr/local/share/man

prepend PATH /opt/local/bin /opt/local/sbin
prepend LD_LIBRARY_PATH /opt/local/lib /opt/local/lib32 /opt/local/lib64
prepend MANPATH /opt/local/man /opt/local/share/man

prepend PATH /opt/*/bin
prepend LD_LIBRARY_PATH /opt/*/lib /opt/*/lib32 /opt/*/lib64
prepend MANPATH /opt/*/man /opt/*/share/man

for envtool in pyenv rbenv; do
    prepend PATH "$HOME/.${envtool}/bin"
    if command_exists $envtool; then
        if [ -z "$BASHRC_INITIALIZED" ]; then
            # First initialization.
            eval "$($envtool init -)"
        else
            # Already initialized once.
            prepend PATH "$HOME/.$envtool/shims"
        fi

        if [ "$(type -t $envtool)" = "function" ]; then
            # Command is a function. Pass function down to next shell.
            export -f $envtool

            # Preserve completion, too.
            if [ -n "$(type -t _$envtool)" ]; then
                export -f _$envtool
                complete -F _$envtool $envtool
            fi
        fi
    fi
done

prepend PATH "$HOME/bin"
prepend LD_LIBRARY_PATH "$HOME/lib" "$HOME/lib32" "$HOME/lib64"
prepend MANPATH "$HOME/man" "$HOME/share/man"

prepend PATH "$HOME/usr/bin"
prepend LD_LIBRARY_PATH "$HOME/usr/lib" "$HOME/usr/lib32" "$HOME/usr/lib64"
prepend MANPATH "$HOME/usr/man" "$HOME/usr/share/man"

append PATH "$HOME/sandbox/android/sdk/platform-tools"
append PATH "$HOME/sandbox/android/sdk/tools"

append PATH "$HOME"/.*-dist/bin

dedupe_path PATH LD_LIBRARY_PATH MANPATH

export PATH LD_LIBRARY_PATH MANPATH

# Put snagged files from bin/snag in obvious place: home.
export SNAG_HOME="$HOME"

# Have bin/screen choose opt screen if installed.
ship SCREEN=/opt/screen/bin/screen
ship SCREENRC_DEFAULT="$HOME/.screenrc-default"

ship ANDROID_HOME="$HOME/sandbox/android/sdk"

export GOPATH="$HOME/.go"
append PATH "$GOPATH/bin"

ship PYTHONSTARTUP="$HOME/.pythonrc.py"

ship R_LIBS_USER="$HOME/.r"

export WORKSPACES_RESERVED=5

ship BAK_HOME="/media/$USER/bak"

receive "$HOME/.config/host/${HOSTNAME:-default}/bashrc"

if [ -z "$SSH_AUTH_SOCK" ]; then
    receive "$HOME/.ssh/agent.bash"
fi

# If not running interactively, don't do anything further.
if [ -z "$PS1" ]; then
    return
fi

# Set prompt PS1 to "user@host dir$ "
export PS1='\u@\h \W\$ '

# Load virtualenvwrapper for Python.
#
# This expects virtualenvwrapper.sh to be symlinked from the
# virtualenvwrapper installation. To disable virtualenvwrapper (and
# therefore speedup shell init), simply remove the symlink.
ship WORKON_HOME="$HOME/.virtualenvs"
ship WORKON_HOME="$HOME/.virtualenvs-$HOSTNAME"
receive "$HOME/bin/virtualenvwrapper.sh" >/dev/null 2>&1

if [ "$USE_VIRTUALENV" != "false" ]; then
    if command_exists workon; then
        workon_walk
    fi
    receive "$PWD/.env/bin/activate"
    receive "$PWD/env/bin/activate"
fi

# Enable color support of ls.
if [ "$TERM" != "dumb" ]; then
    if command_exists dircolors; then
        eval "$(dircolors -b)"
        alias ls='ls --color=auto'
    fi
fi

# Check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s checkwinsize

alias pydoc='python $(which pydoc)' # Support virtualenv.
alias emacs='emacs -nw'

# Set window title to "user@host dir" if terminal detected.
PROMPT_COMMAND='history -a; set_title "${USER}@${HOSTNAME} ${PWD/$HOME/\~}"'
case "$TERM" in
xterm*|rxvt*)
    ;;
screen)
    # STY holds name of screen session, in format of:
    # PID.NAME (PID: screen pid, NAME, sessionname)
    #
    # ${STY#*.} removes everything up to and including the first '.'.
    if [ -n "$STY" ]; then
        PROMPT_COMMAND='history -a; set_title "${USER}@${HOSTNAME} ${PWD/$HOME/\~} [${STY#*.}]"'
    else
        PROMPT_COMMAND='history -a; set_title "${USER}@${HOSTNAME} ${PWD/$HOME/\~} [screen]"'
    fi
    ;;
*)
    PROMPT_COMMAND='history -a'
    ;;
esac
export PROMPT_COMMAND

# Don't put duplicate lines in the history. See bash(1) for more options.
export HISTCONTROL=ignoreboth
export HISTFILESIZE=9999
export HISTSIZE=20000

# Set editor and essential program defaults.
export EDITOR='emacs -nw'
export VISUAL=vi
export TEXEDIT=$EDITOR
export PAGER=less
export GZIP="-9"

# Set locale.
export LC_ALL=en_US.UTF-8

# Make less more friendly for non-text input files, see lesspipe(1).
[[ -x /usr/bin/lesspipe ]] && eval "$(lesspipe)"

export FCEDIT=$EDITOR
export FIGNORE='~'
unset MAILCHECK MAILPATH
unset CDPATH

if [ -z "$BASHRC_INITIALIZED" ]; then
    receive /etc/bash_completion
    receive /usr/share/bash-completion/completions/git

    # Alias git completion to homegit script.
    function _homegit() {
        export GIT_DIR=$HOME/.homegit
        _git "$@"
    }
    function _hometig() {
        export GIT_DIR=$HOME/.homegit
        _tig "$@"
    }
    export -f _homegit _hometig
    complete -o default -o nospace -F _homegit homegit >/dev/null 2>&1
    complete -o default -o nospace -F _hometig hometig >/dev/null 2>&1
fi

# Source all .bashrc found in directory ancestry, in order.
# (Keep this at the end of .bashrc to allow overrides.)
walk_root_to_curdir omit_home source_these .bashrc .env #dir

if [ -z "$BASHRC_INITIALIZED" ]; then
    export BASHRC_INITIALIZED=$(date +%s)
fi
