# A .bashrc for all systems.

. "$HOME"/.bash.d/functions.bash

if [ -z "$PS1" ] && [ -n "$GDMSESSION" ]; then
    # Loaded via X desktop manager. Treat as interactive session.
    export PS1="init> "
fi

export ENVTOOLS="pyenv rbenv" # python, ruby
export PYENV_ROOT="$HOME/.pyenv"

hash -r # Forget all remembered locations of `shopt -s checkhash`.

function rehash() {
    # Re-read bashrc and perform relevant rehash routines.

    . "$HOME"/.bashrc
    for envtool in $ENVTOOLS; do
        command_exists $envtool && $envtool rehash
    done
}

function set_title() {
    # Set window title of xterm-like terminal emulators.

    echo -ne "\033]0;$@\007"
}

function commands() {
    # Display top commands found in shell history.

    history | cut -c8- | sort | uniq -c | sort -rn | head
}

function docker-cleanup() {
    # Clean up exited Docker containers and unused images.

    if ! command_exists docker; then
        echo 'docker: command not found' >&2
        return 2
    fi

    for layer in container image volume; do
        printf "%-10s- " "$layer"
        docker $layer prune --force
    done
}

export -f rehash set_title commands

function call_nvm_use() {
    # Use nvm when .nvmrc is present.
    #
    # Argument $1 is a directory path, as in `walk_root_to_curdir`.

    local dir="${1:-$PWD}"

    if file_exists "$dir"/.nvmrc; then
        # nvm warns that it cannot support the npm prefix config, and
        # recommends using `nvm use --delete-prefix`. However, this
        # --delete-prefix option rewrites the npmrc file. (!)
        #
        # Accordingly, when initializing a bash shell which finds a local
        # .nvmrc, configure npm to have an alternative config file.
        ship NPM_CONFIG_USERCONFIG="$HOME"/.nvm-npmrc

        # Source nvm.sh, and assume that it calls `nvm use` if it finds .nvmrc.
        receive "$HOME"/.nvm/nvm.sh # node
    elif [ "$dir" = "$PWD" ]; then
        # If no .nvmrc found, load `nvm` command without separate config.
        receive "$HOME"/.nvm/nvm.sh # node
    fi
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
        if [ -e "$path" ] && [ ! -d "$path" ]; then
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
    # Useful with `walk_root_to_curdir`.

    for last in "$@"; do true; done
    if [ "$last" != "$HOME" ]; then
        "$@"
    fi
}

export -f source_these walk_root_to_curdir omit_home


# PATH Management
#
# Support multiple hosts with the same .bashrc file, loading each PATH variable
# but only with entries which exist on the filesystem.
#
# Implementation note:
#
# On each shell invocation, each :-delimited path variable is populated with
# filepaths of interest, using `prepend`/`append` functions from
# functions.bash. Invoking `dedupe_paths` (also from functions.bash) later
# ensures that duplicate filepaths drop from each path. This approach ensures a
# clean set of PATH variables with unique filepath entries that exist on the
# filesystem. (Note that directories removed prior to invocation of a child
# interactive shell may remain at low priority in the path.) Path management
# applies on each shell invocation instead of caching results, as to pick up
# filesystem changes.
#
# Existing shells can update paths with the `rehash` function.
#
# Runtime modifications to paths will be overridden when the shell sources this
# .bashrc. Most of the time, this is the desired behavior. When specific
# modifications are needed, create a local .bashrc or .env file. Terminal
# windows in GNU screen or tmux will create shells with working directories
# inside the project, and this ~/.bashrc will find the project-local
# configuration files.
#
# See `source_these` in this ~/.bashrc.

function prepend_paths() {
    # Given root filepath(s) (LIFO), prepend all path variables of interest.

    # Optimization: Build arguments for each path across all filepaths given,
    # then call prepend just once for each path.
    local -a path lib pkg man

    for dir in "$@"; do
        if [ "$dir" = "/" ]; then
            dir=""
        fi

        path+=("$dir"/bin)

        if [ -e "$dir"/bin/reqd ]; then
            # Skip reqd repositories, especially as sbin is unique to setup.
            continue
        fi

        path+=("$dir"/sbin)

        lib+=("$dir"/lib)
        lib+=("$dir"/lib/x86_64-linux-gnu)
        lib+=("$dir"/lib64)
        lib+=("$dir"/lib/i386-linux-gnu)
        lib+=("$dir"/lib32)

        pkg+=("$dir"/lib/pkgconfig)
        pkg+=("$dir"/lib/x86_64-linux-gnu/pkgconfig)
        pkg+=("$dir"/lib64/pkgconfig)
        pkg+=("$dir"/lib/i386-linux-gnu/pkgconfig)
        pkg+=("$dir"/lib32/pkgconfig)

        man+=("$dir"/man)
        man+=("$dir"/share/man)
    done

    prepend PATH "${path[@]}"
    prepend LD_LIBRARY_PATH "${lib[@]}"
    prepend PKG_CONFIG_PATH "${pkg[@]}"
    prepend MANPATH "${man[@]}"
}

prepend_paths /opt/local /opt/* /usr/local /usr /

for envtool in $ENVTOOLS; do
    prepend_paths "$HOME/.${envtool}"
    command_exists $envtool && prepend PATH "$HOME/.$envtool/shims"
done

command_exists opam && file_exists "$HOME"/.opam && eval "$(opam env)" # ocaml

# Load programming environments which only require setting PATH.
#
# * cask for emacs
# * cargo for rust
#
prepend PATH "$HOME"/.cask/bin "$HOME"/.cargo/bin

# On `npm install -g` permissions errors, ensure ~/.npmrc is configured:
#
#     npm config set prefix '~/.npm-global'
#
prepend_paths "$HOME"/.npm-global

walk_root_to_curdir call_nvm_use

append PATH "$HOME"/src/android/sdk/platform-tools
append PATH "$HOME"/src/android/sdk/tools

append PATH "$HOME"/.*-dist/bin

prepend LD_LIBRARY_PATH /usr/lib/libreoffice/program

prepend_paths \
    "$HOME" \
    "$HOME"/.box/opt/* "$HOME"/.box/usr "$HOME"/.box \
    "$HOME"/.opt/* \
    "$HOME"/.local

# Add Makefile.d paths here to maintain priority within .bashrc PATH settings.
prepend PATH \
        "$PWD"/.reqd/opt/*/bin \
        "$PWD"/.reqd/usr/bin \
        "$PWD"/.reqd/bin \
        "$PWD"/env/bin \
        "$PWD"/.env/bin \
        "$PWD"/.venv/bin \
        "$PWD"/node_modules/.bin

ship GEM_HOME="$PWD"/.reqd/opt/ruby

dedupe_path PATH LD_LIBRARY_PATH PKG_CONFIG_PATH MANPATH

export PATH LD_LIBRARY_PATH PKG_CONFIG_PATH MANPATH

# Disable symlink creation in Vagrant share folders on VirtualBox.
export VAGRANT_DISABLE_VBOXSYMLINKCREATE=1

# Put snagged files from ~/bin/snag in an obvious place: $HOME.
export SNAG_HOME="$HOME"

ship SCREENRC_DEFAULT="$HOME"/.screenrc-default

ship ANDROID_HOME="$HOME"/src/android/sdk

export GOPATH="$HOME"/.go
append PATH "$GOPATH/bin"

ship PYTHONSTARTUP="$HOME"/.pythonrc.py

ship R_LIBS_USER="$HOME"/.r

ship DEVKITPRO=/opt/devkitpro
ship DEVKITPPC=$DEVKITPRO/devkitPPC
ship DEVKITARM=$DEVKITPRO/devkitARM

export WORKSPACES_RESERVED=5

# Set BAK_HOME for ~/bin/bak, using NFS then volume labeled "bak" if mounted.
export BAK_HOME="$HOME"/.bak
ship BAK_HOME="$HOME"/doc/bak # Symlink to NFS location.
ship BAK_HOME="/media/$USER/bak"

# Set locale.
export LC_ALL=en_US.UTF-8

# Ignore commands that start with spaces: ignorespace.
# Ignore consecutive duplicate commands: ignoredups.
# Ignore both: ignoreboth.
export HISTCONTROL=ignoreboth
export HISTFILESIZE=9999
export HISTSIZE=20000

# Set editor and essential program defaults.
export EDITOR='emacs -nw'
export VISUAL=vi
export TEXEDIT=$EDITOR
export PAGER=less

# Make `less` more friendly for non-text input files; see lesspipe(1).
[[ -x /usr/bin/lesspipe ]] && eval "$(lesspipe)"

# Stylize `less`.
# In effect: add bold white to `man`, replace $PAGER inverse with bold blue.
#
#         Foreground  Background
#
# black           30          40
# red             31          41
# green           32          42
# yellow          33          43
# blue            34          44
# magenta         35          45
# cyan            36          46
# white           37          47
#
# Each double-digit number is ANSI color code, outlined above. 1 is bold.
export LESS_TERMCAP_mb=$(printf "\e[1;37m")     # mode blink start
export LESS_TERMCAP_md=$(printf "\e[1;37m")     # mode bold start
export LESS_TERMCAP_me=$(printf "\e[0m")        # mode end
export LESS_TERMCAP_se=$(printf "\e[0m")        # mode standout end
export LESS_TERMCAP_so=$(printf "\e[1;34m")     # mode standout start
export LESS_TERMCAP_ue=$(printf "\e[0m")        # mode underline end
export LESS_TERMCAP_us=$(printf "\e[1;37m")     # mode underline start

alias pydoc=pydoc3
alias emacs='emacs -nw'

receive "$HOME"/.config/host/${HOSTNAME:-default}/bashrc

if [ -z "$SSH_AUTH_SOCK" ]; then
    receive "$HOME"/.ssh/agent.bash
fi

# If not running interactively, don't do anything further.
if [ -z "$PS1" ]; then
    return
fi

# Set prompt PS1 to "user@host dir$ "
export PS1='\u@\h \W\$ '

function _default_completion_loader() {
    # Find the default completion loader, call it.

    # Get the default completion spec; there should be only one line.
    # Example output:
    #
    #     complete -o bashdefault -o default -F _completion_loader -D
    #
    local line=$(complete -p -D 2>/dev/null)

    # Parse name of completion function.
    line=${line##*-F} # Remove everything up to and including -F.
    line=$(echo $line | cut -f1 -d' ') # Remove first space & everything after.

    local fn=$line

    if [ -n "$fn" ]; then
        $fn "$@"
    fi
}

declare -A _xspecs # Prevent completion _xspecs[.] lookup being a syntax error.

function _completion_loader() {
    # Default bash completion handler to load specifications lazily.

    if [ -z "$BASH_COMPLETION_LOADED" ]; then
        receive /etc/bash_completion

        # The default completion loader may have a new definition.
        # Call it again to ensure completion is fully initialized.
        #
        # Otherwise, completing some commands could result in another 124,
        # i.e. the `return 124` from the newly defined completion loader,
        # after bash has restarted the completion process. This would result in
        # completion failing on certain commands on the first tab in the shell
        # session.
        #
        # In other words, flush out another default 124 return value so that
        # everything is ready when bash restarts the completion process.
        _default_completion_loader "$@"

        for envtool in $ENVTOOLS; do
            command_exists $envtool && eval "$($envtool init -)"
        done

        receive "$HOME"/.nvm/bash_completion

        receive /usr/share/bash-completion/completions/git

        function _homegit() {
            # Use git completion for `homegit` with .homegit GIT_DIR.
            GIT_DIR="$HOME"/.homegit _git "$@"
        }
        function _hometig() {
            # Use tig completion for `hometig` with .homegit GIT_DIR.
            GIT_DIR="$HOME"/.homegit _tig "$@"
        }
        export -f _homegit _hometig
        complete -o default -o nospace -F _homegit homegit >/dev/null 2>&1
        complete -o default -o nospace -F _hometig hometig >/dev/null 2>&1

        export BASH_COMPLETION_LOADED=$(date +%s)

        # Restart completion process.
        return 124
    fi
}

complete -D -F _completion_loader -o bashdefault -o default >/dev/null 2>&1

# Force reload of _completion_loader.
unset BASH_COMPLETION_LOADED

# Configure shell if running inside Jupyter notebook.
if [ -n "$JPY_PARENT_PID" ]; then
    unset HISTFILE
    _completion_loader
fi

# Enable color support of ls.
if [ "$TERM" != "dumb" ]; then
    if command_exists dircolors; then
        eval "$(dircolors -b)"
        alias ls='ls --color=auto'
    fi
fi

# Check that a command found in the hash table exists before trying to execute
# it. If a hashed command no longer exists, a normal path search is performed.
shopt -s checkhash

# Check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s checkwinsize

# Save all lines of a multiple-line command in the same history entry.
# This allows easy re-editing of multi-line commands.
shopt -s cmdhist

# Set window title to "user@host dir" if terminal detected.
PROMPT_COMMAND='history -a; set_title "${USER}@${HOSTNAME} ${PWD/$HOME/\~}"'
case "$TERM" in
xterm*|rxvt*)
    ;;
screen*)
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

export FCEDIT=$EDITOR
export FIGNORE='~'
unset MAILCHECK MAILPATH
unset CDPATH

# Source all .bashrc found in directory ancestry, in order.
# (Keep this at the end of .bashrc to allow overrides.)
walk_root_to_curdir omit_home source_these .bashrc .env #dir
