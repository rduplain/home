# A .bashrc for all systems.

. "$HOME"/.bash.d/functions.bash

if [ -z "$PS1" ] && [ -n "$GDMSESSION" ]; then
    # Loaded via X desktop manager. Treat as interactive session.
    export PS1="init> "
fi

export ENVTOOLS="pyenv rbenv" # python, ruby
export PYENV_ROOT="$HOME/.pyenv"

export BASHRC_CACHE="$HOME"/.bash_cache

hash -r # Forget all remembered locations of `shopt -s checkhash`.

function rehash() {
    # Re-read bashrc and perform relevant rehash routines.
    #
    # Limitation: loaded completion functions are not rehashed.

    rm -f "$BASHRC_CACHE"

    for envtool in $ENVTOOLS; do
        command_exists $envtool && $envtool rehash
    done

    . "$HOME"/.bashrc
}

function set_title() {
    # Set window title of xterm-like terminal emulators.

    # xterm accepts ESC]0;<title>BEL to set icon name and window title.
    # \033 and \007 are ASCII escape and bell characters, respectively.
    #
    # These escape codes are called "OSC" for "Operating System Command".
    # See `man 4 console_codes`.
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

    for layer in container volume image system; do
        printf "%-10s- " "$layer"
        docker $layer prune --force
    done
}

export -f rehash set_title commands

function nvm_use() {
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
# interactive shell may remain at low priority in the path.)
#
# Path management applies on each shell invocation instead of caching results,
# as to pick up filesystem changes. Existing shells can update paths with the
# `rehash` function.
#
# Runtime modifications to paths will be overridden when the shell sources this
# .bashrc. Most of the time, this is the desired behavior. When specific
# modifications are needed, create a local .bashrc or .env file in the project
# or parent directory. Terminal windows in GNU screen or tmux will create
# shells with working directories inside the project, and this ~/.bashrc will
# find the project-local configuration files.
#
# See `source_these` in this ~/.bashrc.

function prepend_paths() {
    # Given root filepath(s), LIFO, prepend all path variables of interest.

    # Optimization: Build arguments for each path across all filepaths given,
    # then call prepend just once for each path.
    local -a path lib pkg man

    for dir in "$@"; do
        if [ "$dir" = "/" ]; then
            dir=""
        fi

        path+=("$dir"/bin)

        if [ -e "$dir"/bin/reqd ] || [ -e "$dir"/sbin/reqd-rev ]; then
            # Skip reqd repositories, especially as sbin is unique to setup.
            continue
        fi

        path+=("$dir"/sbin)

        man+=("$dir"/man)
        man+=("$dir"/share/man)

        if [ -e "$dir"/.skip-lib ]; then
            # Skip lib installations marked with this special .skip-lib file.
            #
            # This is especially useful for installations with lib directories
            # entirely for the benefit of respective bin executables.
            continue
        fi

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
    done

    prepend PATH "${path[@]}"
    prepend LD_LIBRARY_PATH "${lib[@]}"
    prepend PKG_CONFIG_PATH "${pkg[@]}"
    prepend MANPATH "${man[@]}"
}

if [ -e "$BASHRC_CACHE" ]; then
    . "$BASHRC_CACHE"
else
    prepend_paths /opt/local /opt/* /usr/local /usr /

    prepend_paths \
        "$HOME" \
        "$HOME"/.box/opt/* "$HOME"/.box/usr "$HOME"/.box \
        "$HOME"/.opt/* \
        "$HOME"/.local

    for envtool in $ENVTOOLS; do
        prepend_paths "$HOME/.${envtool}"
        when_command $envtool prepend PATH "$HOME/.$envtool/shims"
    done

    command_exists opam && file_exists ~/.opam && eval "$(opam env)" # ocaml

    # Load programming environments which only require setting PATH.
    prepend PATH "$HOME"/.cask/bin "$HOME"/.cargo/bin # emacs, rust

    # On `npm install -g` permissions errors, ensure ~/.npmrc is configured:
    #
    #     npm config set prefix '~/.npm-global'
    #
    prepend_paths "$HOME"/.npm-global

    cat > "$BASHRC_CACHE" <<EOF
export PATH="$PATH"
export LD_LIBRARY_PATH="$LD_LIBRARY_PATH"
export PKG_CONFIG_PATH="$PKG_CONFIG_PATH"
export MANPATH="$MANPATH"
EOF
fi

walk_root_to_curdir nvm_use

# Add Makefile.d paths here to maintain priority within .bashrc PATH settings.
prepend PATH \
        "$PWD"/bin \
        "$PWD"/.reqd/opt/*/bin \
        "$PWD"/.reqd/usr/bin \
        "$PWD"/.reqd/bin \
        "$PWD"/env/bin \
        "$PWD"/.env/bin \
        "$PWD"/venv/bin \
        "$PWD"/.venv/bin \
        "$PWD"/node_modules/.bin

ship GEM_HOME="$PWD"/.reqd/opt/ruby

dedupe_path PATH LD_LIBRARY_PATH PKG_CONFIG_PATH MANPATH

export PATH LD_LIBRARY_PATH PKG_CONFIG_PATH MANPATH

# Disable symlink creation in Vagrant share folders on VirtualBox.
export VAGRANT_DISABLE_VBOXSYMLINKCREATE=1

# Put snagged files from ~/bin/snag in a place where they'll be obvious: $HOME.
export SNAG_HOME="$HOME"

ship SCREENRC_DEFAULT="$HOME"/.screenrc-default

export GOPATH="$HOME"/.go
append PATH "$GOPATH/bin"

ship PYTHONSTARTUP="$HOME"/.pythonrc.py

ship R_LIBS_USER="$HOME"/.r

export PULUMI_SKIP_UPDATE_CHECK=true

append PATH "$HOME"/src/kotlin-language-server/server/build/install/server/bin

export WORKSPACES_RESERVED=5

# Set BAK_HOME for ~/bin/bak, using NFS then volume labeled "bak" if mounted.
export BAK_HOME="$HOME"/.bak     # Default location.
ship BAK_HOME="$HOME"/doc/bak    # Symlink to NFS location (if exists).
ship BAK_HOME="/media/$USER/bak" # External drive with `bak` label (if exists).

# Set locale.
export LC_ALL=en_US.UTF-8

# Ignore commands that start with spaces: ignorespace.
# Ignore consecutive duplicate commands: ignoredups.
# Ignore both: ignoreboth.
export HISTCONTROL=ignoreboth
export HISTFILESIZE=1000000
export HISTSIZE=$HISTFILESIZE

# Set editor and essential program defaults.
export EDITOR=emacs
export VISUAL=vi
export TEXEDIT=$EDITOR
export PAGER=less

export BRZ_LOG=/dev/null  # Disable ~/.brz.log file.
export BZR_LOG=/dev/null  # Disable ~/.bzr.log file.

# Make `less` more friendly for non-text input files; see lesspipe(1).
[[ -x /usr/bin/lesspipe ]] && eval "$(lesspipe)"

# Stylize `less`.
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
#
# In effect: add bold white to `man`, replace $PAGER inverse with bold blue.
#
export LESS_TERMCAP_mb=$(printf "\e[1;37m")     # mode blink start
export LESS_TERMCAP_md=$(printf "\e[1;37m")     # mode bold start
export LESS_TERMCAP_me=$(printf "\e[0m")        # mode end
export LESS_TERMCAP_se=$(printf "\e[0m")        # mode standout end
export LESS_TERMCAP_so=$(printf "\e[1;34m")     # mode standout start
export LESS_TERMCAP_ue=$(printf "\e[0m")        # mode underline end
export LESS_TERMCAP_us=$(printf "\e[1;37m")     # mode underline start

# Configure to show color without line wrapping (because could break color).
LESS="--quit-if-one-screen --raw-control-chars --chop-long-lines --tabs=4"
export LESS

# Set `grep --color` output to blue.
export GREP_COLOR="01;34"

alias pydoc='python3 -m pydoc'
alias emacs='emacs -nw'

function detect() {
    # Report success if any given file detected on filesystem.

    for file in "$@"; do
        # Use `ls` in case of globs.
        if ls $file >/dev/null 2>&1; then
            return 0
        fi
    done

    return 1
}

if detect *.py Pipfile pyproject.toml requirements.txt; then
    CODE=python
elif detect *.rb *.gemspec Gemfile; then
    CODE=ruby
fi

# Set commands based on programming language when shell starts in project root.
case "$CODE" in
    python)
        alias doc=pydoc
        ;;
    ruby)
        alias doc=ri
        ;;
    *)
        alias doc='echo "doc: unknown environment. (see ~/.bashrc)"'
        ;;
esac

if [ -z "$SSH_AUTH_SOCK" ]; then
    ship SSH_AUTH_SOCK="/run/user/$(id -u)/keyring/ssh"
fi

receive "$HOME"/.config/host/${HOSTNAME:-default}/bashrc

if [ -e "$HOME"/.ssh/agent.bash ]; then
    PREVIOUS_SSH_AUTH_SOCK="$SSH_AUTH_SOCK"
    CONFIG_SSH_AUTH_SOCK=$(grep SSH_AUTH_SOCK "$HOME"/.ssh/agent.bash |
                               awk -F= '{ print $2 }')
    if [ -S "$CONFIG_SSH_AUTH_SOCK" ]; then
        . "$HOME"/.ssh/agent.bash
    fi
fi

# If not running interactively, don't do anything further.
if [ -z "$PS1" ]; then
    return
fi

# Set prompt PS1 to `user@host dir$ `.
if [ -r /etc/hostname ]; then
    # Use documented hostname, especially in case of chroot.
    export PS1="\u@$(< /etc/hostname) \W\$ "
else
    export PS1='\u@\h \W\$ '
fi

function _copy_function() {
    # Copy function definition to a new name.
    #
    # This is useful for injecting functionality into completion functions.

    if [ $# -ne 2 ]; then
        echo "usage: _copy_function ORIGINAL_NAME NEW_NAME" >&2
        return 2
    fi

    if [ -z "$1" ]; then
        echo "_copy_function: error: no shell function: $1" >&2
        return 1
    fi

    eval "$(echo "${2}()"; declare -f "$1" | tail -n+2)"
}

function _default_completion_loader() {
    # Find the default completion loader, call it.

    # Get the default completion spec; there should be only one line.
    # Example output:
    #
    #     complete -o bashdefault -o default -F _completion_loader -D
    #
    local line=$(complete -p -D 2>/dev/null)

    # Parse name of completion function.
    line=${line##*-F}                  # Remove everything up-to/including -F.
    line=$(echo $line | cut -f1 -d' ') # Remove first space & everything after.

    local fn=$line

    if [ -n "$fn" ] && [ "$fn" != "___completion_boot" ]; then
        $fn "$@"
    fi
}

# Prevent completion _xspecs[.] lookup being a syntax error in bash-completion.
declare -A _xspecs >/dev/null 2>&1 || true

function ___completion_custom() {
    for envtool in $ENVTOOLS; do
        receive "$HOME/.${envtool}/completions/${envtool}.bash"
    done

    receive "$HOME"/.nvm/bash_completion

    for file in "$HOME"/.box/opt/conda/etc/bash_completion.d/*; do
        receive "$file"
    done

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

    function _git_auto_clone() {
        COMPREPLY=(
            $(compgen -W "$(git-auto-clone --available 2>/dev/null)" \
                      -- "${COMP_WORDS[COMP_CWORD]}")
        )
    }

    function _git_back_to() {
        _git_checkout
    }

    function _git_diff_base() {
        _git_checkout
    }

    function _git_full_rebase() {
        # Add `git full-rebase` command-line flags to tab-completion.
        local flags word
        flags=(--all --max= --non-interactive)
        word="${COMP_WORDS[COMP_CWORD]}"
        COMPREPLY+=($(compgen -W "$(echo "${flags[@]}")" -- "$word"))
    }

    receive /usr/share/bash-completion/completions/screen

    # Inject completions from `screen --sessions` if available.
    if silently screen --sessions-available &&
       command_exists _screen_sessions &&
       ! command_exists _screen_sessions_orig;
    then
        _copy_function _screen_sessions _screen_sessions_orig
        function _screen_sessions() {
            _screen_sessions_orig

            local word
            word="${COMP_WORDS[COMP_CWORD]}"
            COMPREPLY+=($(compgen -W "$(screen --sessions)" -- "$word"))
        }
    fi
}

function ___completion_boot() {
    # Default bash completion handler to load specifications lazily.

    if [ -z "$BASH_COMPLETION_LOADED" ]; then
        receive /etc/bash_completion
        receive /usr/local/share/bash-completion/bash_completion

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

        ___completion_custom

        export BASH_COMPLETION_LOADED=$(date +%s)

        # Restart completion process.
        return 124
    fi
}

if command_exists brew && [ "$BASH_VERSINFO" -eq 3 ]; then
    # Source completion scripts from brew-installed completions on macOS bash.
    receive $(brew --prefix)/etc/bash_completion
    receive $(brew --prefix)/Homebrew/completions/bash/brew
    ___completion_custom
else
    # Install default bootstrap function for default completion loader.
    complete -D -F ___completion_boot -o bashdefault -o default >/dev/null 2>&1
fi

# Force reload of ___completion_boot.
#
# Note: This does not affect loaded completions, only the default completer.
unset BASH_COMPLETION_LOADED

# Configure shell if running inside Jupyter notebook.
if [ -n "$JPY_PARENT_PID" ]; then
    unset HISTFILE
    ___completion_boot  # Support in-notebook completion.
fi

if [ -e "$HOME"/.android/sdk ]; then
    export ANDROID_SDK_ROOT="$HOME"/.android/sdk
    alias adb="$ANDROID_SDK_ROOT"/platform-tools/adb
    alias fastboot="$ANDROID_SDK_ROOT"/platform-tools/fastboot
fi

# Support commands that expect system interpreter with dpkg-installed packages.
USE_SYSTEM_PATH=(
    dropbox
    gnucash
    steam
)

for cmd in "${USE_SYSTEM_PATH[@]}"; do
    if command_exists $cmd; then
        eval "alias $cmd='PATH=/usr/local/bin:/usr/bin:/bin $(which $cmd)'"
    fi
done

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
TILDE='~'
PROMPT_COMMAND='history -a; set_title "${USER}@${HOSTNAME} ${PWD/$HOME/$TILDE}"'
case "$TERM" in
xterm*|rxvt*)
    ;;
screen*)
    # STY holds name of screen session, in format of:
    # PID.NAME (PID: screen pid, NAME, sessionname)
    #
    # ${STY#*.} removes everything up to and including the first '.'.
    if [ -n "$STY" ]; then
        PROMPT_COMMAND='history -a; set_title "${USER}@${HOSTNAME} ${PWD/$HOME/$TILDE} [${STY#*.}]"'
    else
        PROMPT_COMMAND='history -a; set_title "${USER}@${HOSTNAME} ${PWD/$HOME/$TILDE} [screen]"'
    fi
    ;;
*)
    PROMPT_COMMAND='history -a'
    ;;
esac
export PROMPT_COMMAND

export DEBIAN_FRONTEND=noninteractive
export FCEDIT=$EDITOR
export FIGNORE='~'
unset MAILCHECK MAILPATH
unset CDPATH

Q=https://raw.githubusercontent.com/rduplain/qwerty.sh/v0.8/qwerty.sh
alias qwerty.sh="curl --proto '=https' --tlsv1.2 -sSf $Q | Q=$Q sh -s -"

receive "$HOME"/.bashrc-local
receive "$HOME"/.env

# Source all .bashrc and .env found in directory ancestry, in order.
# (Keep this at the end of .bashrc to allow overrides.)
walk_root_to_curdir omit_home source_these \
    .env .env-local \
    .bashrc .bashrc-local
