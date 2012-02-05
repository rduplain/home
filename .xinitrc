#!/bin/sh

userresources=$HOME/.Xresources
usermodmap=$HOME/.Xmodmap
sysresources=/etc/X11/xinit/.Xresources
sysmodmap=/etc/X11/xinit/.Xmodmap

if [ -f $sysresources ]; then
    xrdb -merge $sysresources
fi

if [ -f $sysmodmap ]; then
    xmodmap $sysmodmap
fi

if [ -f "$userresources" ]; then
    xrdb -merge "$userresources"
fi

if [ -f "$usermodmap" ]; then
    xmodmap "$usermodmap"
fi

# source $HOME/.ssh-agent.bash
# ssh-add -l # exit code 1 on no identities, exit code 2 on could not connect
# if [ $? -eq 2 ]; then
#    ssh-agent > $HOME/.ssh-agent.bash
#    source $HOME/.ssh-agent.bash
# fi
# export SSH_AUTH_SOCK SSH_AGENT_PID

exec ssh-agent startfluxbox
