#!/bin/bash

# xmonad startup script should only run items once

function CommandExists() {
    command -v "$1" >/dev/null 2>&1
}

function CommandRunning() {
    pgrep "$1" >/dev/null 2>&1
}

function ExistsAndNotRunning() {
    CommandExists "$1" && ! CommandRunning "$1"
}

# System tray
pkill trayer
pkill xmobar
if ExistsAndNotRunning trayer ]; then
    trayer --edge top \
        --align right \
        --SetDockType true \
        --SetPartialStrut true \
        --expand true \
        --width 99% \
        --margin 10 \
        --transparent true \
        --tint 0x5f5f5f \
        --height 26 \
        --monitor 0 &
fi

# Power manager
if ExistsAndNotRunning xfce4-power-manager; then
    xfce4-power-manager &
fi

# Set the default X cursor to the usual pointer
if CommandExists xsetroot; then
    xsetroot -cursor_name left_ptr &
fi

# Taffybar
# if ExistsAndNotRunning taffybar ; then
#     taffybar &
# fi

# Redshift
if ExistsAndNotRunning redshift; then
    redshift &
fi

# Autolock
# if ExistsAndNotRunning xautolock ; then
# xautolock -time 1 -locker "if ! grep 'RUNNING' /proc/asound/card*/pcm*/sub*/status;then xscreensaver-command -lock; else echo 'Sound on'; fi"
# fi

# Wallpaper
if CommandExists feh; then
    feh --no-fehbg --bg-scale ~/Pictures/bkg3_bkg5.png
elif ExistsAndNotRunning nitrogen; then
    nitrogen --restore &
fi

# Screensaver
if ExistsAndNotRunning xscreensaver; then
    xscreensaver -no-splash &
fi

# compton
if ExistsAndNotRunning compton; then
    compton -b &
fi

# Network Applet
if ExistsAndNotRunning nm-applet; then
    nm-applet &
fi

# Bluetooth Applet
if ExistsAndNotRunning blueman-applet; then
    blueman-applet &
fi

# Google Drive
if ExistsAndNotRunning insync; then
    insync start &
fi

# xbindkeys
if CommandExists xbindkeys; then
    xbindkeys &
fi

# parcellite GTK+ clipboard manager
if ExistsAndNotRunning parcellite; then
    parcellite -d &
fi

# polkit gnome authenticator for the GUI pop-up to ask for root password
# i.e. when using Add/Remove Software application
if ! CommandRunning polkit-gnome; then
    if CommandExists /usr/lib/polkit-gnome/polkit-gnome-authentication-agent-1; then
        /usr/lib/polkit-gnome/polkit-gnome-authentication-agent-1 &
    fi
fi

exit 0
