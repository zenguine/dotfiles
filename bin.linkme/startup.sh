#! /bin/sh

xrdb -merge .Xresources &

xsetroot -solid black &
eval $(cat ~/.fehbg) &

/usr/lib/polkit-gnome/polkit-gnome-authentication-agent-1 &

trayer --edge top --align right --SetDockType true --SetPartialStrut true --expand true --width 10 --height 15 --transparent true --tint 0x000000 &

# gajim &
xscreensaver -nosplash &

# gnome-settings-daemon &

# if [ -x /usr/bin/gnome-power-manager ] ; then
#    sleep 1
#    gnome-power-manager &
# fi

if [ -x /usr/bin/nm-applet ] ; then
   nm-applet --sm-disable &
fi

dropbox start &
gtk-redshift -l 42.21:-71.5 -t 5700:3100 &
# firefox &
# clementine &
deluge &
tmux-init & # Set up initial tmux environment

# export WINDOW_MANAGER=xmonad
# gnome-session &
