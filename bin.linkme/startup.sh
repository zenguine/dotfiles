xmodmap -e 'remove Lock = Caps_Lock' &
xmodmap -e 'keysym Caps_Lock = Control_L' &
xmodmap -e 'add Control = Control_L' &
gnome-power-manager &
trayer &
gnome-volume-control-applet &
gvim &
firefox &
clementine &
deluge &

# export WINDOW_MANAGER=xmonad
# gnome-session &
