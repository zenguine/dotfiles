xrandr --newmode "1920x1200_60.00" 193.25 1920 2056 2256 2592 1200 1203 1209 1245 -hsync +vsync
xrandr --addmode VGA1 1920x1200_60.00
xrandr --output LVDS1 --auto --output VGA1 --mode 1920x1200_60.00 --right-of LVDS1
