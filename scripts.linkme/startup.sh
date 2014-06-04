#! /bin/sh

xrdb -merge .Xresources &

# Start up gpg-agent
# . ~/scripts/gpg-agent.sh

# dropbox start &
redshift &
deluge &
t-init & # Set up initial tmux environment
echo 'emacs --daemon & exit' | zsh &
echo 'xmodmap ~/.xmodmaprc > /home/jcullen/xtempoutput.txt & exit' | zsh &
