c() { cd ~/code/$1; }
_c() { _files -W ~/code -/; }
compdef _c c

cdh() { cd ~/$1; }
_cdh() { _files -W ~ -/; }
compdef _cdh cdh

fancy-ctrl-z () {
  if [[ $#BUFFER -eq 0 ]]; then
    fg
    zle redisplay
  else
    zle push-input
  fi
}
zle -N fancy-ctrl-z
bindkey '^Z' fancy-ctrl-z


