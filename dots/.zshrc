#history
HISTFILE=~/.histfile
SAVEHIST=10000
HISTSIZE=$SAVEHIST
setopt INC_APPEND_HISTORY	# Don't wait until the shell exits
setopt SHARE_HISTORY		# Share history between all sessions
setopt HIST_IGNORE_SPACE	# Ignore entries starting with Space
setopt HIST_IGNORE_DUPS		# dup
setopt HIST_IGNORE_ALL_DUPS	# li
setopt HIST_SAVE_NO_DUPS	# cates

PS1="%~$ "
TERM="xterm"
export MANPAGER="nvim -c 'set ft=man' -"

#stuff from autoconfig
unsetopt beep
bindkey -v
zstyle :compinstall filename '/home/ks/.zshrc'
autoload -Uz compinit
compinit

#aliases
alias la='ls -A'
alias ll='ls -lha'
alias br='brightnessctl set 0 && read && brightnessctl set 100%'
alias gts='git status'
alias gta='git add'
alias gtc='git commit -m'
alias gtd='git diff'
alias gtp='git push -u origin main'
alias gtl='git log'
alias yta='youtube-dl -f bestaudio/m4a'
alias ytd="youtube-dl -f 'bestvideo[height<=?1080]+bestaudio/best'"
alias icat="kitty +kitten icat"
alias bye="xscreensaver-command -lock"
alias cd1="cd .."
alias cd2="cd ../.."
alias cd3="cd ../../.."
alias cd4="cd ../../../.."
alias cd5="cd ../../../../.."
