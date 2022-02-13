#put the following line in your /etc/zsh/zshenv and uncomment it
#export ZDOTDIR=$HOME/.config/zsh
SAVEHIST=1000000
HISTSIZE=$SAVEHIST
setopt INC_APPEND_HISTORY	# Don't wait until the shell exits
setopt SHARE_HISTORY		# Share history between all sessions
setopt HIST_IGNORE_SPACE	# Ignore entries starting with Space
setopt HIST_IGNORE_DUPS		# dup
setopt HIST_IGNORE_ALL_DUPS	# li
setopt HIST_SAVE_NO_DUPS	# cates

PS1='%~$ '
TERM='xterm'
#export MANPAGER='nvim -c 'set ft=man' -'
#mkdir -p ~/.local/state/zsh && touch ~/.local/state/zsh/history
export HISTFILE="$HOME/.local/state/zsh/history"
unsetopt beep
bindkey -v
zstyle :compinstall filename '/home/ks/.config/zsh/.zshrc'
autoload -Uz compinit
#mkdir -p ~/.cache/zsh/zcompdump-5.8
compinit -d ~/.cache/zsh/zcompdump-5.8

#aliases
alias v='nvim'
alias s='sudo'
alias m='man'
alias sc='shellcheck'
alias f8='flake8'
alias la='ls -A'
alias ll='ls -lha'
alias br='brightnessctl set 0 && read && brightnessctl set 100%'
alias pm='pulsemixer'
alias vim='nvim'
alias gts='git status'
alias gta='git add'
alias gtc='git commit -m'
alias gtd='git diff'
alias gtp='git push -u origin main'
alias gtl='git log'
alias yta="yt-dlp -f 'bestaudio/best' -f 'm4a'"
alias ytd="yt-dlp -f 'bestvideo[height<=?1080]+bestaudio/best' -f 'mp4'"
alias ytdd="yt-dlp -f 'bestvideo[height<=?720]+bestaudio/best' -f 'mp4'"
alias ytddd="yt-dlp -f 'bestvideo[height<=?480]+bestaudio/best' -f 'mp4'"
alias icat='kitty +kitten icat'
alias smpv='mpv "$(ls | shuf | head -n 1)"'
alias cd1='cd ..'
alias cd2='cd ../..'
alias cd3='cd ../../..'
alias cd4='cd ../../../..'
alias cd5='cd ../../../../..'
alias rm='rm -I --preserve-root' #Every alias has a story ...

wal -Rq
